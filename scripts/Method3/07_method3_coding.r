############################################################
# URL coding using the first 100 words of the article
############################################################

library(httr)
library(jsonlite)
library(dplyr)
library(readxl)
library(openxlsx)
library(rvest)
library(xml2)

############################################################
# Configuration
############################################################
Sys.setenv(OPENAI_API_KEY = "")
OPENAI_API_KEY <- Sys.getenv("OPENAI_API_KEY")

INPUT_FILE <- "/Users/alice.dorio/Desktop/data/data/test_APICHATGPT_1000.xlsx"

OUT_DIR <- "/Users/alice.dorio/Desktop/data/codifica_100parole_contenuto"
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

N_RIGHE <- NULL

############################################################
# Text truncation (first 100 words)
############################################################

estrai_prime_100_parole <- function(testo) {
  testo <- gsub("\\s+", " ", testo)
  parole <- strsplit(testo, " ", fixed = FALSE)[[1]]
  if (length(parole) <= 100) {
    return(paste(parole, collapse = " "))
  } else {
    return(paste(parole[1:100], collapse = " "))
  }
}

############################################################
# Web content download
############################################################

scarica_contenuto_url <- function(url) {
  if (is.na(url) || trimws(as.character(url)) == "")
    return(list(successo = FALSE, testo = "", errore = "url_missing"))
  
  cat("Scarico:", url, "\n")
  tryCatch({
    response <- GET(
      url,
      user_agent("Mozilla/5.0 (compatible; Research-Bot)"),
      timeout(15)
    )
    sc <- status_code(response)
    if (sc != 200)
      return(list(successo = FALSE, testo = "", errore = paste0("HTTP ", sc)))
    
    html_content <- content(response, "text", encoding = "UTF-8")
    doc <- read_html(html_content)
    
    xml_find_all(doc, ".//script|.//style|.//noscript") %>% xml_remove()
    
    titolo <- doc %>% html_element("title") %>% html_text2()
    if (is.na(titolo)) titolo <- ""
    
    meta_desc <- doc %>%
      html_elements("meta[name='description'], meta[property='og:description']") %>%
      html_attr("content") %>%
      paste(collapse = " ")
    if (length(meta_desc) == 0) meta_desc <- ""
    
    corpo_selettori <- c("article", ".article", "#article", ".content", ".post", "main", ".entry-content")
    corpo <- ""
    for (selettore in corpo_selettori) {
      elementi <- doc %>% html_elements(selettore)
      if (length(elementi) > 0) {
        corpo <- elementi %>% html_text2() %>% paste(collapse = " ")
        break
      }
    }
    if (nchar(corpo) < 100) {
      corpo <- doc %>% html_elements("p") %>% html_text2() %>% paste(collapse = " ")
    }
    
    testo_completo <- paste(titolo, meta_desc, corpo, sep = "\n\n")
    testo_pulito <- gsub("\\s+", " ", testo_completo) %>% trimws()
    
    testo_100 <- estrai_prime_100_parole(testo_pulito)
    
    return(list(successo = TRUE, testo = testo_100, errore = ""))
    
  }, error = function(e) {
    return(list(successo = FALSE, testo = "", errore = as.character(e$message)))
  })
}

############################################################
# GPT call (first 100 words)
############################################################

call_gpt_con_100parole <- function(testo_articolo) {
  if (is.na(OPENAI_API_KEY) || OPENAI_API_KEY == "") {
    stop("OPENAI_API_KEY mancante")
  }
  prompt <- paste0(
    "Sei un esperto codificatore di contenuti giornalistici. ",
    "Analizza SOLO le prime ~100 parole dell'articolo fornito e codifica secondo il codebook.\n\n",
    
    "RESTITUISCI SOLO UN JSON VALIDO con questi campi esatti:\n",
    "{\n",
    '  "societal_actors": 0/1,\n',
    '  "decision_authorities": 0/1,\n',
    '  "policy_plan": 0/1,\n',
    '  "actors_concerned": 0/1,\n',
    '  "individual_societal_R": 0/1/2,\n',
    '  "episodic_thematic_F": 0/1/2,\n',
    '  "personal_impersonal_R": 0/1/2,\n',
    '  "emotional_unemotional_R": 0/1/2\n',
    "}\n\n",
    
    "CODEBOOK per la codifica delle notizie:\n\n",
    
    "DIMENSIONE TOPIC - Rilevanza Politica (4 aspetti binari 0/1):\n",
    "1. societal_actors: Due o più attori sociali in disaccordo su una questione sociale\n",
    "   0 = non presente; 1 = presente\n",
    "2. decision_authorities: Autorità decisionali coinvolte (legislativo, esecutivo, giudiziario)\n",
    "   0 = non presente; 1 = presente\n",
    "3. policy_plan: Sostanza di una decisione/misura/programma pianificato o realizzato\n",
    "   0 = non presente; 1 = presente\n",
    "4. actors_concerned: Persone/gruppi interessati o coinvolti\n",
    "   0 = non presente; 1 = presente\n\n",
    
    "DIMENSIONI FOCUS (scale 0/1/2):\n",
    "5. individual_societal_R: Rilevanza individuale vs sociale\n",
    "   0 = individuale; 1 = misto; 2 = sociale\n",
    "6. episodic_thematic_F: Framing episodico vs tematico\n",
    "   0 = episodico; 1 = misto; 2 = tematico\n\n",
    
    "DIMENSIONI STILE (scale 0/1/2):\n",
    "7. personal_impersonal_R: Stile personale vs impersonale\n",
    "   0 = personale; 1 = misto; 2 = impersonale\n",
    "8. emotional_unemotional_R: Stile emotivo vs non-emotivo\n",
    "   0 = emotivo; 1 = misto; 2 = non-emotivo\n\n",
    
    "ESEMPI DI CODIFICA:\n",
    "- Titolo su conflitto politico tra partiti → societal_actors=1, decision_authorities=1\n",
    "- Titolo su cronaca locale singolo evento → societal_actors=0, individual_societal_R=0, episodic_thematic_F=0\n",
    "- Titolo su riforma di legge → policy_plan=1, decision_authorities=1, episodic_thematic_F=2\n",
    "- Titolo emotivo con testimonianze → emotional_unemotional_R=0, personal_impersonal_R=0\n\n",
    
    "TESTO DA ANALIZZARE (prime ~100 parole):\n",
    "-------------------------------------------\n",
    testo_articolo,
    "\n-------------------------------------------\n\n",
    
    "Solo se davvero impossibile determinare un valore, usa 999 per quel campo specifico.\n",
    "IMPORTANTE: Rispondi SOLO con il JSON, senza alcun testo aggiuntivo."
  )
  res <- tryCatch({
    response <- POST(
      url = "https://api.openai.com/v1/chat/completions",
      add_headers(
        Authorization = paste("Bearer", OPENAI_API_KEY),
        "Content-Type" = "application/json"
      ),
      encode = "json",
      body = list(
        model = "gpt-4o-mini",
        temperature = 0,
        messages = list(list(role = "user", content = prompt))
      ),
      timeout(60)
    )
    sc <- status_code(response)
    if (sc < 200 || sc >= 300) stop(paste0("HTTP API error: ", sc))
    
    result <- content(response, "parsed", simplifyVector = FALSE)
    if (is.null(result$choices) || length(result$choices) == 0) stop("no_choice")
    
    gpt_text <- result$choices[[1]]$message$content
    
    codes <- tryCatch(fromJSON(gpt_text), error = function(e) {
      m <- regmatches(gpt_text, regexpr("\\{[\\s\\S]*\\}", gpt_text))
      if (length(m) == 0) stop("invalid_json_response")
      fromJSON(m)
    })
    
    list(
      societal_actors_gold = codes$societal_actors,
      decision_authorities_gold = codes$decision_authorities,
      policy_plan_gold = codes$policy_plan,
      actors_concerned_gold = codes$actors_concerned,
      individual_societal_R_gold = codes$individual_societal_R,
      episodic_thematic_F_gold = codes$episodic_thematic_F,
      personal_impersonal_R_gold = codes$personal_impersonal_R,
      emotional_unemotional_R_gold = codes$emotional_unemotional_R
    )
    
  }, error = function(e) {
    list(error = TRUE, message = as.character(e$message))
  })
  
  return(res)
}

############################################################
# Main processing loop
############################################################

cat("=== CODIFICA CON PRIME 100 PAROLE ===\n")
cat("Carico i dati...\n")
data <- read_xlsx(INPUT_FILE)

if (is.null(N_RIGHE) || !is.numeric(N_RIGHE) || N_RIGHE <= 0) {
  N_RIGHE <- nrow(data)
}
N_RIGHE <- min(as.integer(N_RIGHE), nrow(data))
cat("Processando righe 1..", N_RIGHE, " su ", nrow(data), " totali\n", sep = "")

output_xlsx <- file.path(OUT_DIR, "codifica_100parole.xlsx")
output_csv  <- file.path(OUT_DIR, "codifica_100parole.csv")

expected_gold <- c(
  "societal_actors_gold","decision_authorities_gold","policy_plan_gold",
  "actors_concerned_gold","individual_societal_R_gold","episodic_thematic_F_gold",
  "personal_impersonal_R_gold","emotional_unemotional_R_gold"
)

for (c in expected_gold) {
  if (!c %in% names(data)) data[[c]] <- NA_integer_
}

if (!"contenuto_scaricato" %in% names(data)) data$contenuto_scaricato <- ""
if (!"scaricamento_ok" %in% names(data)) data$scaricamento_ok <- FALSE
if (!"errore_scaricamento" %in% names(data)) data$errore_scaricamento <- ""
if (!"error_message" %in% names(data)) data$error_message <- NA_character_

max_consecutive_failures <- 5L
consecutive_failures <- 0L
processed <- 0L
max_attempts_gpt <- 3L

safe_call_gpt_with_retries <- function(testo, attempts = max_attempts_gpt) {
  for (a in seq_len(attempts)) {
    cat(" GPT call attempt", a, "of", attempts, "...\n")
    res <- call_gpt_con_100parole(testo)
    if (is.list(res) && !is.null(res$error) && res$error) {
      cat("  -> GPT error:", res$message, "\n")
      Sys.sleep(2 ^ a)
      next
    }
    return(res)
  }
  return(list(error = TRUE, message = "max_retries_exceeded"))
}

cat("Inizio ciclo di codifica (salvataggio riga-per-riga)...\n\n")

for (i in seq_len(N_RIGHE)) {
  cat("=== RIGA", i, "/", N_RIGHE, "===\n")
  url_i <- as.character(data$url[i])
  cat("URL:", ifelse(is.na(url_i) || url_i == "", "(mancante)", url_i), "\n")
  
  risultato_download <- scarica_contenuto_url(url_i)
  data$contenuto_scaricato[i] <- risultato_download$testo
  data$scaricamento_ok[i] <- risultato_download$successo
  data$errore_scaricamento[i] <- risultato_download$errore
  
  if (isTRUE(risultato_download$successo) && nchar(risultato_download$testo) > 20) {
    cat("Contenuto (100 parole) ottenuto. Chiamo GPT...\n")
    res <- safe_call_gpt_with_retries(risultato_download$testo, attempts = max_attempts_gpt)
    
    if (is.list(res) && !is.null(res$error) && res$error) {
      cat("Errore GPT definitivo per riga", i, ":", res$message, "\n")
      data$error_message[i] <- res$message
      for (c in expected_gold) data[[c]][i] <- 999L
      consecutive_failures <- consecutive_failures + 1L
    } else {
      codes <- res
      for (f in expected_gold) {
        if (!is.null(codes[[f]])) {
          data[[f]][i] <- suppressWarnings(as.integer(codes[[f]]))
        } else {
          if (is.na(data[[f]][i])) data[[f]][i] <- NA_integer_
        }
      }
      data$error_message[i] <- NA_character_
      consecutive_failures <- 0L
      cat("Codifica GPT completata per riga", i, "\n")
    }
  } else {
    cat("Download fallito o contenuto troppo corto:", risultato_download$errore, "\n")
    for (c in expected_gold) data[[c]][i] <- 999L
    data$error_message[i] <- paste0("download_error: ", risultato_download$errore)
    consecutive_failures <- consecutive_failures + 1L
  }
  
  processed <- processed + 1L
  
  tryCatch({
    openxlsx::write.xlsx(data, output_xlsx, overwrite = TRUE)
  }, error = function(e) {
    cat("Warning: errore salvataggio parziale xlsx:", e$message, "\n")
  })
  
  tryCatch({
    write.csv(data, output_csv, row.names = FALSE, na = "")
  }, error = function(e) {
    cat("Warning: errore salvataggio parziale csv:", e$message, "\n")
  })
  
  if (consecutive_failures >= max_consecutive_failures) {
    cat("\n⚠️ Troppe failure consecutive (", consecutive_failures, "). Interrompo il run per sicurezza.\n", sep = "")
    break
  }
  
  Sys.sleep(2)
}

cat("\n=== REPORT FINALE ===\n")
cat("File XLSX:", output_xlsx, "\n")
cat("File CSV :", output_csv, "\n")
cat("Righe processate:", processed, "/", N_RIGHE, "\n")
cat("URL scaricate con successo:", sum(as.integer(data$scaricamento_ok[1:processed])), "/", processed, "\n")

if (sum(!data$scaricamento_ok[1:processed]) > 0) {
  cat("\nErrori di scaricamento (prime 20):\n")
  print(head(data[!data$scaricamento_ok, c("url", "errore_scaricamento", "error_message")], 20))
}

cat("\nEsempi di codifica (prime 3 righe con successo):\n")
good_idx <- which(as.logical(data$scaricamento_ok[1:processed]))
if (length(good_idx) > 0) {
  esempi <- data[good_idx[1:min(3, length(good_idx))],
                 c("headline", expected_gold, "error_message"), drop = FALSE]
  print(esempi)
} else {
  cat("Nessuna riga con scaricamento OK per mostrare esempi.\n")
}

cat("\n--- FINE ---\n")
