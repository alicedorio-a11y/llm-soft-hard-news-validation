############################################################
# HEADLINE-ONLY GPT CODING PIPELINE
############################################################

library(httr)
library(jsonlite)
library(dplyr)
library(readxl)
library(openxlsx)

# ===================== CONFIGURATION =====================
# OpenAI API credentials and input dataset
Sys.setenv(OPENAI_API_KEY = "")
OPENAI_API_KEY <- Sys.getenv("OPENAI_API_KEY")

INPUT_FILE <- "/Users/alice.dorio/Desktop/data/test_APICHATGPT_1000.xlsx"

# Number of rows to process (NULL = full dataset)
N_RIGHE <- NULL

# ===================== GPT CALL FUNCTION =====================
# Headline-only GPT coding based on the predefined codebook
call_gpt <- function(headline) {

  prompt <- paste0(
    "Sei un esperto codificatore di contenuti giornalistici. ",
    "Analizza il TITOLO della notizia e codifica secondo il codebook fornito.\n\n",
    
    "IMPORTANTE: Basati principalmente sul TITOLO della notizia.\n\n",
    
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
    
    "TITOLO DA ANALIZZARE: ", headline, "\n\n",
    
    "ANALIZZA ATTENTAMENTE IL TITOLO e assegna i valori appropriati secondo il codebook. ",
    "Solo se davvero impossibile determinare, usa 999 per quel campo specifico.\n",
    "IMPORTANTE: Rispondi SOLO con il JSON, nessun testo aggiuntivo."
  )
  
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
      response_format = list(type = "json_object"),
      messages = list(
        list(role = "user", content = prompt)
      )
    )
  )
  
  result <- content(response, "parsed")
  gpt_text <- result$choices[[1]]$message$content
  codes <- fromJSON(gpt_text)
  
  return(list(
    societal_actors_gold = codes$societal_actors,
    decision_authorities_gold = codes$decision_authorities,
    policy_plan_gold = codes$policy_plan,
    actors_concerned_gold = codes$actors_concerned,
    individual_societal_R_gold = codes$individual_societal_R,
    episodic_thematic_F_gold = codes$episodic_thematic_F,
    personal_impersonal_R_gold = codes$personal_impersonal_R,
    emotional_unemotional_R_gold = codes$emotional_unemotional_R
  ))
}

# ===================== DATA PROCESSING =====================
# Data loading and output structure initialisation
cat("Carico i dati...\n")
data <- read_xlsx(INPUT_FILE)

if (is.null(N_RIGHE) || !is.numeric(N_RIGHE) || N_RIGHE <= 0) {
  N_RIGHE <- nrow(data)
} else {
  N_RIGHE <- min(N_RIGHE, nrow(data))
}

if (!"error_message" %in% names(data)) data$error_message <- NA_character_
expected_cols <- c("societal_actors_gold","decision_authorities_gold","policy_plan_gold",
                   "actors_concerned_gold","individual_societal_R_gold","episodic_thematic_F_gold",
                   "personal_impersonal_R_gold","emotional_unemotional_R_gold")
for (c in expected_cols) if (!c %in% names(data)) data[[c]] <- NA_integer_

cat("Inizio codifica di", nrow(data), "titoli...\n")

# Row-wise headline-only GPT coding
for(i in seq_len(N_RIGHE)) {
  cat("Processo riga", i, "di", N_RIGHE, "\n")
  
  if (is.na(data$headline[i]) || trimws(as.character(data$headline[i])) == "") {
    data$error_message[i] <- "headline_missing"
    next
  }
 
  res <- tryCatch(call_gpt(data$headline[i]), error = function(e) list(error = TRUE, message = e$message))
  if (is.list(res) && !is.null(res$error) && res$error) {
    data$error_message[i] <- res$message
    data[i, expected_cols] <- 999L
    cat("  Errore riga", i, "->", res$message, " (fallback 999)\n")
  } else {
    codes <- res
    fields <- c("societal_actors","decision_authorities","policy_plan","actors_concerned",
                "individual_societal_R","episodic_thematic_F","personal_impersonal_R","emotional_unemotional_R")
    for (f in fields) {
      tgt <- paste0(f, "_gold")
      val <- NULL
      if (!is.null(codes[[tgt]])) val <- codes[[tgt]]
      if (is.null(val) && !is.null(codes[[f]])) val <- codes[[f]]
      if (!is.null(val)) data[[tgt]][i] <- suppressWarnings(as.integer(val))
    }
    data$error_message[i] <- NA_character_
  }
  Sys.sleep(1)
}

# ===================== OUTPUT =====================
output_file <- "data/risultati_codifica_semplice.xlsx"
desktop_output <- "/Users/alice.dorio/Desktop/risultati_codifica_semplice.xlsx"

openxlsx::write.xlsx(data, output_file, overwrite = TRUE)
openxlsx::write.xlsx(data, desktop_output, overwrite = TRUE)

system(paste("open", shQuote(output_file)))

cat("\n=== COMPLETATO ===\n")
cat("File salvato:", output_file, "\n")
cat("Righe processate:", nrow(data), "\n")

cat("\nPrime 3 righe:\n")
print(head(data, 3))

############################################################
# INTER-CODER RELIABILITY ANALYSIS
############################################################

library(ggplot2)
library(gridExtra)
library(corrplot)

cat("\n=== ANALISI INTER-CODER RELIABILITY ===\n")

# Variable-level agreement between manual and GPT coding
variabili <- list(
  list(manual = "societal_actors", gold = "societal_actors_gold", nome = "Societal Actors"),
  list(manual = "decision_authorities", gold = "decision_authorities_gold", nome = "Decision Authorities"),
  list(manual = "policy_plan", gold = "policy_plan_gold", nome = "Policy Plan"),
  list(manual = "actors_concerned", gold = "actors_concerned_gold", nome = "Actors Concerned"),
  list(manual = "individual_societal_R", gold = "individual_societal_R_gold", nome = "Individual-Societal"),
  list(manual = "episodic_thematic_F", gold = "episodic_thematic_F_gold", nome = "Episodic-Thematic"),
  list(manual = "personal_impersonal_R", gold = "personal_impersonal_R_gold", nome = "Personal-Impersonal"),
  list(manual = "emotional_unemotional_R", gold = "emotional_unemotional_R_gold", nome = "Emotional-Unemotional")
)

# Computation of agreement and Spearman correlations
risultati_reliability <- data.frame()
grafici <- list()

for(i in 1:length(variabili)) {
  var <- variabili[[i]]
  
  dati_validi <- data %>%
    filter(!is.na(.data[[var$manual]]) & !is.na(.data[[var$gold]]) &
           .data[[var$manual]] != 999 & .data[[var$gold]] != 999)
  
  if(nrow(dati_validi) > 0) {
   
    agreement <- sum(dati_validi[[var$manual]] == dati_validi[[var$gold]]) / nrow(dati_validi)
    
    correlation <- cor(dati_validi[[var$manual]], dati_validi[[var$gold]], 
                      method = "spearman")
    
    risultati_reliability <- rbind(risultati_reliability, data.frame(
      Variabile = var$nome,
      N_casi_validi = nrow(dati_validi),
      Percentage_Agreement = round(agreement * 100, 1),
      Correlazione_Spearman = round(correlation, 3)
    ))
    
    # Visual summary of inter-coder reliability
    p <- ggplot(dati_validi, aes_string(x = var$manual, y = var$gold)) +
      geom_jitter(width = 0.1, height = 0.1, alpha = 0.7, size = 2) +
      geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
      labs(title = paste("Inter-Coder Reliability:", var$nome),
           subtitle = paste("Agreement:", round(agreement*100,1), "% | Correlation:", round(correlation,3)),
           x = "Codifica Manuale",
           y = "Codifica GPT") +
      theme_minimal() +
      theme(plot.title = element_text(size = 10),
            plot.subtitle = element_text(size = 8))
    
    grafici[[i]] <- p
  }
}

# Display reliability table
cat("\nRISULTATI INTER-CODER RELIABILITY:\n")
print(risultati_reliability)

cat("\nCreazione grafici...\n")

# Bar plot: percentage agreement
p_agreement <- ggplot(risultati_reliability, aes(x = reorder(Variabile, Percentage_Agreement), 
                                                y = Percentage_Agreement)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  geom_text(aes(label = paste0(Percentage_Agreement, "%")), 
            hjust = -0.1, size = 3) +
  coord_flip() +
  labs(title = "Inter-Coder Agreement (%)",
       x = "Variabili",
       y = "Percentage Agreement") +
  theme_minimal() +
  ylim(0, 100)

# Bar plot: Spearman correlations
p_correlation <- ggplot(risultati_reliability, aes(x = reorder(Variabile, Correlazione_Spearman), 
                                                  y = Correlazione_Spearman)) +
  geom_col(fill = "darkgreen", alpha = 0.7) +
  geom_text(aes(label = Correlazione_Spearman), 
            hjust = -0.1, size = 3) +
  coord_flip() +
  labs(title = "Correlazioni Spearman",
       x = "Variabili",
       y = "Correlazione") +
  theme_minimal() +
  ylim(0, 1)

# Export of reliability figures (PDF)
pdf("inter_coder_reliability_analysis.pdf", width = 16, height = 12)

grid.arrange(p_agreement, p_correlation, ncol = 2, 
             top = "INTER-CODER RELIABILITY: GPT vs Codifica Manuale")

if(length(grafici) > 0) {
  for(i in seq(1, length(grafici), by = 4)) {
    end_idx <- min(i + 3, length(grafici))
    do.call(grid.arrange, c(grafici[i:end_idx], ncol = 2))
  }
}

dev.off()

# Export of summary figure (PNG)
png("inter_coder_reliability_summary.png", width = 1200, height = 800)
grid.arrange(p_agreement, p_correlation, ncol = 2, 
             top = "INTER-CODER RELIABILITY: GPT vs Codifica Manuale")
dev.off()

cat("\n=== ANALISI COMPLETATA ===\n")
cat("Grafici salvati in:\n")
cat("- inter_coder_reliability_analysis.pdf (completo)\n")
cat("- inter_coder_reliability_summary.png (riassunto)\n")
cat("\nStatistiche di affidabilità salvate nella tabella 'risultati_reliability'\n")

# Additional descriptive analysis for emotional_unemotional_R (if available)
if("emotional_unemotional_R" %in% names(data) && any(!is.na(data$emotional_unemotional_R))) {
  sub <- data %>% filter(!is.na(emotional_unemotional_R))
  
  cat("\n=== ANALISI emotional_unemotional_R ===\n")
  
  descrittive <- sub %>%
    summarise(
      Media = mean(emotional_unemotional_R, na.rm = TRUE),
      Mediana = median(emotional_unemotional_R, na.rm = TRUE),
      SD = sd(emotional_unemotional_R, na.rm = TRUE),
      Min = min(emotional_unemotional_R, na.rm = TRUE),
      Max = max(emotional_unemotional_R, na.rm = TRUE),
      N = n()
    )
  
  print(descrittive)
  
  write.xlsx(descrittive, "data/analisi_emotional_unemotional_R.xlsx", overwrite = TRUE)
  
  cat("Risultati analisi emotional_unemotional_R salvati in 'data/analisi_emotional_unemotional_R.xlsx'\n")
} else {
  message("Salto analisi emotional_unemotional_R (colonna mancante o solo NA)")
}
