############################################################
# RELIABILITY AND CLASSIFICATION METRICS ANALYSIS
# (offline analysis, no API calls)
############################################################

suppressPackageStartupMessages({
  library(readxl)
  library(openxlsx)
  library(dplyr)
  library(ggplot2)
  library(gridExtra)
  library(corrplot)
})

# ===================== CONFIGURATION =====================

# Input dataset containing manual and GPT-coded variables
# Output files are saved in the same directory

INPUT_FILE <- "/Users/alice.dorio/Desktop/data/codifiche_completate/METODO1_HeadlineOnly/CODIFICA/risultati_codifica_semplice.xlsx"

OUT_DIR <- dirname(INPUT_FILE)
OUT_XLSX <- file.path(OUT_DIR, "analisi_affidabilita_metriche2.xlsx")
OUT_PDF  <- file.path(OUT_DIR, "analisi_metriche_summary2.pdf")
OUT_PNG  <- file.path(OUT_DIR, "analisi_metriche_summary2.png")

# Definition of variables and measurement level (binary vs ordinal)
VAR_DEF <- tibble::tribble(
  ~manual,                    ~gold,                           ~nome,                   ~tipo,
  "societal_actors",          "societal_actors_gold",          "Societal Actors",       "binary",
  "decision_authorities",     "decision_authorities_gold",     "Decision Authorities",  "binary",
  "policy_plan",              "policy_plan_gold",              "Policy Plan",           "binary",
  "actors_concerned",         "actors_concerned_gold",         "Actors Concerned",      "binary",
  "individual_societal_R",    "individual_societal_R_gold",    "Individual–Societal",   "ordinal",
  "episodic_thematic_F",      "episodic_thematic_F_gold",      "Episodic–Thematic",     "ordinal",
  "personal_impersonal_R",    "personal_impersonal_R_gold",    "Personal–Impersonal",   "ordinal",
  "emotional_unemotional_R",  "emotional_unemotional_R_gold",  "Emotional–Unemotional", "ordinal"
)

# Utility functions for reliability and association metrics
# (safe wrappers to handle missing packages and invalid inputs)

safe_kappa <- function(x, y, weighted = FALSE) {

  if (!requireNamespace("irr", quietly = TRUE)) return(NA_real_)
  df <- data.frame(x = x, y = y)
  if (weighted) {
    out <- try(irr::kappa2(df, weight = "squared")$value, silent = TRUE)
  } else {
    out <- try(irr::kappa2(df, weight = "unweighted")$value, silent = TRUE)
  }
  if (inherits(out, "try-error")) return(NA_real_) else return(as.numeric(out))
}

safe_spearman <- function(x, y) {
  out <- suppressWarnings(try(cor(as.numeric(x), as.numeric(y), method = "spearman", use="complete.obs"), silent = TRUE))
  if (inherits(out, "try-error")) return(NA_real_) else return(as.numeric(out))
}

binary_prf <- function(truth, pred, positive = 1) {
  truth <- factor(truth, levels = c(0,1))
  pred  <- factor(pred,  levels = c(0,1))
  cm <- table(truth, pred) # rows: truth, cols: pred
  TP <- cm["1","1"] %||% 0; FP <- cm["0","1"] %||% 0
  TN <- cm["0","0"] %||% 0; FN <- cm["1","0"] %||% 0
  precision <- ifelse((TP+FP) == 0, NA, TP/(TP+FP))
  recall    <- ifelse((TP+FN) == 0, NA, TP/(TP+FN))
  f1        <- ifelse(is.na(precision) | is.na(recall) | (precision+recall)==0, NA, 2*precision*recall/(precision+recall))
  list(precision=precision, recall=recall, f1=f1, cm=cm)
}

macro_prf_multiclass <- function(truth, pred) {
  lv <- sort(unique(c(truth, pred)))
  truth <- factor(truth, levels = lv)
  pred  <- factor(pred,  levels = lv)
  cm <- table(truth, pred)
  per_class <- lapply(lv, function(cls) {
    TP <- cm[as.character(cls), as.character(cls)] %||% 0
    FP <- sum(cm[, as.character(cls)]) - TP
    FN <- sum(cm[as.character(cls), ]) - TP
    precision <- ifelse((TP+FP)==0, NA, TP/(TP+FP))
    recall    <- ifelse((TP+FN)==0, NA, TP/(TP+FN))
    f1        <- ifelse(is.na(precision)|is.na(recall)|(precision+recall)==0, NA, 2*precision*recall/(precision+recall))
    c(precision=precision, recall=recall, f1=f1)
  })
  per_class_mat <- do.call(rbind, per_class)
  macro_precision <- mean(per_class_mat[,"precision"], na.rm = TRUE)
  macro_recall    <- mean(per_class_mat[,"recall"],    na.rm = TRUE)
  macro_f1        <- mean(per_class_mat[,"f1"],        na.rm = TRUE)
  list(macro_precision=macro_precision, macro_recall=macro_recall, macro_f1=macro_f1, cm=cm, per_class=per_class_mat, levels = lv)
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

# Data loading and basic cleaning
# (exclusion of missing values and fallback codes)

cat("Carico:", INPUT_FILE, "\n")
df <- read_xlsx(INPUT_FILE)

summary_rows <- list()
conf_mats <- list()
plots <- list()

# Variable-level reliability and classification metrics
# Metrics are computed according to the measurement level

for (r in seq_len(nrow(VAR_DEF))) {
  manual <- VAR_DEF$manual[r]
  gold   <- VAR_DEF$gold[r]
  nome   <- VAR_DEF$nome[r]
  tipo   <- VAR_DEF$tipo[r]

  if (!all(c(manual, gold) %in% names(df))) {
    warning(sprintf("Variabile %s: colonne mancanti (%s / %s). La salto.", nome, manual, gold))
    next
  }

  dati <- df %>%
    select(all_of(c(manual, gold))) %>%
    filter(!is.na(.data[[manual]]), !is.na(.data[[gold]])) %>%
    filter(.data[[manual]] != 999, .data[[gold]] != 999)

  n_validi <- nrow(dati)
  if (n_validi == 0) {
    summary_rows[[length(summary_rows)+1]] <- data.frame(
      Variabile = nome, Tipo = tipo, N_validi = 0,
      Accuracy = NA, Kappa = NA, Weighted_Kappa = NA, Spearman = NA,
      Precision = NA, Recall = NA, F1 = NA, Macro_Precision = NA, Macro_Recall = NA, Macro_F1 = NA,
      stringsAsFactors = FALSE
    )
    next
  }

  accuracy <- mean(dati[[manual]] == dati[[gold]])

  kappa_unw <- safe_kappa(dati[[manual]], dati[[gold]], weighted = FALSE)

  weighted_kappa <- if (tipo == "ordinal") safe_kappa(dati[[manual]], dati[[gold]], weighted = TRUE) else NA
  rho_spearman   <- if (tipo == "ordinal") safe_spearman(dati[[manual]], dati[[gold]]) else NA

  if (tipo == "binary") {
    m <- binary_prf(dati[[manual]], dati[[gold]], positive = 1)
    precision <- m$precision; recall <- m$recall; f1 <- m$f1
    macro_p <- NA; macro_r <- NA; macro_f1 <- NA
    conf_mats[[nome]] <- m$cm
  } else {
    m <- macro_prf_multiclass(dati[[manual]], dati[[gold]])
    precision <- NA; recall <- NA; f1 <- NA
    macro_p <- m$macro_precision; macro_r <- m$macro_recall; macro_f1 <- m$macro_f1
    conf_mats[[nome]] <- m$cm
  }

  summary_rows[[length(summary_rows)+1]] <- data.frame(
    Variabile = nome,
    Tipo = tipo,
    N_validi = n_validi,
    Accuracy = round(accuracy, 3),
    Kappa = round(kappa_unw, 3),
    Weighted_Kappa = round(weighted_kappa, 3),
    Spearman = round(rho_spearman, 3),
    Precision = ifelse(is.na(precision), NA, round(precision, 3)),
    Recall    = ifelse(is.na(recall),    NA, round(recall, 3)),
    F1        = ifelse(is.na(f1),        NA, round(f1, 3)),
    Macro_Precision = ifelse(is.na(macro_p), NA, round(macro_p, 3)),
    Macro_Recall    = ifelse(is.na(macro_r), NA, round(macro_r, 3)),
    Macro_F1        = ifelse(is.na(macro_f1), NA, round(macro_f1, 3)),
    stringsAsFactors = FALSE
  )

  y2 <- if (tipo == "binary") (ifelse(is.na(f1), NA, f1)) else (ifelse(is.na(macro_f1), NA, macro_f1))
  label2 <- if (tipo == "binary") "F1 (positivo=1)" else "Macro F1 (0/1/2)"
  plot_df <- data.frame(Metrica = c("Accuracy", label2),
                        Valore  = c(accuracy, y2))
  p <- ggplot(plot_df, aes(x = Metrica, y = Valore)) +
    geom_col(width = 0.6, alpha = 0.75) +
    geom_text(aes(label = round(Valore, 3)), vjust = -0.5, size = 3) +
    ylim(0, 1) +
    labs(title = paste0(nome, " — N=", n_validi),
         y = "Valore", x = NULL) +
    theme_minimal(base_size = 11)
  plots[[length(plots)+1]] <- p
}

summary_tbl <- dplyr::bind_rows(summary_rows)

cat("\n=== RISULTATI SINTESI ===\n")
print(summary_tbl)

############################################################
# Inter-coder reliability: agreement and monotonic association
############################################################

cat("\n=== ANALISI INTER-CODER RELIABILITY ===\n")
variabili <- list(
  list(manual = "societal_actors",            gold = "societal_actors_gold",           nome = "Societal Actors"),
  list(manual = "decision_authorities",       gold = "decision_authorities_gold",      nome = "Decision Authorities"),
  list(manual = "policy_plan",                gold = "policy_plan_gold",               nome = "Policy Plan"),
  list(manual = "actors_concerned",           gold = "actors_concerned_gold",          nome = "Actors Concerned"),
  list(manual = "individual_societal_R",      gold = "individual_societal_R_gold",     nome = "Individual–Societal"),
  list(manual = "episodic_thematic_F",        gold = "episodic_thematic_F_gold",       nome = "Episodic–Thematic"),
  list(manual = "personal_impersonal_R",      gold = "personal_impersonal_R_gold",     nome = "Personal–Impersonal"),
  list(manual = "emotional_unemotional_R",    gold = "emotional_unemotional_R_gold",   nome = "Emotional–Unemotional")
)

risultati_reliability <- data.frame(stringsAsFactors = FALSE)
grafici_ic <- list()

for (v in variabili) {
  if (!all(c(v$manual, v$gold) %in% names(df))) {
    warning("Skip inter-coder for ", v$nome, ": colonne mancanti")
    next
  }
  dati_validi <- df %>%
    filter(!is.na(.data[[v$manual]]), !is.na(.data[[v$gold]])) %>%
    filter(.data[[v$manual]] != 999, .data[[v$gold]] != 999)
  if (nrow(dati_validi) == 0) next

  agreement <- mean(dati_validi[[v$manual]] == dati_validi[[v$gold]])
  correlation <- tryCatch(cor(dati_validi[[v$manual]], dati_validi[[v$gold]], method = "spearman", use = "complete.obs"),
                          error = function(e) NA_real_)

  risultati_reliability <- rbind(risultati_reliability, data.frame(
    Variabile = v$nome,
    N_casi_validi = nrow(dati_validi),
    Percentage_Agreement = round(agreement * 100, 1),
    Correlazione_Spearman = round(as.numeric(correlation), 3),
    stringsAsFactors = FALSE
  ))

  p <- ggplot(dati_validi, aes_string(x = v$manual, y = v$gold)) +
    geom_jitter(width = 0.1, height = 0.1, alpha = 0.7, size = 2) +
    geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
    labs(title = paste("Inter-Coder:", v$nome),
         subtitle = paste0("Agreement: ", round(agreement*100,1), "% | Spearman: ", round(as.numeric(correlation),3)),
         x = "Manuale", y = "GPT") +
    theme_minimal() + theme(plot.title = element_text(size = 10), plot.subtitle = element_text(size = 8))
  grafici_ic[[length(grafici_ic)+1]] <- p
}

cat("\nRISULTATI INTER-CODER:\n")
print(risultati_reliability)

ic_pdf <- file.path(OUT_DIR, "inter_coder_reliability_analysis2.pdf")
ic_png <- file.path(OUT_DIR, "inter_coder_reliability_summary2.png")
if (nrow(risultati_reliability) > 0) {
  p_agreement <- ggplot(risultati_reliability, aes(x = reorder(Variabile, Percentage_Agreement), y = Percentage_Agreement)) +
    geom_col(fill = "steelblue", alpha = 0.8) + geom_text(aes(label = paste0(Percentage_Agreement, "%")), hjust = -0.1, size = 3) +
    coord_flip() + labs(title = "Inter-Coder Agreement (%)", x = NULL, y = "Agreement %") + theme_minimal() + ylim(0,100)
  p_corr <- ggplot(risultati_reliability, aes(x = reorder(Variabile, Correlazione_Spearman), y = Correlazione_Spearman)) +
    geom_col(fill = "darkgreen", alpha = 0.8) + geom_text(aes(label = Correlazione_Spearman), hjust = -0.1, size = 3) +
    coord_flip() + labs(title = "Spearman Correlation", x = NULL, y = "Spearman rho") + theme_minimal() + ylim(0,1)

  pdf(ic_pdf, width = 12, height = 9)
  grid.arrange(p_agreement, p_corr, ncol = 2, top = "INTER-CODER RELIABILITY")
  if (length(grafici_ic) > 0) {
    for (i in seq(1, length(grafici_ic), by = 4)) {
      end_idx <- min(i+3, length(grafici_ic))
      do.call(grid.arrange, c(grafici_ic[i:end_idx], ncol = 2))
    }
  }
  dev.off()

  png(ic_png, width = 1200, height = 800)
  grid.arrange(p_agreement, p_corr, ncol = 2, top = "INTER-CODER RELIABILITY")
  dev.off()
  cat("Inter-coder grafici salvati:\n -", ic_pdf, "\n -", ic_png, "\n")
} else {
  cat("Nessun dato valido per inter-coder reliability.\n")
}

# Export of summary tables and confusion matrices
wb <- createWorkbook()

addWorksheet(wb, "Summary")
writeData(wb, "Summary", summary_tbl)

for (nm in names(conf_mats)) {
  addWorksheet(wb, paste0(substr(nm, 1, 28))) # Excel limita i nomi, taglio a 31-3 di margine
  writeData(wb, paste0(substr(nm, 1, 28)), as.data.frame.matrix(conf_mats[[nm]]), rowNames = TRUE)
}

if (exists("risultati_reliability") && nrow(risultati_reliability) > 0) {
  addWorksheet(wb, "InterCoder")
  writeData(wb, "InterCoder", risultati_reliability)
}

saveWorkbook(wb, OUT_XLSX, overwrite = TRUE)
cat("\nFile Excel salvato in:", OUT_XLSX, "\n")

############################################################
# Graphical diagnostics and summary figures
# (used for internal validation and robustness checks)
############################################################
if (nrow(summary_tbl) > 0) {
  
  p_acc <- ggplot(summary_tbl, aes(x = reorder(Variabile, Accuracy), y = Accuracy)) +
    geom_col(alpha = 0.8) +
    geom_text(aes(label = round(Accuracy,3)), hjust = -0.1, size = 3) +
    coord_flip() + ylim(0,1) +
    labs(title = "Accuracy per variabile", x = NULL, y = "Accuracy") +
    theme_minimal(base_size = 11)

  p_kappa <- ggplot(summary_tbl, aes(x = reorder(Variabile, Kappa), y = Kappa)) +
    geom_col(alpha = 0.8) +
    geom_text(aes(label = round(Kappa,3)), hjust = -0.1, size = 3) +
    coord_flip() + ylim(0,1) +
    labs(title = "Cohen's Kappa (unweighted)", x = NULL, y = "Kappa") +
    theme_minimal(base_size = 11)

  f1_val <- ifelse(summary_tbl$Tipo == "binary", summary_tbl$F1, summary_tbl$Macro_F1)
  p_f1 <- ggplot(transform(summary_tbl, F1_plot = f1_val),
                 aes(x = reorder(Variabile, F1_plot), y = F1_plot)) +
    geom_col(alpha = 0.8) +
    geom_text(aes(label = round(F1_plot,3)), hjust = -0.1, size = 3) +
    coord_flip() + ylim(0,1) +
    labs(title = "F1 (binaria) / Macro-F1 (ordinali)", x = NULL, y = "F1") +
    theme_minimal(base_size = 11)

  pdf(OUT_PDF, width = 12, height = 9)
  gridExtra::grid.arrange(p_acc, p_kappa, p_f1, ncol = 2,
                          top = "Metriche di affidabilità e classificazione")
 
  if (length(plots) > 0) {
    for (i in seq(1, length(plots), by = 4)) {
      end_idx <- min(i + 3, length(plots))
      do.call(gridExtra::grid.arrange, c(plots[i:end_idx], ncol = 2))
    }
  }
  dev.off()

  png(OUT_PNG, width = 1400, height = 900)
  gridExtra::grid.arrange(p_acc, p_kappa, p_f1, ncol = 2,
                          top = "Metriche di affidabilità e classificazione")
  dev.off()

  cat("Grafici salvati in:\n- ", OUT_PDF, "\n- ", OUT_PNG, "\n")
} else {
  cat("Nessuna riga valida dopo la pulizia (NA/999): salto i grafici.\n")
}

cat("\n=== COMPLETATO ===\n")
