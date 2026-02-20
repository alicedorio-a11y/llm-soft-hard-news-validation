############################################################
# Reliability and performance analysis
############################################################

suppressPackageStartupMessages({
  library(readxl)
  library(openxlsx)
  library(dplyr)
  library(ggplot2)
  library(gridExtra)
})

############################################################
# Paths
############################################################

INPUT_FILE <- "/Users/alice.dorio/Desktop/data/codifiche_completate/METODO3_contenuto_100paroleOnly/CODIFICA/codifica_100parole.xlsx"
OUT_DIR    <- "/Users/alice.dorio/Desktop/data/codifiche_completate/METODO3_contenuto_100paroleOnly/ANALISI"

if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

OUT_XLSX <- file.path(OUT_DIR, "analisi_affidabilita_metriche_content1000parole.xlsx")
OUT_CSV  <- file.path(OUT_DIR, "analisi_affidabilita_metriche_content1000parole.csv")
OUT_PDF  <- file.path(OUT_DIR, "analisi_metriche_summary_content1000parole.pdf")
OUT_PNG  <- file.path(OUT_DIR, "analisi_metriche_summary_content1000parole.png")
IC_PDF   <- file.path(OUT_DIR, "inter_coder_reliability_content1000parole.pdf")
IC_PNG   <- file.path(OUT_DIR, "inter_coder_reliability_content1000parole.png")

############################################################
# Variable definitions
############################################################
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

############################################################
# Utility functions
############################################################
`%||%` <- function(a, b) if (!is.null(a)) a else b

safe_kappa <- function(x, y, weighted = FALSE) {
  if (!requireNamespace("irr", quietly = TRUE)) return(NA_real_)
  df <- data.frame(x = x, y = y)
  out <- try(
    irr::kappa2(df, weight = if (weighted) "squared" else "unweighted")$value,
    silent = TRUE
  )
  if (inherits(out, "try-error")) NA_real_ else as.numeric(out)
}

safe_spearman <- function(x, y) {
  out <- suppressWarnings(try(cor(as.numeric(x), as.numeric(y),
                                  method = "spearman", use = "complete.obs"),
                              silent = TRUE))
  if (inherits(out, "try-error")) NA_real_ else as.numeric(out)
}

binary_prf <- function(truth, pred, positive = 1) {
  truth <- factor(truth, levels = c(0,1))
  pred  <- factor(pred,  levels = c(0,1))
  cm <- table(truth, pred)
  TP <- cm["1","1"] %||% 0; FP <- cm["0","1"] %||% 0
  TN <- cm["0","0"] %||% 0; FN <- cm["1","0"] %||% 0
  precision <- ifelse((TP+FP) == 0, NA, TP/(TP+FP))
  recall    <- ifelse((TP+FN) == 0, NA, TP/(TP+FN))
  f1        <- ifelse(is.na(precision) | is.na(recall) | (precision+recall)==0,
                      NA, 2*precision*recall/(precision+recall))
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
    f1        <- ifelse(is.na(precision)|is.na(recall)|(precision+recall)==0,
                        NA, 2*precision*recall/(precision+recall))
    c(precision=precision, recall=recall, f1=f1)
  })
  per_class_mat <- do.call(rbind, per_class)
  list(
    macro_precision = mean(per_class_mat[,"precision"], na.rm = TRUE),
    macro_recall    = mean(per_class_mat[,"recall"],    na.rm = TRUE),
    macro_f1        = mean(per_class_mat[,"f1"],        na.rm = TRUE),
    cm              = cm
  )
}

############################################################
# Data loading
############################################################
cat("Carico:", INPUT_FILE, "\n")

df <- read_xlsx(INPUT_FILE)
names(df) <- trimws(names(df))

cat("Colonne trovate:\n")
print(names(df))

############################################################
# Main reliability and performance analysis
############################################################

summary_rows <- list()
conf_mats <- list()
plots <- list()

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
      Precision = NA, Recall = NA, F1 = NA,
      Macro_Precision = NA, Macro_Recall = NA, Macro_F1 = NA,
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

cat("\n=== RISULTATI SINTESI (content) ===\n")
print(summary_tbl)

############################################################
# Inter-coder reliability, export, and plots
############################################################

cat("\n=== ANALISI INTER-CODER RELIABILITY (content) ===\n")

variabili_ic <- list(
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

for (v in variabili_ic) {
  if (!all(c(v$manual, v$gold) %in% names(df))) {
    warning("Skip inter-coder per ", v$nome, ": colonne mancanti")
    next
  }

  dati_validi <- df %>%
    filter(!is.na(.data[[v$manual]]), !is.na(.data[[v$gold]])) %>%
    filter(.data[[v$manual]] != 999, .data[[v$gold]] != 999)

  if (nrow(dati_validi) == 0) next

  agreement <- mean(dati_validi[[v$manual]] == dati_validi[[v$gold]])
  correlation <- tryCatch(cor(dati_validi[[v$manual]], dati_validi[[v$gold]],
                              method = "spearman", use = "complete.obs"),
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
         subtitle = paste0("Agreement: ", round(agreement*100,1),
                           "% | Spearman: ", round(as.numeric(correlation),3)),
         x = "Manuale", y = "GPT") +
    theme_minimal(base_size = 9)
  grafici_ic[[length(grafici_ic)+1]] <- p
}

if (nrow(risultati_reliability) > 0) {
  p_agreement <- ggplot(risultati_reliability,
                        aes(x = reorder(Variabile, Percentage_Agreement),
                            y = Percentage_Agreement)) +
    geom_col(fill = "steelblue", alpha = 0.8) +
    geom_text(aes(label = paste0(Percentage_Agreement, "%")), hjust = -0.1, size = 3) +
    coord_flip() + ylim(0,100) +
    labs(title = "Inter-Coder Agreement (%)", x = NULL, y = "Agreement %") +
    theme_minimal()

  p_corr <- ggplot(risultati_reliability,
                   aes(x = reorder(Variabile, Correlazione_Spearman),
                       y = Correlazione_Spearman)) +
    geom_col(fill = "darkgreen", alpha = 0.8) +
    geom_text(aes(label = Correlazione_Spearman), hjust = -0.1, size = 3) +
    coord_flip() + ylim(0,1) +
    labs(title = "Spearman Correlation", x = NULL, y = "Spearman rho") +
    theme_minimal()

  pdf(IC_PDF, width = 12, height = 9)
  grid.arrange(p_agreement, p_corr, ncol = 2, top = "INTER-CODER RELIABILITY (content)")
  if (length(grafici_ic) > 0) {
    for (i in seq(1, length(grafici_ic), by = 4)) {
      end_idx <- min(i+3, length(grafici_ic))
      do.call(grid.arrange, c(grafici_ic[i:end_idx], ncol = 2))
    }
  }
  dev.off()

  png(IC_PNG, width = 1200, height = 800)
  grid.arrange(p_agreement, p_corr, ncol = 2, top = "INTER-CODER RELIABILITY (content)")
  dev.off()

  cat("Inter-coder grafici salvati in:\n -", IC_PDF, "\n -", IC_PNG, "\n")
} else {
  cat("Nessun dato valido per inter-coder reliability.\n")
}

wb <- createWorkbook()
addWorksheet(wb, "Summary")
writeData(wb, "Summary", summary_tbl)

for (nm in names(conf_mats)) {
  wsname <- substr(nm, 1, 28)
  addWorksheet(wb, wsname)
  writeData(wb, wsname, as.data.frame.matrix(conf_mats[[nm]]), rowNames = TRUE)
}

if (nrow(risultati_reliability) > 0) {
  addWorksheet(wb, "InterCoder")
  writeData(wb, "InterCoder", risultati_reliability)
}

saveWorkbook(wb, OUT_XLSX, overwrite = TRUE)
cat("File Excel salvato in:", OUT_XLSX, "\n")

write.csv2(summary_tbl, OUT_CSV, row.names = FALSE, na = "")
cat("File CSV salvato in:", OUT_CSV, "\n")

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
                          top = "Metriche di affidabilità e classificazione (content)")
  if (length(plots) > 0) {
    for (i in seq(1, length(plots), by = 4)) {
      end_idx <- min(i + 3, length(plots))
      do.call(gridExtra::grid.arrange, c(plots[i:end_idx], ncol = 2))
    }
  }
  dev.off()

  png(OUT_PNG, width = 1400, height = 900)
  gridExtra::grid.arrange(p_acc, p_kappa, p_f1, ncol = 2,
                          top = "Metriche di affidabilità e classificazione (content)")
  dev.off()

  cat("Grafici salvati in:\n- ", OUT_PDF, "\n- ", OUT_PNG, "\n")
} else {
  cat("Nessuna riga valida dopo i filtri (NA/999): salto i grafici.\n")
}

cat("\n=== COMPLETATO ===\n")