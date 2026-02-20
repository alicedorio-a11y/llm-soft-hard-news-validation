############################################################
# Scale construction: manual coding
############################################################

library(dplyr)

add_manual_scales <- function(df) {
  df %>%
    mutate(
      topic_manual_scale  = societal_actors + decision_authorities + policy_plan + actors_concerned,
      focus_manual_scale  = individual_societal_R + episodic_thematic_F,
      style_manual_scale  = personal_impersonal_R + emotional_unemotional_R,
      hardness_manual_scale = topic_manual_scale + focus_manual_scale + style_manual_scale
    )
}

df_m1 <- add_manual_scales(df_m1)
df_m2 <- add_manual_scales(df_m2)
df_m3 <- add_manual_scales(df_m3)

############################################################
# Scale construction: GPT-based coding by input method
############################################################

# Method 1: Headline-only input (df_m1)
df_m1 <- df_m1 %>%
  mutate(
    topic_gpt_m1_scale  = societal_actors_gold + decision_authorities_gold + policy_plan_gold + actors_concerned_gold,
    focus_gpt_m1_scale  = individual_societal_R_gold + episodic_thematic_F_gold,
    style_gpt_m1_scale  = personal_impersonal_R_gold + emotional_unemotional_R_gold,
    hardness_gpt_m1_scale = topic_gpt_m1_scale + focus_gpt_m1_scale + style_gpt_m1_scale
  )


# Method 2: Full-content input (df_m2)
df_m2 <- df_m2 %>%
  mutate(
    topic_gpt_m2_scale  = societal_actors_gold + decision_authorities_gold + policy_plan_gold + actors_concerned_gold,
    focus_gpt_m2_scale  = individual_societal_R_gold + episodic_thematic_F_gold,
    style_gpt_m2_scale  = personal_impersonal_R_gold + emotional_unemotional_R_gold,
    hardness_gpt_m2_scale = topic_gpt_m2_scale + focus_gpt_m2_scale + style_gpt_m2_scale
  )


# Method 3: First 100 words input (df_m3)
df_m3 <- df_m3 %>%
  mutate(
    topic_gpt_m3_scale  = societal_actors_gold + decision_authorities_gold + policy_plan_gold + actors_concerned_gold,
    focus_gpt_m3_scale  = individual_societal_R_gold + episodic_thematic_F_gold,
    style_gpt_m3_scale  = personal_impersonal_R_gold + emotional_unemotional_R_gold,
    hardness_gpt_m3_scale = topic_gpt_m3_scale + focus_gpt_m3_scale + style_gpt_m3_scale
  )

str(df_m1$topic_manual_scale)
str(df_m2$topic_gpt_m2_scale)
str(df_m3$hardness_gpt_m3_scale)

############################################################
# Convergent validity: Spearman correlations
############################################################

spearman <- function(x, y) {
  ok <- complete.cases(x, y)
  if (sum(ok) < 3) return(NA_real_)
  suppressWarnings(cor(x[ok], y[ok], method = "spearman"))
}

correlations_summary <- data.frame(
  dimension = rep(c("Topic", "Focus", "Style", "Hardness"), each = 3),
  metodo    = rep(c("M1_Headline", "M2_fullcontent", "M3_100words"), times = 4),
  spearman  = c(
    spearman(df_m1$topic_manual_scale, df_m1$topic_gpt_m1_scale),
    spearman(df_m2$topic_manual_scale, df_m2$topic_gpt_m2_scale),
    spearman(df_m3$topic_manual_scale, df_m3$topic_gpt_m3_scale),
    spearman(df_m1$focus_manual_scale, df_m1$focus_gpt_m1_scale),
    spearman(df_m2$focus_manual_scale, df_m2$focus_gpt_m2_scale),
    spearman(df_m3$focus_manual_scale, df_m3$focus_gpt_m3_scale),
    spearman(df_m1$style_manual_scale, df_m1$style_gpt_m1_scale),
    spearman(df_m2$style_manual_scale, df_m2$style_gpt_m2_scale),
    spearman(df_m3$style_manual_scale, df_m3$style_gpt_m3_scale),
    spearman(df_m1$hardness_manual_scale, df_m1$hardness_gpt_m1_scale),
    spearman(df_m2$hardness_manual_scale, df_m2$hardness_gpt_m2_scale),
    spearman(df_m3$hardness_manual_scale, df_m3$hardness_gpt_m3_scale)
  )
)

correlations_summary

library(writexl)

results_dir <- "/Users/alice.dorio/Desktop/data/codifiche_completate/RISULTATI"
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)

write_xlsx(
  correlations_summary,
  path = file.path(results_dir, "correlazioni_soft_hard_topic_focus_style_metodi.xlsx")
)


############################################################
# Descriptive statistics of scale distributions
############################################################
library(dplyr)
library(writexl)

############################################################
# Descriptive statistics: manual coding
############################################################

desc_manual_df <- data.frame(
  Metodo     = "Manual",
  
  Topic_min  = min(df_m1$topic_manual_scale, na.rm = TRUE),
  Topic_max  = max(df_m1$topic_manual_scale, na.rm = TRUE),
  Topic_mean = mean(df_m1$topic_manual_scale, na.rm = TRUE),
  Topic_sd   = sd(df_m1$topic_manual_scale, na.rm = TRUE),
  
  Focus_min  = min(df_m1$focus_manual_scale, na.rm = TRUE),
  Focus_max  = max(df_m1$focus_manual_scale, na.rm = TRUE),
  Focus_mean = mean(df_m1$focus_manual_scale, na.rm = TRUE),
  Focus_sd   = sd(df_m1$focus_manual_scale, na.rm = TRUE),
  
  Style_min  = min(df_m1$style_manual_scale, na.rm = TRUE),
  Style_max  = max(df_m1$style_manual_scale, na.rm = TRUE),
  Style_mean = mean(df_m1$style_manual_scale, na.rm = TRUE),
  Style_sd   = sd(df_m1$style_manual_scale, na.rm = TRUE),
  
  Hard_min   = min(df_m1$hardness_manual_scale, na.rm = TRUE),
  Hard_max   = max(df_m1$hardness_manual_scale, na.rm = TRUE),
  Hard_mean  = mean(df_m1$hardness_manual_scale, na.rm = TRUE),
  Hard_sd    = sd(df_m1$hardness_manual_scale, na.rm = TRUE)
)
############################################################
# Descriptive statistics: GPT-based coding
############################################################

# Method 1: Headline-only
desc_m1_df <- data.frame(
  Metodo     = "M1_Headline",
  
  Topic_min  = min(df_m1$topic_gpt_m1_scale, na.rm = TRUE),
  Topic_max  = max(df_m1$topic_gpt_m1_scale, na.rm = TRUE),
  Topic_mean = mean(df_m1$topic_gpt_m1_scale, na.rm = TRUE),
  Topic_sd   = sd(df_m1$topic_gpt_m1_scale, na.rm = TRUE),
  
  Focus_min  = min(df_m1$focus_gpt_m1_scale, na.rm = TRUE),
  Focus_max  = max(df_m1$focus_gpt_m1_scale, na.rm = TRUE),
  Focus_mean = mean(df_m1$focus_gpt_m1_scale, na.rm = TRUE),
  Focus_sd   = sd(df_m1$focus_gpt_m1_scale, na.rm = TRUE),
  
  Style_min  = min(df_m1$style_gpt_m1_scale, na.rm = TRUE),
  Style_max  = max(df_m1$style_gpt_m1_scale, na.rm = TRUE),
  Style_mean = mean(df_m1$style_gpt_m1_scale, na.rm = TRUE),
  Style_sd   = sd(df_m1$style_gpt_m1_scale, na.rm = TRUE),
  
  Hard_min   = min(df_m1$hardness_gpt_m1_scale, na.rm = TRUE),
  Hard_max   = max(df_m1$hardness_gpt_m1_scale, na.rm = TRUE),
  Hard_mean  = mean(df_m1$hardness_gpt_m1_scale, na.rm = TRUE),
  Hard_sd    = sd(df_m1$hardness_gpt_m1_scale, na.rm = TRUE)
)

# Method 2: Full-content
desc_m2_df <- data.frame(
  Metodo     = "M2_fullcontent",
  
  Topic_min  = min(df_m2$topic_gpt_m2_scale, na.rm = TRUE),
  Topic_max  = max(df_m2$topic_gpt_m2_scale, na.rm = TRUE),
  Topic_mean = mean(df_m2$topic_gpt_m2_scale, na.rm = TRUE),
  Topic_sd   = sd(df_m2$topic_gpt_m2_scale, na.rm = TRUE),
  
  Focus_min  = min(df_m2$focus_gpt_m2_scale, na.rm = TRUE),
  Focus_max  = max(df_m2$focus_gpt_m2_scale, na.rm = TRUE),
  Focus_mean = mean(df_m2$focus_gpt_m2_scale, na.rm = TRUE),
  Focus_sd   = sd(df_m2$focus_gpt_m2_scale, na.rm = TRUE),
  
  Style_min  = min(df_m2$style_gpt_m2_scale, na.rm = TRUE),
  Style_max  = max(df_m2$style_gpt_m2_scale, na.rm = TRUE),
  Style_mean = mean(df_m2$style_gpt_m2_scale, na.rm = TRUE),
  Style_sd   = sd(df_m2$style_gpt_m2_scale, na.rm = TRUE),
  
  Hard_min   = min(df_m2$hardness_gpt_m2_scale, na.rm = TRUE),
  Hard_max   = max(df_m2$hardness_gpt_m2_scale, na.rm = TRUE),
  Hard_mean  = mean(df_m2$hardness_gpt_m2_scale, na.rm = TRUE),
  Hard_sd    = sd(df_m2$hardness_gpt_m2_scale, na.rm = TRUE)
)

# Method 3: First 100 words
desc_m3_df <- data.frame(
  Metodo     = "M3_100words",
  
  Topic_min  = min(df_m3$topic_gpt_m3_scale, na.rm = TRUE),
  Topic_max  = max(df_m3$topic_gpt_m3_scale, na.rm = TRUE),
  Topic_mean = mean(df_m3$topic_gpt_m3_scale, na.rm = TRUE),
  Topic_sd   = sd(df_m3$topic_gpt_m3_scale, na.rm = TRUE),
  
  Focus_min  = min(df_m3$focus_gpt_m3_scale, na.rm = TRUE),
  Focus_max  = max(df_m3$focus_gpt_m3_scale, na.rm = TRUE),
  Focus_mean = mean(df_m3$focus_gpt_m3_scale, na.rm = TRUE),
  Focus_sd   = sd(df_m3$focus_gpt_m3_scale, na.rm = TRUE),
  
  Style_min  = min(df_m3$style_gpt_m3_scale, na.rm = TRUE),
  Style_max  = max(df_m3$style_gpt_m3_scale, na.rm = TRUE),
  Style_mean = mean(df_m3$style_gpt_m3_scale, na.rm = TRUE),
  Style_sd   = sd(df_m3$style_gpt_m3_scale, na.rm = TRUE),
  
  Hard_min   = min(df_m3$hardness_gpt_m3_scale, na.rm = TRUE),
  Hard_max   = max(df_m3$hardness_gpt_m3_scale, na.rm = TRUE),
  Hard_mean  = mean(df_m3$hardness_gpt_m3_scale, na.rm = TRUE),
  Hard_sd    = sd(df_m3$hardness_gpt_m3_scale, na.rm = TRUE)
)

# Combine descriptive statistics from manual coding and all LLM-based methods into a single summary table
desc_all <- dplyr::bind_rows(desc_manual_df, desc_m1_df, desc_m2_df, desc_m3_df)

desc_all

output_path <- file.path(results_dir, "descrittive_scale_metodi_completo.xlsx")

write_xlsx(desc_all, path = output_path)

message("File salvato in: ", output_path)

library(ggplot2)
library(dplyr)

############################################################
# Convergent validity: Spearman correlations (Hardness)
############################################################

library(dplyr)
library(psych)
library(writexl)

results_dir <- "/Users/alice.dorio/Desktop/data/codifiche_completate/RISULTATI"
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)

############################################################
# Recompute manual Hardness scales (defensive replication)
############################################################
# This step redundantly reconstructs the manual scales to guarantee internal consistency across datasets prior to correlation analyses.

add_manual_scales <- function(df) {
  df %>%
    mutate(
      topic_manual_scale  = societal_actors + decision_authorities + policy_plan + actors_concerned,
      focus_manual_scale  = individual_societal_R + episodic_thematic_F,
      style_manual_scale  = personal_impersonal_R + emotional_unemotional_R,
      hardness_manual_scale = topic_manual_scale + focus_manual_scale + style_manual_scale
    )
}

df_m1 <- add_manual_scales(df_m1)
df_m2 <- add_manual_scales(df_m2)
df_m3 <- add_manual_scales(df_m3)

############################################################
# Spearman correlations: manual vs GPT Hardness
############################################################

spearman_hard <- list(
  M1_Headline         = cor.test(df_m1$hardness_manual_scale, df_m1$hardness_gpt_m1_scale, method = "spearman"),
  M2_fullcontent = cor.test(df_m2$hardness_manual_scale, df_m2$hardness_gpt_m2_scale, method = "spearman"),
  M3_100words    = cor.test(df_m3$hardness_manual_scale, df_m3$hardness_gpt_m3_scale, method = "spearman")
)

hardness_spearman_df <- data.frame(
  Metodo   = c("M1_Headline", "M2_fullcontent", "M3_100words"),
  rho      = c(
    unname(spearman_hard$M1_Headline$estimate),
    unname(spearman_hard$M2_fullcontent$estimate),
    unname(spearman_hard$M3_100words$estimate)
  ),
  p_value  = c(
    spearman_hard$M1_Headline$p.value,
    spearman_hard$M2_fullcontent$p.value,
    spearman_hard$M3_100words$p.value
  )
)

hardness_spearman_df

write_xlsx(
  hardness_spearman_df,
  path = file.path(results_dir, "correlazioni_hardness_spearman_metodi.xlsx")
)

############################################################
# Diagnostic binary Soft/Hard classification (appendix-only)
############################################################
#
# The following binary classification is derived from the
# continuous Hardness scale using a fixed cutoff. This step
# is implemented solely as a diagnostic robustness check.
#
# It is NOT part of the core analytical strategy of the thesis,
# which relies on continuous scale-based validation.
#
############################################################

cutoff <- 6

df_m1 <- df_m1 %>%
  mutate(
    hard_soft_manual   = if_else(hardness_manual_scale >= cutoff, 1L, 0L),
    hard_soft_gpt_m1   = if_else(hardness_gpt_m1_scale   >= cutoff, 1L, 0L)
  )

df_m2 <- df_m2 %>%
  mutate(
    hard_soft_manual   = if_else(hardness_manual_scale >= cutoff, 1L, 0L),
    hard_soft_gpt_m2   = if_else(hardness_gpt_m2_scale   >= cutoff, 1L, 0L)
  )

df_m3 <- df_m3 %>%
  mutate(
    hard_soft_manual   = if_else(hardness_manual_scale >= cutoff, 1L, 0L),
    hard_soft_gpt_m3   = if_else(hardness_gpt_m3_scale   >= cutoff, 1L, 0L)
  )

############################################################
# Confusion matrix and classification metrics
############################################################


confusion_metrics <- function(manual, model, metodo_label) {
  ok <- complete.cases(manual, model)
  manual <- manual[ok]
  model  <- model[ok]
  
  TP <- sum(manual == 1 & model == 1)
  TN <- sum(manual == 0 & model == 0)
  FP <- sum(manual == 0 & model == 1)
  FN <- sum(manual == 1 & model == 0)
  
  accuracy  <- (TP + TN) / (TP + TN + FP + FN)
  precision <- ifelse((TP + FP) == 0, NA, TP / (TP + FP))
  recall    <- ifelse((TP + FN) == 0, NA, TP / (TP + FN))
  F1        <- ifelse(is.na(precision) | is.na(recall) | (precision + recall) == 0,
                      NA,
                      2 * precision * recall / (precision + recall))
  
  data.frame(
    Metodo    = metodo_label,
    TP = TP, TN = TN, FP = FP, FN = FN,
    Accuracy  = accuracy,
    Precision = precision,
    Recall    = recall,
    F1        = F1
  )
}


############################################################
# Apply diagnostic classification to all methods
############################################################

metrics_m1 <- confusion_metrics(df_m1$hard_soft_manual, df_m1$hard_soft_gpt_m1, "M1_Headline")
metrics_m2 <- confusion_metrics(df_m2$hard_soft_manual, df_m2$hard_soft_gpt_m2, "M2_fullcontent")
metrics_m3 <- confusion_metrics(df_m3$hard_soft_manual, df_m3$hard_soft_gpt_m3, "M3_100words")

metrics_all <- bind_rows(metrics_m1, metrics_m2, metrics_m3)
metrics_all

write_xlsx(
  metrics_all,
  path = file.path(results_dir, "metriche_soft_hard_metodi.xlsx")
)

