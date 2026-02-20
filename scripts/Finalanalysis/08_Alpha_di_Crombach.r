############################################################
# SCALE RELIABILITY VALIDATION (CRONBACH'S ALPHA)
# Manual coding and LLM-based coding (Methods 1–3)
############################################################


install.packages("readxl")
install.packages("psych")
install.packages("dplyr")
install.packages("writexl")

library(readxl)
library(psych)
library(dplyr)
library(writexl)

############################################################
# Data cleaning function for coding variables
# Values outside the admissible range (0–2) are set to NA
############################################################

clean_coding_vars <- function(df) {
  coding_cols_all <- c(
    "societal_actors", "societal_actors_gold",
    "decision_authorities", "decision_authorities_gold",
    "policy_plan", "policy_plan_gold",
    "actors_concerned", "actors_concerned_gold",
    "individual_societal_R", "individual_societal_R_gold",
    "episodic_thematic_F", "episodic_thematic_F_gold",
    "personal_impersonal_R", "personal_impersonal_R_gold",
    "emotional_unemotional_R", "emotional_unemotional_R_gold"
  )
  
  coding_cols <- intersect(coding_cols_all, names(df))
  
  df %>%
    mutate(
      across(
        all_of(coding_cols),
        ~ {
          x <- suppressWarnings(as.numeric(.))
          x[!is.na(x) & !(x %in% 0:2)] <- NA
          x
        }
      )
    )
}

############################################################
# METHOD 1 – Manual coding: scale reliability
############################################################

file_path_m1 <- "/Users/alice.dorio/Desktop/data/codifiche_completate/METODO1_HeadlineOnly/CODIFICA/risultati_codifica_semplice.xlsx"

df_m1 <- read_excel(file_path_m1)
df_m1 <- clean_coding_vars(df_m1)

colnames(df_m1)

# Definition of scale components (manual coding)

topic_manual_vars <- c(
  "societal_actors",
  "decision_authorities",
  "policy_plan",
  "actors_concerned"
)

focus_manual_vars <- c(
  "individual_societal_R",
  "episodic_thematic_F"
)

style_manual_vars <- c(
  "personal_impersonal_R",
  "emotional_unemotional_R"
)

topic_manual_df <- df_m1 %>% select(all_of(topic_manual_vars))
focus_manual_df <- df_m1 %>% select(all_of(focus_manual_vars))
style_manual_df <- df_m1 %>% select(all_of(style_manual_vars))

str(topic_manual_df)
str(focus_manual_df)
str(style_manual_df)

alpha_topic_manual <- psych::alpha(topic_manual_df)
alpha_focus_manual <- psych::alpha(focus_manual_df)
alpha_style_manual <- psych::alpha(style_manual_df)

alpha_topic_manual
alpha_focus_manual
alpha_style_manual

# Summary of Cronbach’s alpha (manual coding)

alpha_manual_summary <- data.frame(
  dimension = c("Topic", "Focus", "Style"),
  alpha_raw = c(
    alpha_topic_manual$total$raw_alpha,
    alpha_focus_manual$total$raw_alpha,
    alpha_style_manual$total$raw_alpha
  ),
  alpha_std = c(
    alpha_topic_manual$total$std.alpha,
    alpha_focus_manual$total$std.alpha,
    alpha_style_manual$total$std.alpha
  ),
  average_r = c(
    alpha_topic_manual$total$average_r,
    alpha_focus_manual$total$average_r,
    alpha_style_manual$total$average_r
  )
)

alpha_manual_summary$ci_lower <- c(
  alpha_topic_manual$ci[1],
  alpha_focus_manual$ci[1],
  alpha_style_manual$ci[1]
)

alpha_manual_summary$ci_upper <- c(
  alpha_topic_manual$ci[3],
  alpha_focus_manual$ci[3],
  alpha_style_manual$ci[3]
)

results_dir <- "/Users/alice.dorio/Desktop/data/codifiche_completate/RISULTATI"
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)

write_xlsx(
  alpha_manual_summary,
  path = file.path(results_dir, "alpha_codifica_manual_dimensions.xlsx")
)

############################################################
# METHOD 1 – GPT coding (headline-only): scale reliability
############################################################

topic_gpt_m1_vars <- c(
  "societal_actors_gold",
  "decision_authorities_gold",
  "policy_plan_gold",
  "actors_concerned_gold"
)

focus_gpt_m1_vars <- c(
  "individual_societal_R_gold",
  "episodic_thematic_F_gold"
)

style_gpt_m1_vars <- c(
  "personal_impersonal_R_gold",
  "emotional_unemotional_R_gold"
)

topic_gpt_m1_df <- df_m1 %>% select(all_of(topic_gpt_m1_vars))
focus_gpt_m1_df <- df_m1 %>% select(all_of(focus_gpt_m1_vars))
style_gpt_m1_df <- df_m1 %>% select(all_of(style_gpt_m1_vars))

alpha_topic_gpt_m1 <- psych::alpha(topic_gpt_m1_df)
alpha_focus_gpt_m1 <- psych::alpha(focus_gpt_m1_df)
alpha_style_gpt_m1 <- psych::alpha(style_gpt_m1_df)

alpha_topic_gpt_m1
alpha_focus_gpt_m1
alpha_style_gpt_m1

alpha_gpt_m1_summary <- data.frame(
  dimension = c("Topic", "Focus", "Style"),
  alpha_raw = c(
    alpha_topic_gpt_m1$total$raw_alpha,
    alpha_focus_gpt_m1$total$raw_alpha,
    alpha_style_gpt_m1$total$raw_alpha
  ),
  alpha_std = c(
    alpha_topic_gpt_m1$total$std.alpha,
    alpha_focus_gpt_m1$total$std.alpha,
    alpha_style_gpt_m1$total$std.alpha
  ),
  average_r = c(
    alpha_topic_gpt_m1$total$average_r,
    alpha_focus_gpt_m1$total$average_r,
    alpha_style_gpt_m1$total$average_r
  )
)

write_xlsx(
  alpha_gpt_m1_summary,
  path = file.path(results_dir, "alpha_codifica_gpt_metodo1.xlsx")
)

############################################################
# METHOD 2 – GPT coding (full content): scale reliability
############################################################

file_path_m2 <- "/Users/alice.dorio/Desktop/data/codifiche_completate/METODO2_contenuto/CODIFICA/Codifca_senzacontenutoesplicito_peranalisi.xlsx"

df_m2 <- read_excel(file_path_m2)
df_m2 <- clean_coding_vars(df_m2)

colnames(df_m2)
str(df_m2)

topic_gpt_m2_vars <- c(
  "societal_actors_gold",
  "decision_authorities_gold",
  "policy_plan_gold",
  "actors_concerned_gold"
)

focus_gpt_m2_vars <- c(
  "individual_societal_R_gold",
  "episodic_thematic_F_gold"
)

style_gpt_m2_vars <- c(
  "personal_impersonal_R_gold",
  "emotional_unemotional_R_gold"
)

topic_gpt_m2_df <- df_m2 %>% select(all_of(topic_gpt_m2_vars))
focus_gpt_m2_df <- df_m2 %>% select(all_of(focus_gpt_m2_vars))
style_gpt_m2_df <- df_m2 %>% select(all_of(style_gpt_m2_vars))

alpha_topic_gpt_m2 <- psych::alpha(topic_gpt_m2_df)
alpha_focus_gpt_m2 <- psych::alpha(focus_gpt_m2_df)
alpha_style_gpt_m2 <- psych::alpha(style_gpt_m2_df)

alpha_topic_gpt_m2
alpha_focus_gpt_m2
alpha_style_gpt_m2

alpha_gpt_m2_summary <- data.frame(
  dimension = c("Topic", "Focus", "Style"),
  alpha_raw = c(
    alpha_topic_gpt_m2$total$raw_alpha,
    alpha_focus_gpt_m2$total$raw_alpha,
    alpha_style_gpt_m2$total$raw_alpha
  ),
  alpha_std = c(
    alpha_topic_gpt_m2$total$std.alpha,
    alpha_focus_gpt_m2$total$std.alpha,
    alpha_style_gpt_m2$total$std.alpha
  ),
  average_r = c(
    alpha_topic_gpt_m2$total$average_r,
    alpha_focus_gpt_m2$total$average_r,
    alpha_style_gpt_m2$total$average_r
  )
)

write_xlsx(
  alpha_gpt_m2_summary,
  path = file.path(results_dir, "alpha_codifica_gpt_metodo2_contenuto.xlsx")
)

############################################################
# METHOD 3 – GPT coding (first 100 words): scale reliability
############################################################

file_path_m3 <- "/Users/alice.dorio/Desktop/data/codifiche_completate/METODO3_contenuto_100paroleOnly/CODIFICA/codifica_100parole.xlsx"

df_m3 <- read_excel(file_path_m3)
df_m3 <- clean_coding_vars(df_m3)

colnames(df_m3)
str(df_m3)

topic_gpt_m3_vars <- c(
  "societal_actors_gold",
  "decision_authorities_gold",
  "policy_plan_gold",
  "actors_concerned_gold"
)

focus_gpt_m3_vars <- c(
  "individual_societal_R_gold",
  "episodic_thematic_F_gold"
)

style_gpt_m3_vars <- c(
  "personal_impersonal_R_gold",
  "emotional_unemotional_R_gold"
)

topic_gpt_m3_df <- df_m3 %>% select(all_of(topic_gpt_m3_vars))
focus_gpt_m3_df <- df_m3 %>% select(all_of(focus_gpt_m3_vars))
style_gpt_m3_df <- df_m3 %>% select(all_of(style_gpt_m3_vars))

alpha_topic_gpt_m3 <- psych::alpha(topic_gpt_m3_df)
alpha_focus_gpt_m3 <- psych::alpha(focus_gpt_m3_df)
alpha_style_gpt_m3 <- psych::alpha(style_gpt_m3_df)

alpha_topic_gpt_m3
alpha_focus_gpt_m3
alpha_style_gpt_m3

alpha_gpt_m3_summary <- data.frame(
  dimension = c("Topic", "Focus", "Style"),
  alpha_raw = c(
    alpha_topic_gpt_m3$total$raw_alpha,
    alpha_focus_gpt_m3$total$raw_alpha,
    alpha_style_gpt_m3$total$raw_alpha
  ),
  alpha_std = c(
    alpha_topic_gpt_m3$total$std.alpha,
    alpha_focus_gpt_m3$total$std.alpha,
    alpha_style_gpt_m3$total$std.alpha
  ),
  average_r = c(
    alpha_topic_gpt_m3$total$average_r,
    alpha_focus_gpt_m3$total$average_r,
    alpha_style_gpt_m3$total$average_r
  )
)

write_xlsx(
  alpha_gpt_m3_summary,
  path = file.path(results_dir, "alpha_codifica_gpt_metodo3_100parole.xlsx")
)

df_m1 <- clean_coding_vars(df_m1)
df_m2 <- clean_coding_vars(df_m2)
df_m3 <- clean_coding_vars(df_m3)

