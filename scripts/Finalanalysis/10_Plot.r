############################################################
# Convergent validity: Manual vs GPT Hardness scores
# Comparison across three LLM input methods
############################################################

library(ggplot2)
library(dplyr)

# === Build unified long-format dataset ===

df_plot <- bind_rows(
  df_m1 %>% 
    select(hardness_manual_scale, hardness_gpt_m1_scale) %>% 
    rename(hardness_gpt = hardness_gpt_m1_scale) %>%
    mutate(method = "M1 – Headline only"),
  
  df_m2 %>% 
    select(hardness_manual_scale, hardness_gpt_m2_scale) %>% 
    rename(hardness_gpt = hardness_gpt_m2_scale) %>%
    mutate(method = "M2 – Full content"),
  
  df_m3 %>% 
    select(hardness_manual_scale, hardness_gpt_m3_scale) %>% 
    rename(hardness_gpt = hardness_gpt_m3_scale) %>%
    mutate(method = "M3 – First 100 words")
)

df_plot <- df_plot %>%
  filter(!is.na(hardness_manual_scale), !is.na(hardness_gpt))

# === Color palette ===

palette_methods <- c(
  "M1 – Headline only"   = "#1f77b4",
  "M2 – Full content"   = "#d62728",
  "M3 – First 100 words"= "#2ca02c"
)

# === Final plot ===

p <- ggplot(df_plot, aes(
  x = hardness_manual_scale,
  y = hardness_gpt,
  color = method
)) +
  geom_jitter(
    alpha = 0.45,
    width = 0.15,
    height = 0.15,
    size = 1.2
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    size = 1.3,
    alpha = 0.18
  ) +
  geom_abline(
    intercept = 0,
    slope = 1,
    color = "black",
    linetype = "dashed"
  ) +
  scale_color_manual(values = palette_methods) +
  labs(
    title = "Convergent validity of Hardness scores",
    subtitle = "Manual coding vs LLM-based coding across three input methods",
    x = "Manual Hardness score (0–12)",
    y = "GPT Hardness score",
    color = "LLM method"
  ) +
  theme_minimal(base_size = 17) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

print(p)

############################################################
# Save final figure (landscape format)
############################################################

output_path_graph <- file.path(
  "/Users/alice.dorio/Desktop/data/codifiche_completate/RISULTATI",
  "hardness_convergent_validity_llm_methods.png"
)

ggsave(
  filename = output_path_graph,
  plot = p,
  width = 16,   # landscape
  height = 9,   # landscape
  dpi = 300
)

message("Figure saved to: ", output_path_graph)