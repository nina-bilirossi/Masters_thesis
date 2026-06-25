library(dplyr)
library(tidyr)
library(knitr)

# 1. Overall Mean and SE for the entire dataset
overall_stats <- data %>%
  summarise(across(
    c(spei_negative, FI_state, pr_score),
    list(
      mean = ~ mean(.x, na.rm = TRUE),
      se   = ~ sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x))),
      min  = ~ min(.x, na.rm = TRUE),
      max  = ~ max(.x, na.rm = TRUE)
    ),
    .names = "{.col}_{.fn}"
  )) %>%
  pivot_longer(
    everything(),
    names_to = c("Variable", ".value"),
    names_pattern = "(.*)_(mean|se|min|max)"
  ) %>%
  mutate(agri_group = "Overall")

# 2. Group-specific Mean, SE, Min, and Max (High vs Low Agri)
group_stats <- df_sections_agri %>%
  filter(agri_group %in% c("High agri", "Low agri")) %>% 
  group_by(agri_group) %>%
  summarise(across(
    c(spei_negative, FI_state, pr_score),
    list(
      mean = ~ mean(.x, na.rm = TRUE),
      se   = ~ sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x))),
      min  = ~ min(.x, na.rm = TRUE),
      max  = ~ max(.x, na.rm = TRUE)
    ),
    .names = "{.col}_{.fn}"
  )) %>%
  pivot_longer(
    cols = -agri_group,
    names_to = c("Variable", ".value"),
    names_pattern = "(.*)_(mean|se|min|max)"
  )

# 3. Combine both and format into LaTeX
summary_table <- bind_rows(overall_stats, group_stats) %>%
  dplyr::select(agri_group, Variable, mean, se, min, max) %>%
  arrange(desc(agri_group), Variable)

# Generate Clean LaTeX Code
latex_output <- kable(
  summary_table, 
  format = "latex", 
  booktabs = TRUE, 
  digits = 3, # Rounds all numeric columns to 3 decimal places
  col.names = c("Group", "Variable", "Mean", "Std. Error", "Min", "Max"),
  caption = "Summary Statistics for Climate Shocks and State Scores",
  label = "tab:summary_stats"
)

# Print the code to your R console so you can copy/paste it into your .tex document
cat(latex_output)


library(ggplot2)
library(dplyr)

# 1. Input the summary statistics from your dataset
plot_data <- data.frame(
  Group = c("Overall", "Overall", "Overall", "Low agri", "Low agri", "Low agri", "High agri", "High agri", "High agri"),
  Variable = c("FI_state", "pr_score", "spei_negative", "FI_state", "pr_score", "spei_negative", "FI_state", "pr_score", "spei_negative"),
  Mean = c(0.276, 0.325, -0.388, 0.255, 0.277, -0.496, 0.324, 0.265, -0.330),
  SE = c(0.021, 0.034, 0.041, 0.031, 0.033, 0.076, 0.032, 0.030, 0.057)
)

# 2. Calculate 95% Confidence Intervals & lock factor ordering
plot_data <- plot_data %>%
  mutate(
    ci_lower = Mean - 1.96 * SE,
    ci_upper = Mean + 1.96 * SE,
    Group = factor(Group, levels = c("Overall", "Low agri", "High agri"))
  )

# 3. Create the whisker plot
ggplot(plot_data, aes(x = Group, y = Mean, color = Group)) +
  geom_point(size = 3.5) + # Draws the center mean point
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, size = 0.8) + # Draws the whiskers
  facet_wrap(~ Variable, scales = "free_y") + # Separates metrics into clean individual panels
  scale_color_manual(values = c("Overall" = "#4A4A4A", "Low agri" = "#1F77B4", "High agri" = "#FF7F0E")) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Climate Shocks and State Scores Across Agricultural Groups",
    subtitle = "Whiskers represent 95% Confidence Intervals (Mean ± 1.96 * SE)",
    y = "Estimated Score / Value",
    x = ""
  ) +
  theme(
    strip.text = element_text(face = "bold", size = 12), # Bold panel headers
    panel.grid.minor = element_blank(),
    legend.position = "none" # Subtitles and x-axis make a legend redundant
  )


t.test(spei_negative ~ agri_group, data = df_sections_agri)
