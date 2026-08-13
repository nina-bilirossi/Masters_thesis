
# ==============================================================================
# This script makes general summary statistics for the PLFS data
# The files 03 and 04 provide more detailed insights for group-specific trends 
# (gender, rural/urban, general population) and baseline labor market indicators

# ==============================================================================

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)
library(ggplot2)
library(patchwork)
library(ggrepel)


india <- st_read("~/Desktop/MSC THESIS/Data works/Code/Data/geography/State_District_Sub-district_Boundary_of_entire_India/State Boundary.shp")

# df for informality rates: 
# first order: which states have the highest informality rates?
# second order: do all states have the same variability in informality rates?

# ============================================================
# 2. PLFS Informality — Summary Statistics
# ============================================================
# Assumes columns: state, year, informality_rate
# Adjust column names as needed

plfs <- plfs_deep

# --- 2a. State-level summary ---
informality_state_summary <- plfs %>%
  group_by(STATE) %>%
  summarise(
    mean_inf  = mean(s_casual_w_lf_PS_unw, na.rm = TRUE),
    sd_inf    = sd(s_casual_w_lf_PS_unw, na.rm = TRUE),
    min_inf   = min(s_casual_w_lf_PS_unw, na.rm = TRUE),
    max_inf   = max(s_casual_w_lf_PS_unw, na.rm = TRUE),
    range_inf = max_inf - min_inf,
    .groups = "drop"
  ) %>%
  arrange(desc(mean_inf))   # highest informality first

print(informality_state_summary)
informality_summary <- plfs %>%
  summarise(
    mean_inf  = mean(s_casual_w_lf_PS_unw, na.rm = TRUE),
    median_inf = median(s_casual_w_lf_PS_unw, na.rm = TRUE),
    sd_inf    = sd(s_casual_w_lf_PS_unw, na.rm = TRUE),
    min_inf   = min(s_casual_w_lf_PS_unw, na.rm = TRUE),
    max_inf   = max(s_casual_w_lf_PS_unw, na.rm = TRUE),
    count_inf   = n()
  )

# informality_summary <- informality_state_summary %>%
#   summarise(
#     mean_inf  = mean(mean_inf, na.rm = TRUE),
#     sd_inf    = mean(sd_inf, na.rm = TRUE),
#     min_inf   = min(min_inf, na.rm = TRUE),
#     max_inf   = max(max_inf, na.rm = TRUE),
#     range_inf = max(max_inf) - min(min_inf)
#   )
print(informality_summary)

# --- 2b. Ranked lists ---
# First order: highest average informality
informality_rank_mean <- informality_state_summary %>%
  dplyr::select(STATE, mean_inf) %>%
  mutate(rank = rank(-mean_inf))

# Second order: highest variability in informality
informality_rank_sd <- informality_state_summary %>%
  dplyr::select(state_name, sd_inf) %>%
  mutate(rank = rank(-sd_inf))

print(informality_rank_mean)
print(informality_rank_sd)

# ============================================================
# 2. PLFS Informality — Plots
# ============================================================

# --- Plot D: Ranked mean informality by state ---
ggplot(informality_state_summary, aes(x = reorder(STATE, mean_inf), y = mean_inf)) +
  geom_col(fill = "tomato") +
  coord_flip() +
  labs(
    title = "Mean Informality Rate by State",
    x = NULL, y = "Mean Informality Rate"
  ) +
  theme_minimal()

# --- Plot E: Variability — SD of informality by state ---
ggplot(informality_state_summary, aes(x = reorder(STATE, sd_inf), y = sd_inf)) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  labs(
    title = "Variability in Informality Rate by State (SD)",
    x = NULL, y = "SD of Informality Rate"
  ) +
  theme_minimal()

# --- Plot F: Boxplot — distribution of informality per state over years ---
ggplot(plfs, aes(x = reorder(state_name, s_casual_w_lf_PS_unw, FUN = median), y = s_casual_w_lf_PS_unw)) +
  geom_boxplot(fill = "salmon", outlier.size = 1) +
  coord_flip() +
  labs(
    title = "State-Level Distribution of Informality Rate",
    x = NULL, y = "Informality Rate"
  ) +
  theme_minimal()

# --- Plot G: Scatter — mean vs SD (is high informality also more volatile?) ---
ggplot(informality_state_summary, aes(x = mean_inf, y = sd_inf, label = STATE)) +
  geom_point(colour = "tomato", size = 2) +
  ggrepel::geom_text_repel(size = 3) +    # install ggrepel if needed
  labs(
    title = "Mean vs Variability of Informality by State",
    x = "Mean Informality Rate", y = "SD of Informality Rate"
  ) +
  theme_minimal()
