# Make summary statistics for 
# 1. Labor data: state level informality distribution
# 2. SPEI data: state level distribution of SPEI12 (weighted and unweighted; is there a difference between the 2?) 

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)
library(ggplot2)
library(patchwork)

india <- st_read("~/Desktop/MSC THESIS/Data works/Code/Data/geography/State_District_Sub-district_Boundary_of_entire_India/State Boundary.shp")

# --- 1. SPEI ---

spei_weighted_long <- spei_yearly %>%
  pivot_longer(
    cols = starts_with("weighted_SPEI12_"),
    names_to = "year",
    names_prefix = "weighted_SPEI12_",
    values_to = "spei12"
  ) %>%
  mutate(year = as.integer(year), type = "Weighted") |> 
  filter(!grepl("disputed", STATE, ignore.case = TRUE))

spei_unweighted_long <- spei_yearly %>%
  pivot_longer(
    cols = starts_with("unweighted_SPEI12_"),
    names_to = "year",
    names_prefix = "unweighted_SPEI12_",
    values_to = "spei12"
  ) %>%
  mutate(year = as.integer(year), type = "Unweighted") |> 
  filter(!grepl("disputed", STATE, ignore.case = TRUE))

spei_combined <- bind_rows(spei_weighted_long, spei_unweighted_long)

boxplot_comparison_spei <- ggplot(spei_combined, 
       aes(x = reorder(STATE, spei12, FUN = median), 
           y = spei12, 
           fill = type)) +
  geom_boxplot(outlier.size = 0.8, alpha = 0.7) +
  scale_fill_manual(values = c("Weighted" = "steelblue", 
                               "Unweighted" = "tomato")) +
  coord_flip() +
  labs(
    title = "State-Level Distribution of SPEI12: Weighted vs Unweighted",
    x = NULL, y = "SPEI12",
    fill = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "top")


# df for SPEI: spei_yearly
# which states see the most variation? 
# the interyear variation higher in some states than others? 
# is there a difference between weighted and unweighted SPEI12?


# ============================================================
# 1. SPEI — Summary Statistics
# ============================================================

# --- 1a. State-level summary (mean, sd, min, max across years) ---
spei_state_summary <- spei_weighted_long %>%
  group_by(STATE) %>%
  summarise(
    mean_spei  = mean(spei12, na.rm = TRUE),
    sd_spei    = sd(spei12, na.rm = TRUE),
    min_spei   = min(spei12, na.rm = TRUE),
    max_spei   = max(spei12, na.rm = TRUE),
    range_spei = max_spei - min_spei,
    .groups = "drop"
  ) %>%
  arrange(desc(sd_spei))   # states with most inter-year variation first

print(spei_state_summary)

spei_summary <- spei_weighted_long %>%
  summarise(
    mean_inf  = mean(spei12, na.rm = TRUE),
    median_inf = median(spei12, na.rm = TRUE),
    sd_inf    = sd(spei12, na.rm = TRUE),
    min_inf   = min(spei12, na.rm = TRUE),
    max_inf   = max(spei12, na.rm = TRUE),
    count_inf   = n()
  )

print(spei_summary)

# --- 1b. Ranked: which states have highest inter-year variation? ---
spei_variation_rank <- spei_state_summary %>%
  dplyr::select(STATE, sd_spei, range_spei) %>%
  mutate(rank_sd = rank(-sd_spei))

print(spei_variation_rank)

# ============================================================
# 1. SPEI — Plots
# ============================================================

# --- Plot A: SD by state (inter-year variation ranking) ---
ggplot(spei_state_summary, aes(x = reorder(STATE, sd_spei), y = sd_spei)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Inter-Year Variation in SPEI12 by State",
    x = NULL, y = "SD of SPEI12 across years"
  ) +
  theme_minimal()

# --- Plot B: Distribution of SPEI12 per state (boxplot) ---
ggplot(spei_long_stats, aes(x = reorder(STATE, spei12, FUN = median), y = spei12)) +
  geom_boxplot(fill = "lightblue", outlier.size = 1) +
  coord_flip() +
  labs(
    title = "State-Level Distribution of SPEI12, weighted",
    x = NULL, y = "SPEI12"
  ) +
  theme_minimal()

# --- Plot C: Heatmap — SPEI12 by state × year ---
ggplot(spei_weighted_long, aes(x = year, y = reorder(STATE, spei12, FUN = mean), fill = spei12)) +
  geom_tile() +
  scale_fill_gradient2(low = "brown", mid = "white", high = "darkblue", midpoint = 0) +
  labs(
    title = "SPEI12 by State and Year",
    x = "Year", y = NULL, fill = "SPEI12"
  ) +
  theme_minimal()

# Physical map

india_map_data <- india %>%
  left_join(
    spei_state_summary %>% 
      dplyr::select(STATE, mean_spei, sd_spei) %>%
      left_join(spei_variation_rank %>% dplyr::select(STATE, rank_sd), by = "STATE"),
    by = c("STATE" = "STATE")
  ) |>  filter(!grepl("disputed", STATE, ignore.case = TRUE))

unmatched <- india_map_data %>% 
  filter(is.na(sd_spei)) %>% 
  pull(STATE)
print(unmatched)

# mean spei
map_mean <- ggplot(india_map_data) +
  geom_sf(aes(fill = mean_spei), color = "white", linewidth = 0.3) +
  geom_sf_text(aes(label = STATE), size = 2, color = "black") + # state names
  scale_fill_gradient2(
    low      = "brown",       # dry / negative SPEI
    mid      = "lightyellow",
    high     = "darkblue",    # wet / positive SPEI
    midpoint = 0,
    na.value = "grey80",
    name     = "Mean SPEI12"
  ) +
  labs(
    title    = "Average SPEI12 by State",
    subtitle = "Brown = drier, Blue = wetter on average"
  ) +
  theme_void() +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 9),
    legend.position = "right"
  )
map_mean

# spei variation
map_variation <- ggplot(india_map_data) +
  geom_sf(aes(fill = sd_spei), color = "white", linewidth = 0.3) +
  scale_fill_gradient(
    low  = "lightyellow",
    high = "darkblue",
    na.value = "grey80",        # unmatched states show as grey
    name = "SD of SPEI12"
  ) +
  labs(
    title    = "Inter-Year Variation in SPEI12 by Indian State",
    subtitle = "Darker = higher year-to-year variability in drought index",
    caption  = "Source: spei_variation_rank"
  ) +
  theme_void() +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10),
    legend.position = "right"
  )

# --- 2. PLFS ---

# df for informality rates: 
# first order: which states have the highest informality rates?
# second order: do all states have the same variability in informality rates?


# ============================================================
# 2. PLFS Informality — Summary Statistics
# ============================================================
# Assumes columns: state, year, informality_rate
# Adjust column names as needed

# --- 2a. State-level summary ---
informality_state_summary <- plfs %>%
  group_by(state_name) %>%
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
  dplyr::select(state_name, mean_inf) %>%
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
ggplot(informality_state_summary, aes(x = reorder(state_name, mean_inf), y = mean_inf)) +
  geom_col(fill = "tomato") +
  coord_flip() +
  labs(
    title = "Mean Informality Rate by State",
    x = NULL, y = "Mean Informality Rate"
  ) +
  theme_minimal()

# --- Plot E: Variability — SD of informality by state ---
ggplot(informality_state_summary, aes(x = reorder(state_name, sd_inf), y = sd_inf)) +
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
ggplot(informality_state_summary, aes(x = mean_inf, y = sd_inf, label = state_name)) +
  geom_point(colour = "tomato", size = 2) +
  ggrepel::geom_text_repel(size = 3) +    # install ggrepel if needed
  labs(
    title = "Mean vs Variability of Informality by State",
    x = "Mean Informality Rate", y = "SD of Informality Rate"
  ) +
  theme_minimal()

install.packages("ggrepel")
library(ggrepel)

# ============================================================
# SPEI12 over time — one line per state
# ============================================================

ggplot(spei_weighted_long, aes(x = year, y = spei12, group = STATE, color = STATE)) +
  geom_line(linewidth = 0.6, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  labs(
    title    = "SPEI12 over Time by State",
    subtitle = "Dashed line = 0 (neutral); below = drier, above = wetter",
    x        = "Year",
    y        = "SPEI12",
    color    = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

ggplot(spei_weighted_long, aes(x = year, y = spei12, group = STATE)) +
  geom_line(linewidth = 0.5, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  facet_wrap(~ STATE, ncol = 6) +
  labs(
    title = "SPEI12 over Time by State",
    x = "Year", y = "SPEI12"
  ) +
  theme_minimal() +
  theme(
    axis.text.x  = element_text(size = 6, angle = 45, hjust = 1),
    axis.text.y  = element_text(size = 6),
    strip.text   = element_text(size = 7, face = "bold"),
    panel.grid.minor = element_blank()
  )
