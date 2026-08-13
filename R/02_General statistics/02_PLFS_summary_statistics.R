
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
informality_state_summary <- plfs |> 
  group_by(STATE) |> 
  summarise(
    mean_inf  = mean(s_casual_w_lf_PS_unw, na.rm = TRUE),
    sd_inf    = sd(s_casual_w_lf_PS_unw, na.rm = TRUE),
    min_inf   = min(s_casual_w_lf_PS_unw, na.rm = TRUE),
    max_inf   = max(s_casual_w_lf_PS_unw, na.rm = TRUE),
    range_inf = max_inf - min_inf,
    .groups = "drop"
  ) |> 
  arrange(desc(mean_inf))   # highest informality first

print(informality_state_summary)
informality_summary <- plfs |> 
  summarise(
    mean_inf  = mean(s_casual_w_lf_PS_unw, na.rm = TRUE),
    median_inf = median(s_casual_w_lf_PS_unw, na.rm = TRUE),
    sd_inf    = sd(s_casual_w_lf_PS_unw, na.rm = TRUE),
    min_inf   = min(s_casual_w_lf_PS_unw, na.rm = TRUE),
    max_inf   = max(s_casual_w_lf_PS_unw, na.rm = TRUE),
    count_inf   = n()
  )

# informality_summary <- informality_state_summary |> 
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
informality_rank_mean <- informality_state_summary |> 
  dplyr::select(STATE, mean_inf) |> 
  mutate(rank = rank(-mean_inf))

# Second order: highest variability in informality
informality_rank_sd <- informality_state_summary |> 
  dplyr::select(state_name, sd_inf) |> 
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


# Numerical summary statistics to include in the thesis:

library(dplyr)
library(knitr)
library(kableExtra)

# Step 1: get baseline informality (2017)
baseline_inf <- df_sections |> 
  filter(year == 2017) |> 
  dplyr::select(STATE, baseline_informality = s_casual_w_worker_PS_unw)

# Step 2: collapse to state level
summary_table <- df_sections |> 
  group_by(STATE) |> 
  summarise(
    mean_spei_neg   = mean(spei_negative, na.rm = TRUE),
    sd_spei     = sd(spei_negative,   na.rm = TRUE),
    mean_FI     = mean(FI_state,      na.rm = TRUE),
    sd_FI       = sd(FI_state,        na.rm = TRUE),
    mean_PR     = mean(pr_score,      na.rm = TRUE),
    sd_PR       = sd(pr_score,        na.rm = TRUE),
    population  = mean(state_pop,     na.rm = TRUE),
    agri_share  = mean(agri_share,    na.rm = TRUE)
  ) |> 
  left_join(baseline_inf, by = "STATE") |> 
  arrange(STATE)

# Step 3: print to console
print(summary_table, n = Inf)

# Step 4: export to LaTeX
summary_table |> 
  mutate(
    population = scales::comma(round(population)),
    across(c(mean_spei_neg, sd_spei, mean_FI, sd_FI, mean_PR, sd_PR,
             agri_share, baseline_informality),
           ~ round(., 3))
  ) |> 
  kbl(
    format   = "latex",
    booktabs = TRUE,
    caption  = "State-level summary statistics",
    label    = "tab:state_summary",
    col.names = c("State",
                  "Mean", "SD",       # SPEI
                  "Mean", "SD",       # FI
                  "Mean", "SD",       # PR
                  "Population",
                  "Agri. share",
                  "Baseline inf."),
    digits = 3,
    linesep = ""
  ) |> 
  add_header_above(c(" " = 1,
                     "SPEI" = 2,
                     "Flood index" = 2,
                     "Precip. index" = 2,
                     " " = 3)) |> 
  kable_styling(latex_options = c("hold_position", "scale_down"),
                font_size = 9) |> 
  save_kable("/Users/ninabilirossi/Desktop/MSC THESIS/state_summary_table.tex")


# ================ GENERAL STATISTICS (NOT STATE SPECIFIC) =====================
library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)

# ---- Variable definitions ----

vars <- c(
  # Climate indices
  "spei_spei12",
  "spei_negative",
  "FI_state",
  "pr_score",
  # Informality shares
  "s_casual_w_worker_PS_unw",
  "s_casual_w_worker_PS_f_unw",
  "s_casual_w_worker_PS_m_unw",
  "s_casual_w_worker_PS_rur_unw",
  "s_casual_w_worker_PS_urb_unw",
  # Labor force shares
  "lf_share_unw",
  "lf_share_m_unw",
  "lf_share_f_unw",
  "lf_share_urb_unw",
  "lf_share_rur_unw",
  # Worker shares
  "worker_share_unw",
  "worker_share_m_unw",
  "worker_share_f_unw",
  "worker_share_urb_unw",
  "worker_share_rur_unw"
)

labels <- c(
  # Climate indices
  "SPEI-12",
  "SPEI-12 (negative only)",
  "Flood index",
  "Extreme precipitation",
  # Informality shares
  "Informality share",
  "Informality share (female)",
  "Informality share (male)",
  "Informality share (rural)",
  "Informality share (urban)",
  # Labor force shares
  "Labor force participation rate",
  "Labor force participation rate (male)",
  "Labor force participation rate (female)",
  "Labor force participation rate (urban)",
  "Labor force participation rate (rural)",
  # Worker shares
  "Worker share",
  "Worker share (male)",
  "Worker share (female)",
  "Worker share (urban)",
  "Worker share (rural)"
)

# ---- Compute summary statistics ----

summary_stats <- data |> 
  summarise(across(all_of(vars), list(
    n    = ~ sum(!is.na(.)),
    mean = ~ mean(., na.rm = TRUE),
    sd   = ~ sd(.,   na.rm = TRUE),
    min  = ~ min(.,  na.rm = TRUE),
    max  = ~ max(.,  na.rm = TRUE)
  ))) |> 
  pivot_longer(everything(),
               names_to  = c("variable", "stat"),
               names_sep = "_(?=[^_]+$)") |> 
  pivot_wider(names_from = stat, values_from = value) |> 
  mutate(Variable = labels) |> 
  dplyr::select(Variable, n, mean, sd, min, max)

# ---- Print to console ----
print(summary_stats, n = Inf)

# ---- Export to LaTeX ----
summary_stats |> 
  mutate(
    n    = as.integer(n),
    across(c(mean, sd, min, max), ~ round(., 3))
  ) |> 
  kbl(
    format    = "latex",
    booktabs  = TRUE,
    caption   = "Summary statistics (pooled state-year observations)",
    label     = "tab:sumstats",
    col.names = c("Variable", "N", "Mean", "SD", "Min", "Max"),
    linesep   = ""
  ) |> 
  kable_styling(latex_options = "hold_position") |> 
  pack_rows("Climate indices",       1,  4) |> 
  pack_rows("Informality shares",    5,  9) |> 
  pack_rows("Labor force share",    10, 14) |> 
  pack_rows("Worker share",         15, 19) |> 
  save_kable("/Users/ninabilirossi/Desktop/MSC THESIS/CUTE_stats.tex")


