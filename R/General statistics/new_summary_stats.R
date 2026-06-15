library(dplyr)
library(knitr)
library(kableExtra)

# Step 1: get baseline informality (2017)
baseline_inf <- df_sections %>%
  filter(year == 2017) %>%
  select(STATE, baseline_informality = s_casual_w_worker_PS_unw)

# Step 2: collapse to state level
summary_table <- df_sections %>%
  group_by(STATE) %>%
  summarise(
    mean_spei_neg   = mean(spei_negative, na.rm = TRUE),
    sd_spei     = sd(spei_negative,   na.rm = TRUE),
    mean_FI     = mean(FI_state,      na.rm = TRUE),
    sd_FI       = sd(FI_state,        na.rm = TRUE),
    mean_PR     = mean(pr_score,      na.rm = TRUE),
    sd_PR       = sd(pr_score,        na.rm = TRUE),
    population  = mean(state_pop,     na.rm = TRUE),
    agri_share  = mean(agri_share,    na.rm = TRUE)
  ) %>%
  left_join(baseline_inf, by = "STATE") %>%
  arrange(STATE)

# Step 3: print to console
print(summary_table, n = Inf)

# Step 4: export to LaTeX
summary_table %>%
  mutate(
    population = scales::comma(round(population)),
    across(c(mean_spei_neg, sd_spei, mean_FI, sd_FI, mean_PR, sd_PR,
             agri_share, baseline_informality),
           ~ round(., 3))
  ) %>%
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
  ) %>%
  add_header_above(c(" " = 1,
                     "SPEI" = 2,
                     "Flood index" = 2,
                     "Precip. index" = 2,
                     " " = 3)) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"),
                font_size = 9) %>%
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

summary_stats <- df_sections %>%
  summarise(across(all_of(vars), list(
    n    = ~ sum(!is.na(.)),
    mean = ~ mean(., na.rm = TRUE),
    sd   = ~ sd(.,   na.rm = TRUE),
    min  = ~ min(.,  na.rm = TRUE),
    max  = ~ max(.,  na.rm = TRUE)
  ))) %>%
  pivot_longer(everything(),
               names_to  = c("variable", "stat"),
               names_sep = "_(?=[^_]+$)") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(Variable = labels) %>%
  select(Variable, n, mean, sd, min, max)

# ---- Print to console ----
print(summary_stats, n = Inf)

# ---- Export to LaTeX ----
summary_stats %>%
  mutate(
    n    = as.integer(n),
    across(c(mean, sd, min, max), ~ round(., 3))
  ) %>%
  kbl(
    format    = "latex",
    booktabs  = TRUE,
    caption   = "Summary statistics (pooled state-year observations)",
    label     = "tab:sumstats",
    col.names = c("Variable", "N", "Mean", "SD", "Min", "Max"),
    linesep   = ""
  ) %>%
  kable_styling(latex_options = "hold_position") %>%
  pack_rows("Climate indices",       1,  4) %>%
  pack_rows("Informality shares",    5,  9) %>%
  pack_rows("Labor force share",    10, 14) %>%
  pack_rows("Worker share",         15, 19) %>%
  save_kable("/Users/ninabilirossi/Desktop/MSC THESIS/CUTE_stats.tex")
