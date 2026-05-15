library(tidyverse)
library(dplyr)

plfs_deep <- read.csv('/Users/ninabilirossi/Desktop/MSC THESIS/Data works/My data prep/PLFS processed/deeper-PLFS_all_s_N_unw.csv')

# ── 1. Check structure ────────────────────────────────────────────────────────
glimpse(plfs_deep)

# Identify share variables
share_vars <- names(plfs_deep)[grepl("^(lf_share|worker_share)", names(plfs_deep))]
cat("Share variables found:\n"); print(share_vars)

inf_vars <- names(plfs_deep)[grepl("^(s_casual)", names(plfs_deep))]
cat("Informality variables found:\n"); print(inf_vars)


# ── 2. Basic descriptives (overall) ──────────────────────────────────────────
plfs_deep |>
  select(all_of(share_vars)) |>
  summary() |>
  print()

# More detailed: mean, sd, min, max per variable
plfs_deep |>
  select(all_of(share_vars)) |>
  pivot_longer(everything(), names_to = "variable", values_to = "value") |>
  group_by(variable) |>
  summarise(
    n       = sum(!is.na(value)),
    mean    = mean(value, na.rm = TRUE),
    sd      = sd(value,   na.rm = TRUE),
    min     = min(value,  na.rm = TRUE),
    max     = max(value,  na.rm = TRUE),
    .groups = "drop"
  ) |>
  print(n = Inf)

# ── 3. By state ───────────────────────────────────────────────────────────────
plfs_deep |>
  select(state_name, all_of(share_vars)) |>
  pivot_longer(-state_name, names_to = "variable", values_to = "value") |>
  group_by(state_name, variable) |>
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd   = sd(value,   na.rm = TRUE),
    min  = min(value,  na.rm = TRUE),
    max  = max(value,  na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(variable, state_name) |>
  print(n = Inf)

# ── 4. Graphs: share evolution over time, by state ───────────────────────────
# Reshape to long for plotting
plfs_long <- plfs_deep |>
  select(state_name, time, all_of(share_vars)) |>
  pivot_longer(-c(state_name, time), names_to = "variable", values_to = "value")

# One plot per share variable, all states overlaid
for (var in share_vars) {
  p <- plfs_long |>
    filter(variable == var) |>
    ggplot(aes(x = time, y = value, group = state_name, color = state_name)) +
    geom_line() +
    geom_point(size = 1.5) +
    labs(
      title = paste("Evolution of", var, "by state"),
      x     = "Year",
      y     = var,
      color = "State"
    ) +
    theme_minimal() +
    theme(legend.position = "right",
          legend.text     = element_text(size = 7),
          axis.text.x     = element_text(angle = 45, hjust = 1))
  
  print(p)
}

# ── 5. Population-weighted India aggregate ────────────────────────────────────

# We need pop_tot_unw for lf_share (overall) and worker_share (overall)
# and pop_x_unw for the subgroup shares
# Assuming these are in the dataset; adjust names if needed

plfs_india <- plfs_deep |>
  group_by(time) |>
  summarise(
    lf_share_unw         = weighted.mean(lf_share_unw,         pop_workingage_unw, na.rm = TRUE),
    lf_share_m_unw       = weighted.mean(lf_share_m_unw,       pop_m_unw,   na.rm = TRUE),
    lf_share_f_unw       = weighted.mean(lf_share_f_unw,       pop_f_unw,   na.rm = TRUE),
    lf_share_urb_unw     = weighted.mean(lf_share_urb_unw,     pop_urb_unw, na.rm = TRUE),
    lf_share_rur_unw     = weighted.mean(lf_share_rur_unw,     pop_rur_unw, na.rm = TRUE),
    worker_share_unw     = weighted.mean(worker_share_unw,     pop_workingage_unw, na.rm = TRUE),
    worker_share_m_unw   = weighted.mean(worker_share_m_unw,   pop_m_unw,   na.rm = TRUE),
    worker_share_f_unw   = weighted.mean(worker_share_f_unw,   pop_f_unw,   na.rm = TRUE),
    worker_share_urb_unw = weighted.mean(worker_share_urb_unw, pop_urb_unw, na.rm = TRUE),
    worker_share_rur_unw = weighted.mean(worker_share_rur_unw, pop_rur_unw, na.rm = TRUE),
     s_casual_w_lf_PS_unw     = weighted.mean(s_casual_w_lf_PS_unw,     pop_workingage_unw, na.rm = TRUE),
     s_casual_w_lf_PS_m_unw   = weighted.mean(s_casual_w_lf_PS_m_unw,   pop_m_unw,   na.rm = TRUE),
     s_casual_w_lf_PS_f_unw   = weighted.mean(s_casual_w_lf_PS_f_unw,   pop_f_unw,   na.rm = TRUE),
     s_casual_w_lf_PS_urb_unw = weighted.mean(s_casual_w_lf_PS_urb_unw, pop_urb_unw, na.rm = TRUE),
     s_casual_w_lf_PS_rur_unw = weighted.mean(s_casual_w_lf_PS_rur_unw, pop_rur_unw, na.rm = TRUE),
    s_casual_w_worker_PS_unw     = weighted.mean(s_casual_w_worker_PS_unw,     worker_PS_unw, na.rm = TRUE),
    s_casual_w_worker_PS_m_unw   = weighted.mean(s_casual_w_worker_PS_m_unw,   worker_PS_m_unw,   na.rm = TRUE),
    s_casual_w_worker_PS_f_unw   = weighted.mean(s_casual_w_worker_PS_f_unw,   worker_PS_f_unw,   na.rm = TRUE),
    s_casual_w_worker_PS_urb_unw = weighted.mean(s_casual_w_worker_PS_urb_unw, worker_PS_urb_unw, na.rm = TRUE),
    s_casual_w_worker_PS_rur_unw = weighted.mean(s_casual_w_worker_PS_rur_unw, worker_PS_rur_unw, na.rm = TRUE),
    .groups = "drop"
  )

print(plfs_india)

# ── 6. Graphs: India-level weighted shares over time ─────────────────────────

# -- LF shares (5 lines) ------------------------------------------------------
plfs_india |>
  pivot_longer(
    cols      = starts_with("lf_share"),
    names_to  = "variable",
    values_to = "value"
  ) |>
  mutate(variable = dplyr::recode(variable,
                           "lf_share_unw"     = "Overall",
                           "lf_share_m_unw"   = "Male",
                           "lf_share_f_unw"   = "Female",
                           "lf_share_urb_unw" = "Urban",
                           "lf_share_rur_unw" = "Rural"
  )) |>
  ggplot(aes(x = time, y = value, color = variable, group = variable)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c(
    "Overall" = "black",
    "Male"    = "steelblue",
    "Female"  = "tomato",
    "Urban"   = "darkorange",
    "Rural"   = "forestgreen"
  )) +
  labs(
    title = "Labor Force Share — India (population weighted)",
    x     = "Year",
    y     = "LF Share",
    color = NULL
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# -- Worker shares (5 lines) --------------------------------------------------
plfs_india |>
  pivot_longer(
    cols      = starts_with("worker_share"),
    names_to  = "variable",
    values_to = "value"
  ) |>
  mutate(variable = dplyr::recode(variable,
                           "worker_share_unw"     = "Overall",
                           "worker_share_m_unw"   = "Male",
                           "worker_share_f_unw"   = "Female",
                           "worker_share_urb_unw" = "Urban",
                           "worker_share_rur_unw" = "Rural"
  )) |>
  ggplot(aes(x = time, y = value, color = variable, group = variable)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c(
    "Overall" = "black",
    "Male"    = "steelblue",
    "Female"  = "tomato",
    "Urban"   = "darkorange",
    "Rural"   = "forestgreen"
  )) +
  labs(
    title = "Worker Share — India (population weighted)",
    x     = "Year",
    y     = "Worker Share",
    color = NULL
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# -- Informality shares (5 lines) --------------------------------------------------
plfs_india |>
  pivot_longer(
    cols      = starts_with("s_casual_w_worker_PS"),
    names_to  = "variable",
    values_to = "value"
  ) |>
  mutate(variable = dplyr::recode(variable,
                           "s_casual_w_worker_PS_unw"     = "Overall",
                           "s_casual_w_worker_PS_m_unw"   = "Male",
                           "s_casual_w_worker_PS_f_unw"   = "Female",
                           "s_casual_w_worker_PS_urb_unw" = "Urban",
                           "s_casual_w_worker_PS_rur_unw" = "Rural"
  )) |>
  ggplot(aes(x = time, y = value, color = variable, group = variable)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c(
    "Overall" = "black",
    "Male"    = "steelblue",
    "Female"  = "tomato",
    "Urban"   = "darkorange",
    "Rural"   = "forestgreen"
  )) +
  labs(
    title = "Informality — India (working-population-weighted)",
    x     = "Year",
    y     = "Informality/workers",
    color = NULL
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

