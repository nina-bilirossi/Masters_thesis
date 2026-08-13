
# ==============================================================================
# This script runs regressions to analyze the impact of the drougth and flood
# indices on different labor market measures, with states divided according to 
# their agricultural share of employment (high/low group).

# Results are presented in stargazer tables for export.

# see Manuscript: Empirical Strategy for details.

# ==============================================================================


# AGRI STATE SPLIT

df_sections_agri <- df_sections_agri %>%
  mutate(
    # =========================================================================
    # 1. PRINCIPAL STATUS (PS) UNEMPLOYMENT RATES
    # =========================================================================
    unemp_PS_unw     = (labor_force_PS_unw - worker_PS_unw) / labor_force_PS_unw,
    unemp_PS_m_unw   = (labor_force_PS_m_unw - worker_PS_m_unw) / labor_force_PS_m_unw,
    unemp_PS_f_unw   = (labor_force_PS_f_unw - worker_PS_f_unw) / labor_force_PS_f_unw,
    unemp_PS_rur_unw = (labor_force_PS_rur_unw - worker_PS_rur_unw) / labor_force_PS_rur_unw,
    unemp_PS_urb_unw = (labor_force_PS_urb_unw - worker_PS_urb_unw) / labor_force_PS_urb_unw
  )


# ============================================================
# SPEI models — LF share, Worker share, Unemployment
# ============================================================

# LF share
m_agri_large_spei_lf <- lm(lf_share_unw ~ spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2 +
                             spei_neg_spei_lag3 +
                             factor(STATE) + factor(year) + factor(STATE):year,
                           data = df_sections_agri %>% filter(agri_group == "High agri"), weights = state_pop)


m_agri_small_spei_lf <- lm(lf_share_unw ~ spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2 +
                             spei_neg_spei_lag3 +
                             factor(STATE) + factor(year) + factor(STATE):year,
                           data = df_sections_agri %>% filter(agri_group == "Low agri"), weights = state_pop)

# Worker share
m_agri_large_spei_ws <- lm(worker_share_unw ~ spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2 +
                             spei_neg_spei_lag3 +
                             factor(STATE) + factor(year) + factor(STATE):year,
                           data = df_sections_agri %>% filter(agri_group == "High agri"), weights = state_pop)
m_agri_small_spei_ws <- lm(worker_share_unw ~ spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2 +
                             spei_neg_spei_lag3 +
                             factor(STATE) + factor(year) + factor(STATE):year,
                           data = df_sections_agri %>% filter(agri_group == "Low agri"), weights = state_pop)

# Unemployment
m_agri_large_spei_un <- lm(unemp_PS_unw ~ spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2 +
                             spei_neg_spei_lag3 +
                             factor(STATE) + factor(year) + factor(STATE):year,
                           data = df_sections_agri %>% filter(agri_group == "High agri"), weights = state_pop)
m_agri_small_spei_un <- lm(unemp_PS_unw ~ spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2 +
                             spei_neg_spei_lag3 +
                             factor(STATE) + factor(year) + factor(STATE):year,
                           data = df_sections_agri %>% filter(agri_group == "Low agri"), weights = state_pop)

# ============================================================
# FI models — LF share, Worker share, Unemployment
# ============================================================

# LF share
m_agri_large_fi_lf <- lm(lf_share_unw ~ FI_state + FI_lag1 + FI_lag2 +
                           factor(STATE) + factor(year) + factor(STATE):year,
                         data = df_sections_agri %>% filter(agri_group == "High agri"), weights = state_pop)
m_agri_small_fi_lf <- lm(lf_share_unw ~ FI_state + FI_lag1 + FI_lag2 +
                           factor(STATE) + factor(year) + factor(STATE):year,
                         data = df_sections_agri %>% filter(agri_group == "Low agri"), weights = state_pop)

# Worker share
m_agri_large_fi_ws <- lm(worker_share_unw ~ FI_state + FI_lag1 + FI_lag2 +
                           factor(STATE) + factor(year) + factor(STATE):year,
                         data = df_sections_agri %>% filter(agri_group == "High agri"), weights = state_pop)
m_agri_small_fi_ws <- lm(worker_share_unw ~ FI_state + FI_lag1 + FI_lag2 +
                           factor(STATE) + factor(year) + factor(STATE):year,
                         data = df_sections_agri %>% filter(agri_group == "Low agri"), weights = state_pop)

# Unemployment
m_agri_large_fi_un <- lm(unemp_PS_unw ~ FI_state + FI_lag1 + FI_lag2 +
                           factor(STATE) + factor(year) + factor(STATE):year,
                         data = df_sections_agri %>% filter(agri_group == "High agri"), weights = state_pop)
m_agri_small_fi_un <- lm(unemp_PS_unw ~ FI_state + FI_lag1 + FI_lag2 +
                           factor(STATE) + factor(year) + factor(STATE):year,
                         data = df_sections_agri %>% filter(agri_group == "Low agri"), weights = state_pop)

# ============================================================
# Clustered SEs
# ============================================================

se_large_spei_lf <- cluster_se(m_agri_large_spei_lf)
se_small_spei_lf <- cluster_se(m_agri_small_spei_lf)
se_large_spei_ws <- cluster_se(m_agri_large_spei_ws)
se_small_spei_ws <- cluster_se(m_agri_small_spei_ws)
se_large_spei_un <- cluster_se(m_agri_large_spei_un)
se_small_spei_un <- cluster_se(m_agri_small_spei_un)

se_large_fi_lf <- cluster_se(m_agri_large_fi_lf)
se_small_fi_lf <- cluster_se(m_agri_small_fi_lf)
se_large_fi_ws <- cluster_se(m_agri_large_fi_ws)
se_small_fi_ws <- cluster_se(m_agri_small_fi_ws)
se_large_fi_un <- cluster_se(m_agri_large_fi_un)
se_small_fi_un <- cluster_se(m_agri_small_fi_un)

# ============================================================
# LF SHARE — SPEI
# ============================================================
stargazer(
  m_agri_large_spei_lf, m_agri_small_spei_lf,
  se = list(se_large_spei_lf[, 2], se_small_spei_lf[, 2]),
  p  = list(se_large_spei_lf[, 4], se_small_spei_lf[, 4]),
  title          = "Effect of Negative SPEI-12 on Labor Force Share, High vs. Low Agri-Dependent States",
  dep.var.labels = "Labor Force Share (unweighted)",
  covariate.labels = c(
    "Negative SPEI-12",
    "Neg. SPEI-12 Lag 1",
    "Neg. SPEI-12 Lag 2",
    "Neg. SPEI-12 Lag 3"),
  column.labels  = c("High agri states", "Low agri states"),
  omit = c("factor\\(STATE\\)", "factor\\(year\\)", "factor\\(STATE\\):year", "Constant"),
  omit.stat    = c("f", "ser"),
  notes        = "Working age: 15–64. States split at median agricultural employment share.",
  notes.append = FALSE,
  type         = "text",
  label        = "tab:agri_split_lf_share_spei"
)

# ============================================================
# LF SHARE — FI
# ============================================================
stargazer(
  m_agri_large_fi_lf, m_agri_small_fi_lf,
  se = list(se_large_fi_lf[, 2], se_small_fi_lf[, 2]),
  p  = list(se_large_fi_lf[, 4], se_small_fi_lf[, 4]),
  title          = "Effect of Flood Index on Labor Force Share, High vs. Low Agri-Dependent States",
  dep.var.labels = "Labor Force Share (unweighted)",
  covariate.labels = c(
    "Flood Index",
    "FI Lag 1",
    "FI Lag 2"),
  column.labels  = c("High agri states", "Low agri states"),
  omit = c("factor\\(STATE\\)", "factor\\(year\\)", "factor\\(STATE\\):year", "Constant"),
  omit.stat    = c("f", "ser"),
  notes        = "Working age: 15–64. States split at median agricultural employment share. Excludes states with missing FI data.",
  notes.append = FALSE,
  type         = "text",
  label        = "tab:agri_split_lf_share_fi"
)

# ============================================================
# WORKER SHARE — SPEI
# ============================================================
stargazer(
  m_agri_large_spei_ws, m_agri_small_spei_ws,
  se = list(se_large_spei_ws[, 2], se_small_spei_ws[, 2]),
  p  = list(se_large_spei_ws[, 4], se_small_spei_ws[, 4]),
  title          = "Effect of Negative SPEI-12 on Worker Share, High vs. Low Agri-Dependent States",
  dep.var.labels = "Worker Share (unweighted)",
  covariate.labels = c(
    "Negative SPEI-12",
    "Neg. SPEI-12 Lag 1",
    "Neg. SPEI-12 Lag 2",
    "Neg. SPEI-12 Lag 3"),
  column.labels  = c("High agri states", "Low agri states"),
  omit = c("factor\\(STATE\\)", "factor\\(year\\)", "factor\\(STATE\\):year", "Constant"),
  omit.stat    = c("f", "ser"),
  notes        = "Working age: 15–64. States split at median agricultural employment share.",
  notes.append = FALSE,
  type         = "text",
  label        = "tab:agri_split_worker_share_spei"
)

# ============================================================
# WORKER SHARE — FI
# ============================================================
stargazer(
  m_agri_large_fi_ws, m_agri_small_fi_ws,
  se = list(se_large_fi_ws[, 2], se_small_fi_ws[, 2]),
  p  = list(se_large_fi_ws[, 4], se_small_fi_ws[, 4]),
  title          = "Effect of Flood Index on Worker Share, High vs. Low Agri-Dependent States",
  dep.var.labels = "Worker Share (unweighted)",
  covariate.labels = c(
    "Flood Index",
    "FI Lag 1",
    "FI Lag 2"),
  column.labels  = c("High agri states", "Low agri states"),
  omit = c("factor\\(STATE\\)", "factor\\(year\\)", "factor\\(STATE\\):year", "Constant"),
  omit.stat    = c("f", "ser"),
  notes        = "Working age: 15–64. States split at median agricultural employment share. Excludes states with missing FI data.",
  notes.append = FALSE,
  type         = "text",
  label        = "tab:agri_split_worker_share_fi"
)


# ============================================================
# UNEMPLOYMENT — SPEI
# ============================================================
stargazer(
  m_agri_large_spei_un, m_agri_small_spei_un,
  se = list(se_large_spei_un[, 2], se_small_spei_un[, 2]),
  p  = list(se_large_spei_un[, 4], se_small_spei_un[, 4]),
  title          = "Effect of Negative SPEI-12 on Unemployment, High vs. Low Agri-Dependent States",
  dep.var.labels = "Unemployment (PS, unweighted)",
  covariate.labels = c(
    "Negative SPEI-12",
    "Neg. SPEI-12 Lag 1",
    "Neg. SPEI-12 Lag 2",
    "Neg. SPEI-12 Lag 3"),
  column.labels  = c("High agri states", "Low agri states"),
  omit = c("factor\\(STATE\\)", "factor\\(year\\)", "factor\\(STATE\\):year", "Constant"),
  omit.stat    = c("f", "ser"),
  notes        = "Working age: 15–64. States split at median agricultural employment share.",
  notes.append = FALSE,
  type         = "text",
  label        = "tab:agri_split_unemployment_spei"
)

# ============================================================
# UNEMPLOYMENT — FI
# ============================================================
stargazer(
  m_agri_large_fi_un, m_agri_small_fi_un,
  se = list(se_large_fi_un[, 2], se_small_fi_un[, 2]),
  p  = list(se_large_fi_un[, 4], se_small_fi_un[, 4]),
  title          = "Effect of Flood Index on Unemployment, High vs. Low Agri-Dependent States",
  dep.var.labels = "Unemployment (PS, unweighted)",
  covariate.labels = c(
    "Flood Index",
    "FI Lag 1",
    "FI Lag 2"),
  column.labels  = c("High agri states", "Low agri states"),
  omit = c("factor\\(STATE\\)", "factor\\(year\\)", "factor\\(STATE\\):year", "Constant"),
  omit.stat    = c("f", "ser"),
  notes        = "Working age: 15–64. States split at median agricultural employment share. Excludes states with missing FI data.",
  notes.append = FALSE,
  type         = "text",
  label        = "tab:agri_split_unemployment_fi"
)
# ============================================================
# HIGH AGRI — SPEI (LF share, Worker share, Unemployment)
# ============================================================
stargazer(
  m_agri_large_spei_lf, m_agri_large_spei_ws, m_agri_large_spei_un,
  se = list(se_large_spei_lf[, 2], se_large_spei_ws[, 2], se_large_spei_un[, 2]),
  p  = list(se_large_spei_lf[, 4], se_large_spei_ws[, 4], se_large_spei_un[, 4]),
  title            = "Effect of Negative SPEI-12 on Labor Market Outcomes, High Agri-Dependent States",
  dep.var.caption  = "",
  dep.var.labels   = c("", "", ""),
  column.labels    = c("LF share", "Worker share", "Unemployment"),
  column.separate  = c(1, 1, 1),
  covariate.labels = c(
    "Negative SPEI-12",
    "Neg. SPEI-12 Lag 1",
    "Neg. SPEI-12 Lag 2",
    "Neg. SPEI-12 Lag 3"),
  omit = c("factor\\(STATE\\)", "factor\\(year\\)", "factor\\(STATE\\):year", "Constant"),
  omit.stat    = c("f", "ser"),
  notes.append = TRUE,
  type         = "latex",
  label        = "tab:high_agri_LM_D")

# ============================================================
# HIGH AGRI — FI (LF share, Worker share, Unemployment)
# ============================================================
stargazer(
  m_agri_large_fi_lf, m_agri_large_fi_ws, m_agri_large_fi_un,
  se = list(se_large_fi_lf[, 2], se_large_fi_ws[, 2], se_large_fi_un[, 2]),
  p  = list(se_large_fi_lf[, 4], se_large_fi_ws[, 4], se_large_fi_un[, 4]),
  title            = "Effect of Flood Index on Labor Market Outcomes, High Agri-Dependent States",
  dep.var.caption  = "",
  dep.var.labels   = c("", "", ""),
  column.labels    = c("LF share", "Worker share", "Unemployment"),
  column.separate  = c(1, 1, 1),
  covariate.labels = c(
    "Flood Index",
    "FI Lag 1",
    "FI Lag 2"),
  omit = c("factor\\(STATE\\)", "factor\\(year\\)", "factor\\(STATE\\):year", "Constant"),
  omit.stat    = c("f", "ser"),
  notes.append = TRUE,
  type         = "latex",
  label        = "tab:high_agri_LM_FI")

# ============================================================
# LOW AGRI — SPEI (LF share, Worker share, Unemployment)
# ============================================================
stargazer(
  m_agri_small_spei_lf, m_agri_small_spei_ws, m_agri_small_spei_un,
  se = list(se_small_spei_lf[, 2], se_small_spei_ws[, 2], se_small_spei_un[, 2]),
  p  = list(se_small_spei_lf[, 4], se_small_spei_ws[, 4], se_small_spei_un[, 4]),
  title            = "Effect of Negative SPEI-12 on Labor Market Outcomes, Low Agri-Dependent States",
  dep.var.caption  = "",
  dep.var.labels   = c("", "", ""),
  column.labels    = c("LF share", "Worker share", "Unemployment"),
  column.separate  = c(1, 1, 1),
  covariate.labels = c(
    "Negative SPEI-12",
    "Neg. SPEI-12 Lag 1",
    "Neg. SPEI-12 Lag 2",
    "Neg. SPEI-12 Lag 3"),
  omit = c("factor\\(STATE\\)", "factor\\(year\\)", "factor\\(STATE\\):year", "Constant"),
  omit.stat    = c("f", "ser"),
  notes.append = TRUE,
  type         = "latex",
  label        = "tab:low_agri_LM_D")

# ============================================================
# LOW AGRI — FI (LF share, Worker share, Unemployment)
# ============================================================
stargazer(
  m_agri_small_fi_lf, m_agri_small_fi_ws, m_agri_small_fi_un,
  se = list(se_small_fi_lf[, 2], se_small_fi_ws[, 2], se_small_fi_un[, 2]),
  p  = list(se_small_fi_lf[, 4], se_small_fi_ws[, 4], se_small_fi_un[, 4]),
  title            = "Effect of Flood Index on Labor Market Outcomes, Low Agri-Dependent States",
  dep.var.caption  = "",
  dep.var.labels   = c("", "", ""),
  column.labels    = c("LF share", "Worker share", "Unemployment"),
  column.separate  = c(1, 1, 1),
  covariate.labels = c(
    "Flood Index",
    "FI Lag 1",
    "FI Lag 2"),
  omit = c("factor\\(STATE\\)", "factor\\(year\\)", "factor\\(STATE\\):year", "Constant"),
  omit.stat    = c("f", "ser"),
  notes.append = TRUE,
  type         = "latex",
  label        = "tab:low_agri_LM_FI")
