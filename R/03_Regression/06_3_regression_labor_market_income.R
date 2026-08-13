# GDP STATE SPLIT

# 1. Classify states into 2 groups based on GDP per capita
# 2. Run regressions for each group separately

# --- Income grouping ---
high_income_states <- c(
  "HIMACHAL PRADESH",
  "ANDAMAN & NICOBAR",
  "GUJARAT",
  "TELANGANA",
  "TAMIL NADU",
  "MAHARASHTRA",
  "UTTARAKHAND",
  "KARNATAKA",
  "KERALA",
  "PUDUCHERRY",
  "CHANDIGARH",
  "SIKKIM",
  "DELHI",
  "GOA"
)
low_income_states <- c(
  "BIHAR",
  "UTTAR PRADESH",
  "JHARKHAND",
  "MANIPUR",
  "MADHYA PRADESH",
  "ODISHA",
  "MEGHALAYA",
  "CHHATTISGARH",
  "JAMMU AND KASHMIR",
  "WEST BENGAL",
  "RAJASTHAN",
  "TRIPURA",
  "ANDHRA PRADESH",
  "MIZORAM",
  "ARUNACHAL PRADESH",
  "PUNJAB"
)

df_income <- data %>%
  mutate(
    income_group = case_when(
      STATE %in% high_income_states ~ "High income",
      STATE %in% low_income_states  ~ "Low income",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("Low income", "High income"))
  )

# Sanity check — any STATE values that didn't match either list?
df_income %>%
  filter(is.na(income_group)) %>%
  distinct(STATE)

income_states_df <- tibble(
  STATE = c(high_income_states, low_income_states),
  income_group = c(
    rep("High income", length(high_income_states)),
    rep("Low income", length(low_income_states))
  )
)

# 2 regressions
df_sections_income <- df_income %>%
  filter(!is.na(income_group)) |>
  filter(STATE != "ARUNACHAL PRADESH") |>
  filter(STATE != "MEGHALAYA")

df_sections_income <- df_sections_income %>%
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
m_income_high_spei_lf <- lm(lf_share_unw ~ spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2 +
                              spei_neg_spei_lag3 +
                              factor(STATE) + factor(year) + factor(STATE):year,
                            data = df_sections_income %>% filter(income_group == "High income"), weights = state_pop)

m_income_low_spei_lf <- lm(lf_share_unw ~ spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2 +
                             spei_neg_spei_lag3 +
                             factor(STATE) + factor(year) + factor(STATE):year,
                           data = df_sections_income %>% filter(income_group == "Low income"), weights = state_pop)

# Worker share
m_income_high_spei_ws <- lm(worker_share_unw ~ spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2 +
                              spei_neg_spei_lag3 +
                              factor(STATE) + factor(year) + factor(STATE):year,
                            data = df_sections_income %>% filter(income_group == "High income"), weights = state_pop)
m_income_low_spei_ws <- lm(worker_share_unw ~ spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2 +
                             spei_neg_spei_lag3 +
                             factor(STATE) + factor(year) + factor(STATE):year,
                           data = df_sections_income %>% filter(income_group == "Low income"), weights = state_pop)

# Unemployment
m_income_high_spei_un <- lm(unemp_PS_unw ~ spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2 +
                              spei_neg_spei_lag3 +
                              factor(STATE) + factor(year) + factor(STATE):year,
                            data = df_sections_income %>% filter(income_group == "High income"), weights = state_pop)
m_income_low_spei_un <- lm(unemp_PS_unw ~ spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2 +
                             spei_neg_spei_lag3 +
                             factor(STATE) + factor(year) + factor(STATE):year,
                           data = df_sections_income %>% filter(income_group == "Low income"), weights = state_pop)

# ============================================================
# FI models — LF share, Worker share, Unemployment
# ============================================================

# LF share
m_income_high_fi_lf <- lm(lf_share_unw ~ FI_state + FI_lag1 + FI_lag2 +
                            factor(STATE) + factor(year) + factor(STATE):year,
                          data = df_sections_income %>% filter(income_group == "High income"), weights = state_pop)
m_income_low_fi_lf <- lm(lf_share_unw ~ FI_state + FI_lag1 + FI_lag2 +
                           factor(STATE) + factor(year) + factor(STATE):year,
                         data = df_sections_income %>% filter(income_group == "Low income"), weights = state_pop)

# Worker share
m_income_high_fi_ws <- lm(worker_share_unw ~ FI_state + FI_lag1 + FI_lag2 +
                            factor(STATE) + factor(year) + factor(STATE):year,
                          data = df_sections_income %>% filter(income_group == "High income"), weights = state_pop)
m_income_low_fi_ws <- lm(worker_share_unw ~ FI_state + FI_lag1 + FI_lag2 +
                           factor(STATE) + factor(year) + factor(STATE):year,
                         data = df_sections_income %>% filter(income_group == "Low income"), weights = state_pop)

# Unemployment
m_income_high_fi_un <- lm(unemp_PS_unw ~ FI_state + FI_lag1 + FI_lag2 +
                            factor(STATE) + factor(year) + factor(STATE):year,
                          data = df_sections_income %>% filter(income_group == "High income"), weights = state_pop)
m_income_low_fi_un <- lm(unemp_PS_unw ~ FI_state + FI_lag1 + FI_lag2 +
                           factor(STATE) + factor(year) + factor(STATE):year,
                         data = df_sections_income %>% filter(income_group == "Low income"), weights = state_pop)

# ============================================================
# Clustered SEs
# ============================================================

se_high_spei_lf <- cluster_se(m_income_high_spei_lf)
se_low_spei_lf <- cluster_se(m_income_low_spei_lf)
se_high_spei_ws <- cluster_se(m_income_high_spei_ws)
se_low_spei_ws <- cluster_se(m_income_low_spei_ws)
se_high_spei_un <- cluster_se(m_income_high_spei_un)
se_low_spei_un <- cluster_se(m_income_low_spei_un)

se_high_fi_lf <- cluster_se(m_income_high_fi_lf)
se_low_fi_lf <- cluster_se(m_income_low_fi_lf)
se_high_fi_ws <- cluster_se(m_income_high_fi_ws)
se_low_fi_ws <- cluster_se(m_income_low_fi_ws)
se_high_fi_un <- cluster_se(m_income_high_fi_un)
se_low_fi_un <- cluster_se(m_income_low_fi_un)

# ============================================================
# LF SHARE — SPEI
# ============================================================
stargazer(
  m_income_high_spei_lf, m_income_low_spei_lf,
  se = list(se_high_spei_lf[, 2], se_low_spei_lf[, 2]),
  p  = list(se_high_spei_lf[, 4], se_low_spei_lf[, 4]),
  title          = "Effect of Negative SPEI-12 on Labor Force Share, High vs. Low Income States",
  dep.var.labels = "Labor Force Share (unweighted)",
  covariate.labels = c(
    "Negative SPEI-12",
    "Neg. SPEI-12 Lag 1",
    "Neg. SPEI-12 Lag 2",
    "Neg. SPEI-12 Lag 3"),
  column.labels  = c("High income states", "Low income states"),
  omit = c("factor\\(STATE\\)", "factor\\(year\\)", "factor\\(STATE\\):year", "Constant"),
  omit.stat    = c("f", "ser"),
  notes        = "Working age: 15–64. States split by GDP per capita into high vs. low income groups.",
  notes.append = FALSE,
  type         = "text",
  label        = "tab:income_split_lf_share_spei"
)

# ============================================================
# LF SHARE — FI
# ============================================================
stargazer(
  m_income_high_fi_lf, m_income_low_fi_lf,
  se = list(se_high_fi_lf[, 2], se_low_fi_lf[, 2]),
  p  = list(se_high_fi_lf[, 4], se_low_fi_lf[, 4]),
  title          = "Effect of Flood Index on Labor Force Share, High vs. Low Income States",
  dep.var.labels = "Labor Force Share (unweighted)",
  covariate.labels = c(
    "Flood Index",
    "FI Lag 1",
    "FI Lag 2"),
  column.labels  = c("High income states", "Low income states"),
  omit = c("factor\\(STATE\\)", "factor\\(year\\)", "factor\\(STATE\\):year", "Constant"),
  omit.stat    = c("f", "ser"),
  notes        = "Working age: 15–64. States split by GDP per capita into high vs. low income groups. Excludes states with missing FI data.",
  notes.append = FALSE,
  type         = "text",
  label        = "tab:income_split_lf_share_fi"
)

# ============================================================
# WORKER SHARE — SPEI
# ============================================================
stargazer(
  m_income_high_spei_ws, m_income_low_spei_ws,
  se = list(se_high_spei_ws[, 2], se_low_spei_ws[, 2]),
  p  = list(se_high_spei_ws[, 4], se_low_spei_ws[, 4]),
  title          = "Effect of Negative SPEI-12 on Worker Share, High vs. Low Income States",
  dep.var.labels = "Worker Share (unweighted)",
  covariate.labels = c(
    "Negative SPEI-12",
    "Neg. SPEI-12 Lag 1",
    "Neg. SPEI-12 Lag 2",
    "Neg. SPEI-12 Lag 3"),
  column.labels  = c("High income states", "Low income states"),
  omit = c("factor\\(STATE\\)", "factor\\(year\\)", "factor\\(STATE\\):year", "Constant"),
  omit.stat    = c("f", "ser"),
  notes        = "Working age: 15–64. States split by GDP per capita into high vs. low income groups.",
  notes.append = FALSE,
  type         = "text",
  label        = "tab:income_split_worker_share_spei"
)

# ============================================================
# WORKER SHARE — FI
# ============================================================
stargazer(
  m_income_high_fi_ws, m_income_low_fi_ws,
  se = list(se_high_fi_ws[, 2], se_low_fi_ws[, 2]),
  p  = list(se_high_fi_ws[, 4], se_low_fi_ws[, 4]),
  title          = "Effect of Flood Index on Worker Share, High vs. Low Income States",
  dep.var.labels = "Worker Share (unweighted)",
  covariate.labels = c(
    "Flood Index",
    "FI Lag 1",
    "FI Lag 2"),
  column.labels  = c("High income states", "Low income states"),
  omit = c("factor\\(STATE\\)", "factor\\(year\\)", "factor\\(STATE\\):year", "Constant"),
  omit.stat    = c("f", "ser"),
  notes        = "Working age: 15–64. States split by GDP per capita into high vs. low income groups. Excludes states with missing FI data.",
  notes.append = FALSE,
  type         = "text",
  label        = "tab:income_split_worker_share_fi"
)

# ============================================================
# UNEMPLOYMENT — SPEI
# ============================================================
stargazer(
  m_income_high_spei_un, m_income_low_spei_un,
  se = list(se_high_spei_un[, 2], se_low_spei_un[, 2]),
  p  = list(se_high_spei_un[, 4], se_low_spei_un[, 4]),
  title          = "Effect of Negative SPEI-12 on Unemployment, High vs. Low Income States",
  dep.var.labels = "Unemployment (PS, unweighted)",
  covariate.labels = c(
    "Negative SPEI-12",
    "Neg. SPEI-12 Lag 1",
    "Neg. SPEI-12 Lag 2",
    "Neg. SPEI-12 Lag 3"),
  column.labels  = c("High income states", "Low income states"),
  omit = c("factor\\(STATE\\)", "factor\\(year\\)", "factor\\(STATE\\):year", "Constant"),
  omit.stat    = c("f", "ser"),
  notes        = "Working age: 15–64. States split by GDP per capita into high vs. low income groups.",
  notes.append = FALSE,
  type         = "text",
  label        = "tab:income_split_unemployment_spei"
)

# ============================================================
# UNEMPLOYMENT — FI
# ============================================================
stargazer(
  m_income_high_fi_un, m_income_low_fi_un,
  se = list(se_high_fi_un[, 2], se_low_fi_un[, 2]),
  p  = list(se_high_fi_un[, 4], se_low_fi_un[, 4]),
  title          = "Effect of Flood Index on Unemployment, High vs. Low Income States",
  dep.var.labels = "Unemployment (PS, unweighted)",
  covariate.labels = c(
    "Flood Index",
    "FI Lag 1",
    "FI Lag 2"),
  column.labels  = c("High income states", "Low income states"),
  omit = c("factor\\(STATE\\)", "factor\\(year\\)", "factor\\(STATE\\):year", "Constant"),
  omit.stat    = c("f", "ser"),
  notes        = "Working age: 15–64. States split by GDP per capita into high vs. low income groups. Excludes states with missing FI data.",
  notes.append = FALSE,
  type         = "text",
  label        = "tab:income_split_unemployment_fi"
)


## JOINED TABLES

# ============================================================
# HIGH INCOME — SPEI (LF share, Worker share, Unemployment)
# ============================================================
stargazer(
  m_income_high_spei_lf, m_income_high_spei_ws, m_income_high_spei_un,
  se = list(se_high_spei_lf[, 2], se_high_spei_ws[, 2], se_high_spei_un[, 2]),
  p  = list(se_high_spei_lf[, 4], se_high_spei_ws[, 4], se_high_spei_un[, 4]),
  title            = "Effect of Negative SPEI-12 on Labor Market Outcomes, High Income States",
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
  label        = "tab:high_income_LM_D"
)

# ============================================================
# HIGH INCOME — FI (LF share, Worker share, Unemployment)
# ============================================================
stargazer(
  m_income_high_fi_lf, m_income_high_fi_ws, m_income_high_fi_un,
  se = list(se_high_fi_lf[, 2], se_high_fi_ws[, 2], se_high_fi_un[, 2]),
  p  = list(se_high_fi_lf[, 4], se_high_fi_ws[, 4], se_high_fi_un[, 4]),
  title            = "Effect of Flood Index on Labor Market Outcomes, High Income States",
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
  label        = "tab:high_income_LM_FI"
)

# ============================================================
# LOW INCOME — SPEI (LF share, Worker share, Unemployment)
# ============================================================
stargazer(
  m_income_low_spei_lf, m_income_low_spei_ws, m_income_low_spei_un,
  se = list(se_low_spei_lf[, 2], se_low_spei_ws[, 2], se_low_spei_un[, 2]),
  p  = list(se_low_spei_lf[, 4], se_low_spei_ws[, 4], se_low_spei_un[, 4]),
  title            = "Effect of Negative SPEI-12 on Labor Market Outcomes, Low Income States",
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
  label        = "tab:low_income_LM_D"
)

# ============================================================
# LOW INCOME — FI (LF share, Worker share, Unemployment)
# ============================================================
stargazer(
  m_income_low_fi_lf, m_income_low_fi_ws, m_income_low_fi_un,
  se = list(se_low_fi_lf[, 2], se_low_fi_ws[, 2], se_low_fi_un[, 2]),
  p  = list(se_low_fi_lf[, 4], se_low_fi_ws[, 4], se_low_fi_un[, 4]),
  title            = "Effect of Flood Index on Labor Market Outcomes, Low Income States",
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
  label        = "tab:low_income_LM_FI"
)

