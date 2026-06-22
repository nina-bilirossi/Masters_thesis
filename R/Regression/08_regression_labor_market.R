# what is the effect of the shocks on labor force participation (lf/pop), employment (workers/lf), unemployment
library(stargazer)
library(dplyr)

# ── SOME SUMMARY STATISTICS & data cleaning ──────────────

source("/Users/ninabilirossi/Desktop/MSC THESIS/Data works/Code/General statistics/labor_data_baseline_comparison.R")

data <- data %>%
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

data_flood <- data |> filter(STATE != "ARUNACHAL PRADESH") |> filter(STATE != "MEGHALAYA")

# ══════════════════════════════════════════════════════════════════════════════
# REGRESSIONs
# ══════════════════════════════════════════════════════════════════════════════

# ================================= DROUGHTS ===================================

# GENERAL POP

m1a <- lm(lf_share_unw ~ spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2+ spei_neg_spei_lag3+
           factor(STATE) + factor(year) + factor(STATE):year,
         data = data
         , weights = state_pop
) 

m1b <- lm(worker_share_unw ~ spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2+ spei_neg_spei_lag3+
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data
          , weights = state_pop
) 

m1c <- lm(unemp_PS_unw ~ spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2+spei_neg_spei_lag3+
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data
          , weights = state_pop) 


se1a <- cluster_se(m1a)
se1b <- cluster_se(m1b)
se1c <- cluster_se(m1c)

stargazer(
  m1a, m1b, m1c,
  se = list(se1a[, 2], se1b[, 2], se1c[, 2]),
  p  = list(se1a[, 4], se1b[, 4], se1c[, 4]),
  title          = "Effect of Climate Shocks on Labor market (PS)",
  dep.var.labels = c("","",""),
  covariate.labels = c(
    "Negative SPEI-12",
    "Neg. SPEI-12 Lag 1",
    "Neg. SPEI-12 Lag 2"),
  column.labels   = c("LF share", "Worker share", "Unemployment"),
  omit = c("factor\\(STATE\\)",
           "factor\\(year\\)",
           "factor\\(STATE\\):year",
           "Constant"),
  column.separate = c(1, 1),
  omit.stat    = c("f", "ser"),
  notes        = "Working age: 15–64.", # Columns (3)–(5) exclude Arunachal Pradesh and Meghalaya (missing FI data).
  notes.append = FALSE,
  type         = "text",
  label        = "tab:expanded_all")

# ==============================================================================
# GENDERED MODELS
# male
m2a <- lm(lf_share_m_unw ~ spei_neg_spei_lag1 + spei_neg_spei_lag2+ spei_neg_spei_lag3+
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data
          , weights = pop_m_unw
) 

m2b <- lm(worker_share_m_unw ~  spei_neg_spei_lag1 + spei_neg_spei_lag2+ spei_neg_spei_lag3+
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data
          , weights = pop_m_unw
) 

m2c <- lm(unemp_PS_m_unw ~ spei_neg_spei_lag1 + spei_neg_spei_lag2+spei_neg_spei_lag3+
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data
          , weights = pop_m_unw) 


se2a <- cluster_se(m2a)
se2b <- cluster_se(m2b)
se2c <- cluster_se(m2c)


# FEMALE

m3a <- lm(lf_share_f_unw ~  spei_neg_spei_lag1 + spei_neg_spei_lag2+ spei_neg_spei_lag3+
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data
          , weights = pop_f_unw
) 

m3b <- lm(worker_share_f_unw ~  spei_neg_spei_lag1 + spei_neg_spei_lag2 + spei_neg_spei_lag3+
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data
          , weights = pop_f_unw
) 

m3c <- lm(unemp_PS_f_unw ~ spei_neg_spei_lag1 + spei_neg_spei_lag2+spei_neg_spei_lag3+
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data
          , weights = pop_f_unw) 


se3a <- cluster_se(m3a)
se3b <- cluster_se(m3b)
se3c <- cluster_se(m3c)

#Display both 
stargazer(
  m2a, m2b, m2c,
  se = list(se2a[, 2], se2b[, 2], se2c[, 2]),
  p  = list(se2a[, 4], se2b[, 4], se2c[, 4]),
  title          = "Effect of Climate Shocks on Labor market (PS, male population)",
  dep.var.labels = c("","",""),
  column.labels   = c("LF share", "Worker share", "Unemployment"),
  omit = c("factor\\(STATE\\)",
           "factor\\(year\\)",
           "factor\\(STATE\\):year",
           "Constant"),
  column.separate = c(1, 1),
  omit.stat    = c("f", "ser"),
  notes        = "Working age: 15–64.", # Columns (3)–(5) exclude Arunachal Pradesh and Meghalaya (missing FI data).
  notes.append = FALSE,
  type         = "text",
  label        = "tab:expanded_all"
)

stargazer(
  m3a, m3b, m3c,
  se = list(se3a[, 2], se3b[, 2], se3c[, 2]),
  p  = list(se3a[, 4], se3b[, 4], se3c[, 4]),
  title          = "Effect of Climate Shocks on Labor market (PS, female population)",
  dep.var.labels = c("","",""),
  column.labels   = c("LF share", "Worker share", "Unemployment"),
  omit = c("factor\\(STATE\\)",
           "factor\\(year\\)",
           "factor\\(STATE\\):year",
           "Constant"),
  column.separate = c(1, 1),
  omit.stat    = c("f", "ser"),
  notes        = "Working age: 15–64.",
  notes.append = FALSE,
  type         = "text",
  label        = "tab:expanded_all"
)

# ==============================================================================
# sectoral   MODELS

# rural 
m4a <- lm(lf_share_rur_unw ~ spei_negative +spei_neg_spei_lag1 + spei_neg_spei_lag2+ spei_neg_spei_lag3+
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data
          , weights = pop_rur_unw
) 

m4b <- lm(worker_share_rur_unw ~ spei_negative +spei_neg_spei_lag1 + spei_neg_spei_lag2+ spei_neg_spei_lag3+
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data
          , weights = pop_rur_unw
) 

m4c <- lm(unemp_PS_rur_unw ~ spei_negative +spei_neg_spei_lag1 + spei_neg_spei_lag2+spei_neg_spei_lag3+
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data
          , weights = pop_rur_unw) 


se4a <- cluster_se(m4a)
se4b <- cluster_se(m4b)
se4c <- cluster_se(m4c)


# urban

m5a <- lm(lf_share_urb_unw ~ spei_neg_spei_lag1 + spei_neg_spei_lag2+ spei_neg_spei_lag3+
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data
          , weights = pop_urb_unw
) 

m5b <- lm(worker_share_urb_unw ~ spei_neg_spei_lag1 + spei_neg_spei_lag2 + spei_neg_spei_lag3+
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data
          , weights = pop_urb_unw
) 

m5c <- lm(unemp_PS_urb_unw ~ spei_neg_spei_lag1 + spei_neg_spei_lag2+spei_neg_spei_lag3+
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data
          , weights = pop_urb_unw) 


se5a <- cluster_se(m5a)
se5b <- cluster_se(m5b)
se5c <- cluster_se(m5c)

#Display both 
stargazer(
  m4a, m4b, m4c,
  se = list(se4a[, 2], se4b[, 2], se4c[, 2]),
  p  = list(se4a[, 4], se4b[, 4], se4c[, 4]),
  title          = "Effect of Climate Shocks on Labor market (PS, rural population)",
  dep.var.labels = c("","",""),
  column.labels   = c("LF share", "Worker share", "Unemployment"),
  omit = c("factor\\(STATE\\)",
           "factor\\(year\\)",
           "factor\\(STATE\\):year",
           "Constant"),
  column.separate = c(1, 1),
  omit.stat    = c("f", "ser"),
  notes        = "Working age: 15–64.", # Columns (3)–(5) exclude Arunachal Pradesh and Meghalaya (missing FI data).
  notes.append = FALSE,
  type         = "text",
  label        = "tab:expanded_all"
)

stargazer(
  m5a, m5b, m5c,
  se = list(se5a[, 2], se5b[, 2], se5c[, 2]),
  p  = list(se5a[, 4], se5b[, 4], se5c[, 4]),
  title          = "Effect of Climate Shocks on Labor market (PS, urban population)",
  dep.var.labels = c("","",""),
  omit = c("factor\\(STATE\\)",
           "factor\\(year\\)",
           "factor\\(STATE\\):year",
           "Constant"),
  column.labels   = c("LF share", "Worker share", "Unemployment"),
  column.separate = c(1, 1),
  omit.stat    = c("f", "ser"),
  notes        = "Working age: 15–64.",
  notes.append = FALSE,
  type         = "text",
  label        = "tab:expanded_all"
)


# ================================= FLOODS =====================================

# GENERAL POP
m1a <- lm(lf_share_unw ~ FI_lag1 + FI_lag2 +
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data_flood
          , weights = state_pop
) 

m1b <- lm(worker_share_unw ~ FI_lag1 + FI_lag2 +
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data_flood
          , weights = state_pop
) 

m1c <- lm(unemp_PS_unw ~ FI_lag1 + FI_lag2 +
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data_flood
          , weights = state_pop) 


se1a <- cluster_se(m1a)
se1b <- cluster_se(m1b)
se1c <- cluster_se(m1c)

stargazer(
  m1a, m1b, m1c,
  se = list(se1a[, 2], se1b[, 2], se1c[, 2]),
  p  = list(se1a[, 4], se1b[, 4], se1c[, 4]),
  title          = "Effect of Climate Shocks on Labor market (PS)",
  dep.var.labels = c("","",""),
  column.labels   = c("LF share", "Worker share", "Unemployment"),
  omit = c("factor\\(STATE\\)",
           "factor\\(year\\)",
           "factor\\(STATE\\):year",
           "Constant"),
  column.separate = c(1, 1),
  omit.stat    = c("f", "ser"),
  notes        = "Working age: 15–64.", 
  notes.append = FALSE,
  type         = "text",
  label        = "tab:expanded_all")

# ==============================================================================
# GENDERED MODELS
# male
m2a <- lm(lf_share_m_unw ~ FI_lag1 + FI_lag2 +
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data_flood
          , weights = pop_m_unw
) 

m2b <- lm(worker_share_m_unw ~ FI_lag1 + FI_lag2 +
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data_flood
          , weights = pop_m_unw
) 

m2c <- lm(unemp_PS_m_unw ~ FI_lag1 + FI_lag2 +
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data_flood
          , weights = pop_m_unw) 


se2a <- cluster_se(m2a)
se2b <- cluster_se(m2b)
se2c <- cluster_se(m2c)


# FEMALE

m3a <- lm(lf_share_f_unw ~ FI_lag1 + FI_lag2 +
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data_flood
          , weights = pop_f_unw
) 

m3b <- lm(worker_share_f_unw ~ FI_lag1 + FI_lag2 +
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data_flood
          , weights = pop_f_unw
) 

m3c <- lm(unemp_PS_f_unw ~ FI_lag1 + FI_lag2 +
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data_flood
          , weights = pop_f_unw) 


se3a <- cluster_se(m3a)
se3b <- cluster_se(m3b)
se3c <- cluster_se(m3c)

#Display both 
stargazer(
  m2a, m2b, m2c,
  se = list(se2a[, 2], se2b[, 2], se2c[, 2]),
  p  = list(se2a[, 4], se2b[, 4], se2c[, 4]),
  title          = "Effect of Climate Shocks on Labor market (PS, male population)",
  dep.var.labels = c("","",""),
  column.labels   = c("LF share", "Worker share", "Unemployment"),
  omit = c("factor\\(STATE\\)",
           "factor\\(year\\)",
           "factor\\(STATE\\):year",
           "Constant"),
  column.separate = c(1, 1),
  omit.stat    = c("f", "ser"),
  notes        = "Working age: 15–64.", # Columns (3)–(5) exclude Arunachal Pradesh and Meghalaya (missing FI data).
  notes.append = FALSE,
  type         = "text",
  label        = "tab:expanded_all"
)

stargazer(
  m3a, m3b, m3c,
  se = list(se3a[, 2], se3b[, 2], se3c[, 2]),
  p  = list(se3a[, 4], se3b[, 4], se3c[, 4]),
  title          = "Effect of Climate Shocks on Labor market (PS, female population)",
  dep.var.labels = c("","",""),
  column.labels   = c("LF share", "Worker share", "Unemployment"),
  omit = c("factor\\(STATE\\)",
           "factor\\(year\\)",
           "factor\\(STATE\\):year",
           "Constant"),
  column.separate = c(1, 1),
  omit.stat    = c("f", "ser"),
  notes        = "Working age: 15–64.",
  notes.append = FALSE,
  type         = "text",
  label        = "tab:expanded_all"
)

# ==============================================================================
# sectoral   MODELS

# rural
m4a <- lm(lf_share_rur_unw ~ FI_state + FI_lag1 + FI_lag2 +
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data_flood
          , weights = pop_rur_unw
) 

m4b <- lm(worker_share_rur_unw ~ FI_state + FI_lag1 + FI_lag2 +
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data_flood
          , weights = pop_rur_unw
) 

m4c <- lm(unemp_PS_rur_unw ~ FI_state + FI_lag1 + FI_lag2 +
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data_flood
          , weights = pop_rur_unw) 


se4a <- cluster_se(m4a)
se4b <- cluster_se(m4b)
se4c <- cluster_se(m4c)


# urban

m5a <- lm(lf_share_urb_unw ~ FI_state + FI_lag1 + FI_lag2 +
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data_flood
          , weights = pop_urb_unw
) 

m5b <- lm(worker_share_urb_unw ~ FI_state + FI_lag1 + FI_lag2 +
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data_flood
          , weights = pop_urb_unw
) 

m5c <- lm(unemp_PS_urb_unw ~ FI_state + FI_lag1 + FI_lag2 +
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data_flood
          , weights = pop_urb_unw) 


se5a <- cluster_se(m5a)
se5b <- cluster_se(m5b)
se5c <- cluster_se(m5c)

#Display both 
stargazer(
  m4a, m4b, m4c,
  se = list(se4a[, 2], se4b[, 2], se4c[, 2]),
  p  = list(se4a[, 4], se4b[, 4], se4c[, 4]),
  title          = "Effect of Climate Shocks on Labor market (PS, rural population)",
  dep.var.labels = c("","",""),
  column.labels   = c("LF share", "Worker share", "Unemployment"),
  omit = c("factor\\(STATE\\)",
           "factor\\(year\\)",
           "factor\\(STATE\\):year",
           "Constant"),
  column.separate = c(1, 1),
  omit.stat    = c("f", "ser"),
  notes        = "Working age: 15–64.", # Columns (3)–(5) exclude Arunachal Pradesh and Meghalaya (missing FI data).
  notes.append = FALSE,
  type         = "text",
  label        = "tab:expanded_all"
)

stargazer(
  m5a, m5b, m5c,
  se = list(se5a[, 2], se5b[, 2], se5c[, 2]),
  p  = list(se5a[, 4], se5b[, 4], se5c[, 4]),
  title          = "Effect of Climate Shocks on Labor market (PS, urban population)",
  dep.var.labels = c("","",""),
  column.labels   = c("LF share", "Worker share", "Unemployment"),
  omit = c("factor\\(STATE\\)",
           "factor\\(year\\)",
           "factor\\(STATE\\):year",
           "Constant"),
  column.separate = c(1, 1),
  omit.stat    = c("f", "ser"),
  notes        = "Working age: 15–64.",
  notes.append = FALSE,
  type         = "text",
  label        = "tab:expanded_all"
)

