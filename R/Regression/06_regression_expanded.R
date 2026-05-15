# REGRESSION WITH BOTH CLIMATE SHOCK AND FLOOD INDEX, WITH THE OPTIMAL LAGS (2 lags for both)
# CANCELLED: REGRESSION WITH ONE CLIMATE SHOCK AT THE TIME, BUT WITH LABOR FORCE AS AN EXPLANATORY VARIABLE

out <- "/Users/ninabilirossi/Desktop/MSC THESIS/Data works/Code/Outputs/regressions/weighted_workers"
dir.create(out, recursive = TRUE, showWarnings = FALSE)

# ══════════════════════════════════════════════════════════════════════════════
# FULL DATASET
# ══════════════════════════════════════════════════════════════════════════════

m1a <- lm(s_casual_w_worker_PS_unw ~ spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2+
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data
          , weights = state_pop
) 

m1b <- lm(s_casual_w_worker_PS_unw ~ FI_state + FI_lag1 + FI_lag2+
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data_flood
          , weights = state_pop
) 

m1c <- lm(s_casual_w_worker_PS_unw ~  spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2 + FI_state + FI_lag1 + FI_lag2 +
          factor(STATE) + factor(year) + factor(STATE):year,
          data = data_flood
          , weights = state_pop
) 

se1a <- cluster_se(m1a)
se1b <- cluster_se(m1b)
se1c <- cluster_se(m1c)

stargazer(
  m1a, m1b, m1c,
  se = list(se1a[, 2], se1b[, 2], se1c[, 2]),
  p  = list(se1a[, 4], se1b[, 4], se1c[, 4]),
  title          = "Effect of Climate Shocks on Informality (PS, weighted)",
  dep.var.labels = "Informality Share (Casual/workers)",
  covariate.labels = c(
    "Negative SPEI-12",
    "Neg. SPEI-12 Lag 1",
    "Neg. SPEI-12 Lag 2",
    "Flood Index",
    "Flood Index Lag 1",
    "Flood Index Lag 2"
  ),
  column.labels   = c("SPEI only", "FI only", "SPEI + FI"),
  omit = c("factor\\(STATE\\)",
           "factor\\(year\\)",
           "factor\\(STATE\\):year"),
  column.separate = c(1, 1),
  omit.stat    = c("f", "ser"),
  notes        = "Working age: 15–64.", # Columns (3)–(5) exclude Arunachal Pradesh and Meghalaya (missing FI data).
  notes.append = FALSE,
  out          = file.path(out, "EXPANDED_all.tex"),
  type         = "latex",
  label        = "tab:expanded_all"
)

cat("✓ Table (Expanded regression) saved.\n")


# ══════════════════════════════════════════════════════════════════════════════
# BY GENDER
# ══════════════════════════════════════════════════════════════════════════════
# 1. FEMALE

m1a_f <- lm(s_casual_w_worker_PS_f_unw ~ spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2 +
          factor(STATE) + factor(year) + factor(STATE):year,
          data = data
          , weights = state_pop
) 

m1b_f <- lm(s_casual_w_worker_PS_f_unw ~ FI_state + FI_lag1 + FI_lag2 +
          factor(STATE) + factor(year) + factor(STATE):year,
          data = data_flood
          , weights = state_pop
) 

m1c_f <- lm(s_casual_w_worker_PS_f_unw ~  spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2 + FI_state + FI_lag1 + FI_lag2 +
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data_flood
          , weights = state_pop
) 

se1a_f <- cluster_se(m1a_f)
se1b_f <- cluster_se(m1b_f)
se1c_f <- cluster_se(m1c_f)

stargazer(
  m1a_f, m1b_f, m1c_f,
  se = list(se1a_f[, 2], se1b_f[, 2], se1c_f[, 2]),
  p  = list(se1a_f[, 4], se1b_f[, 4], se1c_f[, 4]),
  title          = "Effect of Climate Shocks on Informality Among Women (PS, weighted)",
  dep.var.labels = "Informality Share (Casual/workers)",
  covariate.labels = c(
    "Negative SPEI-12",
    "Neg. SPEI-12 Lag 1",
    "Neg. SPEI-12 Lag 2",
    "Flood Index",
    "Flood Index Lag 1",
    "Flood Index Lag 2"
  ),
  omit = c("factor\\(STATE\\)",
           "factor\\(year\\)",
           "factor\\(STATE\\):year"),
  column.labels   = c("SPEI", "FI", "SPEI + FI"),
  column.separate = c(1, 1),
  omit.stat    = c("f", "ser"),
  notes        = "Working age: 15–64.",
  notes.append = FALSE,
  out          = file.path(out, "EXPANDED_female.tex"),
  type         = "latex",
  label        = "tab:expanded_f"
)

cat("✓ Table (Expanded female regression) saved.\n")

# 2. MEN

m1a_m <- lm(s_casual_w_worker_PS_m_unw ~ spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2+
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data
          , weights = state_pop
) 

m1b_m <- lm(s_casual_w_worker_PS_m_unw ~ FI_state + FI_lag1 + FI_lag2 +
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data_flood
          , weights = state_pop
) 

m1c_m <- lm(s_casual_w_worker_PS_m_unw ~  spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2 + FI_state + FI_lag1 + FI_lag2 +
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data_flood
          , weights = state_pop
) 

se1a_m <- cluster_se(m1a_m)
se1b_m <- cluster_se(m1b_m)
se1c_m <- cluster_se(m1c_m)

stargazer(
  m1a_m, m1b_m, m1c_m,
  se = list(se1a_m[, 2], se1b_m[, 2], se1c_m[, 2]),
  p  = list(se1a_m[, 4], se1b_m[, 4], se1c_m[, 4]),
  title          = "Effect of Climate Shocks on Informality Among Men (PS, weighted)",
  dep.var.labels = "Informality Share (Casual/workers)",
  covariate.labels = c(
    "Negative SPEI-12",
    "Neg. SPEI-12 Lag 1",
    "Neg. SPEI-12 Lag 2",
    "Flood Index",
    "Flood Index Lag 1",
    "Flood Index Lag 2"
  ),
  omit = c("factor\\(STATE\\)",
           "factor\\(year\\)",
           "factor\\(STATE\\):year"),
  column.labels   = c("SPEI", "FI", "SPEI + FI"),
  column.separate = c(1, 1),
  omit.stat    = c("f", "ser"),
  notes        = "Working age: 15–64.",
  notes.append = FALSE,
  out          = file.path(out, "EXPANDED_male"),
  type         = "latex",
  label        = "tab:expanded_m"
)

cat("✓ Table (Expanded male regression) saved.\n")


# COMBINED TABLE FOR GENDER

# STARGAZER TABLE
stargazer(
  m1a_f, m1b_f, m1c_f,
  m1a_m, m1b_m, m1c_m,
  se = list(se1a_f[, 2], se1b_f[, 2], se1c_f[, 2], se1a_m[, 2], se1b_m[, 2], se1c_m[, 2]),
  p = list(se1a_f[, 4], se1b_f[, 4], se1c_f[, 4], se1a_m[, 4], se1b_m[, 4], se1c_m[, 4]),
  title = "Effect of Climate Shocks on Informality by Gender",
  dep.var.labels = "Informality Share (Casual/workers)",
  covariate.labels = c(
    "Negative SPEI-12",
    "Neg. SPEI-12 Lag 1",
    "Neg. SPEI-12 Lag 2",
    "Flood Index",
    "Flood Index Lag 1",
    "Flood Index Lag 2"
  ),
  column.labels = c("Female", "Male"),
  column.separate = c(3, 3),
  model.names = FALSE,
  omit = c(
    "factor\\(STATE\\)",
    "factor\\(year\\)",
    "factor\\(STATE\\):year"
  ),
  omit.stat = c("f", "ser"),
  notes = "Working age: 15–64.",
  notes.append = FALSE,
  out = file.path(out, "EXPANDED_gender_combined.tex"),
  type = "latex",
  label = "tab:expanded_gender"
)

# ══════════════════════════════════════════════════════════════════════════════
# BY SETTELMENT TYPE
# ══════════════════════════════════════════════════════════════════════════════
# 1. RURAL

m1a_rur <- lm(s_casual_w_worker_PS_rur_unw ~ spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2 +
          factor(STATE) + factor(year) + factor(STATE):year,
          data = data
          , weights = state_pop
) 

m1b_rur <- lm(s_casual_w_worker_PS_rur_unw ~ FI_state + FI_lag1 + FI_lag2+
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data_flood
          , weights = state_pop
)

m1c_rur <- lm(s_casual_w_worker_PS_rur_unw ~  spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2 + FI_state + FI_lag1 + FI_lag2 +
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data_flood
          , weights = state_pop
) 

se1a_rur <- cluster_se(m1a_rur)
se1b_rur <- cluster_se(m1b_rur)
se1c_rur <- cluster_se(m1c_rur)

stargazer(
  m1a_rur, m1b_rur, m1c_rur, 
  se = list(se1a_rur[, 2], se1b_rur[, 2], se1c_rur[, 2]),
  p  = list(se1a_rur[, 4], se1b_rur[, 4], se1c_rur[, 4]),
  title          = "Effect of Climate Shocks on Informality in Rural Areas (PS, weighted)",
  dep.var.labels = "Informality Share (Casual/workers)",
  covariate.labels = c(
    "Negative SPEI-12",
    "Neg. SPEI-12 Lag 1",
    "Neg. SPEI-12 Lag 2",
    "Flood Index",
    "Flood Index Lag 1",
    "Flood Index Lag 2"  ),
  omit = c("factor\\(STATE\\)",
           "factor\\(year\\)",
           "factor\\(STATE\\):year"),
  column.labels   = c("SPEI", "FI", "SPEI + FI"),
  column.separate = c(1, 1),
  omit.stat    = c("f", "ser"),
  notes        = "Working age: 15–64.",
  notes.append = FALSE,
  out          = file.path(out, "EXPANDED_RURAL.tex"),
  type         = "latex",
  label        = "tab:expanded_rural"
)

cat("✓ Table (Expanded rural regression) saved.\n")

# 2. URBAN

m1a_urb <- lm(s_casual_w_worker_PS_urb_unw ~ spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2 +
          factor(STATE) + factor(year) + factor(STATE):year,
          data = data
          , weights = state_pop
)

m1b_urb <- lm(s_casual_w_worker_PS_urb_unw ~ FI_state + FI_lag1 + FI_lag2+
          factor(STATE) + factor(year) + factor(STATE):year,
          data = data_flood
          , weights = state_pop
)

m1c_urb <- lm(s_casual_w_worker_PS_urb_unw ~  spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2 + FI_state + FI_lag1 + FI_lag2 +
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data_flood
          , weights = state_pop
)

se1a_urb <- cluster_se(m1a_urb)
se1b_urb <- cluster_se(m1b_urb)
se1c_urb <- cluster_se(m1c_urb)

stargazer(
  m1a_urb, m1b_urb, m1c_urb, 
  se = list(se1a_urb[, 2], se1b_urb[, 2], se1c_urb[, 2]),
  p  = list(se1a_urb[, 4], se1b_urb[, 4], se1c_urb[, 4]),
  title          = "Effect of Climate Shocks on Informality in Urban Areas (PS, weighted)",
  dep.var.labels = "Informality Share (Casual/workers)",
  covariate.labels = c(
    "Negative SPEI-12",
    "Neg. SPEI-12 Lag 1",
    "Neg. SPEI-12 Lag 2",
    "Flood Index",
    "Flood Index Lag 1",
    "Flood Index Lag 2" ),
  omit = c("factor\\(STATE\\)",
           "factor\\(year\\)",
           "factor\\(STATE\\):year"),
  column.labels   = c("SPEI"),
  column.separate = c(1, 1),
  omit.stat    = c("f", "ser"),
  notes        = "Working age: 15–64.",
  notes.append = FALSE,
  out          = file.path(out, "EXPANDED_URBAN.tex"),
  type         = "latex",
  label        = "tab:expanded_urban"
)

cat("✓ Table (Expanded urban regression) saved.\n")

# COMBINED TABLE FOR SETTLEMENT

# STARGAZER TABLE
stargazer(
  m1a_rur, m1b_rur, m1c_rur,
  m1a_urb, m1b_urb, m1c_urb,
  se = list(se1a_rur[, 2], se1b_rur[, 2], se1c_rur[, 2], se1a_urb[, 2], se1b_urb[, 2], se1c_urb[, 2]),
  p = list(se1a_rur[, 4], se1b_rur[, 4], se1c_rur[, 4], se1a_urb[, 4], se1b_urb[, 4], se1c_urb[, 4]),
  title = "Effect of Climate Shocks on Informality by Settlement",
  dep.var.labels = "Informality Share (Casual/workers)",
  covariate.labels = c(
    "Negative SPEI-12",
    "Neg. SPEI-12 Lag 1",
    "Neg. SPEI-12 Lag 2",
    "Flood Index",
    "Flood Index Lag 1",
    "Flood Index Lag 2"
  ),
  column.labels = c("Rural", "Urban"),
  column.separate = c(3, 3),
  model.names = FALSE,
  omit = c(
    "factor\\(STATE\\)",
    "factor\\(year\\)",
    "factor\\(STATE\\):year"
  ),
  omit.stat = c("f", "ser"),
  notes = "Working age: 15–64.",
  notes.append = FALSE,
  out = file.path(out, "EXPANDED_settlement_combined.tex"),
  type = "latex",
  label = "tab:expanded_settlement"
)

cat("✓ Table (Expanded settlement regression) saved.\n")
