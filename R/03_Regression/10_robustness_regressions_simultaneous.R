
# ==============================================================================
# This script runs regressions to analyze the impact of the drought and flood, 
# on informality when indices are included simultaneously.

# Results are presented in stargazer tables for export.

# see Manuscript: section 6.3:Robustness for details.

# ==============================================================================


# USING FI

m1a <- lm(s_casual_w_worker_PS_unw ~ spei_negative + spei_neg_spei_lag1 + 
            spei_neg_spei_lag2 + spei_neg_spei_lag3 + 
            FI_state + FI_lag1 + FI_lag2 +
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data_flood, weights = state_pop)

m1b <- lm(s_casual_w_worker_W_unw ~ spei_negative + spei_neg_spei_lag1 + 
            spei_neg_spei_lag2 + spei_neg_spei_lag3 + 
            FI_state + FI_lag1 + FI_lag2 +
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data_flood, weights = state_pop)


# ── Clustered SEs ─────────────────────────────────────────────────────────────
se1a <- cluster_se(m1a)
se1b <- cluster_se(m1b)

# ── Print to console ──────────────────────────────────────────────────────────
stargazer(
  m1a, m1b, 
  se             = list(se1a[, 2], se1b[, 2]),
  p              = list(se1a[, 4], se1b[, 4]),
  title          = "Effect of Climate Shocks on Informality (PS)",
  column.labels  = c("PS status", "W status"),
  dep.var.labels = "Informality (PS)",
  omit           = c("factor\\(STATE\\)", "factor\\(year\\)", "factor\\(STATE\\):year", "Constant"),
  omit.stat      = c("f", "ser"),
  notes          = "",
  notes.append   = FALSE,
  label          = "tab:simulateneousFI",
  type           = "latex"
)



# USING PR
# ── Models ────────────────────────────────────────────────────────────────────

m1a <- lm(s_casual_w_worker_PS_unw ~ spei_negative + spei_neg_spei_lag1 + 
            spei_neg_spei_lag2 + spei_neg_spei_lag3 + 
            pr_score + pr_lag1 + pr_lag2 +
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data, weights = state_pop)

m1b <- lm(s_casual_w_worker_W_unw ~ spei_negative + spei_neg_spei_lag1 + 
            spei_neg_spei_lag2 + spei_neg_spei_lag3 + 
            pr_score + pr_lag1 + pr_lag2 +
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data, weights = state_pop)


# ── Clustered SEs ─────────────────────────────────────────────────────────────
se1a <- cluster_se(m1a)
se1b <- cluster_se(m1b)

# ── Print to console ──────────────────────────────────────────────────────────
stargazer(
  m1a, m1b,
  se             = list(se1a[, 2], se1b[, 2]),
  p              = list(se1a[, 4], se1b[, 4]),
  title          = "Effect of Climate Shocks on Informality",
  dep.var.labels = "Informality (PS)",
  column.labels  = c("PS status", "W status"),
  omit           = c("factor\\(STATE\\)", "factor\\(year\\)", "factor\\(STATE\\):year", "Constant"),
  omit.stat      = c("f", "ser"),
  notes          = "",
  notes.append   = FALSE,
  label          = "tab:simultaneousPR",
  type           = "latex"
)

# ── Weekly Models ────────────────────────────────────────────────────────────────────

m1a <- lm(s_casual_w_worker_W_unw ~ spei_negative + spei_neg_spei_lag1 + 
            spei_neg_spei_lag2 + spei_neg_spei_lag3 + 
            pr_score + pr_lag1 + pr_lag2 +
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data, weights = state_pop)

m1b <- lm(s_casual_w_worker_W_unw ~ spei_negative + spei_neg_spei_lag1 + 
            spei_neg_spei_lag2 + spei_neg_spei_lag3 + 
            pr_score + pr_lag1 + pr_lag2 +
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data, weights = state_pop)

m1c <- lm(s_casual_w_worker_W_unw ~ spei_negative + spei_neg_spei_lag1 + 
            spei_neg_spei_lag2 + spei_neg_spei_lag3 + 
            pr_score + pr_lag1 + pr_lag2 +
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data, weights = state_pop)


# ── Clustered SEs ─────────────────────────────────────────────────────────────
se1a <- cluster_se(m1a)
se1b <- cluster_se(m1b)
se1c <- cluster_se(m1c)

# ── Print to console ──────────────────────────────────────────────────────────
stargazer(
  m1a, m1b, m1c,
  se             = list(se1a[, 2], se1b[, 2], se1c[, 2]),
  p              = list(se1a[, 4], se1b[, 4], se1c[, 4]),
  title          = "Effect of Climate Shocks on Informality (W)",
  dep.var.labels = "Informality (PS)",
  omit           = c("factor\\(STATE\\)", "factor\\(year\\)", "factor\\(STATE\\):year", "Constant"),
  omit.stat      = c("f", "ser"),
  notes          = "",
  notes.append   = FALSE,
  label          = "tab:simultaneousW",
  type           = "text"
)


