
# ══════════════════════════════════════════════════════════════════════════════
# TABLE 1 – FI
# ══════════════════════════════════════════════════════════════════════════════
# ── Models ────────────────────────────────────────────────────────────────────

m1a <- lm(s_casual_w_worker_PS_unw ~ spei_negative + spei_neg_spei_lag1 + 
            spei_neg_spei_lag2 + spei_neg_spei_lag3 + 
            FI_state + FI_lag1 + FI_lag2 +
            spei_negative:FI_state +                        # interaction term
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data_flood, weights = state_pop)

m1b <- lm(s_casual_w_worker_PS_unw ~ spei_negative + spei_neg_spei_lag1 + 
            spei_neg_spei_lag2 + spei_neg_spei_lag3 + 
            FI_state + FI_lag1 + FI_lag2 +
            spei_neg_spei_lag1:FI_lag1 +                        # interaction term
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data_flood, weights = state_pop)

m1c <- lm(s_casual_w_worker_PS_unw ~ spei_negative + spei_neg_spei_lag1 + 
            spei_neg_spei_lag2 + spei_neg_spei_lag3 + 
            FI_state + FI_lag1 + FI_lag3 +
            spei_neg_spei_lag2:FI_lag2 +                        # interaction term
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data_flood, weights = state_pop)

# ── Clustered SEs ─────────────────────────────────────────────────────────────
se1a <- cluster_se(m1a)
se1b <- cluster_se(m1b)
se1c <- cluster_se(m1c)

# ── Print to console ──────────────────────────────────────────────────────────
stargazer(
  m1a, m1b, m1c,
  se             = list(se1a[, 2], se1b[, 2], se1c[, 2]),
  p              = list(se1a[, 4], se1b[, 4], se1c[, 4]),
  title          = "Effect of Climate Shocks on Informality (PS)",
  dep.var.labels = "Informality (PS)",
  column.labels  = c("Neg. SPEI-12", "Flood Index", "PR Index"),
  omit           = c("factor\\(STATE\\)", "factor\\(year\\)", "factor\\(STATE\\):year", "Constant"),
  add.lines = list(
    c("State FE",             "Yes", "Yes", "Yes"),
    c("Year FE",              "Yes", "Yes", "Yes"),
    c("Linear trends", "Yes", "Yes", "Yes")
  ),
  omit.stat      = c("f", "ser"),
  notes          = "",
  notes.append   = FALSE,
  label          = "tab:neg_spei",
  type           = "text"
)


# ══════════════════════════════════════════════════════════════════════════════
# TABLE 2 – PR
# ══════════════════════════════════════════════════════════════════════════════
# ── Models ────────────────────────────────────────────────────────────────────

m1a <- lm(s_casual_w_worker_PS_unw ~ spei_negative + spei_neg_spei_lag1 + 
            spei_neg_spei_lag2 + spei_neg_spei_lag3 + 
            pr_score + pr_lag1 + pr_lag2 +
            spei_negative:pr_score +  spei_neg_spei_lag1:pr_lag1 +   spei_neg_spei_lag2:pr_lag2 +                       # interaction term
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data, weights = state_pop)

m1b <- lm(s_casual_w_worker_PS_unw ~ spei_negative + spei_neg_spei_lag1 + 
            spei_neg_spei_lag2 + spei_neg_spei_lag3 + 
            pr_score + pr_lag1 + pr_lag2 +
            spei_neg_spei_lag1:pr_score +  spei_neg_spei_lag2:pr_lag1 +   spei_neg_spei_lag3:pr_lag2 +                       # interaction term
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data, weights = state_pop)

m1c <- lm(s_casual_w_worker_W_unw ~ spei_negative + spei_neg_spei_lag1 + 
            spei_neg_spei_lag2 + spei_neg_spei_lag3 + 
            pr_score + pr_lag1 +
            spei_neg_spei_lag2:pr_lag2 +                        # interaction term
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data, weights = state_pop)

install.packages("marginaleffects")
library(marginaleffects)

# Marginal effect of drought lag1 at mean flood lag1
avg_slopes(m1a,
           variables = "spei_neg_spei_lag1",
           newdata = datagrid(pr_lag1 = mean(data$pr_score, na.rm = TRUE)))  # mean of a different column

# Marginal effect of flood lag1 at mean drought lag1
avg_slopes(m1a,
           variables = "pr_lag1",
           newdata = datagrid(spei_neg_spei_lag1 = mean(data$spei_negative, na.rm = TRUE)))


# ── Clustered SEs ─────────────────────────────────────────────────────────────
se1a <- cluster_se(m1a)
se1b <- cluster_se(m1b)
se1c <- cluster_se(m1c)

# ── Print to console ──────────────────────────────────────────────────────────
stargazer(
  m1a, m1b, m1c,
  se             = list(se1a[, 2], se1b[, 2], se1c[, 2]),
  p              = list(se1a[, 4], se1b[, 4], se1c[, 4]),
  title          = "Effect of Climate Shocks on Informality (PS)",
  dep.var.labels = "Informality (PS)",
  column.labels  = c("Neg. SPEI-12", "Flood Index", "PR Index"),
  omit           = c("factor\\(STATE\\)", "factor\\(year\\)", "factor\\(STATE\\):year", "Constant"),
  omit.stat      = c("f", "ser"),
  notes          = "",
  notes.append   = FALSE,
  label          = "tab:neg_spei",
  type           = "text"
)

