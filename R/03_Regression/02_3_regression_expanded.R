
# ==============================================================================
# This script runs regressions to analyze the impact of drought and flood indices
# on informality in the general population.

# It overlaps with file 02_1 and 02_2, joining them in a table 
# ready to export to latex.

# ==============================================================================

# MAIN REGRESSIONS – GENERAL POPULATION
data_robust <-  data |> filter(STATE != "PUDUCHERRY" & STATE != "DADRA & NAGAR HAVELI & DAMAN & DIU") |>
  filter(STATE != "JAMMU AND KASHMIR" & STATE != "LADAKH")
data_flood_robust <- data_flood |> filter(STATE != "PUDUCHERRY" & STATE != "DADRA & NAGAR HAVELI & DAMAN & DIU") |> 
  filter(STATE != "JAMMU AND KASHMIR" & STATE != "LADAKH")

# ══════════════════════════════════════════════════════════════════════════════
# FULL DATASET
# ══════════════════════════════════════════════════════════════════════════════

m1a <- lm(s_casual_w_worker_PS_unw ~ spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2+ spei_neg_spei_lag3+
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data
          , weights = state_pop
) 

# m1a <- lm(s_casual_w_worker_W_unw ~ spei_spei12 + spei_spei_lag1 + spei_spei_lag2+ spei_spei_lag3+
#             factor(STATE) + factor(year) + factor(STATE):year,
#           data = data
#           , weights = state_pop
# ) 

m1b <- lm(s_casual_w_worker_PS_unw ~ FI_state + FI_lag1 + FI_lag2+
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data_flood
          , weights = state_pop
) 

# m1c <- lm(s_casual_w_worker_W_unw ~  spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2 + pr_score + pr_lag1 + pr_lag2 +
#           factor(STATE) + factor(year) + factor(STATE):year,
#           data = data
#           , weights = state_pop
# ) 

m1c <- lm(s_casual_w_worker_PS_unw ~  pr_score + pr_lag1 + pr_lag2 +
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data
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
  omit = c("factor\\(STATE\\)",
           "factor\\(year\\)",
           "factor\\(STATE\\):year", "Constant"),
  column.separate = c(1, 1),
  omit.stat    = c("f", "ser"),
  notes        = "Working age: 15–64.", # Columns (3)–(5) exclude Arunachal Pradesh and Meghalaya (missing FI data).
  notes.append = FALSE,
  type         = "text",
  label        = "tab:expanded_all"
)

cat("✓ Table (Expanded regression) saved.\n")
