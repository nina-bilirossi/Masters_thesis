# REGRESSING ON WORKERS

# ── Output paths ───────────────────────────────────────────────────────────────
out  <- "/Users/ninabilirossi/Desktop/MSC THESIS/Data works/Code/Outputs/regressions/weighted_workers"
dir.create(out,  recursive = TRUE, showWarnings = FALSE)

# ══════════════════════════════════════════════════════════════════════════════
# SPEI — Gender subgroups (optimal spec: contemp. + lag1 + lag2)
# Male outcome:   s_casual_w_worker_PS_m_unw
# Female outcome: s_casual_w_worker_PS_f_unw
# ══════════════════════════════════════════════════════════════════════════════

# ── Full SPEI ──────────────────────────────────────────────────────────────────
spei_full_m <- lm(s_casual_w_worker_PS_m_unw ~ spei_spei12 + spei_spei_lag1 + spei_spei_lag2+
                    factor(STATE) + factor(year) + factor(STATE):year,
                  data = data
                  , weights = state_pop
) 

spei_full_f <- lm(s_casual_w_worker_PS_f_unw ~ spei_spei12 + spei_spei_lag1 + spei_spei_lag2+
                    factor(STATE) + factor(year) + factor(STATE):year,
                  data = data
                  , weights = state_pop
) 

se_spei_full_m <- cluster_se(spei_full_m)
se_spei_full_f <- cluster_se(spei_full_f)

stargazer(
  spei_full_m, spei_full_f,
  se = list(se_spei_full_m[, 2], se_spei_full_f[, 2]),
  p  = list(se_spei_full_m[, 4], se_spei_full_f[, 4]),
  title          = "Full SPEI and Casual Labour-Force Participation by Gender (PS, Weighted)",
  dep.var.labels = c("Male Share", "Female Share"),
  omit = c("factor\\(STATE\\)",
           "factor\\(year\\)",
           "factor\\(STATE\\):year"),
  column.labels  = c("Male", "Female"),
  omit.stat    = c("f", "ser"),
  notes        = "Clustered standard errors at the state level in parentheses.",
  notes.append = FALSE,
  out          = file.path(out, "GENDER_SPEI_full.tex"),
  type         = "latex",
  label        = "tab:gender_full_spei"
)
cat("✓ SPEI Full — gender table saved.\n")

# ── Negative SPEI ─────────────────────────────────────────────────────────────
spei_neg_m <- lm(s_casual_w_worker_PS_m_unw ~  spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2+
                   spei_neg_spei_lag3 +
                factor(STATE) + factor(year) + factor(STATE):year,
                data = data
                 , weights = state_pop
) 

spei_neg_f <- lm(s_casual_w_worker_PS_f_unw ~  spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2+
                   spei_neg_spei_lag3 +
                   factor(STATE) + factor(year) + factor(STATE):year,
                 data = data
                 , weights = state_pop
) 

se_spei_neg_m <- cluster_se(spei_neg_m)
se_spei_neg_f <- cluster_se(spei_neg_f)

stargazer(
  spei_neg_m, spei_neg_f,
  se = list(se_spei_neg_m[, 2], se_spei_neg_f[, 2]),
  p  = list(se_spei_neg_m[, 4], se_spei_neg_f[, 4]),
  title          = "Negative SPEI Shocks and Casual Labour-Force Participation by Gender (PS, Weighted)",
  dep.var.labels = c("Male Share", "Female Share"),
  omit = c("factor\\(STATE\\)",
           "factor\\(year\\)",
           "factor\\(STATE\\):year"),
  column.labels  = c("Male", "Female"),
  omit.stat    = c("f", "ser"),
  add.lines = list(
    c("State FE",             "Yes", "Yes"),
    c("Year FE",              "Yes", "Yes"),
    c("State trends", "No", "No")
  ),
  notes.append = FALSE,
  out          = file.path(out, "GENDER_SPEI_neg.tex"),
  type         = "latex",
  label        = "tab:gender_neg_spei"
)

stargazer(
  spei_neg_m, spei_neg_f,
  se = list(se_spei_neg_m[, 2], se_spei_neg_f[, 2]),
  p  = list(se_spei_neg_m[, 4], se_spei_neg_f[, 4]),
  title          = "Casual Labour-Force Participation by Gender (W)",
  dep.var.labels = c("Male Share", "Female Share"),
  omit = c("factor\\(STATE\\)",
           "factor\\(year\\)",
           "factor\\(STATE\\):year", "Constant"),
  column.labels  = c("Male", "Female"),
  omit.stat    = c("f", "ser"),
  notes        = "",
  notes.append = FALSE,
  type         = "text",
  label        = "tab:gender_neg_spei"
)

cat("✓ SPEI Negative — gender table saved.\n")

# ══════════════════════════════════════════════════════════════════════════════
# FLOOD — Gender subgroups (optimal spec: contemp. + lag1 + lag2)
# ══════════════════════════════════════════════════════════════════════════════

# ── FI Index ───────────────────────────────────────────────────────────────────
fi_m <- lm(s_casual_w_worker_PS_m_unw ~  FI_state + FI_lag1 + FI_lag2+
             factor(STATE) + factor(year) + factor(STATE):year,
           data = data_flood
           , weights = state_pop
) 

fi_f <- lm(s_casual_w_worker_PS_f_unw ~ FI_state + FI_lag1 + FI_lag2+
             factor(STATE) + factor(year) + factor(STATE):year,
           data = data_flood
           , weights = state_pop
) 

se_fi_m <- cluster_se(fi_m)
se_fi_f <- cluster_se(fi_f)

stargazer(
  fi_m, fi_f,
  se = list(se_fi_m[, 2], se_fi_f[, 2]),
  p  = list(se_fi_m[, 4], se_fi_f[, 4]),
  title          = "Flood Index (FI) and Casual Labour-Force Participation by Gender (PS)",
  dep.var.labels = c("Male Share", "Female Share"),
  omit = c("factor\\(STATE\\)",
           "factor\\(year\\)",
           "factor\\(STATE\\):year", "Constant"),
  column.labels  = c("Male", "Female"),
  omit.stat    = c("f", "ser"),
  add.lines = list(
    c("State FE",             "Yes", "Yes"),
    c("Year FE",              "Yes", "Yes"),
    c("State trends", "Yes", "Yes")),
  notes.append = FALSE,
  out          = file.path(out, "GENDER_FLOOD_FI.tex"),
  type         = "latex",
  label        = "tab:gender_fi"
)

stargazer(
  fi_m, fi_f,
  se = list(se_fi_m[, 2], se_fi_f[, 2]),
  p  = list(se_fi_m[, 4], se_fi_f[, 4]),
  title          = "Casual Labour-Force Participation by Gender (PS)",
  dep.var.labels = c("Male Share", "Female Share"),
  omit = c("factor\\(STATE\\)",
           "factor\\(year\\)",
           "factor\\(STATE\\):year", "Constant"),
  column.labels  = c("Male", "Female"),
  omit.stat    = c("f", "ser"),
  add.lines = list(
    c("State FE",             "Yes", "Yes"),
    c("Year FE",              "Yes", "Yes"),
    c("State trends", "Yes", "Yes")),
  notes.append = FALSE,
  type         = "text",
  label        = "tab:gender_fi"
)
cat("✓ FI Index — gender table saved.\n")

# ── PR Index ───────────────────────────────────────────────────────────────────
pr_m <- lm(s_casual_w_worker_W_m_unw ~ pr_score + pr_lag1 + #pr_lag2 +
             factor(STATE) + factor(year) + factor(STATE):year,
           data = data
           , weights = state_pop
) 

pr_f <- lm(s_casual_w_worker_W_f_unw ~ pr_score + pr_lag1 + #pr_lag2 +
             factor(STATE) + factor(year) + factor(STATE):year,
           data = data
           , weights = state_pop
)

se_pr_m <- cluster_se(pr_m)
se_pr_f <- cluster_se(pr_f)

stargazer(
  pr_m, pr_f,
  se = list(se_pr_m[, 2], se_pr_f[, 2]),
  p  = list(se_pr_m[, 4], se_pr_f[, 4]),
  title          = "PR Index and Casual Labour-Force Participation by Gender (PS)",
  dep.var.labels = c("Male Share", "Female Share"),
  omit = c("factor\\(STATE\\)",
           "factor\\(year\\)",
           "factor\\(STATE\\):year", "Constant"),
  add.lines = list(
    c("State FE",             "Yes", "Yes"),
    c("Year FE",              "Yes", "Yes"),
    c("State trends", "Yes", "Yes")),
  column.labels  = c("Male", "Female"),
  omit.stat    = c("f", "ser"),
  notes        = "Clustered standard errors at the state level in parentheses. ", #Optimal lag specification (contemp. + 2 lags).
  notes.append = FALSE,
  out          = file.path(out, "GENDER_FLOOD_PR.tex"),
  type         = "latex",
  label        = "tab:gender_pr"
)

stargazer(
  pr_m, pr_f,
  se = list(se_pr_m[, 2], se_pr_f[, 2]),
  p  = list(se_pr_m[, 4], se_pr_f[, 4]),
  title          = "PR Index and Casual Labour-Force Participation by Gender (PS)",
  dep.var.labels = c("Male Share", "Female Share"),
  omit = c("factor\\(STATE\\)",
           "factor\\(year\\)",
           "factor\\(STATE\\):year", "Constant"),
  add.lines = list(
    c("State FE",             "Yes", "Yes"),
    c("Year FE",              "Yes", "Yes"),
    c("State trends", "Yes", "Yes")),
  column.labels  = c("Male", "Female"),
  omit.stat    = c("f", "ser"),
  notes        = "Clustered standard errors at the state level in parentheses. ", #Optimal lag specification (contemp. + 2 lags).
  notes.append = FALSE,
  type         = "text",
  label        = "tab:gender_pr"
)
cat("✓ PR Index — gender table saved.\n")

cat("\nAll gender subgroup tables saved.\n")

# ==============================================================================
# MEGA TABLE
# ==============================================================================

stargazer(
  spei_neg_m, spei_neg_f, fi_m, fi_f,
  se             = list(se_spei_neg_m[, 2], se_spei_neg_f[, 2],
                        se_fi_m[, 2],       se_fi_f[, 2]),
  p              = list(se_spei_neg_m[, 4], se_spei_neg_f[, 4],
                        se_fi_m[, 4],       se_fi_f[, 4]),
  title          = "Casual Labour-Force Participation by Gender (W)",
  dep.var.caption        = "Casual Labour Share", # Centers this clean title across all columns
  dep.var.labels.include = FALSE,
  column.labels  = c("Male", "Female", "Male", "Female"),
  omit           = c("factor\\(STATE\\)", "factor\\(year\\)", "factor\\(STATE\\):year", "Constant"),
  omit.stat      = c("f", "ser"),
  add.lines = list(
    c("State FE",     "Yes", "Yes", "Yes", "Yes"),
    c("Year FE",      "Yes", "Yes", "Yes", "Yes"),
    c("State trends", "Yes", "Yes", "Yes", "Yes")
  ),
  notes          = "",
  # Uncomment and adjust the variable names below to order your coefficients cleanly:
  # order        = c("spei_variable_name", "fi_variable_name"),
  notes.append   = FALSE,
  label          = "tab:gender_combined",
  type           = "text"
)

cat("✓ Combined gender table saved.\n")

