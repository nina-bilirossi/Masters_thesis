# REGRESSING ON WORKERS

# We run a regression with the optimal lags for Flood Index (2 lags for both) and we report the results in a stargazer table.
# Specifications:
# 2 way FE
# Clustered SE at the state level
# run only with 1 measure of informality for now
# test the following flood indices: the FI index (excluding the 2 states taht don't have indices), and the pr index (all states but LAKSHADWEEP, as usual)
# both with 2 lags

out <- "/Users/ninabilirossi/Desktop/MSC THESIS/Data works/Code/Outputs/latex food/FLOOD"
dir.create(out, recursive = TRUE, showWarnings = FALSE)

# ── Informality measure ───────────────────────────────────────────────────────
# s_casual_w_worker_PS_unw  (PS = principal + subsidiary status, unweighted)

# ══════════════════════════════════════════════════════════════════════════════
# TABLE 1 – FI Index with 2 lags (restricted sample: excl. ARUNACHAL & MEGHALAYA)
# ══════════════════════════════════════════════════════════════════════════════

# Spec 1a: contemporaneous only
m1a <- plm(s_casual_w_worker_PS_unw ~ FI_state,
           data   = pdata_fi,
           model  = "within",
           effect = "twoways")

# Spec 1b: + lag 1
m1b <- plm(s_casual_w_worker_PS_unw ~ FI_state + FI_lag1,
           data   = pdata_fi,
           model  = "within",
           effect = "twoways")

# Spec 1c: + lag 1 & lag 2  (optimal)
m1c <- plm(s_casual_w_worker_PS_unw ~ FI_state + FI_lag1 + FI_lag2,
           data   = pdata_fi,
           model  = "within",
           effect = "twoways")

se1a <- cluster_se(m1a)
se1b <- cluster_se(m1b)
se1c <- cluster_se(m1c)

stargazer(
  m1a, m1b, m1c,
  se        = list(se1a[, 2], se1b[, 2], se1c[, 2]),
  p         = list(se1a[, 4], se1b[, 4], se1c[, 4]),
  title     = "Effect of Flood Index (FI) on Casual Labour-Force Participation (PS, Unweighted)",
  dep.var.labels   = "Share Casual Workers (PS, Unw.)",
  covariate.labels = c("Flood Index", "Flood Index Lag 1", "Flood Index Lag 2"),
  # column.labels    = c("Contemp.", "+ Lag 1", "+ Lag 1\\&2"),
  add.lines = list(
    c("State FE",              "Yes", "Yes", "Yes"),
    c("Year FE",               "Yes", "Yes", "Yes"),
    c("Clustered SE (State)",  "Yes", "Yes", "Yes")),
  omit.stat    = c("f", "ser"),
  notes        = "Clustered standard errors at the state level in parentheses. Sample excludes LAKSHADWEEP, ARUNACHAL PRADESH, and MEGHALAYA due to missing flood index data.",
  notes.append = FALSE,
  out          = file.path(out, "table1_FI_index_workers.tex"),
  type         = "latex",
  label        = "tab:fi_index"
)

cat("✓ Table 1 (FI Index) saved.\n")

# ══════════════════════════════════════════════════════════════════════════════
# TABLE 2 – PR Index with 2 lags (full sample excl. LAKSHADWEEP only)
# ══════════════════════════════════════════════════════════════════════════════

# Spec 2a: contemporaneous only
m2a <- plm(s_casual_w_worker_PS_unw ~ pr_score,
           data   = pdata,
           model  = "within",
           effect = "twoways")

# Spec 2b: + lag 1
m2b <- plm(s_casual_w_worker_PS_unw ~ pr_score + pr_lag1,
           data   = pdata,
           model  = "within",
           effect = "twoways")

# Spec 2c: + lag 1 & lag 2  (optimal)
m2c <- plm(s_casual_w_worker_PS_unw ~  pr_score + pr_lag1 + pr_lag2,
           data   = pdata,
           model  = "within",
           effect = "twoways")

se2a <- cluster_se(m2a)
se2b <- cluster_se(m2b)
se2c <- cluster_se(m2c)

stargazer(
  m2a, m2b, m2c,
  se        = list(se2a[, 2], se2b[, 2], se2c[, 2]),
  p         = list(se2a[, 4], se2b[, 4], se2c[, 4]),
  title     = "Effect of Positive Precipitation Index (PR) on Casual Labour-Force Participation (PS, Unweighted)",
  dep.var.labels   = "Share Casual Workers (PS, Unw.)",
  covariate.labels = c("PR Index", "PR Index Lag 1", "PR Index Lag 2"),
  # column.labels    = c("Contemp.", "+ Lag 1", "+ Lag 1\\&2"),
  add.lines = list(
    c("State FE",              "Yes", "Yes", "Yes"),
    c("Year FE",               "Yes", "Yes", "Yes"),
    c("Clustered SE (State)",  "Yes", "Yes", "Yes")
    #c("Sample",                "Full", "Full", "Full")
  ),
  omit.stat    = c("f", "ser"),
  notes        = "Clustered standard errors at the state level in parentheses. Sample excludes LAKSHADWEEP only. PR index captures positive SPEI (excess precipitation) episodes.",
  notes.append = FALSE,
  out          = file.path(out, "table2_PR_index_workers.tex"),
  type         = "latex",
  label        = "tab:pr_index"
)

cat("✓ Table 2 (PR Index) saved.\n")

# ══════════════════════════════════════════════════════════════════════════════
# TABLE 3 – Combined: FI vs PR (optimal lag spec only)
# ══════════════════════════════════════════════════════════════════════════════

stargazer(
  m1c, m2c,
  se        = list(se1c[, 2], se2c[, 2]),
  p         = list(se1c[, 4], se2c[, 4]),
  title     = "Flood Index vs. PR Index: Optimal Lag Specification",
  dep.var.labels   = "Share Casual Workers (PS, Unw.)",
  covariate.labels = c(
    "Flood Index",     "Flood Index Lag 1", "Flood Index Lag 2",
    "PR Index",        "PR Index Lag 1",    "PR Index Lag 2"
  ),
  # column.labels = c("FI Index", "PR Index"),
  add.lines = list(
    c("State FE",              "Yes", "Yes"),
    c("Year FE",               "Yes", "Yes"),
    c("Clustered SE (State)",  "Yes", "Yes")
    #c("Sample",                "Restricted", "Full")
  ),
  omit.stat    = c("f", "ser"),
  notes        = "Clustered standard errors at the state level in parentheses. FI column excludes ARUNACHAL PRADESH and MEGHALAYA due to missing data; PR column uses full sample (excl. LAKSHADWEEP).",
  notes.append = FALSE,
  out          = file.path(out, "table3_FI_vs_PR_workers.tex"),
  type         = "latex",
  label        = "tab:fi_vs_pr"
)

cat("✓ Table 3 (FI vs PR) saved.\n")
cat("\nAll tables written to:", out, "\n")