library(plm)
library(lmtest)
library(sandwich)
library(stargazer)
library(dplyr)

# ── Output paths ───────────────────────────────────────────────────────────────
out  <- "/Users/ninabilirossi/Desktop/MSC THESIS/Data works/Code/Outputs/latex food/robustness"
dir.create(out,  recursive = TRUE, showWarnings = FALSE)

# ── Load data ──────────────────────────────────────────────────────────────────
data <- read.csv("/Users/ninabilirossi/Desktop/MSC THESIS/Data works/Code/Outputs/final material/regression_dataframe.csv") |>
  filter(STATE != "LAKSHADWEEP")

data_flood <- data |>
  filter(STATE != "ARUNACHAL PRADESH") |>
  filter(STATE != "MEGHALAYA")

# ── Panel setup ────────────────────────────────────────────────────────────────
pdata_spei  <- pdata.frame(data,       index = c("STATE", "year"))
pdata_flood <- pdata.frame(data_flood, index = c("STATE", "year"))

# ── Helper: clustered SE at state level ───────────────────────────────────────
cluster_se <- function(model) {
  coeftest(model, vcov = vcovHC(model, type = "HC1", cluster = "group"))
}

# ══════════════════════════════════════════════════════════════════════════════
# SPEI — Gender subgroups (optimal spec: contemp. + lag1 + lag2)
# Male outcome:   s_casual_w_lf_PS_m_unw
# Female outcome: s_casual_w_lf_PS_f_unw
# ══════════════════════════════════════════════════════════════════════════════

# ── Full SPEI ──────────────────────────────────────────────────────────────────
spei_full_m <- plm(s_casual_w_lf_PS_m_unw ~ spei_spei12 + spei_spei_lag1 + spei_spei_lag2,
                   data = pdata_spei, model = "within", effect = "twoways")

spei_full_f <- plm(s_casual_w_lf_PS_f_unw ~ spei_spei12 + spei_spei_lag1 + spei_spei_lag2,
                   data = pdata_spei, model = "within", effect = "twoways")

se_spei_full_m <- cluster_se(spei_full_m)
se_spei_full_f <- cluster_se(spei_full_f)

stargazer(
  spei_full_m, spei_full_f,
  se = list(se_spei_full_m[, 2], se_spei_full_f[, 2]),
  p  = list(se_spei_full_m[, 4], se_spei_full_f[, 4]),
  title          = "Full SPEI and Casual Labour-Force Participation by Gender (PS, Unweighted)",
  dep.var.labels = c("Male Share", "Female Share"),
  #covariate.labels = c("SPEI-12", "SPEI-12 Lag 1", "SPEI-12 Lag 2"),
  column.labels  = c("Male", "Female"),
  add.lines = list(
    c("State FE",             "Yes", "Yes"),
    c("Year FE",              "Yes", "Yes"),
    c("Clustered SE (State)", "Yes", "Yes")
  ),
  omit.stat    = c("f", "ser"),
  notes        = "Clustered standard errors at the state level in parentheses.",
  notes.append = FALSE,
  out          = file.path(out, "table_gender_full_spei.tex"),
  type         = "latex",
  label        = "tab:gender_full_spei"
)
cat("✓ SPEI Full — gender table saved.\n")

# ── Negative SPEI ─────────────────────────────────────────────────────────────
spei_neg_m <- plm(s_casual_w_lf_PS_m_unw ~ spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2,
                  data = pdata_spei, model = "within", effect = "twoways")

spei_neg_f <- plm(s_casual_w_lf_PS_f_unw ~ spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2,
                  data = pdata_spei, model = "within", effect = "twoways")

se_spei_neg_m <- cluster_se(spei_neg_m)
se_spei_neg_f <- cluster_se(spei_neg_f)

stargazer(
  spei_neg_m, spei_neg_f,
  se = list(se_spei_neg_m[, 2], se_spei_neg_f[, 2]),
  p  = list(se_spei_neg_m[, 4], se_spei_neg_f[, 4]),
  title          = "Negative SPEI Shocks and Casual Labour-Force Participation by Gender (PS, Unweighted)",
  dep.var.labels = c("Male Share", "Female Share"),
  #covariate.labels = c("Neg. SPEI-12", "Neg. SPEI-12 Lag 1", "Neg. SPEI-12 Lag 2"),
  column.labels  = c("Male", "Female"),
  add.lines = list(
    c("State FE",             "Yes", "Yes"),
    c("Year FE",              "Yes", "Yes"),
    c("Clustered SE (State)", "Yes", "Yes")
  ),
  omit.stat    = c("f", "ser"),
  notes        = "Clustered standard errors at the state level in parentheses.",
  notes.append = FALSE,
  out          = file.path(out, "table_gender_neg_spei.tex"),
  type         = "latex",
  label        = "tab:gender_neg_spei"
)
cat("✓ SPEI Negative — gender table saved.\n")

# ══════════════════════════════════════════════════════════════════════════════
# FLOOD — Gender subgroups (optimal spec: contemp. + lag1 + lag2)
# ══════════════════════════════════════════════════════════════════════════════

# ── FI Index ───────────────────────────────────────────────────────────────────
fi_m <- plm(s_casual_w_lf_PS_m_unw ~ FI_state + FI_lag1 + FI_lag2,
            data = pdata_flood, model = "within", effect = "twoways")

fi_f <- plm(s_casual_w_lf_PS_f_unw ~ FI_state + FI_lag1 + FI_lag2,
            data = pdata_flood, model = "within", effect = "twoways")

se_fi_m <- cluster_se(fi_m)
se_fi_f <- cluster_se(fi_f)

stargazer(
  fi_m, fi_f,
  se = list(se_fi_m[, 2], se_fi_f[, 2]),
  p  = list(se_fi_m[, 4], se_fi_f[, 4]),
  title          = "Flood Index (FI) and Casual Labour-Force Participation by Gender (PS, Unweighted)",
  dep.var.labels = c("Male Share", "Female Share"),
  #covariate.labels = c("Flood Index", "Flood Index Lag 1", "Flood Index Lag 2"),
  column.labels  = c("Male", "Female"),
  add.lines = list(
    c("State FE",             "Yes", "Yes"),
    c("Year FE",              "Yes", "Yes"),
    c("Clustered SE (State)", "Yes", "Yes")
  ),
  omit.stat    = c("f", "ser"),
  notes        = "Clustered standard errors at the state level in parentheses.", # Sample excludes LAKSHADWEEP, ARUNACHAL PRADESH, and MEGHALAYA. Optimal lag specification (contemp. + 2 lags).
  notes.append = FALSE,
  out          = file.path(out, "table_gender_FI.tex"),
  type         = "latex",
  label        = "tab:gender_fi"
)
cat("✓ FI Index — gender table saved.\n")

# ── PR Index ───────────────────────────────────────────────────────────────────
pr_m <- plm(s_casual_w_lf_PS_m_unw ~ spei_positive + spei_spei_lag1 + spei_spei_lag2,
            data = pdata_spei, model = "within", effect = "twoways")

pr_f <- plm(s_casual_w_lf_PS_f_unw ~ spei_positive + spei_spei_lag1 + spei_spei_lag2,
            data = pdata_spei, model = "within", effect = "twoways")

se_pr_m <- cluster_se(pr_m)
se_pr_f <- cluster_se(pr_f)

stargazer(
  pr_m, pr_f,
  se = list(se_pr_m[, 2], se_pr_f[, 2]),
  p  = list(se_pr_m[, 4], se_pr_f[, 4]),
  title          = "PR Index and Casual Labour-Force Participation by Gender (PS, Unweighted)",
  dep.var.labels = c("Male Share", "Female Share"),
  #covariate.labels = c("PR Index", "PR Index Lag 1", "PR Index Lag 2"),
  column.labels  = c("Male", "Female"),
  add.lines = list(
    c("State FE",             "Yes", "Yes"),
    c("Year FE",              "Yes", "Yes"),
    c("Clustered SE (State)", "Yes", "Yes")
  ),
  omit.stat    = c("f", "ser"),
  notes        = "Clustered standard errors at the state level in parentheses. ", #Optimal lag specification (contemp. + 2 lags).
  notes.append = FALSE,
  out          = file.path(out, "table_gender_PR.tex"),
  type         = "latex",
  label        = "tab:gender_pr"
)
cat("✓ PR Index — gender table saved.\n")

cat("\nAll gender subgroup tables saved.\n")