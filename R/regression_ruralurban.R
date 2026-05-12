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
# SPEI — Sector subgroups (optimal spec: contemp. + lag1 + lag2)
# Rural outcome:  s_casual_w_lf_PS_rur_unw
# Urban outcome: s_casual_w_lf_PS_urb_unw
# ══════════════════════════════════════════════════════════════════════════════

# ── Full SPEI ──────────────────────────────────────────────────────────────────
spei_full_r <- plm(s_casual_w_lf_PS_rur_unw ~ spei_spei12 + spei_spei_lag1 + spei_spei_lag2,
                   data = pdata_spei, model = "within", effect = "twoways")

spei_full_u <- plm(s_casual_w_lf_PS_urb_unw ~ spei_spei12 + spei_spei_lag1 + spei_spei_lag2,
                   data = pdata_spei, model = "within", effect = "twoways")

se_spei_full_r <- cluster_se(spei_full_r)
se_spei_full_u <- cluster_se(spei_full_u)

stargazer(
  spei_full_r, spei_full_u,
  se = list(se_spei_full_r[, 2], se_spei_full_u[, 2]),
  p  = list(se_spei_full_r[, 4], se_spei_full_u[, 4]),
  title          = "Full SPEI and Casual Labour-Force Participation by Sector (PS, Unweighted)",
  dep.var.labels = c("Rural Share", "Urban Share"),
  #covariate.labels = c("SPEI-12", "SPEI-12 Lag 1", "SPEI-12 Lag 2"),
  column.labels  = c("Rural", "Urban"),
  add.lines = list(
    c("State FE",             "Yes", "Yes"),
    c("Year FE",              "Yes", "Yes"),
    c("Clustered SE (State)", "Yes", "Yes")
  ),
  omit.stat    = c("f", "ser"),
  notes        = "Clustered standard errors at the state level in parentheses.",
  notes.append = FALSE,
  out          = file.path(out, "table_sector_full_spei.tex"),
  type         = "latex",
  label        = "tab:sector_full_spei"
)
cat("✓ SPEI Full — sector table saved.\n")

# ── Negative SPEI ─────────────────────────────────────────────────────────────
spei_neg_r <- plm(s_casual_w_lf_PS_rur_unw ~ spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2,
                  data = pdata_spei, model = "within", effect = "twoways")

spei_neg_u <- plm(s_casual_w_lf_PS_urb_unw ~ spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2,
                  data = pdata_spei, model = "within", effect = "twoways")

se_spei_neg_r <- cluster_se(spei_neg_r)
se_spei_neg_u <- cluster_se(spei_neg_u)

stargazer(
  spei_neg_r, spei_neg_u,
  se = list(se_spei_neg_r[, 2], se_spei_neg_u[, 2]),
  p  = list(se_spei_neg_r[, 4], se_spei_neg_u[, 4]),
  title          = "Negative SPEI Shocks and Casual Labour-Force Participation by Sector (PS, Unweighted)",
  dep.var.labels = c("Rural Share", "Urban Share"),
  #covariate.labels = c("Neg. SPEI-12", "Neg. SPEI-12 Lag 1", "Neg. SPEI-12 Lag 2"),
  column.labels  = c("Rural", "Urban"),
  add.lines = list(
    c("State FE",             "Yes", "Yes"),
    c("Year FE",              "Yes", "Yes"),
    c("Clustered SE (State)", "Yes", "Yes")
  ),
  omit.stat    = c("f", "ser"),
  notes        = "Clustered standard errors at the state level in parentheses.",
  notes.append = FALSE,
  out          = file.path(out, "table_sector_neg_spei.tex"),
  type         = "latex",
  label        = "tab:sector_neg_spei"
)
cat("✓ SPEI Negative — sector table saved.\n")

# ══════════════════════════════════════════════════════════════════════════════
# FLOOD — Sector subgroups (optimal spec: contemp. + lag1 + lag2)
# ══════════════════════════════════════════════════════════════════════════════

# ── FI Index ───────────────────────────────────────────────────────────────────
fi_r <- plm(s_casual_w_lf_PS_rur_unw ~ FI_state + FI_lag1 + FI_lag2,
            data = pdata_flood, model = "within", effect = "twoways")

fi_u <- plm(s_casual_w_lf_PS_urb_unw ~ FI_state + FI_lag1 + FI_lag2,
            data = pdata_flood, model = "within", effect = "twoways")

se_fi_r <- cluster_se(fi_r)
se_fi_u <- cluster_se(fi_u)

stargazer(
  fi_r, fi_u,
  se = list(se_fi_r[, 2], se_fi_u[, 2]),
  p  = list(se_fi_r[, 4], se_fi_u[, 4]),
  title          = "Flood Index (FI) and Casual Labour-Force Participation by Sector (PS, Unweighted)",
  dep.var.labels = c("Rural Share", "Urban Share"),
  #covariate.labels = c("Flood Index", "Flood Index Lag 1", "Flood Index Lag 2"),
  column.labels  = c("Rural", "Urban"),
  add.lines = list(
    c("State FE",             "Yes", "Yes"),
    c("Year FE",              "Yes", "Yes"),
    c("Clustered SE (State)", "Yes", "Yes")
  ),
  omit.stat    = c("f", "ser"),
  notes        = "Clustered standard errors at the state level in parentheses.", 
  notes.append = FALSE,
  out          = file.path(out, "table_sector_FI.tex"),
  type         = "latex",
  label        = "tab:sector_fi"
)
cat("✓ FI Index — sector table saved.\n")

# ── PR Index ───────────────────────────────────────────────────────────────────
pr_r <- plm(s_casual_w_lf_PS_rur_unw ~ spei_positive + spei_spei_lag1 + spei_spei_lag2,
            data = pdata_spei, model = "within", effect = "twoways")

pr_u <- plm(s_casual_w_lf_PS_urb_unw ~ spei_positive + spei_spei_lag1 + spei_spei_lag2,
            data = pdata_spei, model = "within", effect = "twoways")

se_pr_r <- cluster_se(pr_r)
se_pr_u <- cluster_se(pr_u)

stargazer(
  pr_r, pr_u,
  se = list(se_pr_r[, 2], se_pr_u[, 2]),
  p  = list(se_pr_r[, 4], se_pr_u[, 4]),
  title          = "PR Index and Casual Labour-Force Participation by Sector (PS, Unweighted)",
  dep.var.labels = c("Rural Share", "Urban Share"),
  #covariate.labels = c("PR Index", "PR Index Lag 1", "PR Index Lag 2"),
  column.labels  = c("Rural", "Urban"),
  add.lines = list(
    c("State FE",             "Yes", "Yes"),
    c("Year FE",              "Yes", "Yes"),
    c("Clustered SE (State)", "Yes", "Yes")
  ),
  omit.stat    = c("f", "ser"),
  notes        = "Clustered standard errors at the state level in parentheses.", 
  notes.append = FALSE,
  out          = file.path(out, "table_sector_PR.tex"),
  type         = "latex",
  label        = "tab:sector_pr"
)
cat("✓ PR Index — sector table saved.\n")

cat("\nAll sector subgroup tables saved.\n")