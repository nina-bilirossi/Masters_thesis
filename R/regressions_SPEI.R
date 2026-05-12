library(plm)
library(lmtest)
library(vars)
library(sandwich)
library(stargazer)
library(dplyr)
library(modelsummary)


# We run a regression with the optimal lags for SPEI Index (2 lags for both) and we report the results in a stargazer table.
# Specifications:
# 2 way FE
# Clustered SE at the state level
# run only with 1 measure of informality for now
# test the following SPEI: only the negative values, and full SPEI
# both with 2 lags

out <- "/Users/ninabilirossi/Desktop/MSC THESIS/Data works/Code/Outputs/latex food/SPEI"
dir.create(out, recursive = TRUE, showWarnings = FALSE)


data <- read.csv("/Users/ninabilirossi/Desktop/MSC THESIS/Data works/Code/Outputs/final material/regression_dataframe.csv") |> 
  filter(STATE != "LAKSHADWEEP")
colnames(data)

# [1] "STATE"                    "spei_spei12"              "spei_negative"           
# [4] "spei_positive"            "spei_spei_lag1"           "spei_spei_lag2"          
# [7] "spei_spei_lag3"           "spei_neg_spei_lag1"       "spei_neg_spei_lag2"      
# [10] "spei_neg_spei_lag3"       "year"                     "FI_state"                
# [13] "FI_lag1"                  "FI_lag2"                  "FI_lag3"                 
# [16] "s_casual_w_lf_PS_unw"     "s_casual_w_lf_P_unw"      "s_casual_w_lf_PS_m_unw"  
# [19] "s_casual_w_lf_P_m_unw"    "s_casual_w_lf_PS_f_unw"   "s_casual_w_lf_P_f_unw"   
# [22] "s_casual_w_lf_PS_rur_unw" "s_casual_w_lf_P_rur_unw"  "s_casual_w_lf_PS_urb_unw"
# [25] "s_casual_w_lf_P_urb_unw"  "data"                     "time"                    
# [28] "total_exposure"           "total_state_pop"          "pr_score"                
# [31] "pr_lag1"                  "pr_lag2"                  "pr_lag3"                 
# [34] "state_pop"     

# ── Panel setup ────────────────────────────────────────────────────────────────
pdata <- pdata.frame(data, index = c("STATE", "year"))



# ── SOME SUMMARY STATISTICS (clean this up somewhere else later) ──────────────

pdata %>%
  dplyr::select(
    "SPEI 12" = spei_spei12, 
    "SPEI 12 Neg"   = spei_negative, 
    "SPEI 12 Pos"   = spei_positive,
    "Flood index" = FI_state,
    "Extreme precipitation" = pr_score, 
    "Informality share" = s_casual_w_lf_PS_unw,
    "Inf.s (f)" = s_casual_w_lf_PS_f_unw,
    "Inf.s (m)" = s_casual_w_lf_PS_m_unw,
    "Inf.s (rural)" = s_casual_w_lf_PS_rur_unw,
    "Inf.s (urban)" = s_casual_w_lf_PS_urb_unw,
    "State population" = state_pop
  ) %>%
  datasummary_skim(
    fun_numeric = list(
      Count = N,
      Mean   = Mean,
      SD     = SD,
      Min    = Min,
      Max    = Max,
    ),
    fmt = 2,
    output = "/Users/ninabilirossi/Desktop/MSC THESIS/Data works/Code/Outputs/latex food/petit_summary.tex"
  )



# ── Helper: clustered SE at state level ───────────────────────────────────────
cluster_se <- function(model) {
  coeftest(model, vcov = vcovHC(model, type = "HC1", cluster = "group"))
}

# ── Informality measure ───────────────────────────────────────────────────────
# s_casual_w_lf_PS_unw  (PS = principal + subsidiary status, unweighted)

# ══════════════════════════════════════════════════════════════════════════════
# TABLE 1 – Full SPEI (spei_spei12) with 2 lags
# ══════════════════════════════════════════════════════════════════════════════


# Spec 1a: contemporaneous only
m1a <- plm(s_casual_w_lf_PS_unw ~ spei_spei12,
           data   = pdata,
           model  = "within",
           effect = "twoways")

# Spec 1b: + lag 1
m1b <- plm(s_casual_w_lf_PS_unw ~ spei_spei12 + spei_spei_lag1,
           data   = pdata,
           model  = "within",
           effect = "twoways")

# Spec 1c: + lag 1 & lag 2  (optimal)
m1c <- plm(s_casual_w_lf_PS_unw ~ spei_spei12 + spei_spei_lag1 + spei_spei_lag2,
           data   = pdata,
           model  = "within",
           effect = "twoways")

se1a <- cluster_se(m1a)
se1b <- cluster_se(m1b)
se1c <- cluster_se(m1c)

stargazer(
  m1a, m1b, m1c,
  se        = list(se1a[, 2], se1b[, 2], se1c[, 2]),
  p         = list(se1a[, 4], se1b[, 4], se1c[, 4]),
  title     = "Effect of Full SPEI on Casual Labour-Force Participation (PS, Unweighted)",
  dep.var.labels = "Share Casual Workers (PS, Unw.)",
  covariate.labels = c("SPEI-12", "SPEI-12 Lag 1", "SPEI-12 Lag 2"),
  # column.labels    = c("Contemp.", "+ Lag 1", "+ Lag 1\\&2"),
  add.lines = list(
    c("State FE",   "Yes", "Yes", "Yes"),
    c("Year FE",    "Yes", "Yes", "Yes"),
    c("Clustered SE (State)", "Yes", "Yes", "Yes")
  ),
  omit.stat  = c("f", "ser"),
  notes      = "Clustered standard errors at the state level in parentheses.",
  notes.append = FALSE,
  out        = file.path(out, "table1_full_spei.tex"),
  type       = "latex",
  label      = "tab:full_spei"
)

cat("✓ Table 1 (Full SPEI) saved.\n")

# ══════════════════════════════════════════════════════════════════════════════
# TABLE 2 – Negative SPEI (spei_negative) with 2 lags
# ══════════════════════════════════════════════════════════════════════════════

# Spec 2a: contemporaneous only
m2a <- plm(s_casual_w_lf_PS_unw ~ spei_negative,
           data   = pdata,
           model  = "within",
           effect = "twoways")

# Spec 2b: + lag 1
m2b <- plm(s_casual_w_lf_PS_unw ~ spei_negative + spei_neg_spei_lag1,
           data   = pdata,
           model  = "within",
           effect = "twoways")

# Spec 2c: + lag 1 & lag 2  (optimal)
m2c <- plm(s_casual_w_lf_PS_unw ~ spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2,
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
  title     = "Effect of Negative SPEI Shocks on Casual Labour-Force Participation (PS, Unweighted)",
  dep.var.labels = "Share Casual Workers (PS, Unw.)",
  covariate.labels = c("Negative SPEI-12", "Neg. SPEI-12 Lag 1", "Neg. SPEI-12 Lag 2"),
  # column.labels    = c("Contemp.", "+ Lag 1", "+ Lag 1\\&2"),
  add.lines = list(
    c("State FE",   "Yes", "Yes", "Yes"),
    c("Year FE",    "Yes", "Yes", "Yes"),
    c("Clustered SE (State)", "Yes", "Yes", "Yes")
  ),
  omit.stat  = c("f", "ser"),
  notes      = "Clustered standard errors at the state level in parentheses.", #Negative SPEI captures drought/dry shock episodes only.
  notes.append = FALSE,
  out        = file.path(out, "table2_neg_spei.tex"),
  type       = "latex",
  label      = "tab:neg_spei"
)

cat("✓ Table 2 (Negative SPEI) saved.\n")

# ══════════════════════════════════════════════════════════════════════════════
# TABLE 3 – Combined: Full vs Negative SPEI (optimal lag spec only)
# ══════════════════════════════════════════════════════════════════════════════

stargazer(
  m1c, m2c,
  se        = list(se1c[, 2], se2c[, 2]),
  p         = list(se1c[, 4], se2c[, 4]),
  title     = "Full vs. Negative SPEI: Optimal Lag Specification",
  dep.var.labels = "Share Casual Workers (PS, Unw.)",
  covariate.labels = c(
    "SPEI-12",         "SPEI-12 Lag 1",     "SPEI-12 Lag 2",
    "Neg. SPEI-12",    "Neg. SPEI-12 Lag 1","Neg. SPEI-12 Lag 2"
  ),
  column.labels = c("Full SPEI", "Negative SPEI"),
  add.lines = list(
    c("State FE",   "Yes", "Yes"),
    c("Year FE",    "Yes", "Yes"),
    c("Clustered SE (State)", "Yes", "Yes")
  ),
  omit.stat  = c("f", "ser"),
  notes      = "Clustered standard errors at the state level in parentheses.",
  notes.append = FALSE,
  out        = file.path(out, "table3_full_vs_neg_spei.tex"),
  type       = "latex",
  label      = "tab:full_vs_neg"
)

cat("✓ Table 3 (Full vs Negative SPEI) saved.\n")
cat("\nAll tables written to:", out, "\n")