library(plm)
library(lmtest)
library(vars)
library(sandwich)
library(stargazer)
library(dplyr)
library(modelsummary)


# REGRESSION WITH BOTH CLIMATE SHOCK AND FLOOD INDEX, WITH THE OPTIMAL LAGS (2 lags for both)
# REGRESSION WITH ONE CLIMATE SHOCK AT THE TIME, BUT WITH LABOR FORCE AS AN EXPLANATORY VARIABLE

out <- "/Users/ninabilirossi/Desktop/MSC THESIS/Data works/Code/Outputs/latex food/robustness"
dir.create(out, recursive = TRUE, showWarnings = FALSE)


data <- read.csv("/Users/ninabilirossi/Desktop/MSC THESIS/Data works/Code/Outputs/final material/regression_dataframe_deep.csv") |> 
  filter(STATE != "LAKSHADWEEP")

# > colnames(data)
# [1] "STATE"                    "spei_spei12"              "spei_negative"            "spei_positive"           
# [5] "spei_spei_lag1"           "spei_spei_lag2"           "spei_spei_lag3"           "spei_neg_spei_lag1"      
# [9] "spei_neg_spei_lag2"       "spei_neg_spei_lag3"       "year"                     "FI_state"                
# [13] "FI_lag1"                  "FI_lag2"                  "FI_lag3"                  "pop_tot_unw"             
# [17] "pop_workingage_unw"       "worker_PS_unw"            "worker_PS_m_unw"          "worker_PS_f_unw"         
# [21] "worker_PS_rur_unw"        "worker_PS_urb_unw"        "labor_force_PS_unw"       "labor_force_PS_m_unw"    
# [25] "labor_force_PS_f_unw"     "labor_force_PS_rur_unw"   "labor_force_PS_urb_unw"   "pop_m_unw"               
# [29] "pop_f_unw"                "pop_rur_unw"              "pop_urb_unw"              "s_casual_w_lf_PS_unw"    
# [33] "s_casual_w_lf_P_unw"      "s_casual_w_lf_PS_m_unw"   "s_casual_w_lf_P_m_unw"    "s_casual_w_lf_PS_f_unw"  
# [37] "s_casual_w_lf_P_f_unw"    "s_casual_w_lf_PS_rur_unw" "s_casual_w_lf_P_rur_unw"  "s_casual_w_lf_PS_urb_unw"
# [41] "s_casual_w_lf_P_urb_unw"  "data"                     "time"                     "lf_share_unw"            
# [45] "worker_share_unw"         "lf_share_m_unw"           "worker_share_m_unw"       "lf_share_f_unw"          
# [49] "worker_share_f_unw"       "lf_share_urb_unw"         "worker_share_urb_unw"     "lf_share_rur_unw"        
# [53] "worker_share_rur_unw"     "total_exposure"           "total_state_pop"          "pr_score"                
# [57] "pr_lag1"                  "pr_lag2"                  "pr_lag3"                  "state_pop"               

# ── Panel setup ────────────────────────────────────────────────────────────────
pdata <- pdata.frame(data, index = c("STATE", "year"))

data_flood <- data |> # drop those states since they have missing data for the flood variable
  filter(STATE != "ARUNACHAL PRADESH") |>   # missing FI data
  filter(STATE != "MEGHALAYA")  
pdata_fi <- pdata.frame(data_flood, index = c("STATE", "year"))

# ── Helper: clustered SE at state level ───────────────────────────────────────
cluster_se <- function(model) {
  coeftest(model, vcov = vcovHC(model, type = "HC1", cluster = "group"))
}

# ── Informality measure ───────────────────────────────────────────────────────
# s_casual_w_lf_PS_unw  (PS = principal + subsidiary status, unweighted)

# ══════════════════════════════════════════════════════════════════════════════
# FULL DATASET
# ══════════════════════════════════════════════════════════════════════════════

m1a <- plm(s_casual_w_lf_PS_unw ~ spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2,
           data   = pdata,
           model  = "within",
           effect = "twoways")

m1b <- plm(s_casual_w_lf_PS_unw ~ spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2
           + lf_share_unw,
           data   = pdata,
           model  = "within",
           effect = "twoways")

m1c <- plm(s_casual_w_lf_PS_unw ~ FI_state + FI_lag1 + FI_lag2,
           data   = pdata_fi,
           model  = "within",
           effect = "twoways")

m1d <- plm(s_casual_w_lf_PS_unw ~ FI_state + FI_lag1 + FI_lag2 + 
             lf_share_unw,
           data   = pdata_fi,
           model  = "within",
           effect = "twoways")

m1e <- plm(s_casual_w_lf_PS_unw ~  spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2 + FI_state + FI_lag1 + FI_lag2 + 
             lf_share_unw,
           data   = pdata_fi,
           model  = "within",
           effect = "twoways")

se1a <- cluster_se(m1a)
se1b <- cluster_se(m1b)
se1c <- cluster_se(m1c)
se1d <- cluster_se(m1d)
se1e <- cluster_se(m1e)

stargazer(
  m1a, m1b, m1c, m1d, m1e,
  se = list(se1a[, 2], se1b[, 2], se1c[, 2], se1d[, 2], se1e[, 2]),
  p  = list(se1a[, 4], se1b[, 4], se1c[, 4], se1d[, 4], se1e[, 4]),
  title          = "Effect of Climate Shocks on Informality (PS, Unweighted)",
  dep.var.labels = "Informality Share (Casual/LF)",
  covariate.labels = c(
    "Negative SPEI-12",
    "Neg. SPEI-12 Lag 1",
    "Neg. SPEI-12 Lag 2",
    "LF Share",
    "Flood Index",
    "Flood Index Lag 1",
    "Flood Index Lag 2"
  ),
  column.labels   = c("SPEI only", "SPEI + LF", "FI only", "FI + LF", "SPEI + FI + LF"),
  column.separate = c(1, 1, 1, 1, 1),
  # add.lines = list(
  #   c("State FE",             "Yes", "Yes", "Yes", "Yes", "Yes"),
  #   c("Year FE",              "Yes", "Yes", "Yes", "Yes", "Yes"),
  #   c("Clustered SE (State)", "Yes", "Yes", "Yes", "Yes", "Yes")
  # ),
  omit.stat    = c("f", "ser"),
  notes        = "Working age: 15–64.", # Columns (3)–(5) exclude Arunachal Pradesh and Meghalaya (missing FI data).
  notes.append = FALSE,
  out          = file.path(out, "expanded_regression.tex"),
  type         = "latex",
  label        = "tab:full_spei"
)

cat("✓ Table (Expanded regression) saved.\n")


# ══════════════════════════════════════════════════════════════════════════════
# BY GENDER
# ══════════════════════════════════════════════════════════════════════════════
# 1. FEMALE

m1a <- plm(s_casual_w_lf_PS_f_unw ~ spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2,
           data   = pdata,
           model  = "within",
           effect = "twoways")

m1b <- plm(s_casual_w_lf_PS_f_unw ~ spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2
           + lf_share_f_unw,
           data   = pdata,
           model  = "within",
           effect = "twoways")

m1c <- plm(s_casual_w_lf_PS_f_unw ~ FI_state + FI_lag1 + FI_lag2,
           data   = pdata_fi,
           model  = "within",
           effect = "twoways")

m1d <- plm(s_casual_w_lf_PS_f_unw ~ FI_state + FI_lag1 + FI_lag2 + 
             lf_share_f_unw,
           data   = pdata_fi,
           model  = "within",
           effect = "twoways")

m1e <- plm(s_casual_w_lf_PS_f_unw ~  spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2 + FI_state + FI_lag1 + FI_lag2 + 
             lf_share_f_unw,
           data   = pdata_fi,
           model  = "within",
           effect = "twoways")

se1a <- cluster_se(m1a)
se1b <- cluster_se(m1b)
se1c <- cluster_se(m1c)
se1d <- cluster_se(m1d)
se1e <- cluster_se(m1e)

stargazer(
  m1a, m1b, m1c, m1d, m1e,
  se = list(se1a[, 2], se1b[, 2], se1c[, 2], se1d[, 2], se1e[, 2]),
  p  = list(se1a[, 4], se1b[, 4], se1c[, 4], se1d[, 4], se1e[, 4]),
  title          = "Effect of Climate Shocks on Informality Among Women (PS, Unweighted)",
  dep.var.labels = "Informality Share (Casual/LF)",
  covariate.labels = c(
    "Negative SPEI-12",
    "Neg. SPEI-12 Lag 1",
    "Neg. SPEI-12 Lag 2",
    "LF Share",
    "Flood Index",
    "Flood Index Lag 1",
    "Flood Index Lag 2"
  ),
  column.labels   = c("SPEI only", "SPEI + LF", "FI only", "FI + LF", "SPEI + FI + LF"),
  column.separate = c(1, 1, 1, 1, 1),
  # add.lines = list(
  #   c("State FE",             "Yes", "Yes", "Yes", "Yes", "Yes"),
  #   c("Year FE",              "Yes", "Yes", "Yes", "Yes", "Yes"),
  #   c("Clustered SE (State)", "Yes", "Yes", "Yes", "Yes", "Yes")
  # ),
  omit.stat    = c("f", "ser"),
  notes        = "Working age: 15–64.",
  notes.append = FALSE,
  out          = file.path(out, "expanded_regression_female.tex"),
  type         = "latex",
  label        = "tab:full_spei"
)

cat("✓ Table (Expanded female regression) saved.\n")

# 2. MEN

m1a <- plm(s_casual_w_lf_PS_m_unw ~ spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2,
           data   = pdata,
           model  = "within",
           effect = "twoways")

m1b <- plm(s_casual_w_lf_PS_m_unw ~ spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2
           + lf_share_m_unw,
           data   = pdata,
           model  = "within",
           effect = "twoways")

m1c <- plm(s_casual_w_lf_PS_m_unw ~ FI_state + FI_lag1 + FI_lag2,
           data   = pdata_fi,
           model  = "within",
           effect = "twoways")

m1d <- plm(s_casual_w_lf_PS_m_unw ~ FI_state + FI_lag1 + FI_lag2 + 
             lf_share_m_unw,
           data   = pdata_fi,
           model  = "within",
           effect = "twoways")

m1e <- plm(s_casual_w_lf_PS_m_unw ~  spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2 + FI_state + FI_lag1 + FI_lag2 + 
             lf_share_m_unw,
           data   = pdata_fi,
           model  = "within",
           effect = "twoways")

se1a <- cluster_se(m1a)
se1b <- cluster_se(m1b)
se1c <- cluster_se(m1c)
se1d <- cluster_se(m1d)
se1e <- cluster_se(m1e)

stargazer(
  m1a, m1b, m1c, m1d, m1e,
  se = list(se1a[, 2], se1b[, 2], se1c[, 2], se1d[, 2], se1e[, 2]),
  p  = list(se1a[, 4], se1b[, 4], se1c[, 4], se1d[, 4], se1e[, 4]),
  title          = "Effect of Climate Shocks on Informality Among Men (PS, Unweighted)",
  dep.var.labels = "Informality Share (Casual/LF)",
  covariate.labels = c(
    "Negative SPEI-12",
    "Neg. SPEI-12 Lag 1",
    "Neg. SPEI-12 Lag 2",
    "LF Share",
    "Flood Index",
    "Flood Index Lag 1",
    "Flood Index Lag 2"
  ),
  column.labels   = c("SPEI only", "SPEI + LF", "FI only", "FI + LF", "SPEI + FI + LF"),
  column.separate = c(1, 1, 1, 1, 1),
  # add.lines = list(
  #   c("State FE",             "Yes", "Yes", "Yes", "Yes", "Yes"),
  #   c("Year FE",              "Yes", "Yes", "Yes", "Yes", "Yes"),
  #   c("Clustered SE (State)", "Yes", "Yes", "Yes", "Yes", "Yes")
  # ),
  omit.stat    = c("f", "ser"),
  notes        = "Working age: 15–64.",
  notes.append = FALSE,
  out          = file.path(out, "expanded_regression_male.tex"),
  type         = "latex",
  label        = "tab:full_spei"
)

cat("✓ Table (Expanded male regression) saved.\n")



# ══════════════════════════════════════════════════════════════════════════════
# BY SETTELMENT TYPE
# ══════════════════════════════════════════════════════════════════════════════
# 1. RURAL

m1a <- plm(s_casual_w_lf_PS_rur_unw ~ spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2,
           data   = pdata,
           model  = "within",
           effect = "twoways")

m1b <- plm(s_casual_w_lf_PS_rur_unw ~ spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2
           + lf_share_rur_unw,
           data   = pdata,
           model  = "within",
           effect = "twoways")

m1c <- plm(s_casual_w_lf_PS_rur_unw ~ FI_state + FI_lag1 + FI_lag2,
           data   = pdata_fi,
           model  = "within",
           effect = "twoways")

m1d <- plm(s_casual_w_lf_PS_rur_unw ~ FI_state + FI_lag1 + FI_lag2 + 
             lf_share_rur_unw,
           data   = pdata_fi,
           model  = "within",
           effect = "twoways")

m1e <- plm(s_casual_w_lf_PS_rur_unw ~  spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2 + FI_state + FI_lag1 + FI_lag2 + 
             lf_share_rur_unw,
           data   = pdata_fi,
           model  = "within",
           effect = "twoways")

se1a <- cluster_se(m1a)
se1b <- cluster_se(m1b)
se1c <- cluster_se(m1c)
se1d <- cluster_se(m1d)
se1e <- cluster_se(m1e)

stargazer(
  m1a, m1b, m1c, m1d, m1e,
  se = list(se1a[, 2], se1b[, 2], se1c[, 2], se1d[, 2], se1e[, 2]),
  p  = list(se1a[, 4], se1b[, 4], se1c[, 4], se1d[, 4], se1e[, 4]),
  title          = "Effect of Climate Shocks on Informality in Rural Areas (PS, Unweighted)",
  dep.var.labels = "Informality Share (Casual/LF)",
  covariate.labels = c(
    "Negative SPEI-12",
    "Neg. SPEI-12 Lag 1",
    "Neg. SPEI-12 Lag 2",
    "LF Share",
    "Flood Index",
    "Flood Index Lag 1",
    "Flood Index Lag 2"  ),
  column.labels   = c("SPEI only", "SPEI + LF", "FI only", "FI + LF", "SPEI + FI + LF"),
  column.separate = c(1, 1, 1, 1, 1),
  # add.lines = list(
  #   c("State FE",             "Yes", "Yes", "Yes", "Yes", "Yes"),
  #   c("Year FE",              "Yes", "Yes", "Yes", "Yes", "Yes"),
  #   c("Clustered SE (State)", "Yes", "Yes", "Yes", "Yes", "Yes")
  # ),
  omit.stat    = c("f", "ser"),
  notes        = "Working age: 15–64.",
  notes.append = FALSE,
  out          = file.path(out, "expanded_regression_rural.tex"),
  type         = "latex",
  label        = "tab:full_spei"
)

cat("✓ Table (Expanded rural regression) saved.\n")

# 2. URBAN

m1a <- plm(s_casual_w_lf_PS_urb_unw ~ spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2,
           data   = pdata,
           model  = "within",
           effect = "twoways")

m1b <- plm(s_casual_w_lf_PS_urb_unw ~ spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2
           + lf_share_urb_unw,
           data   = pdata,
           model  = "within",
           effect = "twoways")

m1c <- plm(s_casual_w_lf_PS_urb_unw ~ FI_state + FI_lag1 + FI_lag2,
           data   = pdata_fi,
           model  = "within",
           effect = "twoways")

m1d <- plm(s_casual_w_lf_PS_urb_unw ~ FI_state + FI_lag1 + FI_lag2 + 
             lf_share_urb_unw,
           data   = pdata_fi,
           model  = "within",
           effect = "twoways")

m1e <- plm(s_casual_w_lf_PS_urb_unw ~  spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2 + FI_state + FI_lag1 + FI_lag2 + 
             lf_share_urb_unw,
           data   = pdata_fi,
           model  = "within",
           effect = "twoways")

se1a <- cluster_se(m1a)
se1b <- cluster_se(m1b)
se1c <- cluster_se(m1c)
se1d <- cluster_se(m1d)
se1e <- cluster_se(m1e)

stargazer(
  m1a, m1b, m1c, m1d, m1e,
  se = list(se1a[, 2], se1b[, 2], se1c[, 2], se1d[, 2], se1e[, 2]),
  p  = list(se1a[, 4], se1b[, 4], se1c[, 4], se1d[, 4], se1e[, 4]),
  title          = "Effect of Climate Shocks on Informality in Urban Areas (PS, Unweighted)",
  dep.var.labels = "Informality Share (Casual/LF)",
  covariate.labels = c(
    "Negative SPEI-12",
    "Neg. SPEI-12 Lag 1",
    "Neg. SPEI-12 Lag 2",
    "LF Share",
    "Flood Index",
    "Flood Index Lag 1",
    "Flood Index Lag 2" ),
  column.labels   = c("SPEI only", "SPEI + LF", "FI only", "FI + LF", "SPEI + FI + LF"),
  column.separate = c(1, 1, 1, 1, 1),
  # add.lines = list(
  #   c("State FE",             "Yes", "Yes", "Yes", "Yes", "Yes"),
  #   c("Year FE",              "Yes", "Yes", "Yes", "Yes", "Yes"),
  #   c("Clustered SE (State)", "Yes", "Yes", "Yes", "Yes", "Yes")
  # ),
  omit.stat    = c("f", "ser"),
  notes        = "Working age: 15–64.",
  notes.append = FALSE,
  out          = file.path(out, "expanded_regression_urban.tex"),
  type         = "latex",
  label        = "tab:full_spei"
)

cat("✓ Table (Expanded urban regression) saved.\n")