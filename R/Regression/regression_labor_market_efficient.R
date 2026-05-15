# ── SOME SUMMARY STATISTICS (clean this up somewhere else later) ──────────────

plfs_deep <- read.csv("/Users/ninabilirossi/Desktop/MSC THESIS/Data works/Code/Outputs/final material/regression_dataframe_deeper.csv") |> 
  filter(STATE != "LAKSHADWEEP")

labor_data <- plfs_deep

# CREATE UNEMPLOYMENT VARIABLES

labor_data$unemp <- (labor_data$labor_force_PS_unw - labor_data$worker_PS_unw) / labor_data$labor_force_PS_unw

# Group-specific unemployment rates
groups <- c("f", "m", "rur", "urb")

for (g in groups) {
  lf_col     <- paste0("labor_force_PS_", g, "_unw")
  worker_col <- paste0("worker_PS_", g, "_unw")
  new_col    <- paste0("unemp_", g)
  labor_data[[new_col]] <- (labor_data[[lf_col]] - labor_data[[worker_col]]) / labor_data[[lf_col]]
}

share_vars <- names(plfs_deep)[grepl("^(lf_share|worker_share|unemp)", names(plfs_deep))]
my_vars    <- c(share_vars, "pop_tot_unw", "pop_workingage_unw")

plabordata <- pdata.frame(labor_data, index = c("STATE", "year"))

plabordata |> select(all_of(my_vars)) |> 
  datasummary_skim(
    fun_numeric = list(Count = N, Mean = Mean, SD = SD, Min = Min, Max = Max),
    fmt    = 2,
    output = "/Users/ninabilirossi/Desktop/MSC THESIS/Data works/Code/Outputs/latex food/labormarket_summary.tex"
  )

# ══════════════════════════════════════════════════════════════════════════════
# HELPER: run a 6-model block (unemp + worker_share, 3 lag specs) and save
# ══════════════════════════════════════════════════════════════════════════════

run_spei_neg_block <- function(pdata, unemp_var, worker_var,
                               title_suffix, file_suffix,
                               dep_labels = c("Unemployment", "Employment")) {
  
  f_unemp  <- as.formula(paste0(unemp_var,  " ~ spei_negative"))
  f_unemp1 <- as.formula(paste0(unemp_var,  " ~ spei_negative + spei_neg_spei_lag1"))
  f_unemp2 <- as.formula(paste0(unemp_var,  " ~ spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2"))
  
  f_work   <- as.formula(paste0(worker_var, " ~ spei_negative"))
  f_work1  <- as.formula(paste0(worker_var, " ~ spei_negative + spei_neg_spei_lag1"))
  f_work2  <- as.formula(paste0(worker_var, " ~ spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2"))
  
  ma <- plm(f_unemp,  data = pdata, model = "within", effect = "twoways")
  mb <- plm(f_unemp1, data = pdata, model = "within", effect = "twoways")
  mc <- plm(f_unemp2, data = pdata, model = "within", effect = "twoways")
  md <- plm(f_work,   data = pdata, model = "within", effect = "twoways")
  me <- plm(f_work1,  data = pdata, model = "within", effect = "twoways")
  mf <- plm(f_work2,  data = pdata, model = "within", effect = "twoways")
  
  sea <- cluster_se(ma); seb <- cluster_se(mb); sec <- cluster_se(mc)
  sed <- cluster_se(md); see <- cluster_se(me); sef <- cluster_se(mf)
  
  stargazer(
    ma, mb, mc, md, me, mf,
    se = list(sea[,2], seb[,2], sec[,2], sed[,2], see[,2], sef[,2]),
    p  = list(sea[,4], seb[,4], sec[,4], sed[,4], see[,4], sef[,4]),
    title            = paste("Effect of Negative SPEI on Labor Market Outcomes –", title_suffix),
    dep.var.labels   = dep_labels,
    covariate.labels = c("Negative SPEI-12", "Neg. SPEI-12 Lag 1", "Neg. SPEI-12 Lag 2"),
    add.lines = list(
      c("State FE",             rep("Yes", 6)),
      c("Year FE",              rep("Yes", 6)),
      c("Clustered SE (State)", rep("Yes", 6))
    ),
    omit.stat    = c("f", "ser"),
    notes        = "Working age: 15-64.",
    notes.append = FALSE,
    out          = file.path(out, paste0("emp_unemp_neg_spei_", file_suffix, ".tex")),
    type         = "latex",
    label        = paste0("tab:neg_spei_", file_suffix)
  )
  cat("✓ Neg SPEI table saved:", file_suffix, "\n")
}

run_fi_block <- function(pdata, unemp_var, worker_var,
                         title_suffix, file_suffix,
                         dep_labels = c("Unemployment", "Employment")) {
  
  f_unemp  <- as.formula(paste0(unemp_var,  " ~ FI_state"))
  f_unemp1 <- as.formula(paste0(unemp_var,  " ~ FI_state + FI_lag1"))
  f_unemp2 <- as.formula(paste0(unemp_var,  " ~ FI_state + FI_lag1 + FI_lag2"))
  
  f_work   <- as.formula(paste0(worker_var, " ~ FI_state"))
  f_work1  <- as.formula(paste0(worker_var, " ~ FI_state + FI_lag1"))
  f_work2  <- as.formula(paste0(worker_var, " ~ FI_state + FI_lag1 + FI_lag2"))
  
  ma <- plm(f_unemp,  data = pdata, model = "within", effect = "twoways")
  mb <- plm(f_unemp1, data = pdata, model = "within", effect = "twoways")
  mc <- plm(f_unemp2, data = pdata, model = "within", effect = "twoways")
  md <- plm(f_work,   data = pdata, model = "within", effect = "twoways")
  me <- plm(f_work1,  data = pdata, model = "within", effect = "twoways")
  mf <- plm(f_work2,  data = pdata, model = "within", effect = "twoways")
  
  sea <- cluster_se(ma); seb <- cluster_se(mb); sec <- cluster_se(mc)
  sed <- cluster_se(md); see <- cluster_se(me); sef <- cluster_se(mf)
  
  stargazer(
    ma, mb, mc, md, me, mf,
    se = list(sea[,2], seb[,2], sec[,2], sed[,2], see[,2], sef[,2]),
    p  = list(sea[,4], seb[,4], sec[,4], sed[,4], see[,4], sef[,4]),
    title            = paste("Effect of Flood Index on Labor Market Outcomes –", title_suffix),
    dep.var.labels   = dep_labels,
    covariate.labels = c("Flood Index", "Flood Index Lag 1", "Flood Index Lag 2"),
    add.lines = list(
      c("State FE",             rep("Yes", 6)),
      c("Year FE",              rep("Yes", 6)),
      c("Clustered SE (State)", rep("Yes", 6))
    ),
    omit.stat    = c("f", "ser"),
    notes        = "Working age: 15-64.",
    notes.append = FALSE,
    out          = file.path(out, paste0("emp_unemp_FI_", file_suffix, ".tex")),
    type         = "latex",
    label        = paste0("tab:fi_", file_suffix)
  )
  cat("✓ FI table saved:", file_suffix, "\n")
}

# ══════════════════════════════════════════════════════════════════════════════
# TABLE BLOCK 1 – Negative SPEI | All subgroups
# ══════════════════════════════════════════════════════════════════════════════

# -- 1.0  Aggregate (original) ------------------------------------------------
run_spei_neg_block(plabordata,
                   unemp_var    = "unemp",
                   worker_var   = "worker_share_unw",
                   title_suffix = "Aggregate (PS, Unweighted)",
                   file_suffix  = "aggregate")

# -- 1.1  Male ----------------------------------------------------------------
run_spei_neg_block(plabordata,
                   unemp_var    = "unemp_m",
                   worker_var   = "worker_share_m_unw",
                   title_suffix = "Male (PS, Unweighted)",
                   file_suffix  = "male")

# -- 1.2  Female --------------------------------------------------------------
run_spei_neg_block(plabordata,
                   unemp_var    = "unemp_f",
                   worker_var   = "worker_share_f_unw",
                   title_suffix = "Female (PS, Unweighted)",
                   file_suffix  = "female")

# -- 1.3  Rural ---------------------------------------------------------------
run_spei_neg_block(plabordata,
                   unemp_var    = "unemp_rur",
                   worker_var   = "worker_share_rur_unw",
                   title_suffix = "Rural (PS, Unweighted)",
                   file_suffix  = "rural")

# -- 1.4  Urban ---------------------------------------------------------------
run_spei_neg_block(plabordata,
                   unemp_var    = "unemp_urb",
                   worker_var   = "worker_share_urb_unw",
                   title_suffix = "Urban (PS, Unweighted)",
                   file_suffix  = "urban")

# ══════════════════════════════════════════════════════════════════════════════
# TABLE BLOCK 2 – Flood Index | All subgroups
# ══════════════════════════════════════════════════════════════════════════════

labor_data_flood <- labor_data |>
  filter(STATE != "ARUNACHAL PRADESH") |>
  filter(STATE != "MEGHALAYA")

plabordata_fi <- pdata.frame(labor_data_flood, index = c("STATE", "year"))

# -- 2.0  Aggregate (original) ------------------------------------------------
run_fi_block(plabordata_fi,
             unemp_var    = "unemp",
             worker_var   = "worker_share_unw",
             title_suffix = "Aggregate (PS, Unweighted)",
             file_suffix  = "aggregate")

# -- 2.1  Male ----------------------------------------------------------------
run_fi_block(plabordata_fi,
             unemp_var    = "unemp_m",
             worker_var   = "worker_share_m_unw",
             title_suffix = "Male (PS, Unweighted)",
             file_suffix  = "male")

# -- 2.2  Female --------------------------------------------------------------
run_fi_block(plabordata_fi,
             unemp_var    = "unemp_f",
             worker_var   = "worker_share_f_unw",
             title_suffix = "Female (PS, Unweighted)",
             file_suffix  = "female")

# -- 2.3  Rural ---------------------------------------------------------------
run_fi_block(plabordata_fi,
             unemp_var    = "unemp_rur",
             worker_var   = "worker_share_rur_unw",
             title_suffix = "Rural (PS, Unweighted)",
             file_suffix  = "rural")

# -- 2.4  Urban ---------------------------------------------------------------
run_fi_block(plabordata_fi,
             unemp_var    = "unemp_urb",
             worker_var   = "worker_share_urb_unw",
             title_suffix = "Urban (PS, Unweighted)",
             file_suffix  = "urban")