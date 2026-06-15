out <- "/Users/ninabilirossi/Desktop/MSC THESIS/Data works/Code/Outputs/regressions/weighted_workers_optimal_lags"
out <- "/Users/ninabilirossi/Desktop/MSC THESIS/Data works/Code/Outputs/hand picked latex food weekly"
dir.create(out, recursive = TRUE, showWarnings = FALSE)


# ══════════════════════════════════════════════════════════════════════════════
# TABLE 1 – General population
# ══════════════════════════════════════════════════════════════════════════════
# ── Models ────────────────────────────────────────────────────────────────────
m1a <- lm(s_casual_w_worker_W_unw ~ #spei_negative + 
            spei_neg_spei_lag1 + 
            spei_neg_spei_lag2 + spei_neg_spei_lag3 +
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data, weights = state_pop)

m1b <- lm(s_casual_w_worker_W_unw ~ #FI_state + 
            FI_lag1  + FI_lag2 +
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data_flood, weights = state_pop)

m1c <- lm(s_casual_w_worker_W_unw ~ #pr_score 
          + pr_lag1 +  pr_lag2 +
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data, weights = state_pop)

# ── Clustered SEs ─────────────────────────────────────────────────────────────
se1a <- cluster_se(m1a)
se1b <- cluster_se(m1b)
se1c <- cluster_se(m1c)

# ── Save to file ──────────────────────────────────────────────────────────────
stargazer(
  m1a, m1b, m1c,
  se             = list(se1a[, 2], se1b[, 2], se1c[, 2]),
  p              = list(se1a[, 4], se1b[, 4], se1c[, 4]),
  title          = "Effect of Climate Shocks on Casual Labour-Force Participation (W)",
  dep.var.labels = "Informality (PS)",
  column.labels  = c("Neg. SPEI-12", "Flood Index", "PR Index"),
  omit           = c("factor\\(STATE\\)", "factor\\(year\\)", "factor\\(STATE\\):year", "Constant"),
  omit.stat      = c("f", "ser"),
  add.lines = list(
    c("State FE",             "Yes", "Yes", "Yes"),
    c("Year FE",              "Yes", "Yes", "Yes"),
    c("Linear trends", "Yes", "Yes", "Yes")
  ),
  notes          = "",
  notes.append   = FALSE,
  label          = "tab:neg_spei",
  type           = "latex",
  out            = file.path(out, "Base.tex")
)

# ── Print to console ──────────────────────────────────────────────────────────
stargazer(
  m1a, m1b, m1c,
  se             = list(se1a[, 2], se1b[, 2], se1c[, 2]),
  p              = list(se1a[, 4], se1b[, 4], se1c[, 4]),
  title          = "Effect of Climate Shocks on Informality (W)",
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

cat("✓ Table 1 (All pop) saved.\n")

# ══════════════════════════════════════════════════════════════════════════════
# TABLE 1' – General population - positive spei
# ══════════════════════════════════════════════════════════════════════════════
# ── Models ────────────────────────────────────────────────────────────────────
m1ap <- lm(s_casual_w_worker_PS_unw ~ spei_spei12 + spei_spei_lag1 + spei_spei_lag2 + #spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2 + #
            factor(STATE) + factor(year) , #+ factor(STATE):year,
          data = data, weights = state_pop)

# ── Clustered SEs ─────────────────────────────────────────────────────────────
se1ap <- cluster_se(m1ap)


# ── Save to file ──────────────────────────────────────────────────────────────
stargazer(
  m1ap,
  se             = se1ap[, 2],
  p              = se1ap[, 4],
  title          = "Effect of SPEI on Casual Labour-Force Participation (PS, weighted)",
  dep.var.labels = "Informality (PS)",
  column.labels  = c("SPEI-12"),
  omit           = c("factor\\(STATE\\)", "factor\\(year\\)", "factor\\(STATE\\):year", "Constant"),
  omit.stat      = c("f", "ser"),
  notes          = "",
  notes.append   = FALSE,
  label          = "tab:full_spei",
  type           = "latex",
  out            = file.path(out, "fullSPEI_Base.tex")
)

# ── Print to console ──────────────────────────────────────────────────────────
stargazer(
  m1ap,
  se             = list(se1ap[, 2]),
  p              = list(se1ap[, 4]),
  title          = "Effect of SPEI on Casual Labour-Force Participation (PS, weighted)",
  dep.var.labels = "Informality (PS)",
  column.labels  = c("SPEI-12"),
  omit           = c("factor\\(STATE\\)", "factor\\(year\\)", "factor\\(STATE\\):year", "Constant"),
  omit.stat      = c("f", "ser"),
  notes          = "",
  notes.append   = FALSE,
  label          = "tab:full_spei",
  type           = "text"
)

cat("✓ Table 1 prime (All pop, full spei only) saved.\n")

# ══════════════════════════════════════════════════════════════════════════════
# TABLE 2 – General population - without UT
# ══════════════════════════════════════════════════════════════════════════════
# ── Models ────────────────────────────────────────────────────────────────────
m2a <- lm(s_casual_w_worker_W_unw ~ spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2 +
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data_no_UT, weights = state_pop)

m2b <- lm(s_casual_w_worker_W_unw ~ FI_state + FI_lag1 +  # FI_lag2 +
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data_flood_no_UT, weights = state_pop)

m2c <- lm(s_casual_w_worker_W_unw ~ pr_score + pr_lag1 + # pr_lag2 +
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data_no_UT, weights = state_pop)

# ── Clustered SEs ─────────────────────────────────────────────────────────────
se2a <- cluster_se(m2a)
se2b <- cluster_se(m2b)
se2c <- cluster_se(m2c)

# ── Save to file ──────────────────────────────────────────────────────────────
stargazer(
  m2a, m2b, m2c,
  se             = list(se2a[, 2], se2b[, 2], se2c[, 2]),
  p              = list(se2a[, 4], se2b[, 4], se2c[, 4]),
  title          = "Effect of Climate Shocks on Informality (PS, weighted, excl. UTs)",
  dep.var.labels = "Informality (PS)",
  column.labels  = c("Neg. SPEI-12", "Flood Index", "PR Index"),
  omit           = c("factor\\(STATE\\)", "factor\\(year\\)", "factor\\(STATE\\):year", "Constant"),
  omit.stat      = c("f", "ser"),
  notes          = "",
  order = c("spei_negative", "spei_neg_spei_lag1", "spei_neg_spei_lag2",
            "FI_state", "FI_lag1", "FI_lag2",
            "pr_score", "pr_lag1", "pr_lag2"),
  notes.append   = FALSE,
  label          = "tab:no_ut",
  type           = "latex",
  out            = file.path(out, "NoUTs.tex")
)

# ── Print to console ──────────────────────────────────────────────────────────
stargazer(
  m2a, m2b, m2c,
  se             = list(se2a[, 2], se2b[, 2], se2c[, 2]),
  p              = list(se2a[, 4], se2b[, 4], se2c[, 4]),
  title          = "Effect of Climate Shocks on Informality (PS, weighted, excl. UTs)",
  dep.var.labels = "Informality (PS)",
  column.labels  = c("Neg. SPEI-12", "Flood Index", "PR Index"),
  omit           = c("factor\\(STATE\\)", "factor\\(year\\)", "factor\\(STATE\\):year", "Constant"),
  omit.stat      = c("f", "ser"),
  notes          = "",
  order = c("spei_negative", "spei_neg_spei_lag1", "spei_neg_spei_lag2",
            "FI_state", "FI_lag1", "FI_lag2",
            "pr_score", "pr_lag1", "pr_lag2"),
  notes.append   = FALSE,
  label          = "tab:no_ut",
  type           = "text"
)
cat("✓ Table 2 (no UT) saved.\n")

# ══════════════════════════════════════════════════════════════════════════════
# TABLE 3 - by level of agricultural dependence
# ══════════════════════════════════════════════════════════════════════════════

source(file.path("~/Desktop/MSC THESIS/Data works/Code/Master/R/Data cleaning/expanding_dataframe.R"))
library(marginaleffects)

df_sections <- data %>%
  left_join(agri_share, by = "STATE") |> 
  mutate(agri_share_std = as.numeric(scale(agri_share)))

# --- Agri median ---
df_sections <- df_sections %>%
  mutate(
    agri_median = ntile(agri_share, 2),
    agri_group   = case_when(
      agri_median == 1 ~ "Low agri",
      # agri_tercile == 2 ~ "Middle",
      agri_median == 2 ~ "High agri"
    ) %>% factor(levels = c("Low agri", "High agri")) )

df_sections_agri <- df_sections  |> 
  filter(!is.na(agri_group))

print(df_sections_agri['STATE'] %>% unique())

# DROUGHTS
formula_spei <- as.formula("s_casual_w_worker_W_unw ~ spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2 | STATE + year ")#+ STATE[year]")
formula_spei <- as.formula("s_casual_w_worker_PS_unw ~ spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2 | STATE + year ")#+ STATE[year]")


m_agri_large_spei  <- feols(formula_spei,
                       data = df_sections_agri %>% filter(agri_median == 2), vcov = "HC1")
m_agri_small_spei  <- feols(formula_spei,
                       data = df_sections_agri %>% filter(agri_median == 1), vcov = "HC1")

etable(m_agri_large_spei, m_agri_small_spei,
       headers = c("Large agri states", "Small agri states"))

# FLOODS
formula_fi <- as.formula("s_casual_w_worker_W_unw ~ FI_state + FI_lag1 | STATE + year ") #+ STATE[year] ")
m_agri_large_fi  <- feols(formula_fi,
                            data = df_sections_agri %>% filter(agri_median == 2), vcov = "HC1")
m_agri_small_fi  <- feols(formula_fi,
                            data = df_sections_agri %>% filter(agri_median == 1), vcov = "HC1")

etable(m_agri_large_fi, m_agri_small_fi,
       headers = c("Large agri states", "Small agri states"))

# PR
formula_pr <- as.formula("s_casual_w_worker_W_unw ~ pr_score + pr_lag1 | STATE + year")
m_agri_large_pr  <- feols(formula_pr,
                          data = df_sections_agri %>% filter(agri_median == 2), vcov = "HC1")
m_agri_small_pr  <- feols(formula_pr,
                          data = df_sections_agri %>% filter(agri_median == 1), vcov = "HC1")

etable(m_agri_large_pr, m_agri_small_pr,
       headers = c("Large agri states", "Small agri states"))

# WITH lm()

m_agri_large_spei <- lm(s_casual_w_worker_PS_unw ~ spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2 +
                          factor(STATE) + factor(year),# + factor(STATE):year,
                        data = df_sections_agri %>% filter(agri_median == 2), weights = state_pop)
m_agri_small_spei <- lm(s_casual_w_worker_PS_unw ~ spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2 +
                          factor(STATE) + factor(year),# + factor(STATE):year,
                        data = df_sections_agri %>% filter(agri_median == 1), weights = state_pop)

m_agri_large_fi <- lm(s_casual_w_worker_W_unw ~ FI_state + FI_lag1 + #FI_lag2 +
                        factor(STATE) + factor(year) + factor(STATE):year,
                      data = df_sections_agri %>% filter(agri_median == 2), weights = state_pop)
m_agri_small_fi <- lm(s_casual_w_worker_W_unw ~ FI_state + FI_lag1 + #FI_lag2 +
                        factor(STATE) + factor(year) + factor(STATE):year,
                      data = df_sections_agri %>% filter(agri_median == 1), weights = state_pop)

m_agri_large_pr <- lm(s_casual_w_worker_W_unw ~ pr_score + pr_lag1 + #pr_lag2 +
                        factor(STATE) + factor(year) + factor(STATE):year,
                      data = df_sections_agri %>% filter(agri_median == 2), weights = state_pop)
m_agri_small_pr <- lm(s_casual_w_worker_W_unw ~ pr_score + pr_lag1 + #pr_lag2 +
                        factor(STATE) + factor(year) + factor(STATE):year,
                      data = df_sections_agri %>% filter(agri_median == 1), weights = state_pop)


se_large_spei <- cluster_se(m_agri_large_spei)
se_small_spei <- cluster_se(m_agri_small_spei)
se_large_fi   <- cluster_se(m_agri_large_fi)
se_small_fi   <- cluster_se(m_agri_small_fi)
se_large_pr   <- cluster_se(m_agri_large_pr)
se_small_pr   <- cluster_se(m_agri_small_pr)

# ── Save to files ──────────────────────────────────────────────────────────────
stargazer(
  m_agri_large_spei, m_agri_small_spei,
  se             = list(se_large_spei[, 2], se_small_spei[, 2]),
  p              = list(se_large_spei[, 4], se_small_spei[, 4]),
  title          = "Effect of Droughts Shocks by Agricultural Dependence (PS)",
  dep.var.labels = "Share Casual Workers (PS, Unw.)",
  column.labels  = c("High Agri", "Low Agri"),
  omit           = c("factor\\(STATE\\)", "factor\\(year\\)", "factor\\(STATE\\):year", "Constant"),
  omit.stat      = c("f", "ser"),
  add.lines = list(
    c("State FE",             "Yes", "Yes"),
    c("Year FE",              "Yes", "Yes"),
    c("Linear trends", "No", "No")
  ),
  notes          = "",
  order = c("spei_negative", "spei_neg_spei_lag1", "spei_neg_spei_lag2"),
  notes.append   = FALSE,
  label          = "tab:agri_split_SPEI",
  type           = "latex",
  out            = file.path(out, "AgriSplit_SPEI.tex")
)

stargazer(
  m_agri_large_fi,   m_agri_small_fi,
  m_agri_large_pr,   m_agri_small_pr,
  se             = list(se_large_fi[, 2],   se_small_fi[, 2],
                        se_large_pr[, 2],   se_small_pr[, 2]),
  p              = list(se_large_fi[, 4],   se_small_fi[, 4],
                        se_large_pr[, 4],   se_small_pr[, 4]),
  title          = "Effect of Climate Shocks by Agricultural Dependence (W)",
  dep.var.labels = "Share Casual Workers (PS, Unw.)",
  column.labels  = c("High Agri", "Low Agri", "High Agri", "Low Agri"),
  omit           = c("factor\\(STATE\\)", "factor\\(year\\)", "factor\\(STATE\\):year", "Constant"),
  omit.stat      = c("f", "ser"),
  add.lines = list(
    c("State FE",             "Yes", "Yes", "Yes", "Yes"),
    c("Year FE",              "Yes", "Yes", "Yes", "Yes"),
    c("Linear trends", "Yes", "Yes", "Yes", "Yes")
  ),
  notes          = "",
  order = c("FI_state", "FI_lag1", 
            "pr_score", "pr_lag1"),
  notes.append   = FALSE,
  label          = "tab:agri_split_FLOODS",
  type           = "latex",
  out            = file.path(out, "AgriSplit_FLOODS.tex"))
# ── Print to console ──────────────────────────────────────────────────────────────
stargazer(
  m_agri_large_spei, m_agri_small_spei,
  se             = list(se_large_spei[, 2], se_small_spei[, 2]),
  p              = list(se_large_spei[, 4], se_small_spei[, 4]),
  title          = "Effect of Droughts Shocks by Agricultural Dependence (PS, weighted)",
  dep.var.labels = "Share Casual Workers (PS, Unw.)",
  column.labels  = c("High Agri", "Low Agri"),
  omit           = c("factor\\(STATE\\)", "factor\\(year\\)", "factor\\(STATE\\):year", "Constant"),
  omit.stat      = c("f", "ser"),
  add.lines = list(
    c("State FE",             "Yes", "Yes"),
    c("Year FE",              "Yes", "Yes"),
    c("Linear trends", "No", "No")
  ),
  notes          = "",
  order = c("spei_negative", "spei_neg_spei_lag1", "spei_neg_spei_lag2"),
  notes.append   = FALSE,
  label          = "tab:agri_split_SPEI",
  type           = "text")

stargazer(
  m_agri_large_fi,   m_agri_small_fi,
  m_agri_large_pr,   m_agri_small_pr,
  se             = list(se_large_fi[, 2],   se_small_fi[, 2],
                        se_large_pr[, 2],   se_small_pr[, 2]),
  p              = list(se_large_fi[, 4],   se_small_fi[, 4],
                        se_large_pr[, 4],   se_small_pr[, 4]),
  title          = "Effect of Climate Shocks by Agricultural Dependence (PS, weighted)",
  dep.var.labels = "Share Casual Workers (PS, Unw.)",
  column.labels  = c("High Agri", "Low Agri", "High Agri", "Low Agri"),
  omit           = c("factor\\(STATE\\)", "factor\\(year\\)", "factor\\(STATE\\):year", "Constant"),
  omit.stat      = c("f", "ser"),
  add.lines = list(
    c("State FE",             "Yes", "Yes", "Yes", "Yes"),
    c("Year FE",              "Yes", "Yes", "Yes", "Yes"),
    c("Linear trends", "Yes", "Yes", "Yes", "Yes")
  ),
  notes          = "",
  order = c("FI_state", "FI_lag1", 
            "pr_score", "pr_lag1"),
  notes.append   = FALSE,
  label          = "tab:agri_split_FLOODS",
  type           = "text")

cat("✓ Table 3 (Agri split) saved.\n")

