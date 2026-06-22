df_sections_agri <- df_sections  |> 
  filter(!is.na(agri_group))

m_agri_large_spei <- lm(s_casual_w_worker_PS_unw ~ spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2 +
                          spei_neg_spei_lag3 +
                          factor(STATE) + factor(year) + factor(STATE):year,
                        data = df_sections_agri %>% filter(agri_group == "High agri"), weights = state_pop)
m_agri_small_spei <- lm(s_casual_w_worker_PS_unw ~ spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2 +
                          spei_neg_spei_lag3 +
                          factor(STATE) + factor(year) + factor(STATE):year,
                        data = df_sections_agri %>% filter(agri_group == "Low agri"), weights = state_pop)

m_agri_large_fi <- lm(s_casual_w_worker_PS_unw ~ FI_state + FI_lag1 + FI_lag2 +
                        factor(STATE) + factor(year) + factor(STATE):year,
                      data = df_sections_agri %>% filter(agri_group == "High agri"), weights = state_pop)
m_agri_small_fi <- lm(s_casual_w_worker_PS_unw ~ FI_state+ FI_lag1 + FI_lag2 +
                        factor(STATE) + factor(year) + factor(STATE):year,
                      data = df_sections_agri %>% filter(agri_group == "Low agri"), weights = state_pop)

m_agri_large_pr <- lm(s_casual_w_worker_PS_unw ~  pr_score + pr_lag1 + #pr_lag2 +
                        factor(STATE) + factor(year) + factor(STATE):year,
                      data = df_sections_agri %>% filter(agri_group == "High agri"), weights = state_pop)
m_agri_small_pr <- lm(s_casual_w_worker_PS_unw ~ pr_score + pr_lag1 + #pr_lag2 +
                        factor(STATE) + factor(year) + factor(STATE):year,
                      data = df_sections_agri %>% filter(agri_group == "Low agri"), weights = state_pop)


se_large_spei <- cluster_se(m_agri_large_spei)
se_small_spei <- cluster_se(m_agri_small_spei)
se_large_fi   <- cluster_se(m_agri_large_fi)
se_small_fi   <- cluster_se(m_agri_small_fi)
se_large_pr   <- cluster_se(m_agri_large_pr)
se_small_pr   <- cluster_se(m_agri_small_pr)


# ── Print to console ──────────────────────────────────────────────────────────────
stargazer(
  m_agri_large_spei, m_agri_small_spei,
  se             = list(se_large_spei[, 2], se_small_spei[, 2]),
  p              = list(se_large_spei[, 4], se_small_spei[, 4]),
  title          = "Effect of Droughts Shocks by Agricultural Dependence (W)",
  dep.var.labels = "Share Casual Workers (PS, Unw.)",
  column.labels  = c("High Agri", "Low Agri"),
  omit           = c("factor\\(STATE\\)", "factor\\(year\\)", "factor\\(STATE\\):year", "Constant"),
  omit.stat      = c("f", "ser"),
  add.lines = list(
    c("State FE",             "Yes", "Yes"),
    c("Year FE",              "Yes", "Yes"),
    c("Linear trends", "Yes", "Yes")
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
  order = c("FI_state", "FI_lag1", "FI_lag2", 
            "pr_score", "pr_lag1"),
  notes.append   = FALSE,
  label          = "tab:agri_split_FLOODS",
  type           = "text")

stargazer(
  m_agri_large_spei, m_agri_small_spei,
  m_agri_large_fi,   m_agri_small_fi,
  se             = list(se_large_spei[, 2], se_small_spei[, 2], se_large_fi[, 2],   se_small_fi[, 2]
                        ),
  p              = list(se_large_spei[, 4], se_small_spei[, 4], se_large_fi[, 4],   se_small_fi[, 4],
                        se_large_pr[, 4],   se_small_pr[, 4]),
  title          = "Effect of Climate Shocks by Agricultural Dependence (PS)",
  dep.var.labels = "Share Casual Workers (W)",
  column.labels  = c("High Agri", "Low Agri", "High Agri", "Low Agri"),
  omit           = c("factor\\(STATE\\)", "factor\\(year\\)", "factor\\(STATE\\):year", "Constant"),
  omit.stat      = c("f", "ser"),
  notes          = "",
  notes.append   = FALSE,
  label          = "tab:d&f",
  type           = "latex")
