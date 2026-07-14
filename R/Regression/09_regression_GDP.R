# 1. Classify states into 2 groups based on GDP per capita
# 2. Run regressions for each group separately


#1

# --- Income grouping ---
high_income_states <- c(
  "HIMACHAL PRADESH",
  "ANDAMAN & NICOBAR",
  "GUJARAT",
  "TELANGANA",
  "TAMIL NADU",
  "MAHARASHTRA",
  "UTTARAKHAND",
  "KARNATAKA",
  "KERALA",
  "PUDUCHERRY",
  "CHANDIGARH",
  "SIKKIM",
  "DELHI",
  "GOA"
)

low_income_states <- c(
  "BIHAR",
  "UTTAR PRADESH",
  "JHARKHAND",
  "MANIPUR",
  "MADHYA PRADESH",
  "ODISHA",
  "MEGHALAYA",
  "CHHATTISGARH",
  "JAMMU AND KASHMIR",
  "WEST BENGAL",
  "RAJASTHAN",
  "TRIPURA",
  "ANDHRA PRADESH",
  "MIZORAM",
  "ARUNACHAL PRADESH",
  "PUNJAB"
)

df_income <- data %>%
  mutate(
    income_group = case_when(
      STATE %in% high_income_states ~ "High income",
      STATE %in% low_income_states  ~ "Low income",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("Low income", "High income"))
  )

# Sanity check — any STATE values that didn't match either list?
df_income %>%
  filter(is.na(income_group)) %>%
  distinct(STATE)

income_states_df <- tibble(
  STATE = c(high_income_states, low_income_states),
  income_group = c(
    rep("High income", length(high_income_states)),
    rep("Low income", length(low_income_states))
  )
)

unmatched_states1 <- income_states_df %>%
  anti_join(data, by = "STATE") %>%
  distinct(STATE)

unmatched_states2 <- data %>%
  anti_join(income_states_df, by = "STATE") %>%
  distinct(STATE)

unmatched_states1
unmatched_states2


print(unique(data$STATE))
print(unique(df_sections_income$STATE))


# 2 regressions

df_sections_income <- df_income %>%
  filter(!is.na(income_group))

m_income_high_spei <- lm(s_casual_w_worker_PS_unw ~ spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2 +
                           spei_neg_spei_lag3 +
                           factor(STATE) + factor(year), # + factor(STATE):year,
                         data = df_sections_income %>% filter(income_group == "High income"), weights = state_pop)
m_income_low_spei <- lm(s_casual_w_worker_PS_unw ~ spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2 +
                          spei_neg_spei_lag3 +
                          factor(STATE) + factor(year), # + factor(STATE):year,
                        data = df_sections_income %>% filter(income_group == "Low income"), weights = state_pop)

m_income_high_fi <- lm(s_casual_w_worker_PS_unw ~ FI_state + FI_lag1 + FI_lag2 +
                         factor(STATE) + factor(year), # + factor(STATE):year,
                       data = df_sections_income %>% filter(income_group == "High income"), weights = state_pop)
m_income_low_fi <- lm(s_casual_w_worker_PS_unw ~ FI_state + FI_lag1 + FI_lag2 +
                        factor(STATE) + factor(year), # + factor(STATE):year,
                      data = df_sections_income %>% filter(income_group == "Low income"), weights = state_pop)

m_income_high_pr <- lm(s_casual_w_worker_PS_unw ~ pr_score + pr_lag1 + pr_lag2 +
                         factor(STATE) + factor(year), # + factor(STATE):year,
                       data = df_sections_income %>% filter(income_group == "High income"), weights = state_pop)
m_income_low_pr <- lm(s_casual_w_worker_PS_unw ~ pr_score + pr_lag1 + pr_lag2 +
                        factor(STATE) + factor(year), # + factor(STATE):year,
                      data = df_sections_income %>% filter(income_group == "Low income"), weights = state_pop)


se_high_spei <- cluster_se(m_income_high_spei)
se_low_spei  <- cluster_se(m_income_low_spei)
se_high_fi   <- cluster_se(m_income_high_fi)
se_low_fi    <- cluster_se(m_income_low_fi)
se_high_pr   <- cluster_se(m_income_high_pr)
se_low_pr    <- cluster_se(m_income_low_pr)


# ── Print to console ──────────────────────────────────────────────────────────────
stargazer(
  m_income_high_spei, m_income_low_spei,
  se             = list(se_high_spei[, 2], se_low_spei[, 2]),
  p              = list(se_high_spei[, 4], se_low_spei[, 4]),
  title          = "Effect of Drought Shocks by Income Group (PS)",
  dep.var.labels = "Share Casual Workers (PS, Unw.)",
  column.labels  = c("High Income", "Low Income"),
  omit           = c("factor\\(STATE\\)", "factor\\(year\\)", "factor\\(STATE\\):year", "Constant"),
  omit.stat      = c("f", "ser"),
  notes          = "",
  order = c("spei_negative", "spei_neg_spei_lag1", "spei_neg_spei_lag2"),
  notes.append   = FALSE,
  label          = "tab:income_split_SPEI",
  type           = "text")

stargazer(
  m_income_high_fi,   m_income_low_fi,
  m_income_high_pr,   m_income_low_pr,
  se             = list(se_high_fi[, 2],   se_low_fi[, 2],
                        se_high_pr[, 2],   se_low_pr[, 2]),
  p              = list(se_high_fi[, 4],   se_low_fi[, 4],
                        se_high_pr[, 4],   se_low_pr[, 4]),
  title          = "Effect of Climate Shocks by Income Group (PS)",
  dep.var.labels = "Share Casual Workers (PS, Unw.)",
  column.labels  = c("High Income", "Low Income", "High Income", "Low Income"),
  omit           = c("factor\\(STATE\\)", "factor\\(year\\)", "factor\\(STATE\\):year", "Constant"),
  omit.stat      = c("f", "ser"),
  add.lines = list(
    c("State FE",       "Yes", "Yes", "Yes", "Yes"),
    c("Year FE",        "Yes", "Yes", "Yes", "Yes"),
    c("Linear trends",  "Yes", "Yes", "Yes", "Yes")
  ),
  notes          = "",
  order = c("FI_state", "FI_lag1", "FI_lag2",
            "pr_score", "pr_lag1"),
  notes.append   = FALSE,
  label          = "tab:income_split_FLOODS",
  type           = "text")


# ==============================================================================
# MEGA TABLE
# ==============================================================================

stargazer(
  m_income_high_spei, m_income_low_spei,
  m_income_high_fi,   m_income_low_fi,
  m_income_high_pr,   m_income_low_pr,
  se             = list(se_high_spei[, 2], se_low_spei[, 2],
                        se_high_fi[, 2],   se_low_fi[, 2],
                        se_high_pr[, 2],   se_low_pr[, 2]),
  p              = list(se_high_spei[, 4], se_low_spei[, 4],
                        se_high_fi[, 4],   se_low_fi[, 4],
                        se_high_pr[, 4],   se_low_pr[, 4]),
  title          = "Casual Labour-Force Participation by State Income Group",
  dep.var.caption        = "Casual Labour Share (PS)",
  dep.var.labels.include = FALSE,
  column.labels  = c("High Income", "Low Income", "High Income", "Low Income", "High Income", "Low Income"),
  omit           = c("factor\\(STATE\\)", "factor\\(year\\)", "factor\\(STATE\\):year", "Constant"),
  omit.stat      = c("f", "ser"),
  notes          = "State population weights.",
  notes.append   = FALSE,
  label          = "tab:income_all",
  type           = "latex"
)
