reg_data <- data_indu %>% filter(!is.na(share_Agriculture))  # drop unmatched rows

run_spei_model <- function(outcome_var, dat) {
  f <- as.formula(paste0(
    outcome_var, " ~ spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2 + spei_neg_spei_lag3 +
    factor(STATE) + factor(year) + factor(STATE):year"
  ))
  lm(f, data = dat, weights = state_pop)
}

run_fi_model <- function(outcome_var, dat) {
  f <- as.formula(paste0(
    outcome_var, " ~ FI_state + FI_lag1 + FI_lag2 +
    factor(STATE) + factor(year) + factor(STATE):year"
  ))
  lm(f, data = dat, weights = state_pop)
}

run_pr_model <- function(outcome_var, dat) {
  f <- as.formula(paste0(
    outcome_var, " ~ pr_score + pr_lag1 + pr_lag2 +
    factor(STATE) + factor(year) + factor(STATE):year"
  ))
  lm(f, data = dat, weights = state_pop)
}

m_spei <- map(sector_cols, run_spei_model, dat = reg_data) %>% set_names(sector_cols)
m_fi   <- map(sector_cols, run_fi_model,   dat = reg_data) %>% set_names(sector_cols)
m_pr   <- map(sector_cols, run_pr_model,   dat = reg_data) %>% set_names(sector_cols)

se_spei <- map(m_spei, cluster_se)
se_fi   <- map(m_fi,   cluster_se)
se_pr   <- map(m_pr,   cluster_se)

# ══════════════════════════════════════════════════════════════════
# Output tables
# ══════════════════════════════════════════════════════════════════

# ══════════════════════════════════════════════════════════════════
# Table 1: Agriculture, Mining & Quarrying
# ══════════════════════════════════════════════════════════════════
se_vec_agri <- se_fi[["share_Agriculture"]][match(names(coef(m_fi[["share_Agriculture"]])), rownames(se_fi[["share_Agriculture"]])), 2]
p_vec_agri  <- se_fi[["share_Agriculture"]][match(names(coef(m_fi[["share_Agriculture"]])), rownames(se_fi[["share_Agriculture"]])), 4]
se_vec_agri[is.na(se_vec_agri)] <- 0
p_vec_agri[is.na(p_vec_agri)]   <- 1

se_vec_mining <- se_fi[["share_Mining_Quarrying"]][match(names(coef(m_fi[["share_Mining_Quarrying"]])), rownames(se_fi[["share_Mining_Quarrying"]])), 2]
p_vec_mining  <- se_fi[["share_Mining_Quarrying"]][match(names(coef(m_fi[["share_Mining_Quarrying"]])), rownames(se_fi[["share_Mining_Quarrying"]])), 4]
se_vec_mining[is.na(se_vec_mining)] <- 0
p_vec_mining[is.na(p_vec_mining)]   <- 1

stargazer(
  m_fi[["share_Agriculture"]],
  m_fi[["share_Mining_Quarrying"]],
  se             = list(se_vec_agri, se_vec_mining),
  p              = list(p_vec_agri, p_vec_mining),
  title          = "Effect of Flood Shocks on Industry Employment Share (1/5)",
  dep.var.labels = "",
  column.labels  = c("Agriculture", "Mining & Quarrying"),
  omit           = c("factor\\(STATE\\)", "factor\\(year\\)", "factor\\(STATE\\):year", "Constant"),
  omit.stat      = c("f", "ser"),
  order          = c("FI_state", "FI_lag1", "FI_lag2"),
  notes          = "",
  notes.append   = FALSE,
  label          = "tab:sector_FI_1",
  type           = "text"
)

# ══════════════════════════════════════════════════════════════════
# Table 2: Manufacturing, Utilities
# ══════════════════════════════════════════════════════════════════
se_vec_manuf <- se_fi[["share_Manufacturing"]][match(names(coef(m_fi[["share_Manufacturing"]])), rownames(se_fi[["share_Manufacturing"]])), 2]
p_vec_manuf  <- se_fi[["share_Manufacturing"]][match(names(coef(m_fi[["share_Manufacturing"]])), rownames(se_fi[["share_Manufacturing"]])), 4]
se_vec_manuf[is.na(se_vec_manuf)] <- 0
p_vec_manuf[is.na(p_vec_manuf)]   <- 1

se_vec_util <- se_fi[["share_Utilities"]][match(names(coef(m_fi[["share_Utilities"]])), rownames(se_fi[["share_Utilities"]])), 2]
p_vec_util  <- se_fi[["share_Utilities"]][match(names(coef(m_fi[["share_Utilities"]])), rownames(se_fi[["share_Utilities"]])), 4]
se_vec_util[is.na(se_vec_util)] <- 0
p_vec_util[is.na(p_vec_util)]   <- 1

stargazer(
  m_fi[["share_Manufacturing"]],
  m_fi[["share_Utilities"]],
  se             = list(se_vec_manuf, se_vec_util),
  p              = list(p_vec_manuf, p_vec_util),
  title          = "Effect of Flood Shocks on Industry Employment Share (2/5)",
  dep.var.labels = "",
  column.labels  = c("Manufacturing", "Utilities"),
  omit           = c("factor\\(STATE\\)", "factor\\(year\\)", "factor\\(STATE\\):year", "Constant"),
  omit.stat      = c("f", "ser"),
  order          = c("FI_state", "FI_lag1", "FI_lag2"),
  notes          = "",
  notes.append   = FALSE,
  label          = "tab:sector_FI_2",
  type           = "text"
)

# ══════════════════════════════════════════════════════════════════
# Table 3: Construction, Trade/Hotels/Restaurants
# ══════════════════════════════════════════════════════════════════
se_vec_constr <- se_fi[["share_Construction"]][match(names(coef(m_fi[["share_Construction"]])), rownames(se_fi[["share_Construction"]])), 2]
p_vec_constr  <- se_fi[["share_Construction"]][match(names(coef(m_fi[["share_Construction"]])), rownames(se_fi[["share_Construction"]])), 4]
se_vec_constr[is.na(se_vec_constr)] <- 0
p_vec_constr[is.na(p_vec_constr)]   <- 1

se_vec_trade <- se_fi[["share_Trade_Hotels_Restaurants"]][match(names(coef(m_fi[["share_Trade_Hotels_Restaurants"]])), rownames(se_fi[["share_Trade_Hotels_Restaurants"]])), 2]
p_vec_trade  <- se_fi[["share_Trade_Hotels_Restaurants"]][match(names(coef(m_fi[["share_Trade_Hotels_Restaurants"]])), rownames(se_fi[["share_Trade_Hotels_Restaurants"]])), 4]
se_vec_trade[is.na(se_vec_trade)] <- 0
p_vec_trade[is.na(p_vec_trade)]   <- 1

stargazer(
  m_fi[["share_Construction"]],
  m_fi[["share_Trade_Hotels_Restaurants"]],
  se             = list(se_vec_constr, se_vec_trade),
  p              = list(p_vec_constr, p_vec_trade),
  title          = "Effect of Flood Shocks on Industry Employment Share (3/5)",
  dep.var.labels = "",
  column.labels  = c("Construction", "Trade/Hotels"),
  omit           = c("factor\\(STATE\\)", "factor\\(year\\)", "factor\\(STATE\\):year", "Constant"),
  omit.stat      = c("f", "ser"),
  order          = c("FI_state", "FI_lag1", "FI_lag2"),
  notes          = "",
  notes.append   = FALSE,
  label          = "tab:sector_FI_3",
  type           = "text"
)

# ══════════════════════════════════════════════════════════════════
# Table 4: Transport/Storage/Comm, Finance/Insurance/RealEstate
# ══════════════════════════════════════════════════════════════════
se_vec_transport <- se_fi[["share_Transport_Storage_Comm"]][match(names(coef(m_fi[["share_Transport_Storage_Comm"]])), rownames(se_fi[["share_Transport_Storage_Comm"]])), 2]
p_vec_transport  <- se_fi[["share_Transport_Storage_Comm"]][match(names(coef(m_fi[["share_Transport_Storage_Comm"]])), rownames(se_fi[["share_Transport_Storage_Comm"]])), 4]
se_vec_transport[is.na(se_vec_transport)] <- 0
p_vec_transport[is.na(p_vec_transport)]   <- 1

se_vec_finance <- se_fi[["share_Finance_Insurance_RealEstate"]][match(names(coef(m_fi[["share_Finance_Insurance_RealEstate"]])), rownames(se_fi[["share_Finance_Insurance_RealEstate"]])), 2]
p_vec_finance  <- se_fi[["share_Finance_Insurance_RealEstate"]][match(names(coef(m_fi[["share_Finance_Insurance_RealEstate"]])), rownames(se_fi[["share_Finance_Insurance_RealEstate"]])), 4]
se_vec_finance[is.na(se_vec_finance)] <- 0
p_vec_finance[is.na(p_vec_finance)]   <- 1

stargazer(
  m_fi[["share_Transport_Storage_Comm"]],
  m_fi[["share_Finance_Insurance_RealEstate"]],
  se             = list(se_vec_transport, se_vec_finance),
  p              = list(p_vec_transport, p_vec_finance),
  title          = "Effect of Flood Shocks on Industry Employment Share (4/5)",
  dep.var.labels = "",
  column.labels  = c("Transport", "Finance/RealEstate"),
  omit           = c("factor\\(STATE\\)", "factor\\(year\\)", "factor\\(STATE\\):year", "Constant"),
  omit.stat      = c("f", "ser"),
  order          = c("FI_state", "FI_lag1", "FI_lag2"),
  notes          = "",
  notes.append   = FALSE,
  label          = "tab:sector_FI_4",
  type           = "text"
)

# ══════════════════════════════════════════════════════════════════
# Table 5: Community/Social/Personal (remaining single sector)
# ══════════════════════════════════════════════════════════════════
se_vec_comm <- se_fi[["share_Community_Social_Personal"]][match(names(coef(m_fi[["share_Community_Social_Personal"]])), rownames(se_fi[["share_Community_Social_Personal"]])), 2]
p_vec_comm  <- se_fi[["share_Community_Social_Personal"]][match(names(coef(m_fi[["share_Community_Social_Personal"]])), rownames(se_fi[["share_Community_Social_Personal"]])), 4]
se_vec_comm[is.na(se_vec_comm)] <- 0
p_vec_comm[is.na(p_vec_comm)]   <- 1

stargazer(
  m_fi[["share_Community_Social_Personal"]],
  se             = list(se_vec_comm),
  p              = list(p_vec_comm),
  title          = "Effect of Flood Shocks on Industry Employment Share (5/5)",
  dep.var.labels = "",
  column.labels  = "Community/Social",
  omit           = c("factor\\(STATE\\)", "factor\\(year\\)", "factor\\(STATE\\):year", "Constant"),
  omit.stat      = c("f", "ser"),
  order          = c("FI_state", "FI_lag1", "FI_lag2"),
  notes          = "",
  notes.append   = FALSE,
  label          = "tab:sector_FI_5",
  type           = "text"
)

