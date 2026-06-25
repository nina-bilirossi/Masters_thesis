# REGRESSING ON WORKERS
# ── Output paths ───────────────────────────────────────────────────────────────
out  <- "/Users/ninabilirossi/Desktop/MSC THESIS/Data works/Code/Outputs/regressions/weighted_workers"
dir.create(out,  recursive = TRUE, showWarnings = FALSE)

colnames(data)

library(dplyr)

df <- data %>%
  group_by(STATE) %>%
  mutate(
    pop_rur_unw_2017 = max(if_else(year == 2017, pop_rur_unw, NA_real_), na.rm = TRUE),
    pop_urb_unw_2017 = max(if_else(year == 2017, pop_urb_unw, NA_real_), na.rm = TRUE)
  ) %>%
  ungroup()

data <- df
data_flood <- df |> filter(STATE != "ARUNCHAL PRADESH" & STATE != "MEGHALAYA")


# ══════════════════════════════════════════════════════════════════════════════
# SPEI — Sector subgroups (optimal spec: contemp. + lag1 + lag2)
# Rural outcome:  s_casual_w_worker_W_rur_unw
# Urban outcome: s_casual_w_worker_W_urb_unw
# ══════════════════════════════════════════════════════════════════════════════
# 
# # ── Full SPEI ──────────────────────────────────────────────────────────────────
# spei_full_r <- lm(s_casual_w_worker_W_rur_unw ~ spei_spei_lag1 + spei_spei_lag2+
#                     spei_spei_lag3 +
#                      factor(STATE) + factor(year), # + factor(STATE):year,
#                    data = data
#                    , weights = state_pop
# ) 
# 
# spei_full_u <- lm(s_casual_w_worker_W_urb_unw ~ spei_spei_lag1 + spei_spei_lag2+
#                     spei_spei_lag3 +
#                      factor(STATE) + factor(year), # + factor(STATE):year,
#                    data = data
#                    , weights = state_pop
# ) 
# 
# se_spei_full_r <- cluster_se(spei_full_r)
# se_spei_full_u <- cluster_se(spei_full_u)
# 
# stargazer(
#   spei_full_r, spei_full_u,
#   se = list(se_spei_full_r[, 2], se_spei_full_u[, 2]),
#   p  = list(se_spei_full_r[, 4], se_spei_full_u[, 4]),
#   title          = "Full SPEI and Casual Labour-Force Participation by Sector (PS, weighted)",
#   dep.var.labels = c("Rural Share", "Urban Share"),
#   omit = c("factor\\(STATE\\)",
#            "factor\\(year\\)",
#            "factor\\(STATE\\):year", "Constant"),
#   column.labels  = c("Rural", "Urban"),
#   omit.stat    = c("f", "ser"),
#   notes        = " ",
#   notes.append = FALSE,
#   type         = "text",
#   label        = "tab:sector_full_spei"
# )
# cat("✓ SPEI Full — sector table saved.\n")

# ── Negative SPEI ─────────────────────────────────────────────────────────────
spei_neg_r <- lm(s_casual_w_worker_W_rur_unw ~ spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2+
                   spei_neg_spei_lag3 +
                    factor(STATE) + factor(year) + factor(STATE):year,
                  data = data
                  , weights = pop_rur_unw_2017 #state_pop
) 

spei_neg_u <- lm(s_casual_w_worker_W_urb_unw ~ spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2+
                   spei_neg_spei_lag3 +
                  factor(STATE) + factor(year) + factor(STATE):year,
                  data = data
                  , weights = pop_urb_unw_2017 # state_pop
) 

se_spei_neg_r <- cluster_se(spei_neg_r)
se_spei_neg_u <- cluster_se(spei_neg_u)

stargazer(
  spei_neg_r, spei_neg_u,
  se = list(se_spei_neg_r[, 2], se_spei_neg_u[, 2]),
  p  = list(se_spei_neg_r[, 4], se_spei_neg_u[, 4]),
  title          = "Negative SPEI Shocks and Casual Labour-Force Participation by Sector (PS)",
  dep.var.labels = c("Rural Share", "Urban Share"),
  omit = c("factor\\(STATE\\)",
           "factor\\(year\\)",
           "factor\\(STATE\\):year", "Constant"),
  add.lines = list(
    c("State FE",             "Yes", "Yes"),
    c("Year FE",              "Yes", "Yes"),
    c("State-trends", "Yes", "Yes")
  ),
  column.labels  = c("Rural", "Urban"),
  omit.stat    = c("f", "ser"),
  notes        = "Population weights used.",
  notes.append = FALSE,
  type         = "text",
  label        = "tab:sector_neg_spei"
)


# ══════════════════════════════════════════════════════════════════════════════
# FLOOD — Sector subgroups (optimal spec: contemp. + lag1 + lag2)
# ══════════════════════════════════════════════════════════════════════════════

# ── FI Index ───────────────────────────────────────────────────────────────────
fi_r <- lm(s_casual_w_worker_W_rur_unw ~ FI_state + FI_lag1 + FI_lag2+
             factor(STATE) + factor(year) + factor(STATE):year,
           data = data_flood
           , weights = pop_rur_unw_2017
) 

fi_u <- lm(s_casual_w_worker_W_urb_unw ~  FI_state + FI_lag1 + FI_lag2+
             factor(STATE) + factor(year) + factor(STATE):year,
           data = data_flood
           , weights = pop_urb_unw_2017
) 

se_fi_r <- cluster_se(fi_r)
se_fi_u <- cluster_se(fi_u)

stargazer(
  fi_r, fi_u,
  se = list(se_fi_r[, 2], se_fi_u[, 2]),
  p  = list(se_fi_r[, 4], se_fi_u[, 4]),
  title          = "Flood Index (FI) and Casual Labour-Force Participation by Sector (PS)",
  dep.var.labels = c("Rural Share", "Urban Share"),
  omit = c("factor\\(STATE\\)",
           "factor\\(year\\)",
           "factor\\(STATE\\):year", "Constant"),
  add.lines = list(
    c("State-trends", "Yes", "Yes")),
  column.labels  = c("Rural", "Urban"),
  omit.stat    = c("f", "ser"),
  notes        = " ", 
  notes.append = FALSE,
  type         = "text",
  label        = "tab:sector_fi"
)

# ── PR Index ───────────────────────────────────────────────────────────────────
pr_r <- lm(s_casual_w_worker_PS_rur_unw ~ pr_score + pr_lag1 + pr_lag2 +
             factor(STATE) + factor(year) + factor(STATE):year,
           data = data
           , weights = pop_rur_unw_2017
) 

pr_u <- lm(s_casual_w_worker_PS_urb_unw ~ pr_score + pr_lag1 + pr_lag2 +
             factor(STATE) + factor(year) + factor(STATE):year,
           data = data
           , weights = pop_urb_unw_2017
) 
se_pr_r <- cluster_se(pr_r)
se_pr_u <- cluster_se(pr_u)

stargazer(
  pr_r, pr_u,
  se = list(se_pr_r[, 2], se_pr_u[, 2]),
  p  = list(se_pr_r[, 4], se_pr_u[, 4]),
  title          = "PR Index and Casual Labour-Force Participation by Sector (PS)",
  dep.var.labels = c("Rural Share", "Urban Share"),
  omit = c("factor\\(STATE\\)",
           "factor\\(year\\)",
           "factor\\(STATE\\):year", "Constant"),
  column.labels  = c("Rural", "Urban"),
  omit.stat    = c("f", "ser"),
  notes        = " ", 
  notes.append = FALSE,
  out          = file.path(out, "RURUB_FLOOD_PR.tex"),
  type         = "latex",
  label        = "tab:sector_pr"
)
stargazer(
  pr_r, pr_u,
  se = list(se_pr_r[, 2], se_pr_u[, 2]),
  p  = list(se_pr_r[, 4], se_pr_u[, 4]),
  title          = "PR Index and Casual Labour-Force Participation by Sector (PS)",
  dep.var.labels = c("Rural Share", "Urban Share"),
  omit = c("factor\\(STATE\\)",
           "factor\\(year\\)",
           "factor\\(STATE\\):year", "Constant"),
  column.labels  = c("Rural", "Urban"),
  omit.stat    = c("f", "ser"),
  notes        = " ", 
  notes.append = FALSE,
  type         = "text",
  label        = "tab:sector_pr"
)

# ==============================================================================
# MEGA TABLE
# ==============================================================================

stargazer(
  spei_neg_r, spei_neg_u, fi_r, fi_u, pr_r, pr_u,  
  se             = list(se_spei_neg_r[, 2], se_spei_neg_u[, 2],
                        se_fi_r[, 2],       se_fi_u[, 2],
                        se_pr_r[, 2],       se_pr_u[, 2]),         
  p              = list(se_spei_neg_r[, 4], se_spei_neg_u[, 4],
                        se_fi_r[, 4],       se_fi_u[, 4],
                        se_pr_r[, 4],       se_pr_u[, 4]),         
  title          = "Casual Labour-Force Participation by Settlement",
  dep.var.caption        = "Casual Labour Share (W)",
  dep.var.labels.include = FALSE,
  column.labels  = c("Rural", "Urban", "Rural", "Urban", "Rural", "Urban"),  
  omit           = c("factor\\(STATE\\)", "factor\\(year\\)", "factor\\(STATE\\):year", "Constant"),
  omit.stat      = c("f", "ser"),
  notes          = "Rural and urban population weights from 2017 survey used.",
  notes.append   = FALSE,
  label          = "tab:settlementW",
  type           = "latex"
)
