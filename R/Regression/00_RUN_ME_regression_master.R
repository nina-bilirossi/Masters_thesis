# TO RUN BEFORE ANY REGRESSION FILE
# load necessary libraries and data

library(plm)
library(lmtest)
library(vars)
library(sandwich)
library(stargazer)
library(dplyr)
library(modelsummary)

data <- read.csv("/Users/ninabilirossi/Desktop/MSC THESIS/Data works/Code/Outputs/final material/regression_dataframe_deeper.csv") |> 
  filter(STATE != "LAKSHADWEEP") # |> 
#filter(STATE != "JAMMU AND KASHMIR") |>
#filter(STATE != "LADAKH")

data_flood <- data |> # drop those states since they have missing data for the flood variable
  filter(STATE != "ARUNACHAL PRADESH") |>   # missing FI data
  filter(STATE != "MEGHALAYA")               # missing FI data


colnames(data)

# ── Panel setup ────────────────────────────────────────────────────────────────
pdata <- pdata.frame(data, index = c("STATE", "year"))
pdata_fi <- pdata.frame(data_flood, index = c("STATE", "year"))

# ── SOME SUMMARY STATISTICS (clean this up somewhere else later) ──────────────

data %>%
  dplyr::select(
    "SPEI 12" = spei_spei12, 
    "SPEI 12 Neg" = spei_negative, 
    "SPEI 12 Pos" = spei_positive,
    "Flood index" = FI_state,
    "Extreme precipitation" = pr_score, 
    "Informality share (/lf)" = s_casual_w_lf_PS_unw,
    "Informality share (/workforce)" = s_casual_w_worker_PS_unw,
    "Inf.s (f)" = s_casual_w_worker_PS_f_unw,
    "Inf.s (m)" = s_casual_w_worker_PS_m_unw,
    "Inf.s (rural)" = s_casual_w_worker_PS_rur_unw,
    "Inf.s (urban)" = s_casual_w_worker_PS_urb_unw,
    "State population" = state_pop
  ) %>%
  dplyr::mutate(across(everything(), as.numeric)) %>%
  datasummary_skim(
    fun_numeric = list(
      Count = \(x) sum(!is.na(x)),
      Mean  = \(x) mean(x, na.rm = TRUE),
      SD    = \(x) sd(x, na.rm = TRUE),
      Min   = \(x) min(x, na.rm = TRUE),
      Max   = \(x) max(x, na.rm = TRUE)
    ),
    fmt = 2,
    output = "/Users/ninabilirossi/Desktop/MSC THESIS/Data works/Code/Outputs/latex food/petit_summary_workforce.tex"
  )

# ── Helper: clustered SE at state level ───────────────────────────────────────
cluster_se <- function(model) {
  coeftest(model, vcov = vcovHC(model, type = "HC1", cluster = "group"))
}


# -- MOVING FROM PLM TO LM ----------
# TODO: explicit the factors and chage the data
# # Spec 1a: contemporaneous only
# # OLD STRATEGY
# m1a <- plm(s_casual_w_worker_PS_unw ~ spei_spei12,
#            data   = pdata,
#            model  = "within",
#            effect = "twoways")
# 
# #NEW STRATEGY
# m1a <- lm(s_casual_w_worker_PS_unw ~ spei_spei12 +
#             factor(STATE) + factor(year) + factor(STATE):year,
#           data = data)
# 
# # both yield the same estimate!! The R^2s just mean different things
# 
# summary(m1a)
