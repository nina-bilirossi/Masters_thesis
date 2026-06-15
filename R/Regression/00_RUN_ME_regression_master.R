# TO RUN BEFORE ANY REGRESSION FILE
# load necessary libraries and data

library(plm)
library(lmtest)
library(vars)
library(sandwich)
library(stargazer)
library(dplyr)
library(modelsummary)
library(tibble)
library(Hmisc)

# data <- read.csv("/Users/ninabilirossi/Desktop/MSC THESIS/Data works/Code/Outputs/final material/regression_dataframe_deeper.csv") |>
#   filter(STATE != "LAKSHADWEEP") # |>
# #filter(STATE != "JAMMU AND KASHMIR") |>
# #filter(STATE != "LADAKH")

data <- read.csv("/Users/ninabilirossi/Desktop/MSC THESIS/Data works/Code/Outputs/final material/regression_dataframe_weekly.csv") |>
 filter(STATE != "LAKSHADWEEP")

uts_to_drop <- c(
  "ANDAMAN & NICOBAR",
  #"CHANDIGARH",
  "DADRA & NAGAR HAVELI & DAMAN & DIU",
  #"DELHI",
  #"JAMMU AND KASHMIR",  
  #"LADAKH",
  "PUDUCHERRY"
)

data_no_UT <- data %>%
  filter(!STATE %in% uts_to_drop)

data_flood <- data |> # drop those states since they have missing data for the flood variable
  filter(STATE != "ARUNACHAL PRADESH") |>   # missing FI data
  filter(STATE != "MEGHALAYA")               # missing FI data

data_flood_no_UT <- data_flood %>%
  filter(!STATE %in% uts_to_drop)

# colnames(data)

# ── Panel setup ────────────────────────────────────────────────────────────────
pdata <- pdata.frame(data_no_UT, index = c("STATE", "year"))
pdata_fi <- pdata.frame(data_flood_no_UT, index = c("STATE", "year"))

# ── SOME SUMMARY STATISTICS (clean this up somewhere else later) ──────────────

data %>%
  filter(year == 2017) |>
  dplyr::select(
    "SPEI 12" = spei_spei12, 
    "SPEI 12 Neg" = spei_negative, 
    "SPEI 12 Pos" = spei_positive,
    "Flood index" = FI_state,
    "Extreme precipitation" = pr_score, 
    "Informality share (/lf)" = s_casual_w_lf_PS_unw,
    "Informality share (/workforce) PS" = s_casual_w_worker_PS_unw,
    "Informality share (/workforce) P" = s_casual_w_worker_P_unw,
    "Informality share (/workforce) W" = s_casual_w_worker_W_unw,
    "Informality share (/workforce) WD" = s_casual_w_worker_WD_unw,
    # "Inf.s (f)" = s_casual_w_worker_PS_f_unw,
    # "Inf.s (m)" = s_casual_w_worker_PS_m_unw,
    # "Inf.s (rural)" = s_casual_w_worker_PS_rur_unw,
    # "Inf.s (urban)" = s_casual_w_worker_PS_urb_unw,
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
    fmt = 2
    #output = "/Users/ninabilirossi/Desktop/MSC THESIS/Data works/Code/Outputs/latex food/petit_summary_workforce_weekly.tex"
  ) |> print()

# ── Helper: clustered SE at state level ───────────────────────────────────────
cluster_se <- function(model) {
  coeftest(model, vcov = vcovHC(model, type = "HC1", cluster = "group"))
}


#----------------------------



# renamed dataset
df_sum <- data %>%
  filter(year == 2017) |> 
  dplyr::select(
    "SPEI 12" = spei_spei12, 
    "SPEI 12 Neg" = spei_negative, 
    "SPEI 12 Pos" = spei_positive,
    "Flood index" = FI_state,
    "Extreme precipitation" = pr_score, 
    "Informality share (/lf)" = s_casual_w_lf_PS_unw,
    "Informality share (/workforce) PS" = s_casual_w_worker_PS_unw,
    "Informality share (/workforce) P" = s_casual_w_worker_P_unw,
    "Informality share (/workforce) W" = s_casual_w_worker_W_unw,
    "Informality share (/workforce) WD" = s_casual_w_worker_WD_unw,
    "State population" = state_pop
  ) %>%
  mutate(across(everything(), as.numeric))

# variables using population weights
weighted_vars <- c(
  "Informality share (/lf)",
  "Informality share (/workforce) PS",
  "Informality share (/workforce) P",
  "Informality share (/workforce) W",
  "Informality share (/workforce) WD"
)

# build summary table manually
summary_table <- lapply(names(df_sum)[names(df_sum) != "State population"], function(v) {
  
  x <- df_sum[[v]]
  w <- df_sum$`State population`
  
  if (v %in% weighted_vars) {
    
    mean_val <- weighted.mean(x, w, na.rm = TRUE)
    
    sd_val <- sqrt(
      Hmisc::wtd.var(x, weights = w, na.rm = TRUE)
    )
    
  } else {
    
    mean_val <- mean(x, na.rm = TRUE)
    sd_val <- sd(x, na.rm = TRUE)
  }
  
  tibble(
    Variable = v,
    Count = sum(!is.na(x)),
    Weighted_mean = mean_val,
    SD = sd_val,
    Min = min(x, na.rm = TRUE),
    Max = max(x, na.rm = TRUE)
  )
  
}) %>%
  bind_rows()

# print nicely
datasummary_df(
  summary_table,
  fmt = 2
)

