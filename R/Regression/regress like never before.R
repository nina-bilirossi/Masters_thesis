library(plm)
library(lmtest)
library(vars)
library(sandwich)
library(stargazer)


data <- read.csv("/Users/ninabilirossi/Desktop/MSC THESIS/Data works/Code/Outputs/final material/regression_dataframe.csv") |> 
  filter(STATE != "LAKSHADWEEP")
colnames(data)

# > colnames(data)
#  [1] "STATE"                    "spei_spei12"              "spei_negative"            "spei_positive"            "spei_spei_lag1"          
#  [6] "spei_spei_lag2"           "spei_spei_lag3"           "spei_neg_spei_lag1"       "spei_neg_spei_lag2"       "spei_neg_spei_lag3"      
# [11] "year"                     "FI_state"                 "FI_lag1"                  "FI_lag2"                  "FI_lag3"                 
# [16] "s_casual_w_lf_PS_unw"     "s_casual_w_lf_P_unw"      "s_casual_w_lf_PS_m_unw"   "s_casual_w_lf_P_m_unw"    "s_casual_w_lf_PS_f_unw"  
# [21] "s_casual_w_lf_P_f_unw"    "s_casual_w_lf_PS_rur_unw" "s_casual_w_lf_P_rur_unw"  "s_casual_w_lf_PS_urb_unw" "s_casual_w_lf_P_urb_unw" 
# [26] "data"                     "time"                     "state_pop"   

data_flood <- data |> 
  filter(STATE != "ARUNACHAL PRADESH") |> # drop this state since it has missing data for the flood variable
  filter(STATE != "MEGHALAYA") 

# TEST

pdata <- pdata.frame(data, index = c("STATE", "year"))

model_fe <- plm(s_casual_w_lf_PS_unw ~ spei_negative + spei_neg_spei_lag1,
                data = pdata,
                model = "within")

coeftest(model_fe, vcov. = vcovHC, type = "HC1")

summary(model_fe)

# --- LAG SELECTION ---
# finding the optimal lags for spei and flood index (seprarately)

# Define a function to extract AIC/BIC for plm
# plm doesn't have a default AIC method, so we calculate it from the Likelihood
extract_aic_plm <- function(model) {
  n <- length(residuals(model))
  rss <- sum(residuals(model)^2)
  k <- length(coef(model)) + 1 # +1 for variance
  aic <- n * log(rss/n) + 2 * k
  bic <- n * log(rss/n) + log(n) * k
  return(c(AIC = aic, BIC = bic))
}

optimal_lags_spei <- data.frame(Lags = 1:3, AIC = NA, BIC = NA)
optimal_lags_flood <- data.frame(Lags = 1:3, AIC = NA, BIC = NA)

for (i in 1:3) {
  lags_to_include <- paste0("spei_neg_spei_lag", 1:i)
  formula_str <- paste("s_casual_w_lf_PS_unw ~ spei_negative +", paste(lags_to_include, collapse = " + "))
  
  # Run Fixed Effects (within) model
  fit <- plm(as.formula(formula_str), data = pdata, model = "within")
  
  # Store results
  metrics <- extract_aic_plm(fit)
  optimal_lags_spei$AIC[i] <- metrics["AIC"]
  optimal_lags_spei$BIC[i] <- metrics["BIC"]
}

for (i in 1:3) {
  lags_to_include <- paste0("FI_lag", 1:i)
  formula_str <- paste("s_casual_w_lf_PS_unw ~ FI_state +", paste(lags_to_include, collapse = " + "))
  
  # Run Fixed Effects (within) model
  fit <- plm(as.formula(formula_str), data = pdata, model = "within")
  
  # Store results
  metrics <- extract_aic_plm(fit)
  optimal_lags_flood$AIC[i] <- metrics["AIC"]
  optimal_lags_flood$BIC[i] <- metrics["BIC"]
}

print(optimal_lags_spei)
print(optimal_lags_flood)

# conclusion: 2 lags (or 3 if BIC) for spei (droughts) and 2 for floods; 
# AIC suggests 3 Lags: AIC is more "generous" and tends to favor more complex models to ensure no information is left on the table.
# BIC suggests 2 Lags: BIC penalizes extra parameters more harshly. It is telling you that the jump from 2 to 3 lags doesn't improve the model enough to justify the loss of degrees of freedom.
# makes sense if we think of it in terms of "persistence" for droughts and "recovery time" for floods

# ----- LET'S RUN THE FINAL REGRESSION WITH THE OPTIMAL LAGS -----

# 1. Prepare the list of dependent variables
dep_vars <- c(
  "s_casual_w_lf_PS_unw",      # Total
  "s_casual_w_lf_PS_m_unw",    # Male
  "s_casual_w_lf_PS_f_unw",    # Female
  "s_casual_w_lf_PS_rur_unw",  # Rural
  "s_casual_w_lf_PS_urb_unw"   # Urban
)

# 2. Define the independent variables (Main + 2 Lags)
# Note: I'm using the names from your first message
indep_vars <- c(
  "spei_negative", "spei_neg_spei_lag1", "spei_neg_spei_lag2",
  "FI_state", "FI_lag1", "FI_lag2"
)

indep_vars_spei<- c(
  "spei_negative", "spei_neg_spei_lag1", "spei_neg_spei_lag2")

indep_vars_flood <- c("FI_state") #, "FI_lag1", "FI_lag2")

# 3. Create a list to store the results
model_results <- list()

# 4. Run the loop
for (y in dep_vars) {
  # Construct formula: y ~ spei + lags + flood + lags
  form <- as.formula(paste(y, "~", paste(indep_vars_flood, collapse = " + ")))
  
  # Run Two-Way Fixed Effects (effect = "twoways" includes both State and Year)
  model_results[[y]] <- plm(form, 
                            data = data, 
                            index = c("STATE", "year"), 
                            model = "within", 
                            effect = "twoways"
                            )
}

# 5. Viewing results with Clustered Standard Errors
# In panel data, you MUST cluster SEs by your individual unit (STATE)
lapply(model_results, function(m) {
  coeftest(m, vcovHC(m, type = "HC1", cluster = "group"))
})

# This creates a publication-quality table comparing all 5 groups
stargazer(model_results, 
          type = "text", 
          column.labels = c("Total", "Male", "Female", "Rural", "Urban"),
          title = "Impact of SPEI and Floods on Casual Labor Force Participation",
          omit.stat = c("f", "ser"))
