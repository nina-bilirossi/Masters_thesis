library(plm)
library(lmtest)
library(vars)
library(sandwich)
library(stargazer)

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

optimal_lags_spei <- data.frame(Lags = 1:4, AIC = NA, BIC = NA)
optimal_lags_flood <- data.frame(Lags = 1:4, AIC = NA, BIC = NA)
optimal_lags_pr <- data.frame(Lags = 1:4, AIC = NA, BIC = NA)


for (i in 1:3) {
  lags_to_include <- paste0("spei_neg_spei_lag", 1:i)
  formula_str <- paste("s_casual_w_worker_W_unw ~ ", paste(lags_to_include, collapse = " + "))
  
  # Run Fixed Effects (within) model
  fit <- plm(as.formula(formula_str), data = pdata, model = "within")
  
  # Store results
  metrics <- extract_aic_plm(fit)
  optimal_lags_spei$AIC[i] <- metrics["AIC"]
  optimal_lags_spei$BIC[i] <- metrics["BIC"]
}
#test with current year to compare
formula_str <- "s_casual_w_worker_W_unw ~ spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2 + spei_neg_spei_lag3"
fit <- plm(as.formula(formula_str), data = pdata, model = "within")
metrics <- extract_aic_plm(fit)
optimal_lags_spei$AIC[4] <- metrics["AIC"]
optimal_lags_spei$BIC[4] <- metrics["BIC"]

for (i in 1:3) {
  lags_to_include <- paste0("FI_lag", 1:i)
  formula_str <- paste("s_casual_w_worker_W_unw ~ ", paste(lags_to_include, collapse = " + "))
  
  # Run Fixed Effects (within) model
  fit <- plm(as.formula(formula_str), data = pdata_fi, model = "within")
  
  # Store results
  metrics <- extract_aic_plm(fit)
  optimal_lags_flood$AIC[i] <- metrics["AIC"]
  optimal_lags_flood$BIC[i] <- metrics["BIC"]
}
#test with current year to compare
formula_str <- "s_casual_w_worker_W_unw ~ FI_state + FI_lag1 + FI_lag2"
fit <- plm(as.formula(formula_str), data = pdata, model = "within")
metrics <- extract_aic_plm(fit)
optimal_lags_flood$AIC[4] <- metrics["AIC"]
optimal_lags_flood$BIC[4] <- metrics["BIC"]

for (i in 1:3) {
  lags_to_include <- paste0("pr_lag", 1:i)
  formula_str <- paste("s_casual_w_worker_W_unw ~ ", paste(lags_to_include, collapse = " + "))
  
  # Run Fixed Effects (within) model
  fit <- plm(as.formula(formula_str), data = pdata, model = "within")
  
  # Store results
  metrics <- extract_aic_plm(fit)
  optimal_lags_pr$AIC[i] <- metrics["AIC"]
  optimal_lags_pr$BIC[i] <- metrics["BIC"]
}
#test with current year to compare
formula_str <- "s_casual_w_worker_W_unw ~ pr_score + pr_lag1"
fit <- plm(as.formula(formula_str), data = pdata, model = "within")
metrics <- extract_aic_plm(fit)
optimal_lags_pr$AIC[4] <- metrics["AIC"]
optimal_lags_pr$BIC[4] <- metrics["BIC"]

print(optimal_lags_spei)
print(optimal_lags_flood)
print(optimal_lags_pr)

# conclusion: 2 lags (or 3 if BIC) for spei (droughts) and 2 for floods; 
# AIC suggests 3 Lags: AIC is more "generous" and tends to favor more complex models to ensure no information is left on the table.
# BIC suggests 2 Lags: BIC penalizes extra parameters more harshly. It is telling you that the jump from 2 to 3 lags doesn't improve the model enough to justify the loss of degrees of freedom.
# makes sense if we think of it in terms of "persistence" for droughts and "recovery time" for floods