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

pdata_flood <- pdata.frame(data_flood, index = c("STATE", "year"))


# TEST

pdata <- pdata.frame(data, index = c("STATE", "year"))

model_fe <- plm(s_casual_w_lf_PS_unw ~ spei_negative + spei_neg_spei_lag1,
                data = pdata,
                model = "within")

coeftest(model_fe, vcov. = vcovHC, type = "HC1")

summary(model_fe)


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

indep_vars_flood <- c("FI_state", "FI_lag1", "FI_lag2")

indep_vars_rain <- c("pr_score", "pr_lag1","pr_lag2")

# 3. Create a list to store the results
model_results <- list()

# 4. Run the loop
for (y in dep_vars) {
  # Construct formula: y ~ spei + lags + flood + lags
  form <- as.formula(paste(y, "~", paste(indep_vars_rain, collapse = " + ")))
  
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
          type = "latex", 
          out = "/Users/ninabilirossi/Desktop/MSC THESIS/Data works/Code/Outputs/latex food/pr_regression.tex",
          column.labels = c("Total", "Male", "Female", "Rural", "Urban"),
          title = "Impact of SPEI and Floods on Casual Labor Force Participation",
          omit.stat = c("f", "ser"))

stargazer(model_results,
          type = "latex",
          floating = FALSE,       # <--- THIS IS KEY: Removes \begin{table}
          header = FALSE,         # Removes the "sent by stargazer" comment
          # Rename variables to avoid underscores:
          covariate.labels = c("Precip Score", "Lag 1", "Lag 2"), 
          out = "/Users/ninabilirossi/Desktop/MSC THESIS/Data works/Code/Outputs/latex food/pr_regression2.tex")
