# THIS IS ESSENTIALLY A DRAFT

library(Hmisc)
library(corrplot)
library(car)

out <- "/Users/ninabilirossi/Desktop/MSC THESIS/Data works/Code/Outputs/regressions"
# dir.create(out, recursive = TRUE, showWarnings = FALSE)

m_desc <- lm(s_casual_w_worker_PS_unw ~ factor(STATE) + factor(year) + STATE:year,
             data = data)

summary(m_desc)

m <- lm(s_casual_w_worker_PS_unw ~ spei_negative + FI_state +
            factor(STATE) + factor(year) + factor(STATE):year,
          data = data)
summary(m)


vars <- data_flood[, c(
  "spei_negative",
  "spei_neg_spei_lag1",
  "spei_neg_spei_lag2",
  "FI_state",
  "FI_lag1",
  "FI_lag2"
)]

cor(vars, use = "complete.obs")


rcorr(as.matrix(vars))

stargazer(
  vars,
  type = "latex",
  title = "Correlation Matrix",
  out = file.path(out, "correlation_matrix.tex")
)


corrplot(
  cor(vars, use = "complete.obs"),
  method = "number"
)

m_vif <- lm(
  s_casual_w_worker_PS_unw ~ 
    spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2 +
    FI_state + FI_lag1 + FI_lag2,
  data = data_flood
)

vif(m_vif)
