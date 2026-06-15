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


library(Hmisc)
library(corrplot)

# ── Variables to correlate ────────────────────────────────────────────────────
vars <- data %>% select(
  "SPEI-12"       = spei_negative,
  "SPEI Lag 1"    = spei_neg_spei_lag1,
  "SPEI Lag 2"    = spei_neg_spei_lag2,
  "Flood Index"   = FI_state,
  "Flood Lag 1"   = FI_lag1,
  #"Flood Lag 2"   = FI_lag2,
  "PR Score"      = pr_score,
  "PR Lag 1"      = pr_lag1#,
  #"PR Lag 2"      = pr_lag2
)

# ── Correlation matrix ────────────────────────────────────────────────────────
cor_matrix <- cor(vars, use = "complete.obs")

# ── Pretty corrplot for visual inspection ────────────────────────────────────
corrplot(
  cor_matrix,
  method   = "color",        # colored cells instead of just numbers
  type     = "lower",        # lower triangle only (cleaner)
  addCoef.col = "black",     # print numbers on top
  tl.col   = "black",        # label color
  tl.srt   = 45,             # rotate labels
  diag     = FALSE,          # hide diagonal
  number.cex = 0.7
)

# ── Latex correlation table for thesis ───────────────────────────────────────
# round and mask upper triangle for readability
cor_table <- round(cor_matrix, 3)
cor_table[upper.tri(cor_table)] <- NA

stargazer(
  cor_table,
  type    = "latex",
  title   = "Correlation Matrix of Key Explanatory Variables",
  label   = "tab:corr",
  out     = file.path(out, "correlation_matrix.tex")
)

# ── VIF check ─────────────────────────────────────────────────────────────────
m_vif <- lm(
  s_casual_w_worker_PS_unw ~
    spei_negative + spei_neg_spei_lag1 + spei_neg_spei_lag2 +
    FI_state + FI_lag1 ,#+ FI_lag2 +
    #pr_score + pr_lag1 + pr_lag2,
  data = data
)
vif(m_vif)


# ── Partial correlation controlling for year fixed effects ───────────────────
library(ppcor)

# Residualize SPEI and Flood Index on year dummies
# (lm with factor(year) absorbs year fixed effects)
resid_spei <- residuals(lm(spei_negative ~ factor(year), data = data, na.action = na.exclude))
resid_fi   <- residuals(lm(FI_state      ~ factor(year), data = data, na.action = na.exclude))

# Raw correlation (already have this, but recompute for direct comparison)
raw_cor <- cor(data$spei_negative, data$FI_state, use = "complete.obs")

# Partial correlation after removing year fixed effects
partial_cor <- cor(resid_spei, resid_fi, use = "complete.obs")

cat(sprintf("Raw correlation  (SPEI-12 vs Flood Index): %.3f\n", raw_cor))
cat(sprintf("Partial corr, controlling for year FE:     %.3f\n", partial_cor))

# ── Significance test for the partial correlation ────────────────────────────
pcor_test_result <- cor.test(resid_spei, resid_fi)
cat(sprintf("p-value: %.4f\n", pcor_test_result$p.value))
