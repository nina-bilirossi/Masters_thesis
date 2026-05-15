out <- "/Users/ninabilirossi/Desktop/MSC THESIS/Data works/Code/Outputs/latex food"
# dir.create(out, recursive = TRUE, showWarnings = FALSE)



m_desc <- lm(s_casual_w_worker_PS_unw ~ factor(STATE) + factor(year) + STATE:year,
             data = labor_data)

summary(m_desc)

m <- lm(s_casual_w_worker_PS_unw ~ spei_negative + 
            factor(STATE) + factor(year) + factor(STATE):year,
          data = labor_data)
summary(m)
