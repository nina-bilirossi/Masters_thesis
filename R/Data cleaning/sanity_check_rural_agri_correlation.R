# compare agri share with urbanization rate (source: https://cdn.ihs.com/www/blog/india_map_and_urbanization_rates.pdf)

library(tibble)
library(dplyr)
library(stringr)
library(ggplot2)
library(patchwork)

# 1. Re-create the original dataframe
urbanization_df <- tribble(
  ~state_ut,                                    ~urbanization_rate_2011,
  "Delhi",                                      97.5,
  "Chandigarh",                                 97.3,
  "Pondicherry",                                68.3,
  "Goa",                                        62.2,
  "Mizoram (NE)",                               51.5,
  "Tamil Nadu",                                 48.5,
  "Kerala",                                     47.7,
  "Maharashtra",                                45.2,
  "Gujarat",                                    42.6,
  "Karnataka",                                  38.6,
  "Punjab",                                     37.5,
  "Andaman and Nicobar Islands",                35.7,
  "Haryana",                                    34.8,
  "Andhra Pradesh",                             33.5,
  "West Bengal",                                31.9,
  "Uttarakhand",                                30.6,
  "Manipur (NE)",                               30.2,
  "Nagaland (NE)",                              29.0,
  "Madhya Pradesh",                             27.6,
  "Jammu and Kashmir",                          27.2,
  "Tripura (NE)",                               26.2,
  "Sikkim (NE)",                                25.0,
  "Rajasthan",                                  24.9,
  "Jharkhand",                                  24.1,
  "Chhattisgarh",                               23.2,
  "Arunachal Pradesh (NE)",                      22.7,
  "Uttar Pradesh",                              22.3,
  "Meghalaya (NE)",                             20.1,
  "Orissa",                                     16.7,
  "Assam (NE)",                                 14.1,
  "Bihar",                                      11.3,
  "Himachal Pradesh",                           10.0,
  "Telangana (part of Andhra Pradesh at time)", NA
)

# 2. Clean, filter, and rename to match your target list
cleaned_states_df <- urbanization_df %>%
  # Clean up the names dynamically: remove " (NE)" or trailing parentheticals, and uppercase
  mutate(state_ut = str_replace(state_ut, "\\s*\\(NE\\)", "")) %>%
  mutate(state_ut = ifelse(str_detect(state_ut, "Telangana"), "TELANGANA", toupper(state_ut))) %>%
  mutate(state_ut = ifelse(state_ut == "ORISSA", "ODISHA", state_ut)) %>%
  
  # Filter to keep ONLY the 29 states present in your target vector
  filter(state_ut %in% c(
    "ANDHRA PRADESH", "ARUNACHAL PRADESH", "ASSAM", "BIHAR", "CHHATTISGARH", 
    "GOA", "GUJARAT", "HARYANA", "HIMACHAL PRADESH", "JAMMU AND KASHMIR", 
    "JHARKHAND", "KARNATAKA", "KERALA", "MADHYA PRADESH", "MAHARASHTRA", 
    "MANIPUR", "MEGHALAYA", "MIZORAM", "NAGALAND", "ODISHA", "PUNJAB", 
    "RAJASTHAN", "SIKKIM", "TAMIL NADU", "TELANGANA", "TRIPURA", 
    "UTTAR PRADESH", "UTTARAKHAND", "WEST BENGAL"
  )) %>%
  rename(STATE = state_ut)

# View the cleaned dataframe
print(cleaned_states_df, n = Inf)


# 1. Join your PLFS data with the cleaned urbanization data
# (Replacing 'cleaned_states_df' with whatever you named the previous tibble)
merged_data <- cleaned_states_df %>%
  inner_join(plfs_agr_shares, by = "STATE") %>%
  # Drop rows with NA in either column so correlation calculates properly
  filter(!is.na(urbanization_rate_2011), !is.na(share_agri_weighted))

# Add population
state_populations <- data %>%
  distinct(STATE, state_pop)
merged_data <- merged_data %>%
  left_join(state_populations, by = "STATE")


# 
# # 2. Calculate the Correlation Score (Pearson's r)
# # Note: Economic theory suggests a strong negative correlation here 
# # (more urbanized = lower share of employment in agriculture)
# correlation_score1 <- cor(
#   merged_data$urbanization_rate_2011, 
#   merged_data$share_agri_weighted, 
#   method = "pearson"
# )
# 
# cat("Pearson Correlation Coefficient:", round(correlation_score, 3), "\n")
# 
# # 3. Quick plot to visualize the trend
# ggplot(merged_data, aes(x = urbanization_rate_2011, y = share_agri_weighted)) +
#   geom_point(color = "darkblue", size = 3) +
#   geom_smooth(method = "lm", color = "red", se = FALSE, linetype = "dashed") +
#   geom_text(aes(label = STATE), vjust = -0.5, hjust = 0.5, size = 2.5, check_overlap = TRUE) +
#   labs(
#     title = "Urbanization Rate vs. Agricultural Employment Share",
#     subtitle = paste0("Correlation Score: ", round(correlation_score1, 3)),
#     x = "Urbanization Rate (2011 Census %)",
#     y = "Agricultural Employment Share (PLFS %)"
#   ) +
#   theme_minimal()
# 
# correlation_score2 <- cor(
#   merged_data$urbanization_rate_2011, 
#   merged_data$state_pop, 
#   method = "pearson"
# )
# 
# ggplot(merged_data, aes(x = urbanization_rate_2011, y = state_pop)) +
#   geom_point(color = "yellow", size = 3) +
#   geom_smooth(method = "lm", color = "red", se = FALSE, linetype = "dashed") +
#   geom_text(aes(label = STATE), vjust = -0.5, hjust = 0.5, size = 2.5, check_overlap = TRUE) +
#   labs(
#     title = "Urbanization Rate vs. Agricultural Employment Share",
#     subtitle = paste0("Correlation Score: ", round(correlation_score2, 3)),
#     x = "Urbanization Rate (2011 Census %)",
#     y = "State population"
#   ) +
#   theme_minimal()
# 
# correlation_score3 <- cor(
#   merged_data$state_pop, 
#   merged_data$share_agri_weighted, 
#   method = "pearson"
# )
# 
# ggplot(merged_data, aes(x = state_pop, y = share_agri_weighted)) +
#   geom_point(color = "forestgreen", size = 3) +
#   geom_smooth(method = "lm", color = "red", se = FALSE, linetype = "dashed") +
#   geom_text(aes(label = STATE), vjust = -0.5, hjust = 0.5, size = 2.5, check_overlap = TRUE) +
#   labs(
#     title = "Urbanization Rate vs. Agricultural Employment Share",
#     subtitle = paste0("Correlation Score: ", round(correlation_score3, 3)),
#     x = "State population",
#     y = "Agricultural Employment Share (PLFS %)"
#   ) +
#   theme_minimal()


# --- Compute Correlation Scores ---
correlation_score1 <- cor(merged_data$urbanization_rate_2011, merged_data$share_agri_weighted, method = "pearson")
correlation_score2 <- cor(merged_data$urbanization_rate_2011, merged_data$state_pop, method = "pearson")
correlation_score3 <- cor(merged_data$state_pop, merged_data$share_agri_weighted, method = "pearson")


# --- Plot 1: Urbanization vs Agri Share ---
p1 <- ggplot(merged_data, aes(x = urbanization_rate_2011, y = share_agri_weighted)) +
  geom_point(color = "darkblue", size = 3) +
  geom_smooth(method = "lm", color = "red", se = FALSE, linetype = "dashed") +
  geom_text(aes(label = STATE), vjust = -0.5, hjust = 0.5, size = 2.5, check_overlap = TRUE) +
  labs(
    title = "Urbanization vs. Agri Share",
    subtitle = paste0("Correlation: ", round(correlation_score1, 3)),
    x = "Urbanization Rate (2011 Census)",
    y = "Agri Employment Share (PLFS)"
  ) +
  theme_minimal()


# --- Plot 2: Urbanization vs Population ---
p2 <- ggplot(merged_data, aes(x = urbanization_rate_2011, y = state_pop)) +
  geom_point(color = "orange", size = 3) + # Changed yellow to orange for better visibility on white backgrounds
  geom_smooth(method = "lm", color = "red", se = FALSE, linetype = "dashed") +
  geom_text(aes(label = STATE), vjust = -0.5, hjust = 0.5, size = 2.5, check_overlap = TRUE) +
  labs(
    title = "Urbanization vs. Population",
    subtitle = paste0("Correlation: ", round(correlation_score2, 3)),
    x = "Urbanization Rate (2011 Census)",
    y = "State Population"
  ) +
  theme_minimal()


# --- Plot 3: Population vs Agri Share ---
p3 <- ggplot(merged_data, aes(x = state_pop, y = share_agri_weighted)) +
  geom_point(color = "forestgreen", size = 3) +
  geom_smooth(method = "lm", color = "red", se = FALSE, linetype = "dashed") +
  geom_text(aes(label = STATE), vjust = -0.5, hjust = 0.5, size = 2.5, check_overlap = TRUE) +
  labs(
    title = "Population vs. Agri Share",
    subtitle = paste0("Correlation: ", round(correlation_score3, 3)),
    x = "State Population",
    y = "Agri Employment Share (PLFS)"
  ) +
  theme_minimal()


# --- Combine into a single display with Patchwork ---
# This layout puts p1 and p2 side-by-side on top, and stretches p3 across the bottom row.
combined_plot <- p1 / p2 / p3

# Alternatively, if you just want them all side-by-side in one row:
# combined_plot <- p1 + p2 + p3

# Display the combined layout
combined_plot

