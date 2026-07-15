library(tidyverse)
library(fixest)

df_sections <- read.csv("/Users/ninabilirossi/Desktop/MSC THESIS/Data works/Code/Outputs/final material/regression_dataframe_deeper.csv") %>%
  filter(STATE != "LAKSHADWEEP") # Exclude Lakshadweep due to missing data

agri_share_mospi <- tribble(
  ~STATE,                ~agri_share,
  "Andhra Pradesh",       41.0,
  "Assam",                42.8,
  "Bihar",                48.9,
  "Chhattisgarh",         56.5,
  "Delhi",                 0.2,
  "Gujarat",              41.3,
  "Haryana",              23.6,
  "Himachal Pradesh",     51.6,
  "Jharkhand",            44.3,
  "Karnataka",            38.7,
  "Kerala",               20.0,
  "Madhya Pradesh",       58.8,
  "Maharashtra",          41.7,
  "Odisha",               53.3,
  "Punjab",               23.8,
  "Rajasthan",            55.3,
  "Tamil Nadu",           25.7,
  "Telangana",            40.5,
  "Uttarakhand",          33.4,
  "Uttar Pradesh",        48.8,
  "West Bengal",          37.6,
  "Jammu and Kashmir",      40.4
) |>  mutate(STATE = str_to_upper(STATE))



uts_to_drop <- c(
  "ANDAMAN & NICOBAR",
  "CHANDIGARH",
  "DADRA & NAGAR HAVELI & DAMAN & DIU",
  "DELHI",
  #"JAMMU AND KASHMIR",  
  #"LADAKH",
  "PUDUCHERRY"
)



df_sections <- df_sections %>%
  left_join(agri_share_mospi, by = "STATE") |> 
  mutate(agri_share_std = as.numeric(scale(agri_share)))

# colnames(df_sections)

# --- Agri median ---
df_sections <- df_sections %>%
  mutate(
    agri_median = ntile(agri_share, 2),
    agri_group   = case_when(
      agri_median == 1 ~ "Low agri",
      # agri_tercile == 2 ~ "Middle",
      agri_median == 2 ~ "High agri"
    ) %>% factor(levels = c("Low agri", "High agri")) )

# # Split sample (drop middle)
# m_agri_high <- feols(informality ~ shock | state + year,
#                      data = df_sections %>% filter(agri_tercile == 3), vcov = "HC1")
# m_agri_low  <- feols(informality ~ shock | state + year,
#                      data = df_sections %>% filter(agri_tercile == 1), vcov = "HC1")



# --- Population terciles ---
df_sections <- df_sections %>%
  group_by(year) %>%                        # terciles within year if pop varies
  mutate(
    pop_tercile = ntile(state_pop, 3),
    pop_group   = case_when(
      pop_tercile == 1 ~ "Small",
      pop_tercile == 2 ~ "Medium",
      pop_tercile == 3 ~ "Large"
    ) %>% factor(levels = c("Small", "Medium", "Large")),
    pop_std = as.numeric(scale(state_pop))
  ) %>%
  ungroup()


# --- Regions ---

# Official RBI regions
region_map <- tribble(
  ~STATE,                  ~region,
  # Northern
  "Haryana",               "North",
  "Himachal Pradesh",      "North",
  "Jammu & Kashmir",       "North",
  "Punjab",                "North",
  "Rajasthan",             "North",
  "Uttar Pradesh",         "North",
  "Uttarakhand",           "North",
  # North-Eastern
  "Arunachal Pradesh",     "Northeast",
  "Assam",                 "Northeast",
  "Manipur",               "Northeast",
  "Meghalaya",             "Northeast",
  "Mizoram",               "Northeast",
  "Nagaland",              "Northeast",
  "Sikkim",                "Northeast",
  "Tripura",               "Northeast",
  # Eastern
  "Bihar",                 "East",
  "Jharkhand",             "East",
  "Odisha",                "East",
  "West Bengal",           "East",
  # Central
  "Chhattisgarh",          "Central",
  "Madhya Pradesh",        "Central",
  # Western
  "Goa",                   "West",
  "Gujarat",               "West",
  "Maharashtra",           "West",
  # Southern
  "Andhra Pradesh",        "South",
  "Karnataka",             "South",
  "Kerala",                "South",
  "Tamil Nadu",            "South",
  "Telangana",             "South"
) |>   mutate(STATE = str_to_upper(STATE))


uts_to_drop <- c(
  "ANDAMAN & NICOBAR",
  "CHANDIGARH",
  "DADRA & NAGAR HAVELI & DAMAN & DIU",
  "DELHI",
  #"JAMMU AND KASHMIR",  
  #"LADAKH",
  "PUDUCHERRY"
)

df_sections <- df_sections %>%
  filter(!STATE %in% uts_to_drop)

df_sections <- df_sections %>%
  left_join(region_map, by = "STATE") |> 
  mutate(region = factor(region))   # North as reference by default; relevel if needed

# Check merge
df_sections %>% filter(is.na(region)) %>% distinct(STATE)   # should be empty

# ----- INFORMALITY -----

# Compute baseline informality per state (first observed year)
baseline_informality <- df_sections %>%
  group_by(STATE) %>%
  slice_min(year, n = 1) %>%          # first year per state
  select(STATE, baseline_inf = s_casual_w_worker_PS_unw) %>%
  ungroup()

df_sections <- df_sections %>%
  left_join(baseline_informality, by = "STATE") %>%
  mutate(
    inf_tercile = ntile(baseline_inf, 3),
    inf_group   = case_when(
      inf_tercile == 1 ~ "Low informality",
      inf_tercile == 2 ~ "Middle",
      inf_tercile == 3 ~ "High informality"
    ) %>% factor(levels = c("Low informality", "Middle", "High informality")),
    inf_std = as.numeric(scale(baseline_inf))
  )
