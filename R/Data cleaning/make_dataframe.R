# This file contains the code to aggreagte all the data into one dataframe
# Data list (for each year since 2014): 
# State, flood index, SPEI index, informality rate, informality rate f & m, inf rate r & urb, population, year 

library(dplyr)
library(purrr)
library(stringr)

flood_index <- read.csv("/Users/ninabilirossi/Desktop/MSC THESIS/Data works/Code/Outputs/final material/state_flood_index.csv")
spei_index <- read.csv("/Users/ninabilirossi/Desktop/MSC THESIS/Data works/Code/Outputs/final material/spei_with_lags.csv")
population <- read.csv("/Users/ninabilirossi/Desktop/MSC THESIS/Data works/Code/Outputs/final material/state_population.csv")
#plfs_data <- read.csv('/Users/ninabilirossi/Desktop/MSC THESIS/Data works/Code/Outputs/plfs/new-PLFS_all_s_N_unw.csv')
extreme_events <- read.csv("/Users/ninabilirossi/Desktop/MSC THESIS/Data works/Code/Outputs/python results/combined_states_scores_new.csv") |> 
  filter(custom_year >= 2013)

# plfs_deeper <- read.csv('/Users/ninabilirossi/Desktop/MSC THESIS/Data works/My data prep/PLFS processed/deeper-PLFS_all_s_N_unw.csv')
plfs_weekly <- read.csv('/Users/ninabilirossi/Desktop/MSC THESIS/Data works/My data prep/PLFS processed/weekly-PLFS_all_s_N_unw.csv')
plfs_data <- plfs_weekly

length(unique(flood_index$STATE_UT)) # 35 states/UTs
length(unique(extreme_events$STATE_UT))

# --------- PLFS NAME MATCHING ------------
# clean the plfs names so that it matches the rest
states_plfs <- unique(plfs_data$state_name)
states_plfs
states_spei <- unique(spei_index$state_name)

# figuring out which states do not have a match
unmatched_states1 <- plfs_data %>%
  anti_join(spei_index, by = c("state_name")) %>%
  distinct(state_name)
unmatched_states2 <- spei_index %>%
  anti_join(plfs_data, by = c("state_name")) %>%
  distinct(state_name)

unmatched_states1
unmatched_states2

plfs_data <- plfs_data %>%
  mutate(state_name = case_match(
    state_name,
    "JAMMU & KASHMIR" ~ "JAMMU AND KASHMIR",
    "UTTARANCHAL"     ~ "UTTARAKHAND",
    "ORISSA"          ~ "ODISHA",
    "CHATTISGARH"     ~ "CHHATTISGARH",
    "LAKSHDWEEP"      ~ "LAKSHADWEEP",
    "PONDICHERRY"     ~ "PUDUCHERRY",
    "A & N ISLANDS"   ~ "ANDAMAN & NICOBAR",
    "D & N. HAVELI & DAMAN & DIU" ~ "DADRA & NAGAR HAVELI & DAMAN & DIU",
    .default = state_name # This keeps everything else exactly as it was
  ))

# alles guet

# --- ADDING LAGS TO THE FLOOD INDEX AND PR INDEX ---
flood_index_lags <- flood_index |> 
  arrange(STATE_UT, hydro_year) |> 
  group_by(STATE_UT) |> 
  mutate(
    FI_lag1 = dplyr::lag(FI_state, 1),  # previous year
    FI_lag2 = dplyr::lag(FI_state, 2),  # 2 years ago
    FI_lag3 = dplyr::lag(FI_state, 3)   # 3 years ago
  ) |> 
  ungroup() |> 
  filter(hydro_year >= 2014) |>  # keep only years since 2014 (since PLFS data starts in 2017, and we want to keep 3 lags)
  mutate(STATE_UT = case_match(STATE_UT, "ANDAMAN AND NICOBAR ISLANDS" ~ "ANDAMAN & NICOBAR", .default = STATE_UT))

extreme_events_lags <- extreme_events |> 
  arrange(STATE_UT, custom_year) |> 
  group_by(STATE_UT) |> 
  mutate(
    pr_lag1 = dplyr::lag(weighted_score, 1),  # previous year
    pr_lag2 = dplyr::lag(weighted_score, 2),  # 2 years ago
    pr_lag3 = dplyr::lag(weighted_score, 3)   # 3 years ago
  ) |> 
  rename(pr_score = weighted_score) |>
  ungroup() |> 
  filter(custom_year >= 2014)

# --- GENERAL CLEANING ---

population_clean <- population |> rename(STATE = STATE_UT) |> 
  mutate(STATE = case_match(STATE, "ANDAMAN AND NICOBAR ISLANDS" ~ "ANDAMAN & NICOBAR", .default = STATE))

# for the spei, drop the "type" column (all weighted), and add "spei" to all the columns except for year and state_name
spei_index <- spei_index |> 
  dplyr::select(-type) |> 
  rename_with(~ paste0("spei_", .), -c(state_name, year)) |> 
  rename(STATE = state_name) |> 
  mutate("year_start" = year - 1)

# ---

# --- JOIN JOIN JOIN JOIN ---

spei_clean <- spei_index |> dplyr::select(-year) |> rename(year = year_start)
flood_clean <- flood_index_lags |> rename(year = hydro_year) |> rename(STATE = STATE_UT) |> 
  mutate(STATE = case_match(STATE, "UTTAR>KHAND" ~ "UTTARAKHAND", .default = STATE))
plfs_clean <- plfs_data |> mutate(year = as.numeric(str_extract(time, "\\d{4}"))) |>  rename(STATE = state_name)
xtreme_pr_clean <- extreme_events_lags |> rename(year = custom_year) |> rename(STATE = STATE_UT) |> 
  mutate(STATE = case_match(STATE, "ANDAMAN AND NICOBAR ISLANDS" ~ "ANDAMAN & NICOBAR", .default = STATE))


# List of dataframes to join
data_list <- list(spei_clean, flood_clean, plfs_clean, xtreme_pr_clean)

# Join the first three (assuming they all have state AND year)
final_df <- data_list %>% 
  reduce(left_join, by = c("STATE", "year"))

# Final step: Join the population 
# (Since population often doesn't change yearly in these datasets, 
# we join it by state only if 'year' isn't in pop_clean)
if("year" %in% colnames(population_clean)){
  final_df <- final_df %>% left_join(population_clean, by = c("STATE", "year"))
} else {
  final_df <- final_df %>% left_join(population_clean, by = "STATE")
}

final_df <- final_df |> filter(year >= 2017) # keep only years since 2017, since PLFS data starts in 2017

write.csv(final_df, "/Users/ninabilirossi/Desktop/MSC THESIS/Data works/Code/Outputs/final material/regression_dataframe_weekly.csv", row.names = FALSE)
colnames(final_df)
