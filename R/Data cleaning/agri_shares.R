library(tidyverse)
library(dplyr)


# USING PLFS DATA

plfs_agr_shares <- read_csv("/Users/ninabilirossi/Desktop/MSC THESIS/Data works/My data prep/PLFS processed/PLFS_1718_Agri_Share_State_Both_Weights.csv") |> 
  dplyr::select(STATE = state_name, share_agri_unweighted, share_agri_weighted) |> 
  mutate(STATE = str_to_upper(STATE)) |> 
  filter(STATE != "LAKSHADWEEP")


df_sections <- read.csv("/Users/ninabilirossi/Desktop/MSC THESIS/Data works/Code/Outputs/final material/regression_dataframe_weekly.csv") %>%
  filter(STATE != "LAKSHADWEEP")


# unmatched_states1 <- plfs_agr_shares %>%
#   anti_join(df_sections, by = c("STATE")) %>%
#   distinct(STATE)
# unmatched_states2 <- df_sections %>%
#   anti_join(plfs_agr_shares, by = c("STATE")) %>%
#   distinct(STATE)
# 
# unmatched_states1
# unmatched_states2


plfs_agr_shares <- plfs_agr_shares %>%
  mutate(STATE = dplyr::recode(STATE,
                          "D & N. HAVELI & DAMAN & DIU" = "DADRA & NAGAR HAVELI & DAMAN & DIU",
                          "JAMMU & KASHMIR" = "JAMMU AND KASHMIR",
                          "A & N ISLAND" = "ANDAMAN & NICOBAR",
                          "UTTRAKHAND" = "UTTARAKHAND"
                         ))

df_sections <- df_sections %>%
  left_join(plfs_agr_shares, by = "STATE") |> 
  mutate(agri_share_std = as.numeric(scale(share_agri_weighted)))


# LADHAK is anyway exlcluded because it's tiny.

uts_to_drop <- c(
  "ANDAMAN & NICOBAR",
  "CHANDIGARH",
  "DADRA & NAGAR HAVELI & DAMAN & DIU",
  "DELHI",
  #"JAMMU AND KASHMIR",  
  "LADAKH",
  "PUDUCHERRY"
)

df_sections <- df_sections %>%
  filter(!STATE %in% uts_to_drop)

# --- Agri median ---
med <- median(df_sections$share_agri_weighted, na.rm = TRUE)

df_sections <- df_sections %>%
  mutate(
    agri_group = if_else(
      share_agri_weighted <= med,
      "Low agri",
      "High agri"
    ) |> factor(levels = c("Low agri", "High agri"))
  )
unique(df_sections$STATE[df_sections$agri_group == "Low agri"])
unique(df_sections$STATE[df_sections$agri_group == "High agri"])
unique(df_sections$STATE)

