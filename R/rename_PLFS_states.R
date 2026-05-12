library(stringr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(patchwork)

plfs <- read.csv("~/Desktop/MSC THESIS/Data works/Code/Data/PLFS/NSS_PLFS_all_s_unw.csv") 
plfs$year <- as.numeric(str_sub(plfs$time, -4))
plfs <- plfs |>  filter(year > 2016)

states_plfs <- unique(plfs$state_name)
states_plfs
states_spei <- unique(spei_long$state_name)

# figuring out which states do not have a match
unmatched_states1 <- plfs %>%
  anti_join(spei_long, by = c("state_name")) %>%
  distinct(state_name)
unmatched_states2 <- spei_long %>%
  anti_join(plfs, by = c("state_name")) %>%
  distinct(state_name)

unmatched_states1
unmatched_states2

plfs <- plfs %>%
  mutate(state_name = recode(state_name,
                             #"JAMMU AND KASHMIR" = "JAMMU & KASHMIR",
                             "UTTARANCHAL" = "UTTARAKHAND",
                             #"DEHLI" = "DELHI",
                             "ORISSA" = "ODISHA",
                             "CHATTISGARH" = "CHHATTISGARH",
                             "LAKSHDWEEP" = "LAKSHADWEEP",
                             "PONDICHERRY"= "PUDUCHERRY",
                              "A & N ISLANDS" = "ANDAMAN & NICOBAR",
                             #"ANDAMAN AND NICOBAR" = "ANDAMAN & NICOBAR", # why is it there twice?
                             "D & N. HAVELI & DAMAN & DIU" = "DADRA & NAGAR HAVELI & DAMAN & DIU",
                             #"DADRA & NAGAR HAVELI" = "DADRA & NAGAR HAVELI & DAMAN & DIU",
                             #"DAMAN & DIU" = "DADRA & NAGAR HAVELI & DAMAN & DIU"
                             ))


