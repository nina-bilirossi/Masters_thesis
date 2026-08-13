
# ==============================================================================
# This script loads the industry shares data for all sectors, 
# harmonizes it to match the `data` dataset, and merges it into `data`. 

# It also generates summary statistics and graphs, including a stacked bar chart
# of industry shares by state and a national baseline pie chart for 2017-18.

# ==============================================================================


industry_data <- read.csv("~/Desktop/MSC THESIS/Data works/My data prep/PLFS processed/industry/PLFS_all_Sector_Shares_State.csv")

# ══════════════════════════════════════════════════════════════════
# SUMMARY STATS & GRAPHS

library(tidyverse)

# ------------------------------------------------------------------
# Map each weighted share column to its industry name (same grouping
# used in the NIC-2008 -> 9-sector mapping from the do-files)
# ------------------------------------------------------------------
industry_labels <- c(
  share_div0_weighted  = "Agriculture, Forestry & Fishing",
  share_div1_weighted  = "Mining & Quarrying",
  share_div23_weighted = "Manufacturing",
  share_div4_weighted  = "Electricity, Gas, Water & Utilities",
  share_div5_weighted  = "Construction",
  share_div6_weighted  = "Trade, Hotels & Restaurants",
  share_div7_weighted  = "Transport, Storage & Communication",
  share_div8_weighted  = "Financing, Insurance, Real Estate & Business Services",
  share_div9_weighted  = "Community, Social & Personal Services"
)

# ------------------------------------------------------------------
# Filter to the 2017-18 baseline round and reshape to long format
# ------------------------------------------------------------------
baseline <- industry_data |> 
  filter(data == "PLFS_1718") |> 
  dplyr::select(state_name, all_of(names(industry_labels))) |> 
  pivot_longer(
    cols      = -state_name,
    names_to  = "industry_code",
    values_to = "share"
  ) |> 
  mutate(industry = factor(industry_labels[industry_code], levels = industry_labels))

# Order states by their Agriculture share, so the chart reads from
# most agrarian (top) to least agrarian (bottom) economies
state_order <- baseline |> 
  filter(industry == "Agriculture, Forestry & Fishing") |> 
  arrange(share) |> 
  pull(state_name)

baseline <- baseline |> 
  mutate(state_name = factor(state_name, levels = state_order))

# ------------------------------------------------------------------
# Stacked bar chart: one bar per state, segments = industry shares
# ------------------------------------------------------------------
ggplot(baseline, aes(x = state_name, y = share, fill = industry)) +
  geom_col(position = "stack", width = 0.75) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0, 0)) +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title    = " ",
    subtitle = "",
    x        = NULL,
    y        = "Share of Workers",
    fill     = "Industry"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE))

ggsave("~/Desktop/MSC THESIS/Data works/My data prep/PLFS processed/industry/baseline_2017_industry_distribution.png",
       width = 9, height = 11, dpi = 300)


# ══════════════════════════════════════════════════════════════════
# DATA PREP


library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(stargazer)
library(ggplot2)
library(scales)

# ══════════════════════════════════════════════════════════════════
# STEP 1: Load industry shares for ALL sectors, harmonize to match `data`
# ══════════════════════════════════════════════════════════════════

industry_data <- read.csv("~/Desktop/MSC THESIS/Data works/My data prep/PLFS processed/industry/PLFS_all_Sector_Shares_State.csv")

# Full descriptive names -> used ONLY for plot titles/legends
industry_labels <- c(
  share_div0_weighted  = "Agriculture, Forestry & Fishing",
  share_div1_weighted  = "Mining & Quarrying",
  share_div23_weighted = "Manufacturing",
  share_div4_weighted  = "Electricity, Gas, Water & Utilities",
  share_div5_weighted  = "Construction",
  share_div6_weighted  = "Trade, Hotels & Restaurants",
  share_div7_weighted  = "Transport, Storage & Communication",
  share_div8_weighted  = "Financing, Insurance, Real Estate & Business Services",
  share_div9_weighted  = "Community, Social & Personal Services"
)

# Short, syntactically-clean names -> used for actual column names
# (no commas/ampersands, so no backticks needed anywhere downstream)
industry_short <- c(
  share_div0_weighted  = "Agriculture",
  share_div1_weighted  = "Mining",
  share_div23_weighted = "Manufacturing",
  share_div4_weighted  = "Utilities",
  share_div5_weighted  = "Construction",
  share_div6_weighted  = "Trade",
  share_div7_weighted  = "Transport",
  share_div8_weighted  = "Finance",
  share_div9_weighted  = "Services"
)

industry_clean <- industry_data |> 
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
    .default = state_name
  )) |> 
  mutate(year = as.numeric(paste0("20", str_sub(str_remove(data, "PLFS_"), 1, 2)))) |> 
  rename(STATE = state_name) |> 
  dplyr::select(STATE, year, all_of(names(industry_short))) |> 
  rename(!!!setNames(names(industry_short), paste0("share_", industry_short)))
# produces columns: share_Agriculture, share_Mining, share_Manufacturing,
# share_Utilities, share_Construction, share_Trade, share_Transport,
# share_Finance, share_Services

sector_cols <- paste0("share_", industry_short)  # all 9 outcome variable names

# ══════════════════════════════════════════════════════════════════
# STEP 2: Merge into `data`
# ══════════════════════════════════════════════════════════════════

data_indu <- data |> 
  left_join(industry_clean, by = c("STATE", "year"))

# Quick check for join failures
data_indu |> 
  filter(!is.na(spei_negative) | !is.na(FI_state)) |> 
  summarise(across(all_of(sector_cols), ~ sum(is.na(.)), .names = "missing_{.col}"))

# ------------------------------------------------------------------
# Lookup: short column suffix -> full descriptive name (for plot labels)
# ------------------------------------------------------------------
short_to_full <- setNames(industry_labels, industry_short)

# ------------------------------------------------------------------
# National baseline (2017-18) pie chart, weighted by state population
# ------------------------------------------------------------------
baseline_national <- data_indu |> 
  filter(data == "PLFS_1718") |> 
  dplyr::select(STATE, state_pop, all_of(sector_cols)) |> 
  pivot_longer(
    cols      = all_of(sector_cols),
    names_to  = "industry_short",
    values_to = "share"
  ) |> 
  mutate(
    industry_short = str_remove(industry_short, "^share_"),
    industry = factor(short_to_full[industry_short], levels = industry_labels)
  ) |> 
  # weighted average share across states, weights = each state's population
  group_by(industry) |> 
  summarise(national_share = weighted.mean(share, w = state_pop, na.rm = TRUE)) |> 
  ungroup() |> 
  mutate(
    national_share = national_share / sum(national_share),  # renormalize to 100%
    label = paste0(industry, "\n", scales::percent(national_share, accuracy = 0.1))
  )

# ------------------------------------------------------------------
# Pie chart
# ------------------------------------------------------------------
ggplot(baseline_national, aes(x = "", y = national_share, fill = industry)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title    = "", #"National Baseline (2017-18) Distribution of Workers Across Industries",
    subtitle = "", #"Population-weighted average across states",
    fill     = "Industry"
  ) +
  theme_void(base_size = 11) +
  theme(
    legend.position = "right",
    legend.title    = element_text(size = 16, face = "bold"),
    legend.text     = element_text(size = 14),
    plot.title      = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle   = element_text(hjust = 0.5)
  )

ggsave("~/Desktop/MSC THESIS/Data works/My data prep/PLFS processed/industry/baseline_2017_national_pie.png",
       width = 8, height = 6, dpi = 300)

