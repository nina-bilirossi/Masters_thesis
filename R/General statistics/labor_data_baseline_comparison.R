library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# ── SOME SUMMARY STATISTICS ──────────────

plfs_deep <- read.csv("/Users/ninabilirossi/Desktop/MSC THESIS/Data works/Code/Outputs/final material/regression_dataframe_deeper.csv") |> filter(STATE != "LAKSHADWEEP")

labor_data <- plfs_deep

# CREATE UNEMPLOYMENT VARIABLES

labor_data$unemp <- (labor_data$labor_force_PS_unw - labor_data$worker_PS_unw) / labor_data$labor_force_PS_unw

# Group-specific unemployment rates
groups <- c("f", "m", "rur", "urb")

for (g in groups) {
  lf_col     <- paste0("labor_force_PS_", g, "_unw")
  worker_col <- paste0("worker_PS_", g, "_unw")
  new_col    <- paste0("unemp_", g)
  labor_data[[new_col]] <- (labor_data[[lf_col]] - labor_data[[worker_col]]) / labor_data[[lf_col]]
}

share_vars <- names(plfs_deep)[grepl("^(lf_share|worker_share|unemp)", names(plfs_deep))]
my_vars    <- c(share_vars, "pop_tot_unw", "pop_workingage_unw")

labor_data_flood <- labor_data |>
  filter(STATE != "ARUNACHAL PRADESH") |>
  filter(STATE != "MEGHALAYA")

# ==============================================================================
# SUMMARY STATS 
# ==============================================================================

# 1. Filter data for 2017
# (Assuming your dataset has a 'year' or 'YEAR' column)
data_2017 <- labor_data |> 
  filter(year == 2017)

# Helper function for weighted mean
w_mean <- function(val, weight) {
  sum(val * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE)
}

# 2. Compute Baseline Statistics
# We'll calculate them for the General population first, as an example.
# You can duplicate this block for 'm', 'f', 'rur', and 'urb' by swapping the variables 
# (e.g., worker_share_f, pop_workingage_f_unw).

# 1. GENERAL
baseline_stats_general <- data_2017 |>
  summarise(
    Group = "General",
    lfpr_mean = w_mean(lf_share_unw, pop_workingage_unw),
    lfpr_min  = min(lf_share_unw, na.rm = TRUE),
    lfpr_max  = max(lf_share_unw, na.rm = TRUE),
    
    emp_mean  = w_mean(worker_share_unw, pop_workingage_unw),
    emp_min   = min(worker_share_unw, na.rm = TRUE),
    emp_max   = max(worker_share_unw, na.rm = TRUE),
    
    unemp_mean = w_mean(unemp, labor_force_PS_unw),
    unemp_min  = min(unemp, na.rm = TRUE),
    unemp_max  = max(unemp, na.rm = TRUE),
    
    casual_mean = w_mean(s_casual_w_worker_PS_unw, worker_PS_unw),
    casual_min  = min(s_casual_w_worker_PS_unw, na.rm = TRUE),
    casual_max  = max(s_casual_w_worker_PS_unw, na.rm = TRUE)
  )

# 2. FEMALE (Fixed variables to use _f)
baseline_stats_f <- data_2017 |>
  summarise(
    Group = "Female",
    lfpr_mean = w_mean(lf_share_f_unw, pop_f_unw),
    lfpr_min  = min(lf_share_f_unw, na.rm = TRUE),
    lfpr_max  = max(lf_share_f_unw, na.rm = TRUE),
    
    emp_mean  = w_mean(worker_share_f_unw, pop_f_unw), # Fixed
    emp_min   = min(worker_share_f_unw, na.rm = TRUE),
    emp_max   = max(worker_share_f_unw, na.rm = TRUE),
    
    unemp_mean = w_mean(unemp_f, labor_force_PS_f_unw), # Fixed
    unemp_min  = min(unemp_f, na.rm = TRUE),
    unemp_max  = max(unemp_f, na.rm = TRUE),
    
    casual_mean = w_mean(s_casual_w_worker_PS_f_unw, worker_PS_f_unw),
    casual_min  = min(s_casual_w_worker_PS_f_unw, na.rm = TRUE),
    casual_max  = max(s_casual_w_worker_PS_f_unw, na.rm = TRUE)
  )

# 3. MALE (Fixed variables to use _m)
baseline_stats_m <- data_2017 |>
  summarise(
    Group = "Male",
    lfpr_mean = w_mean(lf_share_m_unw, pop_m_unw),
    lfpr_min  = min(lf_share_m_unw, na.rm = TRUE),
    lfpr_max  = max(lf_share_m_unw, na.rm = TRUE),
    
    emp_mean  = w_mean(worker_share_m_unw, pop_m_unw), # Fixed
    emp_min   = min(worker_share_m_unw, na.rm = TRUE),
    emp_max   = max(worker_share_m_unw, na.rm = TRUE),
    
    unemp_mean = w_mean(unemp_m, labor_force_PS_m_unw), # Fixed
    unemp_min  = min(unemp_m, na.rm = TRUE),
    unemp_max  = max(unemp_m, na.rm = TRUE),
    
    casual_mean = w_mean(s_casual_w_worker_PS_m_unw, worker_PS_m_unw),
    casual_min  = min(s_casual_w_worker_PS_m_unw, na.rm = TRUE),
    casual_max  = max(s_casual_w_worker_PS_m_unw, na.rm = TRUE)
  )

#   VIZ SUMMARY STATS

# 1. Combine all groups into one dataframe
combined_stats <- bind_rows(baseline_stats_general, baseline_stats_f, baseline_stats_m)

# 2. Reshape from wide to long format
plot_data <- combined_stats |>
  pivot_longer(
    cols = -Group,
    names_to = c("Metric", "Stat"),
    names_sep = "_"
  ) |>
  pivot_wider(
    names_from = Stat,
    values_from = value
  ) |>
  mutate(
    # Give the metrics clean, readable labels
    Metric = case_when(
      Metric == "lfpr"   ~ "Labor Force Participation",
      Metric == "emp"    ~ "Employment Share",
      Metric == "unemp"  ~ "Unemployment Rate",
      Metric == "casual" ~ "Casual Worker Share",
      TRUE ~ Metric
    ),
    Group = factor(Group, levels = c("General", "Male", "Female"))
  )

# 3. Create the Dot & Whisker Plot
ggplot(plot_data, aes(x = Metric, y = mean, color = Group, group = Group)) +
  # Draw the min/max range bars (with a slight dodge so they don't overlap)
  geom_errorbar(aes(ymin = min, ymax = max), 
                position = position_dodge(width = 0.5), 
                width = 0.2, linewidth = 0.8) +
  # Draw the weighted mean point
  geom_point(position = position_dodge(width = 0.5), size = 3.5) +
  
  # Styling
  theme_minimal(base_size = 12) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = c("General" = "#2c3e50", "Male" = "#2980b9", "Female" = "#e74c3c")) +
  labs(
    #title = "Baseline Labor Market Statistics (India, 2017)",
    #subtitle = "Points represent population-weighted national means; bars show the full Min-Max range across States",
    x = "",
    y = "Share (%)",
    color = "Demographic Group"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(face = "bold", size = 11),
    panel.grid.major.x = element_blank(), # Removes vertical grid lines for clarity
    legend.position = "bottom"
  )

# ==============================================================================
# RURAL URBAN
# ==============================================================================

# 1. GENERAL
baseline_stats_general <- data_2017 |>
  summarise(
    Sector = "General",
    lfpr_mean = w_mean(lf_share_unw, pop_workingage_unw),
    lfpr_min  = min(lf_share_unw, na.rm = TRUE),
    lfpr_max  = max(lf_share_unw, na.rm = TRUE),
    
    emp_mean  = w_mean(worker_share_unw, pop_workingage_unw),
    emp_min   = min(worker_share_unw, na.rm = TRUE),
    emp_max   = max(worker_share_unw, na.rm = TRUE),
    
    unemp_mean = w_mean(unemp, labor_force_PS_unw),
    unemp_min  = min(unemp, na.rm = TRUE),
    unemp_max  = max(unemp, na.rm = TRUE),
    
    casual_mean = w_mean(s_casual_w_worker_PS_unw, worker_PS_unw),
    casual_min  = min(s_casual_w_worker_PS_unw, na.rm = TRUE),
    casual_max  = max(s_casual_w_worker_PS_unw, na.rm = TRUE)
  )

# 2. RURAL (Fixed variables to use _rur)
# Note: If your population column is named 'pop_workingage_rur_unw', swap 'pop_rur_unw' out for it.
baseline_stats_rur <- data_2017 |>
  summarise(
    Sector = "Rural",
    lfpr_mean = w_mean(lf_share_rur_unw, pop_rur_unw),
    lfpr_min  = min(lf_share_rur_unw, na.rm = TRUE),
    lfpr_max  = max(lf_share_rur_unw, na.rm = TRUE),
    
    emp_mean  = w_mean(worker_share_rur_unw, pop_rur_unw),
    emp_min   = min(worker_share_rur_unw, na.rm = TRUE),
    emp_max   = max(worker_share_rur_unw, na.rm = TRUE),
    
    unemp_mean = w_mean(unemp_rur, labor_force_PS_rur_unw),
    unemp_min  = min(unemp_rur, na.rm = TRUE),
    unemp_max  = max(unemp_rur, na.rm = TRUE),
    
    casual_mean = w_mean(s_casual_w_worker_PS_rur_unw, worker_PS_rur_unw),
    casual_min  = min(s_casual_w_worker_PS_rur_unw, na.rm = TRUE),
    casual_max  = max(s_casual_w_worker_PS_rur_unw, na.rm = TRUE)
  )

# 3. URBAN (Fixed variables to use _urb)
baseline_stats_urb <- data_2017 |>
  summarise(
    Sector = "Urban",
    lfpr_mean = w_mean(lf_share_urb_unw, pop_urb_unw),
    lfpr_min  = min(lf_share_urb_unw, na.rm = TRUE),
    lfpr_max  = max(lf_share_urb_unw, na.rm = TRUE),
    
    emp_mean  = w_mean(worker_share_urb_unw, pop_urb_unw),
    emp_min   = min(worker_share_urb_unw, na.rm = TRUE),
    emp_max   = max(worker_share_urb_unw, na.rm = TRUE),
    
    unemp_mean = w_mean(unemp_urb, labor_force_PS_urb_unw),
    unemp_min  = min(unemp_urb, na.rm = TRUE),
    unemp_max  = max(unemp_urb, na.rm = TRUE),
    
    casual_mean = w_mean(s_casual_w_worker_PS_urb_unw, worker_PS_urb_unw),
    casual_min  = min(s_casual_w_worker_PS_urb_unw, na.rm = TRUE),
    casual_max  = max(s_casual_w_worker_PS_urb_unw, na.rm = TRUE)
  )


# 1. Combine all sectors into one dataframe
combined_sectors <- bind_rows(baseline_stats_general, baseline_stats_rur, baseline_stats_urb)

# 2. Reshape from wide to long format
plot_sector_data <- combined_sectors |>
  pivot_longer(
    cols = -Sector,
    names_to = c("Metric", "Stat"),
    names_sep = "_"
  ) |>
  pivot_wider(
    names_from = Stat,
    values_from = value
  ) |>
  mutate(
    # Clean labels for the X-axis
    Metric = case_when(
      Metric == "lfpr"   ~ "Labor Force Participation",
      Metric == "emp"    ~ "Employment Share",
      Metric == "unemp"  ~ "Unemployment Rate",
      Metric == "casual" ~ "Casual Worker Share",
      TRUE ~ Metric
    ),
    Sector = factor(Sector, levels = c("General", "Urban", "Rural"))
  )

# 3. Plotting the Dot & Whisker
ggplot(plot_sector_data, aes(x = Metric, y = mean, color = Sector, group = Sector)) +
  # Range Bars
  geom_errorbar(aes(ymin = min, ymax = max), 
                position = position_dodge(width = 0.5), 
                width = 0.2, linewidth = 0.8) +
  # Weighted Means
  geom_point(position = position_dodge(width = 0.5), size = 3.5) +
  
  # Color palette setup (Green for Rural, Blue for Urban, Dark Gray for General)
  scale_color_manual(values = c(
    "General" = "#2c3e50", 
    "Urban"   = "#2980b9",  # Clear Blue
    "Rural"   = "#27ae60"   # Clean Green
  )) +
  
  # Formatting & Styling
  theme_minimal(base_size = 12) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    #title = "Baseline Labor Market Statistics by Sector (India, 2017)",
    #subtitle = "Points represent population-weighted national means; bars show the full Min-Max range across States",
    x = "",
    y = "Share (%)",
    color = "Sector"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(face = "bold", size = 11),
    panel.grid.major.x = element_blank(), 
    legend.position = "bottom"
  )


# ==============================================================================
# CHART VISUALIZATION OF MEANS
# ==============================================================================

# 1. Calculate absolute totals, then convert to shares
india_shares <- data_2017 |>
  summarise(
    # General
    Pop_General = sum(pop_workingage_unw, na.rm = TRUE),
    LF_General  = sum(labor_force_PS_unw, na.rm = TRUE) / Pop_General,
    Emp_General = sum(worker_PS_unw, na.rm = TRUE) / Pop_General,
    Cas_General = w_mean(s_casual_w_worker_PS_unw, worker_PS_unw),
    
    # Women (f)
    Pop_Women = sum(pop_f_unw, na.rm = TRUE),
    LF_Women  = sum(labor_force_PS_f_unw, na.rm = TRUE) / Pop_Women,
    Emp_Women = sum(worker_PS_f_unw, na.rm = TRUE) / Pop_Women,
    Cas_Women = w_mean(s_casual_w_worker_PS_f_unw, worker_PS_f_unw),
    
    # Men (m)
    Pop_Men = sum(pop_m_unw, na.rm = TRUE),
    LF_Men  = sum(labor_force_PS_m_unw, na.rm = TRUE) / Pop_Men,
    Emp_Men = sum(worker_PS_m_unw, na.rm = TRUE) / Pop_Men,
    Cas_Men = w_mean(s_casual_w_worker_PS_m_unw, worker_PS_m_unw),
    
    # Rural (rur)
    Pop_Rural = sum(pop_rur_unw, na.rm = TRUE),
    LF_Rural  = sum(labor_force_PS_rur_unw, na.rm = TRUE) / Pop_Rural,
    Emp_Rural = sum(worker_PS_rur_unw, na.rm = TRUE) / Pop_Rural,
    Cas_Rural = w_mean(s_casual_w_worker_PS_rur_unw, worker_PS_rur_unw),
    
    # Urban (urb)
    Pop_Urban = sum(pop_urb_unw, na.rm = TRUE),
    LF_Urban  = sum(labor_force_PS_urb_unw, na.rm = TRUE) / Pop_Urban,
    Emp_Urban = sum(worker_PS_urb_unw, na.rm = TRUE) / Pop_Urban,
    Cas_Urban = w_mean(s_casual_w_worker_PS_urb_unw, worker_PS_urb_unw)
  ) |>
  # Reshape data for ggplot
  pivot_longer(
    cols = everything(),
    names_to = c("Metric", "Group"),
    names_sep = "_",
    values_to = "Value"
  ) |>
  pivot_wider(
    names_from = Metric,
    values_from = Value
  )

# 2. Define the scaling factor for the Dual Axis
# We scale percentages so that 100% (1.0) equals the maximum population bar height
scale_factor <- max(india_shares$Pop, na.rm = TRUE) 

# 3. Apply the scaling factor to the shares and prep for plotting
chart_data <- india_shares |>
  mutate(
    LF_Scaled  = LF * scale_factor,
    Emp_Scaled = Emp * scale_factor,
    Cas_Scaled = Cas * scale_factor
  ) |>
  dplyr::select(Group, Pop, LF_Scaled, Emp_Scaled, Cas_Scaled) |>
  pivot_longer(
    cols = c(Pop, LF_Scaled, Emp_Scaled, Cas_Scaled),
    names_to = "Metric",
    values_to = "PlotValue"
  ) |>
  mutate(
    # Set the order of the bars and assign clean labels for the legend
    Metric = factor(Metric, 
                    levels = c("Pop", "LF_Scaled", "Emp_Scaled", "Cas_Scaled"),
                    labels = c("Working-Age Population", 
                               "LF Participation Rate", 
                               "Employment Rate", 
                               "Casual Share of Workers")),
    Group = factor(Group, levels = c("General", "Men", "Women", "Urban", "Rural"))
  )

# 4. Create the Dual-Axis Chart
ggplot(chart_data, aes(x = Group, y = PlotValue, fill = Metric)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  theme_minimal() +
  
  # Configure Colors and Clean Up Labels
  scale_fill_manual(
    values = c(
      "Working-Age Population"  = "#d3d3d3", # Gray
      "LF Participation Rate"   = "#4682b4", # Blue
      "Employment Rate"         = "#2e8b57", # Green
      "Casual Share of Workers" = "#e67e22"  # Orange
    ),
    # This dynamically renames them in the legend using clean units instead of axis directions
    labels = c(
      "Working-Age Population"  = "Working-Age Population (Count)",
      "LF Participation Rate"   = "LF Participation Rate (%)",
      "Employment Rate"         = "Employment Rate (%)",
      "Casual Share of Workers" = "Casual Share of Workers (%)"
    )
  ) +
  
  # Configure Left and Right Y-Axes
  scale_y_continuous(
    name = "Population Size (Count)",
    labels = scales::comma, 
    sec.axis = sec_axis(
      transform = ~ . / scale_factor, 
      name = "Share (%)",
      labels = scales::percent_format(accuracy = 1) 
    )
  ) +
  
  # Labels and Theme Adjustments
  labs(
    x = "",
    fill = "" # Removed "Metric" title to keep the legend ultra-clean
  ) +
  
  # FORCE THE LEGEND INTO 2 COLUMNS
  guides(fill = guide_legend(ncol = 2, byrow = TRUE)) + 
  
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(size = 11, face = "bold"),
    axis.title.y.left = element_text(margin = margin(r = 10)),
    axis.title.y.right = element_text(margin = margin(l = 10)),
    legend.position = "bottom",
    legend.direction = "horizontal", # Changed to horizontal to span across the bottom nicely
    legend.text = element_text(size = 10)
  )


# ==============================================================================
# same thing but with min/max also

# Ensure your w_mean helper function is loaded
w_mean <- function(val, weight) {
  sum(val * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE)
}

# 1. Calculate Summary Stats for Each Group Separately
g_stats <- data_2017 |> summarise(
  Group = "General",
  Pop_mean = sum(pop_workingage_unw, na.rm = TRUE), Pop_min = NA, Pop_max = NA, Pop_sd = NA,
  LF_mean  = w_mean(lf_share_unw, pop_workingage_unw), LF_min = min(lf_share_unw, na.rm = TRUE), LF_max = max(lf_share_unw, na.rm = TRUE), LF_sd = sd(lf_share_unw, na.rm = TRUE),
  Emp_mean = w_mean(worker_share_unw, pop_workingage_unw), Emp_min = min(worker_share_unw, na.rm = TRUE), Emp_max = max(worker_share_unw, na.rm = TRUE), Emp_sd = sd(worker_share_unw, na.rm = TRUE),
  Cas_mean = w_mean(s_casual_w_worker_PS_unw, worker_PS_unw), Cas_min = min(s_casual_w_worker_PS_unw, na.rm = TRUE), Cas_max = max(s_casual_w_worker_PS_unw, na.rm = TRUE), Cas_sd = sd(s_casual_w_worker_PS_unw, na.rm = TRUE)
)

f_stats <- data_2017 |> summarise(
  Group = "Female",
  Pop_mean = sum(pop_f_unw, na.rm = TRUE), Pop_min = NA, Pop_max = NA, Pop_sd = NA,
  LF_mean  = w_mean(lf_share_f_unw, pop_f_unw), LF_min = min(lf_share_f_unw, na.rm = TRUE), LF_max = max(lf_share_f_unw, na.rm = TRUE), LF_sd = sd(lf_share_f_unw, na.rm = TRUE),
  Emp_mean = w_mean(worker_share_f_unw, pop_f_unw), Emp_min = min(worker_share_f_unw, na.rm = TRUE), Emp_max = max(worker_share_f_unw, na.rm = TRUE), Emp_sd = sd(worker_share_f_unw, na.rm = TRUE),
  Cas_mean = w_mean(s_casual_w_worker_PS_f_unw, worker_PS_f_unw), Cas_min = min(s_casual_w_worker_PS_f_unw, na.rm = TRUE), Cas_max = max(s_casual_w_worker_PS_f_unw, na.rm = TRUE), Cas_sd = sd(s_casual_w_worker_PS_f_unw, na.rm = TRUE)
)

m_stats <- data_2017 |> summarise(
  Group = "Male",
  Pop_mean = sum(pop_m_unw, na.rm = TRUE), Pop_min = NA, Pop_max = NA, Pop_sd = NA,
  LF_mean  = w_mean(lf_share_m_unw, pop_m_unw), LF_min = min(lf_share_m_unw, na.rm = TRUE), LF_max = max(lf_share_m_unw, na.rm = TRUE), LF_sd = sd(lf_share_m_unw, na.rm = TRUE),
  Emp_mean = w_mean(worker_share_m_unw, pop_m_unw), Emp_min = min(worker_share_m_unw, na.rm = TRUE), Emp_max = max(worker_share_m_unw, na.rm = TRUE), Emp_sd = sd(worker_share_m_unw, na.rm = TRUE),
  Cas_mean = w_mean(s_casual_w_worker_PS_m_unw, worker_PS_m_unw), Cas_min = min(s_casual_w_worker_PS_m_unw, na.rm = TRUE), Cas_max = max(s_casual_w_worker_PS_m_unw, na.rm = TRUE), Cas_sd = sd(s_casual_w_worker_PS_m_unw, na.rm = TRUE)
)

r_stats <- data_2017 |> summarise(
  Group = "Rural",
  Pop_mean = sum(pop_rur_unw, na.rm = TRUE), Pop_min = NA, Pop_max = NA, Pop_sd = NA,
  LF_mean  = w_mean(lf_share_rur_unw, pop_rur_unw), LF_min = min(lf_share_rur_unw, na.rm = TRUE), LF_max = max(lf_share_rur_unw, na.rm = TRUE), LF_sd = sd(lf_share_rur_unw, na.rm = TRUE),
  Emp_mean = w_mean(worker_share_rur_unw, pop_rur_unw), Emp_min = min(worker_share_rur_unw, na.rm = TRUE), Emp_max = max(worker_share_rur_unw, na.rm = TRUE), Emp_sd = sd(worker_share_rur_unw, na.rm = TRUE),
  Cas_mean = w_mean(s_casual_w_worker_PS_rur_unw, worker_PS_rur_unw), Cas_min = min(s_casual_w_worker_PS_rur_unw, na.rm = TRUE), Cas_max = max(s_casual_w_worker_PS_rur_unw, na.rm = TRUE), Cas_sd = sd(s_casual_w_worker_PS_rur_unw, na.rm = TRUE)
)

u_stats <- data_2017 |> summarise(
  Group = "Urban",
  Pop_mean = sum(pop_urb_unw, na.rm = TRUE), Pop_min = NA, Pop_max = NA, Pop_sd = NA,
  LF_mean  = w_mean(lf_share_urb_unw, pop_urb_unw), LF_min = min(lf_share_urb_unw, na.rm = TRUE), LF_max = max(lf_share_urb_unw, na.rm = TRUE), LF_sd = sd(lf_share_urb_unw, na.rm = TRUE),
  Emp_mean = w_mean(worker_share_urb_unw, pop_urb_unw), Emp_min = min(worker_share_urb_unw, na.rm = TRUE), Emp_max = max(worker_share_urb_unw, na.rm = TRUE), Emp_sd = sd(worker_share_urb_unw, na.rm = TRUE),
  Cas_mean = w_mean(s_casual_w_worker_PS_urb_unw, worker_PS_urb_unw), Cas_min = min(s_casual_w_worker_PS_urb_unw, na.rm = TRUE), Cas_max = max(s_casual_w_worker_PS_urb_unw, na.rm = TRUE), Cas_sd = sd(s_casual_w_worker_PS_urb_unw, na.rm = TRUE)
)

# 2. Combine and Reshape
combined_data <- bind_rows(g_stats, f_stats, m_stats, r_stats, u_stats)

# Define the scaling factor based on the max population total
scale_factor <- max(combined_data$Pop_mean, na.rm = TRUE)

chart_data <- combined_data |>
  pivot_longer(cols = -Group, names_to = c("Metric", "Stat"), names_sep = "_") |>
  pivot_wider(names_from = Stat, values_from = value) |>
  mutate(
    # Scale ALL structural layers (Means, Mins, and Maxes) to match the left axis
    mean_scaled = ifelse(Metric == "Pop", mean, mean * scale_factor),
    min_scaled  = ifelse(Metric == "Pop", min,  min * scale_factor),
    max_scaled  = ifelse(Metric == "Pop", max,  max * scale_factor),
    
    # Factor setups
    Metric = factor(Metric, 
                    levels = c("Pop", "LF", "Emp", "Cas"),
                    labels = c("Working-Age Population", 
                               "LF Participation Rate", 
                               "Employment Rate", 
                               "Casual Share of Workers")),
    Group = factor(Group, levels = c("General", "Male", "Female", "Urban", "Rural"))
  )

ggplot(chart_data, aes(x = Group, y = mean_scaled, fill = Metric)) +
  # Main Bars
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  
  # Min-Max Error Bars (Skips 'Working-Age Population' because its min/max is NA)
  geom_errorbar(
    aes(ymin = min_scaled, ymax = max_scaled, group = Metric),
    position = position_dodge(width = 0.8),
    width = 0.15,      # Width of the error bar caps
    color = "#2c3e50", # Professional dark slate gray for the whiskers
    linewidth = 0.3,
    na.rm = TRUE       # Safely ignores the missing population error bar
  ) +
  
  theme_minimal() +
  
  # Colors and 2-Column Legend Layout
  scale_fill_manual(
    values = c(
      "Working-Age Population"  = "#d3d3d3", 
      "LF Participation Rate"   = "#4682b4", 
      "Employment Rate"         = "#2e8b57", 
      "Casual Share of Workers" = "#e67e22"  
    ),
    labels = c(
      "Working-Age Population"  = "Working-Age Population (Count)",
      "LF Participation Rate"   = "LF Participation Rate (%)",
      "Employment Rate"         = "Employment Rate (%)",
      "Casual Share of Workers" = "Casual Share of Workers (%)"
    )
  ) +
  
  # Left and Right Y-Axes Formatting
  scale_y_continuous(
    name = "Population Size (Count)",
    labels = scales::comma, 
    sec.axis = sec_axis(
      transform = ~ . / scale_factor, 
      name = "Share (%)",
      labels = scales::percent_format(accuracy = 1) 
    )
  ) +
  
  labs(x = "", fill = "") +
  guides(fill = guide_legend(ncol = 2, byrow = TRUE)) + 
  
  theme(
    axis.text.x = element_text(size = 11, face = "bold"),
    axis.title.y.left = element_text(margin = margin(r = 10)),
    axis.title.y.right = element_text(margin = margin(l = 10)),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(size = 10)
  )
