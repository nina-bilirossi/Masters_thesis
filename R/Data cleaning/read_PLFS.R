library(stringr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(patchwork)

setwd('/Users/ninabilirossi/Desktop/MSC THESIS/Data works/Code')
plfs_unw_df <- read.csv("./Data/PLFS/NSS_PLFS_all_s_unw.csv")
plfs_w_df <- read.csv("./Data/PLFS/NSS_PLFS_all_s_unw.csv")

plfs_unw_df$year <- str_sub(plfs_unw_df$time, -4)
plfs_w_df$year <- str_sub(plfs_w_df$time, -4)

plfs_unw_df <- plfs_unw_df |> filter(year>2016) |> mutate(year = as.numeric(year))
plfs_w_df <- plfs_w_df |> filter(year>2016) |> mutate(year = as.numeric(year))


### Gendered ###

plfs_sex <- read.csv("~/Desktop/MSC THESIS/Data works/My data prep/PLFS processed/PLFS_all_s_N_unw.csv")
plfs_sex$year <- str_sub(plfs_sex$time, -4)

ggplot(plfs_sex, aes(x = year, y = s_casual_w_worker_PS_unw, group = state_name)) +
  geom_line() +
  geom_point()


ggplot(plfs_sex, aes(x = year, y = s_casual_w_pop_PS_f_unw, group = state_name, color = state_name)) +
  geom_line() +
  geom_point() +
  labs(title = "Unweighted Casual Worker over Population Share by State, Female",
         x = "Year",
         y = "Casual Worker Participation Rate (%)")


ggplot(plfs_sex, aes(x = year, y = s_casual_w_pop_PS_m_unw, group = state_name, color = state_name)) +
  geom_line() +
  geom_point() +
  labs(title = "Unweighted Casual Worker over Population Share by State, Male",
         x = "Year",
         y = "Casual Worker Participation Rate (%)")

# ggplot(plfs_unw_df, aes(x = year, y = s_casual_w_worker_PS_unw, group = state_name)) +
#   geom_line() +
#   geom_point()

colnames(plfs_sex)


#### Combined state gender plots ####

# Create a list of plots, one per state
state_plots <- lapply(unique(plfs_sex$state_name), function(s) {
  
  state_data <- plfs_sex[plfs_sex$state_name == s, ]
  
  ggplot(state_data, aes(x = year)) +
    geom_line(aes(y = s_casual_w_lf_PS_f_unw, color = "Female"), group = 1) +
    geom_point(aes(y = s_casual_w_lf_PS_f_unw, color = "Female")) +
    geom_line(aes(y = s_casual_w_lf_PS_m_unw, color = "Male"), group = 1) +
    geom_point(aes(y = s_casual_w_lf_PS_m_unw, color = "Male")) +
    geom_line(aes(y = s_casual_w_lf_PS_unw, color = "Overall"), group = 1) +
    geom_point(aes(y = s_casual_w_lf_PS_unw, color = "Overall")) +
    scale_color_manual(values = c("Female" = "red", "Male" = "blue", "Overall" = "black")) +
    labs(title = s,
         x = "Year",
         y = "Casual Worker Share (%) over group labor force",
         color = "Gender") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 9, face = "bold"),
      axis.text = element_text(size = 7),
      legend.position = "none"  # Remove per-plot legend; we'll add one shared legend
    )
})

state_plots
# Combine all plots using patchwork
combined <- wrap_plots(state_plots, ncol = 4) +
  plot_annotation(
    title = "Unweighted Casual Worker over LF Share by State",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
  ) +
  plot_layout(guides = "collect")

# Add shared legend by using one plot with legend visible
state_plots_with_legend <- lapply(seq_along(state_plots), function(i) {
  if (i == 1) state_plots[[i]] + theme(legend.position = "bottom")
  else state_plots[[i]]
})

combined <- wrap_plots(state_plots_with_legend, ncol = 4) +
  plot_annotation(
    title = "Unweighted Casual Worker over LF Share by State",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
  ) +
  plot_layout(guides = "collect")

# Save
n_states <- length(unique(plfs_sex$state_name))
n_rows <- ceiling(n_states / 4)
combined
ggsave("./Output/casual_worker_LF_state.png", combined, width = 16, height = n_rows * 3.5, dpi = 300)

