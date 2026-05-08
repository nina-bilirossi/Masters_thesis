# Run yearly_SPEI.R first to generate the spei_long dataframe

# Filter for weighted data and remove NA/Disputed rows
plot_data <- spei_long %>%
  filter(type == "weighted") %>%
  filter(!is.na(spei12)) %>%
  filter(!grepl("DISPUTED", state_name))

# Split states into two groups for two separate grids
all_states <- sort(unique(plot_data$state_name))
midpoint <- ceiling(length(all_states) / 2)
states_group1 <- all_states[1:midpoint]
states_group2 <- all_states[(midpoint + 1):length(all_states)]

create_state_grid <- function(data_subset, title_suffix) {
  ggplot(data_subset, aes(x = year, y = spei12)) +
    # Add a horizontal line at 0 for reference
    geom_hline(yintercept = 0, color = "gray50", linetype = "dashed") +    # The main line and points
    geom_line(color = "steelblue", size = 1) +
    geom_point(aes(color = spei12 > 0), size = 1.5) +
    # Formatting the "Grid"
    facet_wrap(~state_name, ncol = 4, scales = "free_y") + 
    # Styling
    scale_color_manual(values = c("TRUE" = "blue", "FALSE" = "red"), guide = "none") +
    theme_minimal() +
    labs(
      title = paste("Population-Weighted SPEI12 Trends:", title_suffix),
      subtitle = "Red points indicate drought conditions (SPEI < 0)",
      x = "Year",
      y = "Weighted SPEI12"
    ) +
    theme(
      strip.text = element_text(face = "bold", size = 8),
      axis.text = element_text(size = 7)
    )
}

grid0 <- create_state_grid(plot_data, "Raw data")
print(grid0)

# Generate Group 1 (A-M roughly)
grid1 <- create_state_grid(plot_data %>% filter(state_name %in% states_group1), "Group A")
print(grid1)

# Generate Group 2 (N-Z roughly)
grid2 <- create_state_grid(plot_data %>% filter(state_name %in% states_group2), "Group B")
print(grid2)
