library(dplyr)
library(ggplot2)
library(glue)


inf_countries <- read.csv("./Data/ILOSTAT/ILO data/SDG_0831_SEX_ECO_RT_A-20260305T1828.csv")

inf_countries_tot <- inf_countries |>
  filter(classif1.label == "Economic activity (Broad sector): Total",
         sex.label == "Total")

ggplot(inf_countries_tot, aes(x = obs_value)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Informality Rates",
       x = "Informality Rate",
       y = "Count") +
  theme_minimal()

ggplot(inf_countries_tot, aes(x = obs_value)) +
  geom_histogram(aes(y = ..density..), bins = 20,
                 fill = "lightblue", color = "white") +
  geom_density(color = "red", linewidth = 1) +
  theme_minimal()


inf_countries_timeline <- read.csv("ILO data/SDG_0831_SEX_ECO_RT_A-all.csv")

inf_countries_timeline_tot <- inf_countries_timeline |>
  filter(classif1.label == "Economic activity (Broad sector): Total",
         sex.label == "Total",
         ref_area.label %in% c("World: Lower-middle income",
                               "India", "Brazil", "Africa", 
                               "Mexico", "Indonesia",
                               "Pakistan"))

ggplot(inf_countries_timeline_tot, aes(x = time, y = obs_value, color = ref_area.label)) +
  geom_line() +
  labs(
    x = "Year",
    y = "Informality Rate (%)",
    title = "Informality Rate Over Time by Country/Region",
    color = "Country/Region"
  ) +
  theme_minimal()


### PERCENTILES ###

get_country_percentile <- function(data, country_name, year = NULL) {
  
  # Optionally filter by year
  if (!is.null(year)) {
    data <- data |> filter(time == year)
  }
  
  # Check if country exists
  if (!country_name %in% data$ref_area.label) {
    stop(paste("Country not found:", country_name))
  }
  
  # Get the country's value
  country_value <- data |>
    filter(ref_area.label == country_name) |>
    pull(obs_value)
  
  # If multiple rows (multiple years), take the most recent
  if (length(country_value) > 1) {
    country_value <- data |>
      filter(ref_area.label == country_name) |>
      slice_max(time, n = 1) |>
      pull(obs_value)
    message("Multiple years found — using most recent.")
  }
  
  # Calculate percentile
  all_values <- data$obs_value
  percentile <- round(mean(all_values <= country_value, na.rm = TRUE) * 100, 1)
  
  return(glue("percentile: ", percentile, "  value: ", country_value))
}

get_country_percentile(inf_countries_tot, "India")
