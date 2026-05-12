#install.packages(c("sf", "terra", "exactextractr"))
library(sf)
library(terra)
library(exactextractr)
library(ggplot2)
library(dplyr)
library(raster)
library(glue)
library(tidyr)

# Read administrative boundaries and population files
# sub_districts <- st_read("~/Desktop/MSC THESIS/Data works/Code/Data/geography/State_District_Sub-district_Boundary_of_entire_India/Sub_District Boundary.shp")

states <- st_read("~/Desktop/MSC THESIS/Data works/Code/Data/geography/State_District_Sub-district_Boundary_of_entire_India/State Boundary.shp")
districts <- st_read("~/Desktop/MSC THESIS/Data works/Code/Data/geography/State_District_Sub-district_Boundary_of_entire_India/District_Boundary.shp")
pop_raster <- rast("~/Desktop/MSC THESIS/Data works/Code/Data/geography/ind_ppp_2020.tif")

st_crs(districts)
crs(pop_raster)

# Compute population per district
# 1. Align the CRS first (Transform vector to match raster)
districts_wgs84 <- st_transform(districts, st_crs(pop_raster))

# 2. Run the extraction
# We use 'sum' because population rasters usually represent 'people per pixel'
districts$pop_total <- exact_extract(pop_raster, districts_wgs84, 'sum')

# 3. View results
head(districts[, c("DISTRICT", "pop_total")])

districts_final <- st_drop_geometry(districts) |> 
  dplyr::select(STATE_UT, DISTRICT, pop_total) |> 
  mutate(STATE_UT = ifelse(STATE_UT == "UTTAR>KHAND", "UTTARAKHAND", STATE_UT))

state_population <- districts_final %>%
  group_by(STATE_UT) %>%
  summarise(state_pop = sum(pop_total, na.rm = TRUE))

write.csv(districts_final, "~/Desktop/MSC THESIS/Data works/Code/Outputs/district_population.csv", row.names = FALSE)
write.csv(state_population, "~/Desktop/MSC THESIS/Data works/Code/Outputs/final material/state_population.csv", row.names = FALSE)

compute_state_weighted_SPEI <- function(spei_nc_file, year,
                                        spei_varname = "SPEI12") {
  spei_raster <- rast(spei_nc_file, subds = spei_varname)
  
  districts <- st_transform(districts, crs(pop_raster))
  states <- st_transform(states, crs(pop_raster))
  
  if (!compareCRS(spei_raster, pop_raster)) {
    spei_raster <- project(spei_raster, crs(pop_raster))
  }
  
  states$mean_SPEI12 <- exact_extract(spei_raster, states, 'mean')
  districts$mean_SPEI12 <- exact_extract(spei_raster, districts, 'mean')
  
  state_scores <- districts %>%
    st_drop_geometry() %>%   # <-- ADD THIS
    group_by(STATE_UT) %>%
    summarise(weighted_SPEI12 = sum(mean_SPEI12 * pop_total, na.rm = TRUE) / 
                sum(pop_total, na.rm = TRUE),
              unweighted_SPEI12 = mean(mean_SPEI12, na.rm = TRUE)
    ) |>  rename(STATE = STATE_UT)
  
  # --- CRITICAL FIX: CLEAN NAMES BEFORE JOINING ---
  state_scores <- state_scores %>%
    mutate(STATE = case_when(
      STATE == "UTTAR>KHAND" ~ "UTTARAKHAND",
      STATE == "ANDAMAN AND NICOBAR ISLANDS" ~ "ANDAMAN & NICOBAR",
      TRUE ~ STATE
    ))
  
  # Ensure the 'states' object also has clean names to match
  states$STATE[states$STATE == "UTTAR>KHAND"] <- "UTTARAKHAND"
  
  # For plotting, use a separate spatial version
  state_scores_sf <- states %>%
    left_join(st_drop_geometry(state_scores), by = "STATE")
  
  p_states <- ggplot(state_scores_sf) +
    geom_sf(aes(fill = weighted_SPEI12)) +
    scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
    labs(title = glue("Population-Weighted SPEI12 by State - {year}"))
  
  p_districts <- ggplot(districts) +
    geom_sf(aes(fill = mean_SPEI12)) +
    scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
    labs(title = glue("Mean SPEI12 per District, unweighted - {year}"))
  
  state_scores$STATE[state_scores$STATE == "UTTAR>KHAND"] <- "UTTARAKHAND"
  state_scores$STATE[state_scores$STATE == "ANDAMAN AND NICOBAR ISLANDS"] <- "ANDAMAN & NICOBAR"
  
  
  return(list(
    state_scores = state_scores,   # now a plain dataframe
    plot_states = p_states,
    plot_districts = p_districts
  ))
}


# r <- rast("./copernicus_download/SPEI12_genlogistic_global_era5_moda_ref1991to2020_202506.area-subset.39.100.7.70.nc", subds="SPEI12")

nc_files <- list.files(path = "~/Desktop/MSC THESIS/Data works/Code/Data/copernicus_download/SPEI12_June_2015-2024", full.names = TRUE)
year = 2015
# Initialize SPEI files for all years
spei_yearly <- data.frame(STATE = states$STATE)
# remove the disputed states
spei_yearly <- spei_yearly[!grepl("DISPUTED", spei_yearly[[1]]), , drop = FALSE]

for (file in nc_files) {
  
  result <- compute_state_weighted_SPEI(file, year = year)
  state_scores <- result$state_scores
  
  print("hello")
  print(head(state_scores))
  
  # Match row order
  idx <- match(spei_yearly$STATE, state_scores$STATE)
  
  # Add columns with year suffix
  spei_yearly[[paste0("weighted_SPEI12_", year)]] <-
    state_scores$weighted_SPEI12[idx]
  
  spei_yearly[[paste0("unweighted_SPEI12_", year)]] <-
    state_scores$unweighted_SPEI12[idx]
  
  print(result$plot_states)
  
  year <- year + 1
}

raw <- spei_yearly

spei_long <- spei_yearly %>%
  pivot_longer(
    cols = -STATE,
    names_to = c("type", "year"),
    names_pattern = "(weighted|unweighted)_SPEI12_(\\d+)",
    values_to = "spei12"
  ) %>%
  mutate(year = as.numeric(year)) |> 
  rename(state_name = STATE) |> 
  mutate(state_name = toupper(state_name))

spei_long <- spei_long %>%
  mutate(negative = ifelse(spei12 > 0, 0, spei12)) |> 
  mutate(positive = ifelse(spei12 < 0, 0, spei12))
  
  
# spei_yearly$STATE[spei_yearly$STATE == "JAMMU AND KASHMIR"] <- "JAMMU & KASHMIR"

spei_with_lags <- spei_long |> 
  filter(type == "weighted") |>
  mutate(year = as.numeric(year), spei12 = as.numeric(spei12)) |> 
  arrange(state_name, year) |> 
  group_by(state_name) |> 
  mutate(
    spei_lag1 = dplyr::lag(spei12, 1),  # previous year
    spei_lag2 = dplyr::lag(spei12, 2),  # 2 years ago
    spei_lag3 = dplyr::lag(spei12, 3)   # 3 years ago
  ) |> 
  mutate(
    neg_spei_lag1 = dplyr::lag(negative, 1),  # previous year
    neg_spei_lag2 = dplyr::lag(negative, 2),  # 2 years ago
    neg_spei_lag3 = dplyr::lag(negative, 3)   # 3 years ago
  ) |> 
  ungroup()

spei_with_lags

write.csv(spei_with_lags, "~/Desktop/MSC THESIS/Data works/Code/Outputs/final material/spei_with_lags.csv", row.names = FALSE)
