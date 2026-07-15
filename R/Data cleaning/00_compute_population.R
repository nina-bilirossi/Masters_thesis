library(sf)
library(terra)
library(exactextractr)
library(ggplot2)
library(dplyr)
library(raster)
library(glue)

subdistricts <- st_read("~/Desktop/MSC THESIS/Data works/Code/Data/geography/State_District_Sub-district_Boundary_of_entire_India/District Boundary.shp")
pop_raster <- rast("~/Desktop/MSC THESIS/Data works/Code/Data/geography/ind_ppp_2020.tif")

st_crs(subdistricts)
crs(pop_raster)

subdistricts_wgs84 <- st_transform(subdistricts, crs(pop_raster))

# Extract population sum per district
subdistricts_wgs84$population <- exact_extract(
  pop_raster, 
  subdistricts_wgs84, 
  fun = "sum")

summary(subdistricts_wgs84$population)  # check for unexpected NAs or zeros
# If you want to round (WorldPop values are decimals)
#districts$pop_total <- round(districts$pop_total, 0)

head(subdistricts_wgs84[, c("DISTRICT", "population")])

subdistricts_final <- st_drop_geometry(subdistricts_wgs84) |> 
  dplyr::select(STATE_UT, DISTRICT, population)

write.csv(subdistricts_final, "~/Desktop/MSC THESIS/Data works/Code/Outputs/population.csv", row.names = FALSE)

subdistricts_wgs84 <- st_make_valid(subdistricts_wgs84)
ggplot(subdistricts_wgs84) +
  geom_sf(aes(fill = population)) +
  scale_fill_viridis_c(
    option = "viridis",
    trans = "log10",
    labels = scales::comma
  ) +
  labs(
    title = "Population by District",
    fill = "Population"
  ) +
  theme_void()
