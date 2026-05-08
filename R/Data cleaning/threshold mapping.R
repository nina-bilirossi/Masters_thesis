library(sf)
library(ggplot2)
library(dplyr)
library(stringr)
library(viridis)

# Load data
shepy <- st_read("~/Desktop/MSC THESIS/Data works/Code/Data/geography/State_District_Sub-district_Boundary_of_entire_India/District_Boundary.shp") |> st_make_valid()
#data_og <- read.csv("/Users/ninabilirossi/Desktop/MSC THESIS/Data works/Code/Outputs/thresholds_custom_year.csv")
data <- read.csv('/Users/ninabilirossi/Desktop/MSC THESIS/Data works/Code/Outputs/final material/clean_thresholds.csv')

shapefile <- shepy

setdiff(
  unique(shapefile$DISTRICT),
  unique(data$DISTRICT_CLEAN)
)

setdiff(
  unique(data$DISTRICT_CLEAN),
  unique(shapefile$DISTRICT)
)


clean_district <- function(x) {
  x |>
    str_to_upper() |>
    str_replace_all("[^A-Z0-9 ]", "") |>   # remove all weird symbols
    str_replace_all("\\s+", " ") |>        # normalize spaces
    str_trim()
}
clean_district2 <- function(x) {
  x |>
    clean_district() |>
    str_replace_all(" ", "")
}


shapefile <- shapefile |>
  mutate(DISTRICT = clean_district(DISTRICT)) |> 
  mutate(DISTRICT = clean_district2(DISTRICT))

data <- data |>
  mutate(DISTRICT_CLEAN = clean_district(DISTRICT_CLEAN)) |> 
  mutate(DISTRICT_CLEAN = clean_district2(DISTRICT_CLEAN))

fixes <- tibble::tribble(
  ~DISTRICT_CLEAN, ~DISTRICT,
  
  # Jammu & Kashmir
  "ANANTNG", "ANANTNAG",
  "BADGM", "BADGAM",
  "GANDARBAL", "GANDERBAL",
  "KISHTWR", "KISHTWAR",
  "KULGM", "KULGAM",
  "MRPUR", "MIRPUR",
  "PNCH", "PUNCH",
  "RJAURI", "RAJAURI",
  "RMBAN", "RAMBAN",
  "RISI", "RIASI",
  "SMBA", "SAMBA",
  "SHUPYAN", "SHUPIYAN",
  "SRNAGAR", "SRINAGAR",
  "MUZAFFARBD", "MUZAFFARABAD",
  "BRAMLA", "BARAMULA",
  
  # Himachal
  "LHULSPITI", "LAHULSPITI",
  
  # Chhattisgarh
  "GARIYABANDH", "GARIYABAND",
  "JANJGRCHAMPA", "JANJGIRCHAMPA",
  
  # Assam / NE
  "DARRANG", "DARANG",
  "KARBIANAGLONG", "KARBIANGLONG",
  
  # Maharashtra (many abbreviations)
  "AMARVATI", "AMRAVATI",
  "BHANDRA", "BHANDARA",
  "BULDHNA", "BULDHANA",
  "JLNA", "JALNA",
  "KOLHPUR", "KOLHAPUR",
  "LTR", "LATUR",
  "NGPUR", "NAGPUR",
  "NNDED", "NANDED",
  "NSHIK", "NASHIK",
  "NANDURBR", "NANDURBAR",
  "PLGHAR", "PALGHAR",
  "RYGAD", "RAIGAD",
  "RATNGIRI", "RATNAGIRI",
  "SNGLI", "SANGLI",
  "STRA", "SATARA",
  "SOLPUR", "SOLAPUR",
  "SUBURBANMUMBAI", "MUMBAISUBURBAN",
  "THNE", "THANE",
  "WSHM", "WASHIM",
  "YAVATML", "YAVATMAL",
  "BD", "BEED",
  
  # Odisha
  "ANGUL", "ANUGUL",
  "JAJPUR", "JAJAPUR",
  "KEONJHAR", "KEONJHARKENDUJHAR",
  "NABARANGPUR", "NABARANGAPUR",
  "NUAPADA", "NUAPARHA",
  
  # Punjab
  "MUKTSAR", "SRIMUKTSARSAHIB",
  "SAHIBZADAAJITSINGHNAGAR", "SASNAGARSAHIBZADAAJITSINGHNAGAR",
  
  # Uttarakhand
  "BAGESHWAR", "BGESHWAR",
  "CHAMPAWAT", "CHAMPWAT",
  "HARIDWAR", "HARIDWR",
  "NAINITAL", "NAINITL",
  "PAURIGARHWAL", "PAURIGARHWL",
  "PITHORAGARH", "PITHORGARH",
  "RUDRAPRAYAG", "RUDRAPRAYG",
  "TEHRIGARHWAL", "TEHRIGARHWL",
  "UTTARKASHI", "UTTARKSHI",
  
  # West Bengal
  "KOLKTA", "KOLKATA",
  "PRBAMEDINPUR", "PURBAMEDINPUR",
  "DAKSHINDINAJPUR", "DAKSHINDINJPUR",
  "SOUTHTWENTYFOURPARGANAS", "SOUTH24PARGANAS",
  "MURSHIDABAD", "MURSHIDBD",
  
  # Andhra Pradesh
  "YSRKADAPA", "YSR",
  
  # Tamil Nadu
  "THIRUVALLUR", "TIRUVALLUR",
  
  # Manipur / Mizoram / NE small fixes
  "CHRCHNDPUR", "CHURACHANDPUR",
  "KKCHING", "KAKCHING",
  "KMJANG", "KAMJANG",
  "KNGPOKPI", "KANGPOKPI",
  "TENGNOUPL", "TENGNOUPAL",
  "THOUBL", "THOUBAL",
  "CHAMPHI", "CHAMPHAI",
  "MMIT", "MAMIT",
  "SERCHHP", "SERCHHIP",
  "ZWAL", "AIZWAL",
  "SENPATI", "SENAPATI"
)

data <- data |>
  left_join(fixes, by = "DISTRICT_CLEAN") |>
  mutate(DISTRICT_CLEAN = coalesce(DISTRICT.y, DISTRICT_CLEAN)) |>
  select(-c(DISTRICT.y,DISTRICT_MATCH))

shapefile <- shapefile |>
  left_join(data, by = c("DISTRICT" = "DISTRICT_CLEAN"))

missing <- shapefile |>
  filter(is.na(THRESHOLD_10YR)) |>
  distinct(DISTRICT)

missing

# Plot
d <- ggplot(shapefile) +
  geom_sf(aes(fill = THRESHOLD_10YR), linewidth = 0.1) +
  scale_fill_viridis_c(
    option    = "plasma",
    name      = "threshold (mm/day)",
    na.value  = "grey80") +
  labs(title = "10-year extreme precipitation threshold, district-level" ) +
  theme(plot.title    = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    legend.position = "right")

#print(g)
print(d)


# Save
#ggsave("map.png", width = 10, height = 8, dpi = 300)

# SUMMARY STATISTICS

summary_stats <- data |>
  summarise(
    count   = n(),
    mean    = mean(THRESHOLD_10YR, na.rm = TRUE),
    sd      = sd(THRESHOLD_10YR, na.rm = TRUE),
    min     = min(THRESHOLD_10YR, na.rm = TRUE),
    q25     = quantile(THRESHOLD_10YR, 0.25, na.rm = TRUE),
    median  = median(THRESHOLD_10YR, na.rm = TRUE),
    q75     = quantile(THRESHOLD_10YR, 0.75, na.rm = TRUE),
    max     = max(THRESHOLD_10YR, na.rm = TRUE)
  )

print(summary_stats)
