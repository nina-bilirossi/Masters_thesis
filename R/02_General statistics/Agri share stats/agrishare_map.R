library(sf)
library(ggplot2)

# --- Load your shapefile ---
india <- st_read("~/Desktop/MSC THESIS/Data works/Code/Data/geography/State_District_Sub-district_Boundary_of_entire_India/State Boundary.shp")

# --- Check what the state name column is called ---
names(india)
head(india)

# --- Define your groups (adjust the state name column and spelling to match your shapefile) ---
low_agri <- c(
  "BIHAR", "GOA", "GUJARAT", "HARYANA", "KERALA",
  "MANIPUR", "MIZORAM", "NAGALAND", "PUNJAB", "SIKKIM",
  "TAMIL NADU", "TELANGANA", "TRIPURA", "UTTARAKHAND", "WEST BENGAL"
)
high_agri <- c(
  "ANDHRA PRADESH", "ARUNACHAL PRADESH", "ASSAM",
  "CHHATTISGARH", "HIMACHAL PRADESH", "JAMMU AND KASHMIR",
  "LADAKH",   # <-- added manually as part of j&K, but not used in the analysis
  "JHARKHAND", "KARNATAKA", "MADHYA PRADESH",
  "MAHARASHTRA", "MEGHALAYA", "ODISHA",
  "RAJASTHAN", "UTTAR PRADESH"
)

# --- Tag each feature (replace NAME_1 with whatever your state column is) ---
STATE_COL <- "STATE"   # <-- change this to your actual column name

india$agri_group <- dplyr::case_when(
  grepl("DISPUTED", toupper(india[[STATE_COL]])) ~ "Disputed",
  gsub("&", "AND", toupper(india[[STATE_COL]])) %in% high_agri ~ "High agri share",
  gsub("&", "AND", toupper(india[[STATE_COL]])) %in% low_agri  ~ "Low agri share",
  TRUE ~ "Not in data"
)
# --- Plot ---
ggplot(india) +
  geom_sf(aes(fill = agri_group), color = "white", linewidth = 0.3) +
  scale_fill_manual(
    values = c(
      "High agri share" = "#1D9E75",
      "Low agri share"  = "#D85A30",
      "Not in data"     = "#B4B2A9"
    ),
    name = NULL
  ) +
  labs(
    title    = "Agricultural share of employment by state",
    #subtitle = "Indian states by employment group"
    foot = "Source: PLFS 2017-18, weighted shares. Ladakh is included in J&K. Uts are excluded."
  ) +
  theme_void(base_size = 13) +
  theme(
    legend.position   = "bottom",
    legend.key.size   = unit(0.5, "cm"),
    plot.title        = element_text(face = "bold", hjust = 0.5, margin = margin(b = 4)),
    plot.subtitle     = element_text(hjust = 0.5, color = "grey40", margin = margin(b = 12)),
    plot.margin       = margin(10, 10, 10, 10)
  )

