#Author: Will
#SLR Inundation Visualization
#This script generates coastal inundation maps for the OPC SLR scenarios by overlaying projected water levels (including wave run-up) onto a digital elevation model, 
#and combines these results with parcel or property spatial data to visualize flood exposure and property vulnerability for each scenario.

rm(list=ls())
library(sf)
library(raster)
library(tidyverse)
library(tmap)
library(viridis)


# 1. Load Data

# Load Redfin parcels as sf (from CSV) [output of redfin_data_code_ms.R]
parcels <- read_csv("data/carpinteria/redfin_sf.csv") %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# Load DEM raster
dem <- raster("data/DEM/carpinteria_dem.tif")

# SLR scenario values (meters; update as needed)
slr_scenarios <- list(
  Intermediate = 0.6,      # meters
  Intermediate_High = 1.0,
  High = 1.5
)

# Wave run-up raster (output from waves_ms.R MC workflow; same extent as DEM)
wave_runup <- raster("data/wave_runup/wave_runup.tif")

# 2. Compute Inundation Rasters
inundation_rasters <- list()
for(scenario in names(slr_scenarios)) {
  total_water <- slr_scenarios[[scenario]] + wave_runup
  inundation <- dem < total_water                 # logical raster: 1 if inundated, NA/0 otherwise
  inundation[inundation == FALSE] <- NA           # Convert non-inundated to NA for clean plotting
  inundation_rasters[[scenario]] <- inundation
}

# 3. Extract Inundation Info for Parcels
parcels_vuln <- parcels
for(scenario in names(inundation_rasters)) {
  parcels_vuln[[paste0("inundated_", scenario)]] <-
    raster::extract(inundation_rasters[[scenario]], st_coordinates(parcels)) == 1
}

# 4. Visualization with ggplot2 (static)
dem_df <- as.data.frame(rasterToPoints(dem))
colnames(dem_df) <- c("x", "y", "elevation")

for(scenario in names(inundation_rasters)) {
  inundation <- inundation_rasters[[scenario]]
  inundation_df <- as.data.frame(rasterToPoints(inundation))
  colnames(inundation_df) <- c("x", "y", "inundated")
  p <- ggplot() +
    geom_raster(data = dem_df, aes(x = x, y = y, fill = elevation)) +
    scale_fill_viridis(name = "Elevation (m)") +
    geom_raster(data = inundation_df %>% drop_na(), aes(x = x, y = y),
                fill = "blue", alpha = 0.4) +
    geom_sf(data = parcels_vuln, aes(color = !!sym(paste0("inundated_", scenario)))) +
    scale_color_manual(values = c("FALSE" = "darkgreen", "TRUE" = "red"),
                       labels = c("Dry", "Inundated"),
                       name = "Parcel Status") +
    labs(title = paste("SLR Scenario:", scenario),
         x = "Longitude", y = "Latitude") +
    theme_minimal()
  print(p)
}

# 5. Optional: tmap interactive
tmap_mode("view")
for(scenario in names(inundation_rasters)) {
  tm <- tm_shape(dem) +
    tm_raster(alpha = 0.5) +
    tm_shape(inundation_rasters[[scenario]]) +
    tm_raster(palette = "-Blues", alpha = 0.5, title = "Inundation") +
    tm_shape(parcels_vuln) +
    tm_symbols(col = paste0("inundated_", scenario),
               palette = c("FALSE" = "green", "TRUE" = "red"),
               title.col = "Parcel Status")
  print(tm)
}



r_low <- rast("data/carpinteria/inundation_Intermediate_0.tif")
r_high <- rast("data/carpinteria/inundation_Intermediate_80.tif")
plot(r_low)
plot(r_high)
plot(r_high - r_low)

