#Will Dean
#Modification of the waves_ms.R script to be more robust and spatially accurate by linking 
#wave run-up estimates to individual property parcels, rather than applying a uniform value across the coastline

rm(list=ls())
library(sf)
library(rnaturalearth)
library(rnaturalearthhires)
library(raster)
library(elevatr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)

# Define region (adjust bbox as needed!!!)
bbox <- st_bbox(c(xmin = -119.55, ymin = 34.38, xmax = -119.52, ymax = 34.41), crs = st_crs(4326))
bbox_poly <- st_as_sfc(bbox) |> st_sf()
projected_crs <- 3857

# Load parcels
redfin_df <- read_csv("data/carpinteria/redfin_df.csv")  # correct path
redfin_sf <- st_as_sf(redfin_df, coords = c("LONGITUDE", "LATITUDE"), crs = 4326, remove = FALSE)
redfin_sf$parcel_id <- 1:nrow(redfin_sf)

# Download coastline
land <- ne_download(scale = 10, type = "land", category = "physical") |> st_transform(4326)
us_states <- ne_states(country = "United States of America", returnclass = "sf") |> st_transform(4326)
california_boundary <- us_states[us_states$name == "California", ]
california_land <- st_intersection(land, california_boundary)
coastline <- st_boundary(california_boundary)
coastline_cropped <- st_crop(coastline, bbox)

# Sample points along coastline & offset inland
n_points <- 1000
coastline_densified <- st_segmentize(coastline_cropped, dfMaxLength = 1000)
coastline_lines <- st_cast(coastline_densified, "LINESTRING")
coastline_merged <- st_union(coastline_lines)
coastline_projected <- st_transform(coastline_merged, projected_crs)
coastline_samples <- st_line_sample(coastline_projected, n = n_points, type = "regular") |>
  st_cast("POINT") |> st_sf() |> st_transform(4326)
coords <- st_coordinates(st_transform(coastline_samples, projected_crs))
vectors <- diff(coords)
normals <- matrix(NA, nrow = nrow(vectors), ncol = 2)
for(i in 1:nrow(vectors)){
  v <- vectors[i,]
  norm <- sqrt(sum(v^2))
  normals[i,] <- if(norm == 0) c(1,0) else c(-v[2], v[1])/norm
}
normals <- rbind(normals, normals[nrow(normals),])
offset_coords <- coords + normals * 11
inland_points <- st_as_sf(data.frame(x = offset_coords[,1], y = offset_coords[,2]), coords = c("x","y"), crs = projected_crs)
inland_points <- st_transform(inland_points, 4326)

# Download DEM and get elevation
dem_raster <- get_elev_raster(bbox_poly, z = 14, src = "aws")
coastline_samples$coastal_elv <- raster::extract(dem_raster, coastline_samples)
coastline_samples$near_elv <- raster::extract(dem_raster, inland_points)
coastline_samples$slope <- abs(coastline_samples$near_elv - coastline_samples$coastal_elv) / 11
coastline_samples <- coastline_samples |> 
  filter(!is.na(coastal_elv), !is.na(near_elv), between(coastal_elv, -1, 10),
         between(near_elv, -1, 10),
         slope > 0, slope < 0.2) |> 
  mutate(point_id = row_number())

# Interpolate slope per parcel (weight by distance to coastal points)
redfin_proj <- st_transform(redfin_sf, projected_crs)
coast_proj <- st_transform(coastline_samples, projected_crs)
dist_mat <- st_distance(redfin_proj, coast_proj)

weighted_values <- function(dist_row, values, k = 5){
  nearest <- order(dist_row)[1:k]
  dists <- as.vector(dist_row[nearest])
  weights <- 1/(dists+1e-6)
  sum(values[nearest]*weights)/sum(weights)
}
redfin_sf$interp_slope <- apply(dist_mat, 1, weighted_values, values = coast_proj$slope, k = 5)

# Load NOAA buoy data
g <- 9.81
wave_data <- data.frame()
urlpt1 <- "https://www.ndbc.noaa.gov/view_text_file.php?filename=46053h"
urlp2 <- ".txt.gz&dir=data/historical/stdmet/"
for(year in 2000:2024){
  url <- paste0(urlpt1, year, urlp2)
  if(year >= 2007){
    data <- read.table(url, sep = "", header = FALSE, fill = TRUE)
    wave_tmp <- data %>%
      dplyr::select(V1,V2,V3,V4,V9,V10,V15) %>%
      rename(YYYY=V1, MM=V2, DD=V3, hh=V4, WVHT=V9, DPD=V10, WTMP=V15)
  } else {
    data <- read.table(url, sep = "", header = TRUE, fill = TRUE)
    wave_tmp <- data %>%
      dplyr::select(YYYY, MM, DD, hh, WVHT, DPD, WTMP)
  }
  wave_data <- bind_rows(wave_data, wave_tmp)
}
wave_df <- wave_data %>%
  filter(WVHT < 98, DPD < 99) %>%
  mutate(
    YYYY = as.integer(YYYY),
    WVHT = as.numeric(WVHT),
    DPD = as.numeric(DPD)
  )

# Save annual run-up for each parcel into one csv
results_list <- list()

for(i in 1:nrow(redfin_sf)){
  slope_i <- redfin_sf$interp_slope[i]
  # Calculate all run-ups for the parcel
  run_up <- 1.1 * (
    0.35 * slope_i * sqrt(wave_df$WVHT * (g * wave_df$DPD^2) / (2*pi)) +
      0.5 * sqrt(wave_df$WVHT * (g * wave_df$DPD^2) / (2*pi) * (0.563 * slope_i^2 + 0.004))
  )
  temp_df <- data.frame(
    parcel_id = redfin_sf$parcel_id[i],
    address = redfin_sf$ADDRESS[i],
    latitude = st_coordinates(redfin_sf)[i,2],
    longitude = st_coordinates(redfin_sf)[i,1],
    YYYY = wave_df$YYYY,
    run_up = run_up
  )
  # For each year, keep only the max run-up
  temp_max <- temp_df %>%
    group_by(YYYY) %>%
    summarise(
      parcel_id = first(parcel_id),
      address = first(address),
      latitude = first(latitude),
      longitude = first(longitude),
      max_run_up = max(run_up, na.rm = TRUE),
      .groups = "drop"
    )
  
  results_list[[i]] <- temp_max
}

all_runups_df <- do.call(rbind, results_list)
#Make sure to save to the right folder!!!
write_csv(all_runups_df, file.path("data/carpinteria", "run_up_parcel.csv"))
