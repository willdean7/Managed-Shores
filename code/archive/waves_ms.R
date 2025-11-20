#Jonah Danziger
#Wave data
rm(list=ls()) # clear the environment

library(tidyverse)
library(lubridate)
library(janitor)
library(rvest)
library(parsedate)
library(pracma)
library(readxl)
library(elevatr)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(raster)
library(lwgeom)
library(nngeo)
library(foreach)
library(grid) 



#Estimating the Damage function
#The first part is to estimate slope to get the alphas for the wave run up function
# Download California shapefile

# Download Natural Earth land data
land <- ne_download(scale = 10, type = "land", category = "physical")

# Download US state boundaries using rnaturalearthhires
us_states <- ne_states(country = "United States of America", returnclass = "sf")
# Filter for California
california_boundary <- us_states[us_states$name == "California", ]

# Transform the data to the same CRS
land <- st_transform(land, crs = st_crs(4326))
california_boundary <- st_transform(california_boundary, crs = st_crs(4326))

# Clip the land data to the California boundary
california_land <- st_intersection(land, california_boundary)
# Plot the California shapefile with ggplot2
ggplot(data = california_land) +
  geom_sf(fill = "lightblue", color = "black") +
  labs(title = "Map of California", 
       caption = "Source: Natural Earth GITHUB") +
  theme_minimal()
# Download coastline data
coastline <- st_boundary(california_boundary )

# Define the bounding box (xmin, ymin, xmax, ymax) using the max and min coordinates of case study location
#!!!!!!You can set this to be whatever area you want and it figure this out
bbox <- st_bbox(c(xmin = -119.55, ymin = 34.38, xmax = -119.52, ymax = 34.41), crs = st_crs(4326))

# Generate DEM for the case study area
# Manually construct a polygon from bbox coordinates
bbox_coords <- matrix(
  c(
    bbox["xmin"], bbox["ymin"],
    bbox["xmin"], bbox["ymax"],
    bbox["xmax"], bbox["ymax"],
    bbox["xmax"], bbox["ymin"],
    bbox["xmin"], bbox["ymin"]  # close the polygon
  ),
  ncol = 2,
  byrow = TRUE
)

bbox_poly <- st_sf(
  geometry = st_sfc(st_polygon(list(bbox_coords))),
  crs = st_crs(4326)
)

# Generate DEM from polygon
dem <- get_elev_raster(locations = bbox_poly, z = 14, src = "aws")

# Save DEM to disk .... make sure to put in appropriate folder!!!
writeRaster(dem, "data/carpinteria/dem.tif", overwrite = TRUE)

# Crop the coastline data to the bounding box
coastline_cropped <- st_crop(coastline, bbox)

# Define the resolution of the grid
res <- 0.0025  # resolution in degrees


# Create the grid
grid <- expand.grid(
  lon = seq(bbox['xmin'], bbox['xmax'], by = res),
  lat = seq(bbox['ymin'], bbox['ymax'], by = res)
)

# Convert the grid to an sf object
grid_sf <- st_as_sf(grid, coords = c("lon", "lat"), crs = 4326)

# Plot the grid on top of the cropped coastline
ggplot() +
  geom_sf(data = coastline_cropped, color = "red") +
  geom_sf(data = grid_sf, color = "blue", size = 0.5) +
  labs(title = "Cropped Coastline with Grid",
       caption = "Source: US Census Bureau TIGER/Line Shapefiles") +
  theme_minimal()

grid_sf<-st_intersection(grid_sf, california_land)
#elev_df<-read_csv("C:/Users/jonah/Documents/Research/Wetland Restoration/huntingelev_df.csv")

# Plot the grid on top of the cropped coastline
ggplot() +
  geom_sf(data = coastline_cropped, color = "red") +
  geom_sf(data = grid_sf, color = "blue", size = 0.5) +
  labs(title = "Cropped Coastline with Grid",
       caption = "Source: US Census Bureau TIGER/Line Shapefiles") +
  theme_minimal()

# User-defined number of points to generate
n_points <- 1000  # Change this to control how many points you want

# Choose a projected CRS (e.g., Web Mercator)
projected_crs <- 3857

# Densify the coastline (in lon/lat) for better geometry shape
coastline_densified <- st_segmentize(coastline_cropped, dfMaxLength = 1000)

# Convert to LINESTRING (if it's a collection)
coastline_lines <- st_cast(coastline_densified, "LINESTRING")

# Merge all lines
coastline_merged <- st_union(coastline_lines)

# Reproject to a projected CRS to allow distance-based sampling
coastline_projected <- st_transform(coastline_merged, crs = projected_crs)

# Sample n regularly spaced points
coastline_manypoints <- st_line_sample(coastline_projected, n = n_points, type = "regular")

# Convert to POINTS and transform back to WGS84 (lat/lon)
coastline_manypoints <- st_cast(coastline_manypoints, "POINT")
coastline_manypoints <- st_transform(coastline_manypoints, crs = 4326)

# Create a dataframe with lat/lon
coastline_points_df <- st_as_sf(coastline_manypoints) %>%
  mutate(
    lon = st_coordinates(.)[, 1],
    lat = st_coordinates(.)[, 2]
  )

# Optional: convert to pure dataframe (drop sf geometry column if needed)
coastline_points_df <- as.data.frame(coastline_points_df) |> st_as_sf()

# Ensure coastline_points_df is in a projected CRS so we can measure meters
coastline_projected <- st_transform(coastline_points_df, crs = 3857)

# Convert to matrix of coordinates
coords <- st_coordinates(coastline_projected)

# Calculate direction vectors (tangents)
vectors <- diff(coords)

# Normalize and rotate each vector to get normal vectors (pointing roughly inland)
normals <- apply(vectors, 1, function(v) {
  v <- v / sqrt(sum(v^2))                # normalize
  c(-v[2], v[1])                          # rotate 90 degrees (inland guess)
})
normals <- t(normals)

# Repeat last normal so length matches number of points
normals <- rbind(normals, normals[nrow(normals), ])

# Offset the points inland by 10 meters
offset_coords <- coords + normals * 11

# Create inland points as sf POINT geometry
inland_points <- st_as_sf(
  data.frame(x = offset_coords[, 1], y = offset_coords[, 2]),
  coords = c("x", "y"),
  crs = 3857
)

# Transform back to WGS84
inland_points_wgs84 <- st_transform(inland_points, crs = 4326)


# Get elevation using elevatr (make sure elevatr is installed)
in_elevation_data <- get_elev_point(locations = inland_points_wgs84,z=14, src = "aws")

# Combine with coastal data if needed
inland_with_elevation <- inland_points_wgs84 %>%
  mutate(elevation = in_elevation_data$elevation)

# Get elevation using elevatr (make sure elevatr is installed)
elevation_data <- get_elev_point(locations = coastline_points_df,z=14, src = "aws")

#Putting together the pieces for finding the slope
coastline_points_df <- coastline_points_df|>
  mutate(coastal_elv=elevation_data$elevation, near_elv = inland_with_elevation$elevation)|>
  mutate(slope = abs((near_elv-coastal_elv)/11))

###You can change the buoy that this is taken from 
#Importing wave data
#Retrieve data from this link https://www.ndbc.noaa.gov/obs.shtml
urlpt1="https://www.ndbc.noaa.gov/view_text_file.php?filename=46053h" #Change the station number
urlp2=".txt.gz&dir=data/historical/stdmet/"
# Define the column names
column_names <- c("#YY", "MM", "DD", "hh", "WVHT", "DPD", "WTMP")
# Create an empty dataframe with these column names
wave_data <- data.frame(matrix(ncol = length(column_names), nrow = 0))
names(wave_data) <- column_names
for(i in 2000:2024){
  url<-paste0(urlpt1, i, urlp2)
  print(i)

  if (i>=2007){
    data <- read.table(url, sep = "")

    wave_tmp<-data|>dplyr::select(V1, V2,V3, V4, V9, V10,V15)|>
      rename(YYYY=V1, MM=V2, DD=V3, hh=V4, WVHT=V9, DPD=V10, WTMP=V15)
  }else{
    data <- read.table(url, header = TRUE, sep = "", fill = TRUE)
    wave_tmp<-data|>dplyr::select("YYYY", "MM", "DD", "hh", "WVHT", "DPD", "WTMP")
  }
  wave_data<-rbind(wave_data, wave_tmp)
}

#slope 
slope = median(coastline_points_df$slope)
#Using the run-up equation 
#wave_df <- wave_data |> filter(WVHT<98 & DPD<99)|>mutate(run_up=1.1*(0.35*slope*(WVHT*(g*DPD**2)/(2*pi))**0.5+0.5*(WVHT*(g*DPD**2)/(2*pi)*(0.563*(slope**2)+0.004))**0.5))

# --- Monte Carlo Simulation for Uncertainty in Run-Up ---
set.seed(123)   # for reproducibility
n_sims <- 1000  # number of Monte Carlo draws

# Parameters
slope_mean <- median(coastline_points_df$slope)
slope_sd   <- 0.0075  # midpoint of 0.005–0.01
g <- 9.81

# Clean buoy data first
wave_df <- wave_data |> 
  filter(WVHT < 98 & DPD < 99)

# Monte Carlo: resample slope + buoy conditions
mc_results <- map_dfr(1:n_sims, function(i) {
  # draw slope with Gaussian error
  slope_i <- rnorm(1, mean = slope_mean, sd = slope_sd)
  slope_i <- max(slope_i, 0.0001)  # prevent negatives / zero slope
  
  # resample waves with replacement
  wave_sample <- wave_df %>%
    slice_sample(n = nrow(wave_df), replace = TRUE)
  
  # compute run-up for this simulation
  run_up_i <- 1.1 * (
    0.35 * slope_i * sqrt(wave_sample$WVHT * (g * wave_sample$DPD^2) / (2*pi)) +
      0.5 * sqrt(wave_sample$WVHT * (g * wave_sample$DPD^2) / (2*pi) * (0.563 * slope_i^2 + 0.004))
  )
  
  tibble(
    sim = i,
    YYYY = wave_sample$YYYY,
    run_up = run_up_i
  )
})

mc_yearly <- mc_results %>%
  group_by(sim, YYYY) %>%
  summarise(max_runup = max(run_up, na.rm = TRUE), .groups = "drop")

# Distribution of annual maxima across simulations
mc_summary <- mc_yearly %>%
  group_by(YYYY) %>%
  summarise(
    runup_mean   = mean(max_runup, na.rm = TRUE),
    runup_median = median(max_runup, na.rm = TRUE),
    runup_p5     = quantile(max_runup, 0.05, na.rm = TRUE),
    runup_p95    = quantile(max_runup, 0.95, na.rm = TRUE),
    runup_max    = max(max_runup, na.rm = TRUE),
    .groups = "drop"
  )

# Deterministic run-up (using median slope, no resampling)
det_wave_df <- wave_df |>
  mutate(run_up = 1.1 * (
    0.35 * slope * sqrt(WVHT * (g * DPD^2) / (2*pi)) +
      0.5 * sqrt(WVHT * (g * DPD^2) / (2*pi) * (0.563 * slope^2 + 0.004))
  ))

# Deterministic annual maxima (for comparison)
det_max_wave <- det_wave_df |> 
  group_by(YYYY) |> 
  summarise(run_up = max(run_up, na.rm = TRUE))

# Plot with uncertainty ribbons
ggplot(mc_summary, aes(x = YYYY, y = runup_mean)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = runup_p5, ymax = runup_p95), alpha = 0.2, fill = "skyblue") +
  labs(title = "Annual Max Wave Run-Up with Uncertainty",
       x = "Year", y = "Run-Up (m)") +
  theme_minimal()

# Save in same 24-row format
write.csv(det_max_wave, file = "data/carpinteria/run_up_deterministic.csv", row.names = FALSE)
write.csv(mc_summary,   file = "data/carpinteria/run_up_uncertainty.csv",   row.names = FALSE)

####OLD CODE DO NOT USE
# #WBHT is defined as the shoaling-only estimte wave height at breaking
# #Kr is the refraction coefficient
# #WSHT is defined as the estimated surf heights
# wave_df <- wave_data|> mutate(WBHT=(WVHT**(4/5))*((1/sqrt(g))*(g*DPD/(4*pi))**(2/5))) |> #Estimated Wave Height at Breaking
#   mutate(Kr=-0.0013*WBHT**2+0.1262*WBHT+0.3025) |> mutate(WSHT=WBHT*Kr) |> 
#   filter(WVHT<99)
# 
# #Wave Run Up
# #max wave data example
# max_wave_df<-wave_df|>group_by(YYYY)|>
#   filter(WVHT==max(WVHT))|>
#   ungroup()
# slope <- mean(nearest_df$slope)
# wave_names<-c(colnames(nearest_df), colnames(max_wave_df))
# max_day_wave<-max_wave_df[1,]
# 
# run_up_data_temp<-nearest_df|> mutate(run_up=8*max_day_wave$WSHT[1]*slope)
# run_up_data_temp<-cbind(run_up_data_temp, max_day_wave)
# 
# 
# run_up_data <- data.frame(matrix(ncol = length(column_names), nrow = 0))
# for (i in 1:nrow(max_wave_df)){
#   max_day_wave<-max_wave_df[i,]
#   run_up_data_temp<-nearest_df|> mutate(run_up=8*max_day_wave$WSHT[1]*slope)|>
#     cbind(max_day_wave)
#   run_up_data<-rbind(run_up_data,run_up_data_temp)
#   if (i%%10==0){
#     print(paste("Progress is",i,"out of", nrow(max_wave_df)))
#   }
# }
