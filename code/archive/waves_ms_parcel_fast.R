# parcel-specific wave run-up with vectorized Stockdon, caching, and clean outputs
rm(list = ls())
library(sf)
library(rnaturalearth)
library(rnaturalearthhires)
library(raster)
library(elevatr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)

bbox <- st_bbox(c(xmin = -119.55, ymin = 34.38, xmax = -119.52, ymax = 34.41), crs = st_crs(4326))
projected_crs <- 3857
out_dir <- "data/carpinteria"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)


# NOAA buoy
buoy_station <- "46053"
years <- 2000:2024

# Load Redfin parcels
redfin_df <- read_csv(file.path(out_dir, "redfin_df.csv"), show_col_types = FALSE)
if (!"property_id" %in% names(redfin_df)) {
  # create stable property_id if missing
  suppressPackageStartupMessages(library(digest))
  redfin_df <- redfin_df %>%
    mutate(property_id = digest(paste(ADDRESS,
                                      `ZIP OR POSTAL CODE`,
                                      round(LATITUDE, 6),
                                      round(LONGITUDE, 6))))
}


# keep flood-prone only
redfin_sf <- st_as_sf(redfin_df, coords = c("LONGITUDE", "LATITUDE"), crs = 4326, remove = FALSE) %>%
  filter(hazard_type == "flood")


# carry parcel_id forward from stable id
redfin_sf$parcel_id <- redfin_df$property_id[match(redfin_sf$ADDRESS, redfin_df$ADDRESS)]


# Coastline sampling + local slope
# Land & boundary
land <- ne_download(scale = 10, type = "land", category = "physical", returnclass = "sf") %>% st_transform(4326)
us_states <- ne_states(country = "United States of America", returnclass = "sf") %>% st_transform(4326)
california_boundary <- us_states[us_states$name == "California", ]
coastline <- st_boundary(california_boundary)

# Crop to AOI
bbox_poly <- st_as_sfc(bbox) %>% st_sf()
coastline_cropped <- suppressWarnings(st_crop(coastline, bbox))

# Sample regular points along coastline, offset ~11 m inland using a rotated tangent
n_points <- 1000
coastline_densified <- st_segmentize(coastline_cropped, dfMaxLength = 1000)
coastline_lines <- st_cast(coastline_densified, "LINESTRING")
coastline_merged <- st_union(coastline_lines)
coastline_projected <- st_transform(coastline_merged, projected_crs)
coastline_samples <- st_line_sample(coastline_projected, n = n_points, type = "regular") %>%
  st_cast("POINT") %>% st_sf() %>% st_transform(4326)

coords <- st_coordinates(st_transform(coastline_samples, projected_crs))
vectors <- diff(coords)
# normals (rotate 90°)
normals <- t(apply(vectors, 1, function(v){
  n <- sqrt(sum(v^2)); if (n == 0) return(c(1,0)); c(-v[2], v[1]) / n
}))
# repeat last normal to match rows
normals <- rbind(normals, normals[nrow(normals), ])

# First inland guess (11 m)
offset_coords <- coords + normals * 11
inland_points <- st_as_sf(data.frame(x = offset_coords[,1], y = offset_coords[,2]), coords = c("x","y"), crs = projected_crs) %>%
  st_transform(4326)

# DEM cache
.dem_path <- file.path(out_dir, "dem.tif")
if (file.exists(.dem_path)) {
  dem_raster <- raster::raster(.dem_path)
} else {
  dem_raster <- get_elev_raster(bbox_poly, z = 14, src = "aws")
  writeRaster(dem_raster, .dem_path, overwrite = TRUE)
}

# elevations
coastline_samples$coastal_elv <- raster::extract(dem_raster, coastline_samples)
coastline_samples$near_elv    <- raster::extract(dem_raster, inland_points)
coastline_samples$slope       <- abs(coastline_samples$near_elv - coastline_samples$coastal_elv) / 11

# Filter implausible values and add id
coastline_samples <- coastline_samples %>%
  filter(!is.na(coastal_elv), !is.na(near_elv),
         between(coastal_elv, -1, 10), between(near_elv, -1, 10),
         slope > 0, slope < 0.2) %>%
  mutate(point_id = row_number())

# Interpolate slope to parcels (distance-weighted K-NN)
redfin_proj <- st_transform(redfin_sf, projected_crs)
coast_proj  <- st_transform(coastline_samples, projected_crs)

message("Computing distance matrix (parcels × coastal samples)...")
dist_mat <- st_distance(redfin_proj, coast_proj)

weighted_values <- function(dist_row, values, k = 5){
  nearest <- order(dist_row)[1:k]
  dists <- as.numeric(dist_row[nearest])
  w <- 1/(dists + 1e-6)
  sum(values[nearest] * w) / sum(w)
}

redfin_sf$interp_slope <- apply(dist_mat, 1, weighted_values, values = coast_proj$slope, k = 5)
# fill any NA/Inf with global median
med_slope <- median(coast_proj$slope, na.rm = TRUE)
redfin_sf$interp_slope[!is.finite(redfin_sf$interp_slope)] <- med_slope
redfin_sf$interp_slope[is.na(redfin_sf$interp_slope)] <- med_slope

# ---------------------------
# 3) Buoy time series (cache)
# ---------------------------
cache_buoy <- file.path(out_dir, paste0("buoy_", buoy_station, "_", min(years), "_", max(years), ".csv"))
if (file.exists(cache_buoy)) {
  wave_df <- read_csv(cache_buoy, show_col_types = FALSE)
} else {
  g <- 9.81
  wave_data <- data.frame()
  urlpt1 <- paste0("https://www.ndbc.noaa.gov/view_text_file.php?filename=", buoy_station, "h")
  urlp2  <- ".txt.gz&dir=data/historical/stdmet/"
  for (year in years){
    url <- paste0(urlpt1, year, urlp2)
    message("Downloading NDBC ", buoy_station, " year ", year, "...")
    if (year >= 2007){
      data <- try(read.table(url, sep = "", header = FALSE, fill = TRUE), silent = TRUE)
      if (inherits(data, "try-error")) next
      wave_tmp <- dplyr::select(data, V1,V2,V3,V4,V9,V10,V15)
      names(wave_tmp) <- c("YYYY","MM","DD","hh","WVHT","DPD","WTMP")
    } else {
      data <- try(read.table(url, sep = "", header = TRUE, fill = TRUE), silent = TRUE)
      if (inherits(data, "try-error")) next
      wave_tmp <- dplyr::select(data, YYYY, MM, DD, hh, WVHT, DPD, WTMP)
    }
    wave_data <- bind_rows(wave_data, wave_tmp)
  }
  wave_df <- wave_data %>%
    filter(WVHT < 98, DPD < 99) %>%
    mutate(
      YYYY = as.integer(YYYY),
      WVHT = suppressWarnings(as.numeric(WVHT)),
      DPD  = suppressWarnings(as.numeric(DPD))
    ) %>%
    filter(is.finite(WVHT), is.finite(DPD))
  write_csv(wave_df, cache_buoy)
}

# ---------------------------
# 4) Vectorized Stockdon: annual max R, then parcel coefficient C(slope)
# ---------------------------
# R = sqrt( Hs * (g*Tp^2) / (2*pi) )
g <- 9.81
R <- sqrt(wave_df$WVHT * (g * wave_df$DPD^2) / (2*pi))
wave_R_yearmax <- tibble(YYYY = wave_df$YYYY, R = R) %>%
  group_by(YYYY) %>%
  summarise(R_year_max = max(R, na.rm = TRUE), .groups = "drop")

# C(slope) = 0.35*s + 0.5*sqrt(0.563*s^2 + 0.004)
C <- function(s) 0.35*s + 0.5*sqrt(0.563*s^2 + 0.004)
C_i <- C(redfin_sf$interp_slope)                              # n_parcels
Y   <- sort(unique(wave_R_yearmax$YYYY))                      # n_years
R_y <- wave_R_yearmax$R_year_max[match(Y, wave_R_yearmax$YYYY)]

# Outer product (parcels × years)
runup_mat <- 1.1 * (C_i %o% R_y)

# ---------------------------
# 5) Long table output (annual maxima per parcel)
# ---------------------------
coords_parc <- st_coordinates(redfin_sf)
all_runups_df <- as_tibble(runup_mat) %>%
  mutate(parcel_id = redfin_sf$parcel_id,
         address   = redfin_sf$ADDRESS,
         latitude  = coords_parc[,2],
         longitude = coords_parc[,1]) %>%
  relocate(parcel_id, address, latitude, longitude) %>%
  tidyr::pivot_longer(cols = -c(parcel_id, address, latitude, longitude),
                      names_to = "col", values_to = "max_run_up") %>%
  mutate(YYYY = Y[as.integer(gsub("V", "", col))]) %>%
  select(parcel_id, address, latitude, longitude, YYYY, max_run_up) %>%
  arrange(parcel_id, YYYY)

out_csv <- file.path(out_dir, "run_up_parcel.csv")
write_csv(all_runups_df, out_csv)
message("Saved parcel run-up to: ", out_csv)