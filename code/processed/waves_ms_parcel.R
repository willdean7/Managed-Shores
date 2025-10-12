#Will Dean
# This script is built off of the waves_ms.R script to
# estimate annual maximum wave run-up for EACH PROPERTY PARCEL rather than
# using a single coastline-wide value. Sample the coastline, infer a
# representative local beach slope, use NOAA buoy waves to compute run-up,
# and save annual maxima by parcel.

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

# Study area / projections
# Bounding box (WGS84). Adjust to study area!!!
bbox <- st_bbox(c(xmin = -119.55, ymin = 34.38, xmax = -119.52, ymax = 34.41), crs = st_crs(4326))
bbox_poly   <- st_as_sfc(bbox) |> st_sf()

# Web Mercator used for distance/offset math (meters)
projected_crs <- 3857

# Redfin points with lat/lon. Keep both geometry and original columns.
redfin_df <- read_csv("data/carpinteria/redfin_df.csv")
redfin_sf <- st_as_sf(redfin_df, coords = c("LONGITUDE", "LATITUDE"), crs = 4326, remove = FALSE)

# Create a simple parcel_id index if one doesn't exist
redfin_sf$parcel_id <- seq_len(nrow(redfin_sf))

# Focus: only properties flagged as flood-prone
redfin_sf <- redfin_sf[redfin_sf$hazard_type == "flood", ]

# Coastline extraction
# Get California land polygon and boundary, 2) clip to bbox, 3) take coastline
land        <- ne_download(scale = 10, type = "land", category = "physical") |> st_transform(4326)
us_states   <- ne_states(country = "United States of America", returnclass = "sf") |> st_transform(4326)
california  <- us_states[us_states$name == "California", ]
cal_land    <- st_intersection(land, california)

coastline           <- st_boundary(california)
coastline_cropped   <- st_crop(coastline, bbox)

# Sample points along the coast and compute an inland offset
# Densify the coast to ensure fairly even spacing; sample ~n_points along line.
n_points            <- 1000
coast_densified     <- st_segmentize(coastline_cropped, dfMaxLength = 1000)
coast_lines         <- st_cast(coast_densified, "LINESTRING")
coast_merged        <- st_union(coast_lines)

# Work in meters for offsets and distances
coast_proj          <- st_transform(coast_merged, projected_crs)
coast_samples       <- st_line_sample(coast_proj, n = n_points, type = "regular") |>
  st_cast("POINT") |>
  st_sf() |>
  st_transform(4326)

# Compute unit normals along the sampled polyline to step ~11 m inland
coords   <- st_coordinates(st_transform(coast_samples, projected_crs))
vectors  <- diff(coords)
normals  <- matrix(NA, nrow = nrow(vectors), ncol = 2)

for (i in seq_len(nrow(vectors))) {
  v      <- vectors[i, ]
  norm   <- sqrt(sum(v^2))
  # Rotate by 90° and normalize; default to (1, 0) if degenerate segment
  normals[i, ] <- if (norm == 0) c(1, 0) else c(-v[2], v[1]) / norm
}
# Repeat last normal so lengths align with sampled points
normals <- rbind(normals, normals[nrow(normals), ])

# Apply ~11 m inland offset (in projected coordinates)
offset_coords <- coords + normals * 11
inland_points <- st_as_sf(data.frame(x = offset_coords[, 1], y = offset_coords[, 2]),
                          coords = c("x", "y"), crs = projected_crs) |>
  st_transform(4326)

# Elevation/slope from DEM
# Pull a fairly high-res DEM (AWS source). z controls resolution (higher = finer).
dem_raster <- get_elev_raster(bbox_poly, z = 14, src = "aws")

# Extract elevation at the shoreline sample and at offset inland points
# (suppress warnings when points fall just outside raster bounds)
suppressWarnings({
  coast_samples$coastal_elv <- raster::extract(dem_raster, coast_samples)
  coast_samples$near_elv    <- raster::extract(dem_raster, inland_points)
})

# Approximate beach slope between coast point and inland point (Δz / 11 m)
coast_samples$slope <- abs(coast_samples$near_elv - coast_samples$coastal_elv) / 11

# Basic QA/QC: drop weird elevations or zero/steep slopes unlikely to be beach faces
coast_samples <- coast_samples |>
  filter(
    !is.na(coastal_elv), !is.na(near_elv),
    between(coastal_elv, -1, 10),     # meters NAVD88-ish range guard
    between(near_elv,   -1, 10),
    slope > 0, slope < 0.2            # 0–20% slope as a working beach range
  ) |>
  mutate(point_id = dplyr::row_number())

# Interpolate a local beach slope for each parcel
# Use inverse-distance-weighted average of the k nearest coastal sample points.
redfin_proj <- st_transform(redfin_sf, projected_crs)
coast_proj2 <- st_transform(coast_samples, projected_crs)
dist_mat    <- st_distance(redfin_proj, coast_proj2)  # (n_parcels × n_samples) meters

weighted_values <- function(dist_row, values, k = 5) {
  nearest <- order(dist_row)[1:k]
  dists   <- as.vector(dist_row[nearest])
  w       <- 1 / (dists + 1e-6)           # small epsilon to avoid 1/0
  sum(values[nearest] * w) / sum(w)
}

redfin_sf$interp_slope <- apply(dist_mat, 1, weighted_values,
                                values = coast_proj2$slope, k = 5)

# NOAA buoy waves (NDBC 46053) 
# Build a long historical record of significant wave height (WVHT) and swell period (DPD).
# Notes: The NDBC historical files changed format in ~2007—handle both versions.
g         <- 9.81
wave_data <- data.frame()

urlpt1    <- "https://www.ndbc.noaa.gov/view_text_file.php?filename=46053h"
urlp2     <- ".txt.gz&dir=data/historical/stdmet/"

for (year in 2000:2024) {
  url <- paste0(urlpt1, year, urlp2)
  # Older vs newer header formats
  if (year >= 2007) {
    dat <- try(read.table(url, sep = "", header = FALSE, fill = TRUE), silent = TRUE)
    if (inherits(dat, "try-error")) next
    wave_tmp <- dat |>
      dplyr::select(V1, V2, V3, V4, V9, V10, V15) |>
      dplyr::rename(YYYY = V1, MM = V2, DD = V3, hh = V4, WVHT = V9, DPD = V10, WTMP = V15)
  } else {
    dat <- try(read.table(url, sep = "", header = TRUE, fill = TRUE), silent = TRUE)
    if (inherits(dat, "try-error")) next
    wave_tmp <- dat |>
      dplyr::select(YYYY, MM, DD, hh, WVHT, DPD, WTMP)
  }
  wave_data <- bind_rows(wave_data, wave_tmp)
}

# Clean out sentinel/missing codes and coerce types
wave_df <- wave_data |>
  filter(WVHT < 98, DPD < 99) |>
  mutate(
    YYYY = as.integer(YYYY),
    WVHT = as.numeric(WVHT),   # significant wave height (m)
    DPD  = as.numeric(DPD)     # dominant period (s)
  )

# Run-up calculation per parcel
# Using a Stockdon empirical form. For each
# buoy record, compute run-up given the parcel’s interpolated local slope.
# Then we collapse to ANNUAL MAXIMUM run-up for that parcel/year.

results_list <- vector("list", nrow(redfin_sf))

for (i in seq_len(nrow(redfin_sf))) {
  slope_i <- redfin_sf$interp_slope[i]
  
  # Guard: if slope is missing/NA, skip this parcel
  if (!is.finite(slope_i)) next
  
  # Deep-water wave length L0 ≈ g * T^2 / (2π)
  L0 <- (g * (wave_df$DPD^2)) / (2 * pi)
  
  # Empirical run-up (meters). Factors mirror your original formula.
  run_up <- 1.1 * (
    0.35 * slope_i * sqrt(wave_df$WVHT * L0) +
      0.5  * sqrt(wave_df$WVHT * L0 * (0.563 * slope_i^2 + 0.004))
  )
  
  # Long table of all hourly/daily run-up estimates tied to this parcel
  temp_df <- data.frame(
    parcel_id = redfin_sf$parcel_id[i],
    address   = redfin_sf$ADDRESS[i],
    latitude  = st_coordinates(redfin_sf)[i, 2],
    longitude = st_coordinates(redfin_sf)[i, 1],
    YYYY      = wave_df$YYYY,
    run_up    = run_up
  )
  
  # Collapse to annual maxima for this parcel
  temp_max <- temp_df |>
    group_by(YYYY) |>
    summarise(
      parcel_id  = dplyr::first(parcel_id),
      address    = dplyr::first(address),
      latitude   = dplyr::first(latitude),
      longitude  = dplyr::first(longitude),
      max_run_up = max(run_up, na.rm = TRUE),
      .groups    = "drop"
    )
  
  results_list[[i]] <- temp_max
}

# Stack all parcels together
all_runups_df <- bind_rows(results_list)

# This CSV is what the shiny dashboard reads later to build parcel-specific run-up
# histories (used to compute expected damages by parcel).
# Make sure the folder exists before writing. Make sure you are selecting the proper output folder !!!
out_dir <- "data/carpinteria"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
write_csv(all_runups_df, file.path(out_dir, "run_up_parcel.csv"))