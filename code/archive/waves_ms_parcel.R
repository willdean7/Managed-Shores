#Author: Will Dean
#      Adapted from 'waves_ms.R' by Jonah Danzinger
# Purpose:
#   Parcel-specific annual maximum wave run-up estimation for coastal properties.
#
# Where this fits:
#   This script turns open-ocean wave conditions (from an NDBC buoy) into
#   parcel-level run-up estimates by inferring a local beach slope for each
#   property, then applying a Stockdon-style run-up formula to a long wave record.
#   The output is a table of annual maximum run-up per parcel, which feeds
#   damages-vs-income (NPV) modeling and Shiny visualization.
#
# What it does:
#   (1) Study area & parcels: read Redfin properties, restrict to flood-prone parcels.
#   (2) Coast geometry: sample points along the shoreline and step inland to estimate
#       a representative nearshore slope from DEM elevations.
#   (3) Local slope at parcels: interpolate a parcel-specific slope from nearby
#       sampled coast points (inverse-distance weighting).
#   (4) Wave record: download historical significant wave height & period from NDBC
#       buoy 46053 and clean it.
#   (5) Run-up per parcel: combine each parcel’s local slope with each buoy record
#       to compute run-up, then collapse to ANNUAL MAX per parcel.
#   (6) Save: write a CSV for downstream modeling/visualization.
#
# Key assumptions & caveats:
#   - Slope proxy: The beach “face” slope is approximated by Δz over an ~11 m
#     inland offset from the shoreline sample. This is a pragmatic slope proxy
#     (not a full profile) and assumes the DEM represents the active foreshore.
#   - Vertical datum: Stockdon comparisons are typically relative to mean sea level; 
#      we use run-up as a relative metric for parcel ranking and damages modeling.
#   - Offshore waves: Single-buoy forcing (NDBC 46053) is used as a proxy for the
#     local wave climate; site-specific refraction/shoaling is not modeled here.
#   - CRS/units: Use a projected CRS (EPSG:3857) for any meter-based distances/
#     offsets, and WGS84 (EPSG:4326) for I/O and visualization.

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

# STUDY AREA & PARCELS
# - Define a bounding box for data fetches and quick cropping.
# - Read Redfin parcels; keep both geometry and original lon/lat columns.
# - Filter to flood-prone parcels, since run-up is primarily a flooding driver.
# Bounding box (WGS84). Adjust to study area!!!
bbox <- st_bbox(c(xmin = -119.55, ymin = 34.38, xmax = -119.52, ymax = 34.41), crs = st_crs(4326))
bbox_poly   <- st_as_sfc(bbox) |> st_sf()

# Web Mercator used for distance/offset math (meters)
projected_crs <- 3857

# Redfin points with lat/lon. Keep both geometry and original columns.
redfin_df <- read_csv("data/carpinteria/redfin_df.csv") %>% filter(!is.na(rentalval))
# Convert to sf object
redfin_sf <- st_as_sf(redfin_df, coords = c("LONGITUDE", "LATITUDE"), crs = 4326, remove = FALSE)

# Create a simple parcel_id index
redfin_sf$parcel_id <- seq_len(nrow(redfin_sf))

# Focus on properties flagged as flood-prone
redfin_sf <- redfin_sf[redfin_sf$hazard_type == "flood", ]

# COASTLINE GEOMETRY
# - Extract California, crop to bbox, and get a coastline polyline.
# - Densify and regularly sample points along the shoreline.
# - Compute a small inland offset along unit normals to approximate a foreshore
#   elevation change (Δz), used as a slope proxy.

# Get California land polygon and boundary, clip to bbox, take coastline
land        <- ne_download(scale = 10, type = "land", category = "physical") |> st_transform(4326)
us_states   <- ne_states(country = "United States of America", returnclass = "sf") |> st_transform(4326)
california  <- us_states[us_states$name == "California", ]
cal_land    <- st_intersection(land, california)

# Boundary of California; then crop to bbox to reduce work
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

# Compute unit normals along the sampled polyline to step ~11 m inland to get a pair of points for a slope proxy (Δz / 11 m)
coords   <- st_coordinates(st_transform(coast_samples, projected_crs))
vectors  <- diff(coords)     # segment vectors between points
normals  <- matrix(NA, nrow = nrow(vectors), ncol = 2)

for (i in seq_len(nrow(vectors))) {
  v      <- vectors[i, ]
  norm   <- sqrt(sum(v^2))
  # Rotate by 90° and normalize; default to (1, 0) if degenerate segment
  normals[i, ] <- if (norm == 0) c(1, 0) else c(-v[2], v[1]) / norm
}
# Repeat last normal so lengths align with sampled points
normals <- rbind(normals, normals[nrow(normals), ])

# Apply ~11 m inland offset (in projected meters), then go back to WGS84
offset_coords <- coords + normals * 11
inland_points <- st_as_sf(data.frame(x = offset_coords[, 1], y = offset_coords[, 2]),
                          coords = c("x", "y"), crs = projected_crs) |>
  st_transform(4326)

# ELEVATION & BEACH SLOPE (from DEM)
# - Pull a DEM for the bbox (AWS source via {elevatr}); z controls resolution.
# - Extract elevations at coast samples and inland offset points.
# - Compute slope = |Δz| / 11 m as a working proxy for the beach/foreshore slope.
# - Basic QA/QC filters to remove odd elevations or outlier slopes.

# DEM fetch; z=14 is a reasonable tradeoff for speed vs detail
dem_raster <- get_elev_raster(bbox_poly, z = 14, src = "aws")

# Extract elevation at the shoreline sample and at offset inland points
suppressWarnings({
  coast_samples$coastal_elv <- raster::extract(dem_raster, coast_samples)
  coast_samples$near_elv    <- raster::extract(dem_raster, inland_points)
})

# Approximate beach slope between coast point and inland point (Δz / 11 m)
coast_samples$slope <- abs(coast_samples$near_elv - coast_samples$coastal_elv) / 11

# Basic QA/QC
# Keep plausible nearshore elevations (approx. -1 to 10 m)
# Restrict slopes to a working beach range (0–20%)
coast_samples <- coast_samples |>
  filter(
    !is.na(coastal_elv), !is.na(near_elv),
    between(coastal_elv, -1, 10),     
    between(near_elv,   -1, 10),
    slope > 0, slope < 0.2            
  ) |>
  mutate(point_id = dplyr::row_number())

# PARCEL-SPECIFIC SLOPE
# For each parcel, compute an inverse-distance-weighted average of the k
# nearest coastal sample slopes. This localizes the beach slope to the parcel.
redfin_proj <- st_transform(redfin_sf, projected_crs)
coast_proj2 <- st_transform(coast_samples, projected_crs)

# Distance matrix: parcels (rows) × coastal sample points (cols) in meters
dist_mat    <- st_distance(redfin_proj, coast_proj2)

weighted_values <- function(dist_row, values, k = 5) {
  nearest <- order(dist_row)[1:k]
  dists   <- as.vector(dist_row[nearest])
  w       <- 1 / (dists + 1e-6)           # small epsilon to avoid 1/0
  sum(values[nearest] * w) / sum(w)
}

# Parcel-specific interpolated slope
redfin_sf$interp_slope <- apply(dist_mat, 1, weighted_values,
                                values = coast_proj2$slope, k = 5)

# WAVE CLIMATE (NDBC 46053) & CLEANING
# Build a long historical record of significant wave height (WVHT) and
#   dominant period (DPD) from 2000–2024.
# Handle format differences pre/post 2007, and remove sentinel/missing codes.

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

#RUN-UP BY PARCEL (Stockdon-style) → ANNUAL MAX
#  For each buoy observation, compute run-up using the parcel’s local slope
#  Collapse to annual maxima (key summary metric for damages/thresholds)

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