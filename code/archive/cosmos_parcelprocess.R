# cosmos_parcelprocess.R
# Extract CoSMoS flood metrics (depth, duration, wave height) at parcel points.
# Writes: data/<site>/derived/cosmos_flood_metrics.csv
# Author: Will Dean (Managed Shores)

rm(list=ls())
library(tidyverse)
library(sf)
library(terra)
library(janitor)


# CONFIG
location   <- "carpinteria"                          # change for other sites
data_dir   <- file.path("data", location)
cosmos_dir <- file.path(data_dir, "cosmos")
paths <- list(
  redfin_df       = file.path(data_dir, "redfin_df.csv"),
  flood_depth_dir = file.path(cosmos_dir, "flood_depth"),     # expects subdirs w000, w100
  flood_dur_dir   = file.path(cosmos_dir, "flood_duration"),  # expects subdirs w000, w100
  wave_ht_dir     = file.path(cosmos_dir, "wave_ht")          # expects subdirs w000, w100
)

# clip to a bbox to speed up (adjust per site)
bbox4326 <- st_bbox(c(xmin=-119.55, ymin=34.38, xmax=-119.52, ymax=34.41), crs = st_crs(4326))

# How to parse SLR bin from filenames, e.g. "...SLR025..." -> 25 cm
slr_token_regex <- "(?i)slr\\d{3}"

# Load parcels
stopifnot(file.exists(paths$redfin_df))
parcels_raw <- readr::read_csv(paths$redfin_df, show_col_types = FALSE)

stopifnot(all(c("LONGITUDE","LATITUDE") %in% names(parcels_raw)))
parcels_sf <- parcels_raw |>
  filter(!is.na(LONGITUDE), !is.na(LATITUDE)) |>
  st_as_sf(coords = c("LONGITUDE","LATITUDE"), crs = 4326)

# clip
parcels_sf <- st_crop(parcels_sf, bbox4326)

# stable parcel_id if missing
if (!"parcel_id" %in% names(parcels_sf)) {
  parcels_sf$parcel_id <- seq_len(nrow(parcels_sf))
}

# Keep a simple id/name join for later sanity checks
parcels_key <- parcels_sf |>
  st_drop_geometry() |>
  select(parcel_id, ADDRESS, `ZIP OR POSTAL CODE`)

message("Parcels loaded: ", nrow(parcels_sf))

# Helpers to index rasters, extract values

index_metric <- function(dir_path, slr_regex = slr_token_regex) {
  if (!dir.exists(dir_path)) {
    return(tibble(path = character(), wclass = character(), slr_cm = integer()))
  }
  files <- list.files(dir_path, recursive = TRUE, full.names = TRUE,
                      pattern = "\\.(tif|tiff|img|vrt)$")
  if (!length(files)) {
    return(tibble(path = character(), wclass = character(), slr_cm = integer()))
  }
  tibble(path = files) |>
    mutate(
      wclass = case_when(
        grepl("/w000/", path) ~ "w000",
        grepl("/w100/", path) ~ "w100",
        TRUE ~ NA_character_
      ),
      slr_token = stringr::str_extract(basename(path), slr_regex),
      slr_cm = suppressWarnings(as.integer(stringr::str_replace_all(slr_token, "(?i)slr", "")))
    ) |>
    filter(!is.na(wclass), !is.na(slr_cm))
}

# CoSMoS rasters sometimes encode NoData as very large unsigned ints (e.g., 4294967296)
.clean_nodata <- function(x) {
  x <- as.numeric(x)
  x[!is.finite(x)] <- NA_real_
  x[x > 1e6] <- NA_real_   # treat absurdly large values as NA (then -> 0 later)
  x
}


extract_points <- function(index_tbl, pts_sf, value_name) {
  if (nrow(index_tbl) == 0L) {
    return(tibble(parcel_id = integer(), slr_cm = integer(), storm = character(), !!value_name := numeric()))
  }
  purrr::map_dfr(seq_len(nrow(index_tbl)), function(i) {
    r <- terra::rast(index_tbl$path[i])
    pts <- sf::st_transform(pts_sf, crs = sf::st_crs(r)$wkt)
    vals_raw <- terra::extract(r, terra::vect(pts))[, 2]
    vals <- .clean_nodata(vals_raw)           # <<< clean here
    tibble::tibble(
      parcel_id = pts_sf$parcel_id,
      slr_cm    = index_tbl$slr_cm[i],
      storm     = index_tbl$wclass[i],
      !!value_name := vals
    )
  })
}

to_wide_by_metric <- function(tbl, metric_col) {
  # One row per parcel_id × slr_cm × storm; values are numeric
  # Clean negatives/NA as 0 for metrics that should not be negative
  tbl |>
    mutate(
      !!metric_col := dplyr::if_else(is.finite(.data[[metric_col]]) & .data[[metric_col]] >= 0,
                                     .data[[metric_col]], 0)
    )
}

# Index and extract each metric
idx_depth <- index_metric(paths$flood_depth_dir)
idx_dur   <- index_metric(paths$flood_dur_dir)
idx_wave  <- index_metric(paths$wave_ht_dir)

if (nrow(idx_depth) == 0L) stop("No flood_depth rasters found under: ", paths$flood_depth_dir)
if (nrow(idx_dur)   == 0L) warning("No flood_duration rasters found (duration set will be empty).")
if (nrow(idx_wave)  == 0L) warning("No wave_ht rasters found (wave set will be empty).")

message("Extracting flood_depth...")
depth_long <- extract_points(idx_depth, parcels_sf, "depth_m") |>
  to_wide_by_metric("depth_m")

message("Extracting flood_duration...")
dur_long <- extract_points(idx_dur, parcels_sf, "duration_hr") |>
  to_wide_by_metric("duration_hr")

message("Extracting wave_ht...")
wave_long <- extract_points(idx_wave, parcels_sf, "wave_ht_m") |>
  to_wide_by_metric("wave_ht_m")

# Merge metrics into one tidy table
# Start from depth (required), left-join duration & wave if present
cosmos_tbl <- depth_long |>
  left_join(dur_long,  by = c("parcel_id","slr_cm","storm")) |>
  left_join(wave_long, by = c("parcel_id","slr_cm","storm")) |>
  arrange(parcel_id, slr_cm, storm) |>
  mutate(
    slr_m = slr_cm / 100,                             # centimeters → meters
    storm = dplyr::recode(storm, w000 = "avg", w100 = "100yr")  # nicer labels
  )

# Attach optional address for QA
cosmos_tbl <- cosmos_tbl |>
  left_join(parcels_key, by = "parcel_id")

# Basic QA checks
message("Unique SLR bins (cm): ", paste(sort(unique(cosmos_tbl$slr_cm)), collapse = ", "))
message("Storm classes found: ", paste(sort(unique(cosmos_tbl$storm)), collapse = ", "))
stopifnot(all(cosmos_tbl$depth_m >= 0 | is.na(cosmos_tbl$depth_m)))
if ("duration_hr" %in% names(cosmos_tbl)) {
  stopifnot(all(cosmos_tbl$duration_hr >= 0 | is.na(cosmos_tbl$duration_hr)))
}
if ("wave_ht_m" %in% names(cosmos_tbl)) {
  stopifnot(all(cosmos_tbl$wave_ht_m >= 0 | is.na(cosmos_tbl$wave_ht_m)))
}


# Save
out_dir <- file.path(data_dir, "derived")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

out_csv <- file.path(out_dir, "cosmos_flood_metrics.csv")
readr::write_csv(cosmos_tbl, out_csv)

# Optional: also write a parcel × (slr_bin × storm) wide preview (depth only)
try({
  depth_preview <- cosmos_tbl |>
    select(parcel_id, slr_cm, storm, depth_m) |>
    unite("slr_storm", slr_cm, storm, sep = "_", remove = FALSE) |>
    select(parcel_id, slr_storm, depth_m) |>
    pivot_wider(names_from = slr_storm, values_from = depth_m)
  readr::write_csv(depth_preview, file.path(out_dir, "cosmos_depth_preview_wide.csv"))
  message("✔ Wrote preview: ", file.path(out_dir, "cosmos_depth_preview_wide.csv"))
}, silent = TRUE)
