# extract_cosmos_metrics_unified.R
# Purpose: Extract ALL CoSMoS metrics at parcel locations across all SLR scenarios
# Author: Will Dean (Managed Shores)
# 
# This script consolidates extraction from:
#   - Raster data (flood depth, duration, wave height) - organized by storm subfolder
#   - Vector data (runup, cliff retreat, shoreline change, coastal squeeze)
#
# Input:
#   - data/{site}/redfin_df.csv (property locations with LONGITUDE, LATITUDE)
#   - data/{site}/cosmos/flood_depth/w000/*.tif (and w100/, etc.)
#   - data/{site}/cosmos/flood_duration/w000/*.tif
#   - data/{site}/cosmos/wave_ht/w000/*.tif
#   - data/{site}/cosmos/runup/slr###_w###/*.shp
#   - data/{site}/cosmos/cliff_retreat/*.gpkg
#   - data/{site}/cosmos/shoreline_change/*.gpkg
#   - data/{site}/cosmos/coastal_squeeze/*.gpkg
#
# Output:
#   - data/{site}/derived/cosmos_hazard_metrics.csv
#   - data/{site}/derived/cosmos_hazard_metrics_summary.txt (QA report)

rm(list = ls())
library(terra)
library(sf)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(purrr)


# CONFIGURATION

case_name <- "carpinteria"  # Change for other sites
data_dir <- file.path("data", case_name)
cosmos_dir <- file.path(data_dir, "cosmos")
derived_dir <- file.path(data_dir, "derived")
dir.create(derived_dir, recursive = TRUE, showWarnings = FALSE)

# Storm scenarios: Always extract BOTH w000 (average) and w100 (extreme)
# These provide the bounds needed for retreat planning and sensitivity analysis
raster_storm_primary <- "w000"  # Primary scenario for retreat timing
raster_storm_compare <- "w100"  # Comparison scenario for sensitivity
runup_storm_primary <- "w000"   # Primary runup scenario
runup_storm_compare <- "w100"   # Comparison runup scenario

# # Flood threshold accounts for typical LiDAR DEM vertical uncertainty
# CoSMoS v3.0 uses 2-meter resolution DEMs with typical vertical accuracy 
# of ±15cm (USGS, 2015). We apply a 0.15m threshold to filter model noise.
flood_threshold_m <- 0.15

# Citation:
# Barnard, P.L., et al. (2014). Development of the Coastal Storm Modeling System (CoSMoS) for predicting the impact of storms on high-energy, 
# active-margin coasts. Natural Hazards, 74(2), 1095-1125.

message("CoSMoS Metrics Extraction: ", toupper(case_name))
message("Extracting BOTH w000 (average) and w100 (extreme) scenarios")


# LOAD PARCELS
parcel_path <- file.path(data_dir, "redfin_df.csv")
stopifnot(file.exists(parcel_path))

parcels_raw <- read_csv(parcel_path, show_col_types = FALSE)

# Robust column name detection
lon_col <- intersect(names(parcels_raw), c("LONGITUDE", "longitude", "lon", "x", "X"))[1]
lat_col <- intersect(names(parcels_raw), c("LATITUDE", "latitude", "lat", "y", "Y"))[1]

stopifnot(!is.na(lon_col), !is.na(lat_col))

parcels <- parcels_raw %>%
  filter(!is.na(.data[[lon_col]]), !is.na(.data[[lat_col]])) %>%
  st_as_sf(coords = c(lon_col, lat_col), crs = 4326, remove = FALSE) %>%
  mutate(parcel_id = row_number())


# SECTION A: RASTER EXTRACTIONS (flood depth, duration, wave height)
# Helper: Extract values from rasters organized by storm subfolder
extract_raster_metric <- function(metric_name, storm = primary_storm) {
  metric_dir <- file.path(cosmos_dir, metric_name, storm)
  
  if (!dir.exists(metric_dir)) {
    message("⚠ Skipping ", metric_name, " (directory not found: ", metric_dir, ")")
    return(tibble())
  }
  
  tif_files <- list.files(metric_dir, pattern = "\\.tif$", full.names = TRUE)
  
  if (length(tif_files) == 0) {
    message("⚠ Skipping ", metric_name, " (no .tif files in ", metric_dir, ")")
    return(tibble())
  }
  
  message("→ Extracting ", metric_name, " (", storm, "): ", length(tif_files), " rasters")
  
  results <- map_dfr(tif_files, function(f) {
    # Parse SLR from filename: "flood_depth_slr100_w000_carp.tif" → 100
    fname <- basename(f)
    slr_match <- str_extract(fname, "(?i)slr\\d{3}")
    
    if (is.na(slr_match)) {
      message("  ⚠ Could not parse SLR from: ", fname)
      return(NULL)
    }
    
    slr_cm <- as.numeric(str_extract(slr_match, "\\d{3}"))
    slr_m <- slr_cm / 100
    
    # Read raster and extract at parcel points
    r <- rast(f)
    pts <- st_transform(parcels, crs(r))
    vals_raw <- terra::extract(r, vect(pts), ID = FALSE)[, 1]
    
    # Clean: CoSMoS sometimes uses very large values for NoData
    vals_clean <- ifelse(is.finite(vals_raw) & vals_raw < 1e6, vals_raw, NA_real_)
    
    tibble(
      parcel_id = parcels$parcel_id,
      slr_m = slr_m,
      storm = storm,
      !!sym(metric_name) := vals_clean
    )
  })
  
  results
}

# Extract flood depth (in cm, convert to m) - w000 PRIMARY
depth_df <- extract_raster_metric("flood_depth", raster_storm_primary) %>%
  rename(depth_cm = flood_depth) %>%
  mutate(
    depth_m = depth_cm / 100,
    depth_m = pmax(depth_m, 0, na.rm = TRUE)  # Floor at zero
  ) %>%
  select(-depth_cm)

# Extract flood duration (hours) - w000 PRIMARY
duration_df <- extract_raster_metric("flood_duration", raster_storm_primary) %>%
  rename(duration_hr = flood_duration) %>%
  mutate(duration_hr = pmax(duration_hr, 0, na.rm = TRUE))

# Extract wave height (meters) - w000 PRIMARY
wave_df <- extract_raster_metric("wave_ht", raster_storm_primary) %>%
  rename(wave_ht_m = wave_ht) %>%
  mutate(wave_ht_m = pmax(wave_ht_m, 0, na.rm = TRUE))

# ALWAYS extract w100 scenario for comparison
message("\n→ Extracting w100 (extreme storm) scenario for comparison...")

depth_w100 <- extract_raster_metric("flood_depth", raster_storm_compare) %>%
  rename(depth_cm_w100 = flood_depth) %>%
  mutate(depth_m_w100 = depth_cm_w100 / 100,
         depth_m_w100 = pmax(depth_m_w100, 0, na.rm = TRUE)) %>%
  select(-depth_cm_w100, -storm)  # Remove storm column before merge

duration_w100 <- extract_raster_metric("flood_duration", raster_storm_compare) %>%
  rename(duration_hr_w100 = flood_duration) %>%
  mutate(duration_hr_w100 = pmax(duration_hr_w100, 0, na.rm = TRUE)) %>%
  select(-storm)  # Remove storm column before merge

wave_w100 <- extract_raster_metric("wave_ht", raster_storm_compare) %>%
  rename(wave_ht_m_w100 = wave_ht) %>%
  mutate(wave_ht_m_w100 = pmax(wave_ht_m_w100, 0, na.rm = TRUE)) %>%
  select(-storm)  # Remove storm column before merge

# Merge w100 data (w100 metrics become additional columns alongside w000)
depth_df <- depth_df %>%
  left_join(depth_w100, by = c("parcel_id", "slr_m"))

duration_df <- duration_df %>%
  left_join(duration_w100, by = c("parcel_id", "slr_m"))

wave_df <- wave_df %>%
  left_join(wave_w100, by = c("parcel_id", "slr_m"))

# Merge raster metrics
raster_metrics <- depth_df %>%
  full_join(duration_df, by = c("parcel_id", "slr_m", "storm")) %>%
  full_join(wave_df, by = c("parcel_id", "slr_m", "storm")) %>%
  # Add flooding flag based on threshold
  mutate(flooded = !is.na(depth_m) & depth_m >= flood_threshold_m)

message("  - Unique SLR levels: ", paste(sort(unique(raster_metrics$slr_m)), collapse = ", "), " m")
message("  - Parcels with flooding: ", sum(raster_metrics$flooded, na.rm = TRUE), 
        " / ", nrow(raster_metrics), " parcel-SLR combinations")


# SECTION B: VECTOR EXTRACTIONS
# B1: RUNUP EXPOSURE
# Structure: cosmos/runup/slr###_w###/*.shp
# Strategy: Calculate distance from parcel to nearest runup line

extract_runup_exposure <- function(storm = primary_storm) {
  runup_base <- file.path(cosmos_dir, "runup")
  
  if (!dir.exists(runup_base)) {
    message("⚠ Skipping runup (directory not found)")
    return(tibble())
  }
  
  # Find all scenario folders matching our storm scenario
  scenario_dirs <- list.dirs(runup_base, full.names = TRUE, recursive = FALSE)
  scenario_dirs <- scenario_dirs[grepl(paste0("_", storm, "$"), basename(scenario_dirs))]
  
  if (length(scenario_dirs) == 0) {
    message("⚠ Skipping runup (no scenarios found for storm: ", storm, ")")
    return(tibble())
  }
  
  message("→ Extracting runup exposure: ", length(scenario_dirs), " scenarios")
  
  results <- map_dfr(scenario_dirs, function(sdir) {
    # Parse SLR from folder name: "slr100_w000" → 100
    folder_name <- basename(sdir)
    slr_match <- str_extract(folder_name, "(?i)slr\\d{3}")
    if (is.na(slr_match)) return(NULL)
    
    slr_cm <- as.numeric(str_extract(slr_match, "\\d{3}"))
    slr_m <- slr_cm / 100
    
    # Read all shapefiles in this folder
    shp_files <- list.files(sdir, pattern = "\\.shp$", full.names = TRUE)
    if (length(shp_files) == 0) return(NULL)
    
    # Combine all runup lines for this scenario
    runup_lines <- map(shp_files, ~ st_read(.x, quiet = TRUE)) %>%
      bind_rows() %>%
      st_union()
    
    # Calculate distance from each parcel to runup lines
    parcels_proj <- st_transform(parcels, st_crs(runup_lines))
    distances <- st_distance(parcels_proj, runup_lines)
    min_dist_m <- as.numeric(apply(distances, 1, min))
    
    tibble(
      parcel_id = parcels$parcel_id,
      slr_m = slr_m,
      runup_dist_m = min_dist_m,
      runup_exposed = min_dist_m < 50  # Within 50m = exposed
    )
  })
  
  results
}

runup_df_w000 <- extract_runup_exposure(runup_storm_primary)

# Also extract w100 runup
message("\n→ Extracting w100 runup for comparison...")
runup_df_w100 <- extract_runup_exposure(runup_storm_compare) %>%
  rename(runup_dist_m_w100 = runup_dist_m,
         runup_exposed_w100 = runup_exposed)

# Merge w100 runup data
if (nrow(runup_df_w000) > 0 && nrow(runup_df_w100) > 0) {
  runup_df <- runup_df_w000 %>%
    left_join(runup_df_w100, by = c("parcel_id", "slr_m"))
  message("✓ Runup extraction complete (both w000 and w100): ", 
          sum(runup_df$runup_exposed, na.rm = TRUE), " parcels exposed (w000)")
} else if (nrow(runup_df_w000) > 0) {
  runup_df <- runup_df_w000
  message("✓ Runup extraction complete (w000 only): ", 
          sum(runup_df$runup_exposed, na.rm = TRUE), " exposed parcels")
} else {
  runup_df <- tibble()
}

# B2: SHORELINE CHANGE
# Structure: cosmos/shoreline_change/*.gpkg
# Files include management scenarios (HoldtheLine, NoNourishment, etc.)
# Strategy: Use the most conservative scenario (No_HoldtheLine_NoNourishment)

extract_shoreline_change <- function() {
  shore_dir <- file.path(cosmos_dir, "shoreline_change")
  
  if (!dir.exists(shore_dir)) {
    message("⚠ Skipping shoreline change (directory not found)")
    return(tibble())
  }
  
  gpkg_files <- list.files(shore_dir, pattern = "\\.gpkg$", full.names = TRUE)
  
  if (length(gpkg_files) == 0) {
    message("⚠ Skipping shoreline change (no .gpkg files)")
    return(tibble())
  }
  
  # Prioritize "No_HoldtheLine_NoNourishment" scenario (most conservative)
  priority_file <- gpkg_files[grepl("No_HoldtheLine_NoNourishment_shoreline(?!_uncertainty)", 
                                    basename(gpkg_files), perl = TRUE)]
  
  if (length(priority_file) == 0) {
    # Fallback to any non-uncertainty file
    priority_file <- gpkg_files[!grepl("uncertainty", basename(gpkg_files))]
    if (length(priority_file) == 0) {
      message("⚠ Skipping shoreline change (no suitable files)")
      return(tibble())
    }
  }
  
  message("→ Extracting shoreline change: ", basename(priority_file[1]))
  
  # This is a simplification - in reality, shoreline change files may have 
  # different structures. Adapt based on actual attribute table.
  shore_lines <- st_read(priority_file[1], quiet = TRUE)
  
  # Check if file has multiple SLR scenarios or is a single projection
  # Common fields: "SLR_cm", "Year", "Position", etc.
  
  if ("SLR_cm" %in% names(shore_lines)) {
    # File contains multiple SLR scenarios
    parcels_proj <- st_transform(parcels, st_crs(shore_lines))
    
    results <- shore_lines %>%
      st_drop_geometry() %>%
      distinct(SLR_cm) %>%
      pull(SLR_cm) %>%
      map_dfr(function(slr_cm) {
        slr_m <- slr_cm / 100
        shore_subset <- shore_lines %>% filter(SLR_cm == slr_cm)
        
        distances <- st_distance(parcels_proj, shore_subset)
        min_dist_m <- as.numeric(apply(distances, 1, min))
        
        tibble(
          parcel_id = parcels$parcel_id,
          slr_m = slr_m,
          shore_dist_m = min_dist_m
        )
      })
    
    return(results)
    
  } else {
    # Single projection - assign to a default SLR level or skip
    message("  ⚠ Shoreline file has no SLR_cm field - using as reference only")
    return(tibble())
  }
}

shore_df <- extract_shoreline_change()

if (nrow(shore_df) > 0) {
  message("✓ Shoreline change extraction complete")
}

# B3: CLIFF RETREAT
# Structure: cosmos/cliff_retreat/*.gpkg
# Files: CliffEdgePositions_LetItGo_vFinal, CliffEdgePositionUncertainty_*, etc.

extract_cliff_retreat <- function() {
  cliff_dir <- file.path(cosmos_dir, "cliff_retreat")
  
  if (!dir.exists(cliff_dir)) {
    message("⚠ Skipping cliff retreat (directory not found)")
    return(tibble())
  }
  
  gpkg_files <- list.files(cliff_dir, pattern = "\\.gpkg$", full.names = TRUE)
  
  if (length(gpkg_files) == 0) {
    message("⚠ Skipping cliff retreat (no .gpkg files)")
    return(tibble())
  }
  
  # Use "LetItGo" scenario (natural retreat, most conservative)
  priority_file <- gpkg_files[grepl("LetItGo_vFinal", basename(gpkg_files), ignore.case = TRUE)]
  
  if (length(priority_file) == 0) {
    priority_file <- gpkg_files[!grepl("Uncertainty", basename(gpkg_files))]
  }
  
  if (length(priority_file) == 0) {
    message("⚠ Skipping cliff retreat (no suitable files)")
    return(tibble())
  }
  
  message("→ Extracting cliff retreat: ", basename(priority_file[1]))
  
  cliff_lines <- st_read(priority_file[1], quiet = TRUE)
  
  # Cliff files typically have year-based projections or SLR-based positions
  # Check attribute table structure
  
  # Common approach: File contains multiple cliff edge positions for different years
  # We'll calculate distance to nearest projected cliff edge and flag if < threshold
  
  parcels_proj <- st_transform(parcels, st_crs(cliff_lines))
  distances <- st_distance(parcels_proj, cliff_lines)
  min_dist_m <- as.numeric(apply(distances, 1, min))
  
  # Binary flag: is parcel within 100m of projected cliff retreat zone?
  results <- tibble(
    parcel_id = parcels$parcel_id,
    cliff_dist_m = min_dist_m,
    cliff_exposed = min_dist_m < 100
  )
  
  # If cliff data has temporal/SLR info, expand this to multiple rows per parcel
  # For now, treating as a single "ultimate retreat" scenario
  
  results
}

cliff_df <- extract_cliff_retreat()

if (nrow(cliff_df) > 0) {
  message("✓ Cliff retreat extraction complete: ",
          sum(cliff_df$cliff_exposed, na.rm = TRUE), " exposed parcels")
}

# B4: COASTAL SQUEEZE
# Structure: cosmos/coastal_squeeze/coastal_squeeze_SLR###_carp.gpkg
# This is mainly for context - not directly used in retreat timing

extract_coastal_squeeze <- function() {
  squeeze_dir <- file.path(cosmos_dir, "coastal_squeeze")
  
  if (!dir.exists(squeeze_dir)) {
    message("⚠ Skipping coastal squeeze (directory not found)")
    return(tibble())
  }
  
  gpkg_files <- list.files(squeeze_dir, pattern = "\\.gpkg$", full.names = TRUE)
  
  if (length(gpkg_files) == 0) {
    message("⚠ Skipping coastal squeeze (no .gpkg files)")
    return(tibble())
  }
  
  message("→ Extracting coastal squeeze: ", length(gpkg_files), " scenarios")
  
  # Each file represents a SLR level
  results <- map_dfr(gpkg_files, function(f) {
    fname <- basename(f)
    slr_match <- str_extract(fname, "(?i)SLR\\d{3}")
    if (is.na(slr_match)) return(NULL)
    
    slr_cm <- as.numeric(str_extract(slr_match, "\\d{3}"))
    slr_m <- slr_cm / 100
    
    squeeze_polys <- st_read(f, quiet = TRUE)
    parcels_proj <- st_transform(parcels, st_crs(squeeze_polys))
    
    # Check which parcels fall within squeeze zones
    intersects <- st_intersects(parcels_proj, squeeze_polys, sparse = FALSE)
    in_squeeze <- rowSums(intersects) > 0
    
    tibble(
      parcel_id = parcels$parcel_id,
      slr_m = slr_m,
      in_squeeze_zone = in_squeeze
    )
  })
  
  results
}

squeeze_df <- extract_coastal_squeeze()

if (nrow(squeeze_df) > 0) {
  message("✓ Coastal squeeze extraction complete")
}


# MERGE ALL METRICS

# Safe join helper: only join if dataframe is non-empty and has required columns
safe_join <- function(x, y, by_cols, join_name = "data") {
  if (nrow(y) == 0) {
    message("  ⚠ Skipping ", join_name, " (no data available)")
    return(x)
  }
  
  missing_cols <- setdiff(by_cols, names(y))
  if (length(missing_cols) > 0) {
    message("  ⚠ Skipping ", join_name, " (missing columns: ", paste(missing_cols, collapse = ", "), ")")
    return(x)
  }
  
  left_join(x, y, by = by_cols)
}

hazard_metrics <- raster_metrics %>%
  # Vector metrics that vary by SLR
  safe_join(runup_df, c("parcel_id", "slr_m"), "runup") %>%
  safe_join(shore_df, c("parcel_id", "slr_m"), "shoreline change") %>%
  safe_join(squeeze_df, c("parcel_id", "slr_m"), "coastal squeeze") %>%
  # Cliff data (currently not SLR-specific, so join just by parcel_id)
  safe_join(cliff_df, "parcel_id", "cliff retreat") %>%
  arrange(parcel_id, slr_m)

# ============================================================================
# SAVE & SUMMARIZE
# ============================================================================

out_csv <- file.path(derived_dir, "cosmos_hazard_metrics.csv")
write_csv(hazard_metrics, out_csv)

message("\n✓ Saved: ", out_csv)
message("  Dimensions: ", nrow(hazard_metrics), " rows × ", ncol(hazard_metrics), " columns")

# QA Summary
summary_lines <- c(
  "========================================",
  "CoSMoS Hazard Metrics Extraction Summary",
  "========================================",
  paste("Site:", case_name),
  paste("Date:", Sys.Date()),
  "Storm scenarios: w000 (average) and w100 (extreme)",
  "",
  "DATA COVERAGE:",
  paste("  Total parcels:", n_distinct(hazard_metrics$parcel_id)),
  paste("  SLR levels:", paste(sort(unique(hazard_metrics$slr_m)), collapse = ", "), "m"),
  "",
  "FLOODING METRICS:",
  paste("  Parcel-SLR combos with depth data:", sum(!is.na(hazard_metrics$depth_m))),
  paste("  Parcel-SLR combos flooded (>", flood_threshold_m, "m):", sum(hazard_metrics$flooded, na.rm = TRUE)),
  paste("  Parcel-SLR combos with duration data:", sum(!is.na(hazard_metrics$duration_hr))),
  paste("  Parcel-SLR combos with wave data:", sum(!is.na(hazard_metrics$wave_ht_m))),
  "",
  "VECTOR METRICS:",
  paste("  Parcels with runup data:", sum(!is.na(hazard_metrics$runup_dist_m))),
  paste("  Parcels exposed to runup:", sum(hazard_metrics$runup_exposed, na.rm = TRUE)),
  paste("  Parcels with shoreline data:", sum(!is.na(hazard_metrics$shore_dist_m))),
  paste("  Parcels exposed to cliff retreat:", sum(hazard_metrics$cliff_exposed, na.rm = TRUE)),
  paste("  Parcels in coastal squeeze zones:", sum(hazard_metrics$in_squeeze_zone, na.rm = TRUE)),
  "",
  "NEXT STEPS:",
  "  1. Run interpolate_cosmos_to_years.R to create annual hazard timelines",
  "  2. Run calculate_retreat_years.R to compute optimal retreat timing",
  "========================================"
)

summary_file <- file.path(derived_dir, "cosmos_hazard_metrics_summary.txt")
writeLines(summary_lines, summary_file)

cat("\n")
cat(paste(summary_lines, collapse = "\n"))
cat("\n\n")

message("✓ Summary saved: ", summary_file)
