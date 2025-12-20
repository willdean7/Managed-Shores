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

case_name <- "pacifica"  # Change for other sites
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

# SITE TYPE DETECTION
# Check what types of hazard data are available
message("\n→ Detecting available hazard data...")

has_flood <- dir.exists(file.path(cosmos_dir, "flood_depth"))
has_wave <- dir.exists(file.path(cosmos_dir, "wave_ht"))
has_runup <- dir.exists(file.path(cosmos_dir, "runup"))
has_cliff <- dir.exists(file.path(cosmos_dir, "cliff_retreat"))
has_shore <- dir.exists(file.path(cosmos_dir, "shoreline_change"))
has_squeeze <- dir.exists(file.path(cosmos_dir, "coastal_squeeze"))

site_type <- if (has_flood && !has_cliff) {
  "FLOOD-ONLY"
} else if (has_cliff && !has_flood) {
  "CLIFF-ONLY"
} else if (has_flood && has_cliff) {
  "HYBRID (flood + cliff)"
} else {
  "UNKNOWN"
}

message("  Site type: ", site_type)
message("  Flood data: ", ifelse(has_flood, "✓", "✗"))
message("  Wave data: ", ifelse(has_wave, "✓", "✗"))
message("  Runup data: ", ifelse(has_runup, "✓", "✗"))
message("  Cliff retreat: ", ifelse(has_cliff, "✓", "✗"))
message("  Shoreline change: ", ifelse(has_shore, "✓", "✗"))
message("  Coastal squeeze: ", ifelse(has_squeeze, "✓", "✗"))

if (!has_flood && !has_cliff) {
  stop("ERROR: No hazard data found in ", cosmos_dir, 
       "\nRun cosmos_process.R (for flood sites) or cosmos_process_cliff.R (for cliff sites) first")
}

message("")


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
depth_df <- extract_raster_metric("flood_depth", raster_storm_primary)

# Check if we got any flood data - if not, initialize empty tibbles
if (nrow(depth_df) == 0) {
  message("⚠ No flood depth data - initializing empty dataframes")
  depth_df <- tibble()
  duration_df <- tibble()
  wave_df <- tibble()
  depth_w100 <- tibble()
  duration_w100 <- tibble()
  wave_w100 <- tibble()
  raster_metrics <- tibble()
} else {
  # Continue with normal flood extraction
  depth_df <- depth_df %>%
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
}


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
# CoSMoS 3.1 provides temporal projections with multiple cliff edge positions

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
  
  message("→ Extracting cliff retreat data")
  message("  Files found: ", length(gpkg_files))
  
  # Separate files by scenario type
  letitgo_files <- gpkg_files[grepl("LetItGo|let.*go", basename(gpkg_files), ignore.case = TRUE)]
  holdline_files <- gpkg_files[grepl("HoldTheLine|HoldLine|hold.*line", basename(gpkg_files), ignore.case = TRUE)]
  
  # Prioritize non-uncertainty files
  letitgo_main <- letitgo_files[!grepl("Uncertainty", basename(letitgo_files), ignore.case = TRUE)]
  holdline_main <- holdline_files[!grepl("Uncertainty", basename(holdline_files), ignore.case = TRUE)]
  
  # Use all available files (prefer Let It Go, but include Hold the Line if available)
  files_to_process <- c(letitgo_main, holdline_main)
  
  # Fallback: if no scenario files found, use any non-uncertainty file
  if (length(files_to_process) == 0) {
    files_to_process <- gpkg_files[!grepl("Uncertainty", basename(gpkg_files), ignore.case = TRUE)]
  }
  
  if (length(files_to_process) == 0) {
    message("⚠ Skipping cliff retreat (no suitable files)")
    return(tibble())
  }
  
  # Function to process one cliff file
  process_cliff_file <- function(file_path) {
    fname <- basename(file_path)
    message("  Processing: ", fname)
    
    # Determine scenario from filename
    scenario <- if (grepl("LetItGo|let.*go", fname, ignore.case = TRUE)) {
      "LetItGo"
    } else if (grepl("HoldTheLine|HoldLine|hold.*line", fname, ignore.case = TRUE)) {
      "HoldTheLine"
    } else {
      "Unknown"
    }
    
    cliff_lines <- st_read(file_path, quiet = TRUE)
    message("    Features: ", nrow(cliff_lines))
    
    # Inspect attributes for temporal information
    attr_names <- names(cliff_lines)
    message("    Attributes: ", paste(head(attr_names, 8), collapse = ", "))
    
    has_year <- any(grepl("YEAR|year|Year", attr_names, ignore.case = TRUE))
    has_slr <- any(grepl("SLR|slr", attr_names, ignore.case = TRUE))
    has_name <- "Name" %in% attr_names
    has_desc <- "Description" %in% attr_names
    
    # NEW: Try to extract SLR from Description field (common in KML files)
    if (!has_slr && has_desc) {
      message("    Attempting to parse SLR from Description field...")
      
      # Try to extract SLR values from Description (HTML tables often contain this)
      # Pattern: "SLR: 100 cm" or "SLR__cm: 100" or similar
      cliff_lines <- cliff_lines %>%
        mutate(
          slr_parsed = str_extract(Description, "(?i)SLR[_:\\s]*\\d+") %>%
            str_extract("\\d+") %>%
            as.numeric()
        )
      
      # Check if we successfully parsed SLR
      if (sum(!is.na(cliff_lines$slr_parsed)) > 0) {
        message("    ✓ Extracted SLR from Description: ", 
                length(unique(cliff_lines$slr_parsed[!is.na(cliff_lines$slr_parsed)])), " levels")
        cliff_lines$SLR_cm <- cliff_lines$slr_parsed
        has_slr <- TRUE
      }
    }
    
    # NEW: Try to extract SLR from Name field as backup
    if (!has_slr && has_name) {
      message("    Attempting to parse SLR from Name field...")
      
      # Check if Name field is just numeric (common in CoSMoS KML files)
      cliff_lines <- cliff_lines %>%
        mutate(
          slr_parsed = case_when(
            # Plain number (e.g., "25", "50", "100")
            str_detect(Name, "^\\d+$") ~ as.numeric(Name),
            # Number with units (e.g., "100 cm", "SLR 100")
            TRUE ~ str_extract(Name, "\\d+") %>% as.numeric()
          )
        )
      
      if (sum(!is.na(cliff_lines$slr_parsed)) > 0) {
        message("    ✓ Extracted SLR from Name: ", 
                length(unique(cliff_lines$slr_parsed[!is.na(cliff_lines$slr_parsed)])), " levels")
        message("    SLR values (cm): ", paste(sort(unique(cliff_lines$slr_parsed)), collapse = ", "))
        cliff_lines$SLR_cm <- cliff_lines$slr_parsed
        has_slr <- TRUE
      }
    }
    
    # Update has_slr check to include our new SLR_cm column
    if ("SLR_cm" %in% names(cliff_lines)) {
      has_slr <- TRUE
    }
    
    # Transform parcels to cliff CRS
    parcels_proj <- st_transform(parcels, st_crs(cliff_lines))
    
    # Strategy 1: If data has YEAR or SLR field, extract for each level
    if (has_year || has_slr) {
      year_col <- attr_names[grepl("YEAR|year", attr_names, ignore.case = TRUE)][1]
      slr_col <- attr_names[grepl("SLR", attr_names, ignore.case = TRUE)][1]
      
      # Use parsed SLR_cm if we created it
      if ("SLR_cm" %in% names(cliff_lines) && is.na(slr_col)) {
        slr_col <- "SLR_cm"
      }
      
      temporal_col <- if (!is.na(year_col)) year_col else slr_col
      
      unique_vals <- unique(cliff_lines[[temporal_col]])
      unique_vals <- unique_vals[!is.na(unique_vals)]
      
      message("    Temporal field '", temporal_col, "': ", length(unique_vals), " values")
      message("    Values: ", paste(head(unique_vals, 5), collapse = ", "))
      
      results <- map_dfr(unique_vals, function(val) {
        subset <- cliff_lines %>% filter(.data[[temporal_col]] == val)
        distances <- st_distance(parcels_proj, subset)
        min_dist_m <- as.numeric(apply(distances, 1, min))
        
        # Determine if this is year or SLR based
        if (!is.na(year_col)) {
          tibble(
            parcel_id = parcels$parcel_id,
            scenario = scenario,
            year = as.numeric(val),
            slr_m = NA_real_,
            cliff_dist_m = min_dist_m,
            cliff_exposed_50m = min_dist_m < 50,
            cliff_exposed_100m = min_dist_m < 100
          )
        } else {
          # SLR-based (convert cm to m if needed)
          slr_val <- as.numeric(val)
          if (slr_val > 10) slr_val <- slr_val / 100  # Convert cm to m
          
          tibble(
            parcel_id = parcels$parcel_id,
            scenario = scenario,
            year = NA_real_,
            slr_m = slr_val,
            cliff_dist_m = min_dist_m,
            cliff_exposed_50m = min_dist_m < 50,
            cliff_exposed_100m = min_dist_m < 100
          )
        }
      })
      
      return(results)
    }
    
    # Strategy 2: If Name field has temporal info, parse it
    if (has_name) {
      unique_names <- unique(cliff_lines$Name)
      
      # Try to extract years from Name field
      years_in_names <- str_extract_all(paste(unique_names, collapse = " "), "\\b(20\\d{2}|\\d{4})\\b") %>%
        unlist() %>%
        unique() %>%
        as.numeric()
      
      years_in_names <- years_in_names[years_in_names >= 2020 & years_in_names <= 2150]
      
      if (length(years_in_names) > 1) {
        message("    Found years in Name field: ", paste(years_in_names, collapse = ", "))
        
        results <- map_dfr(years_in_names, function(yr) {
          # Match features with this year in Name
          subset <- cliff_lines %>% filter(grepl(as.character(yr), Name))
          
          if (nrow(subset) == 0) return(NULL)
          
          distances <- st_distance(parcels_proj, subset)
          min_dist_m <- as.numeric(apply(distances, 1, min))
          
          tibble(
            parcel_id = parcels$parcel_id,
            scenario = scenario,
            year = yr,
            slr_m = NA_real_,
            cliff_dist_m = min_dist_m,
            cliff_exposed_50m = min_dist_m < 50,
            cliff_exposed_100m = min_dist_m < 100
          )
        })
        
        if (nrow(results) > 0) return(results)
      }
    }
    
    # Strategy 2.5: If Name field has SLR values (in cm), use lowest as baseline
    if (has_name) {
      unique_names <- unique(cliff_lines$Name)
      
      # Try to extract numeric values (potential SLR in cm)
      slr_in_names <- suppressWarnings(as.numeric(unique_names))
      slr_in_names <- slr_in_names[!is.na(slr_in_names)]
      
      # If we have numeric values that look like SLR (typically 0-500cm range)
      if (length(slr_in_names) > 0 && all(slr_in_names >= 0 & slr_in_names <= 1000)) {
        message("    Found SLR values in Name field (cm): ", paste(sort(slr_in_names), collapse = ", "))
        
        # Use the LOWEST SLR value as baseline (closest to current conditions)
        baseline_slr <- min(slr_in_names)
        message("    Using SLR = ", baseline_slr, "cm as baseline cliff position")
        
        baseline_cliff <- cliff_lines %>% filter(Name == as.character(baseline_slr))
        
        if (nrow(baseline_cliff) > 0) {
          distances <- st_distance(parcels_proj, baseline_cliff)
          min_dist_m <- as.numeric(apply(distances, 1, min))
          
          result <- tibble(
            parcel_id = parcels$parcel_id,
            scenario = scenario,
            year = NA_real_,
            slr_m = baseline_slr / 100,  # Convert cm to m
            cliff_dist_m = min_dist_m,
            cliff_exposed_50m = min_dist_m < 50,
            cliff_exposed_100m = min_dist_m < 100
          )
          
          message("    ✓ Calculated distances using baseline SLR = ", baseline_slr, "cm")
          return(result)
        }
      }
    }
    
    # Strategy 3: Treat as single projection (ultimate retreat)
    message("    Processing as single projection (no temporal data)")
    message("    ⚠️  WARNING: Using ALL cliff positions - distances may represent retreated cliff")
    
    distances <- st_distance(parcels_proj, cliff_lines)
    min_dist_m <- as.numeric(apply(distances, 1, min))
    
    tibble(
      parcel_id = parcels$parcel_id,
      scenario = scenario,
      year = NA_real_,
      slr_m = NA_real_,
      cliff_dist_m = min_dist_m,
      cliff_exposed_50m = min_dist_m < 50,
      cliff_exposed_100m = min_dist_m < 100
    )
  }
  
  # Process all files and combine
  all_results <- map_dfr(files_to_process, process_cliff_file)
  
  if (nrow(all_results) > 0) {
    message("  ✓ Extracted cliff data: ", nrow(all_results), " rows")
    message("    Scenarios: ", paste(unique(all_results$scenario), collapse = ", "))
    
    if (sum(!is.na(all_results$year)) > 0) {
      message("    Years: ", paste(sort(unique(all_results$year[!is.na(all_results$year)])), collapse = ", "))
    }
    if (sum(!is.na(all_results$slr_m)) > 0) {
      message("    SLR levels: ", paste(sort(unique(all_results$slr_m[!is.na(all_results$slr_m)])), collapse = ", "), " m")
    }
  }
  
  all_results
}

cliff_df <- extract_cliff_retreat()

if (nrow(cliff_df) > 0) {
  message("✓ Cliff retreat extraction complete")
  
  # Summarize exposure
  exposure_summary <- cliff_df %>%
    group_by(scenario) %>%
    summarise(
      parcels_50m = sum(cliff_exposed_50m, na.rm = TRUE),
      parcels_100m = sum(cliff_exposed_100m, na.rm = TRUE),
      .groups = "drop"
    )
  
  for (i in 1:nrow(exposure_summary)) {
    s <- exposure_summary[i, ]
    message("  ", s$scenario, ": ", s$parcels_50m, " parcels within 50m, ", 
            s$parcels_100m, " within 100m")
  }
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

# Determine how to merge cliff data based on its structure
message("\n→ Merging hazard metrics...")

# Start with raster metrics (flood data)
# If no flood data exists, create base structure from parcels
if (nrow(raster_metrics) > 0) {
  message("  Starting with flood metrics (", nrow(raster_metrics), " rows)")
  hazard_metrics <- raster_metrics
} else {
  message("  No flood metrics - creating base from parcels")
  # For cliff-only sites, we need to create a base structure
  # Check if cliff data has SLR levels or is just year-based
  if (nrow(cliff_df) > 0 && sum(!is.na(cliff_df$slr_m)) > 0) {
    # Cliff data has SLR levels - use those
    unique_slr <- unique(cliff_df$slr_m[!is.na(cliff_df$slr_m)])
    hazard_metrics <- expand_grid(
      parcel_id = parcels$parcel_id,
      slr_m = unique_slr
    ) %>%
      mutate(storm = "w000")  # Placeholder storm scenario
  } else {
    # No SLR structure - just use parcels as base
    hazard_metrics <- tibble(
      parcel_id = parcels$parcel_id,
      slr_m = NA_real_,
      storm = NA_character_
    )
  }
}

# Join vector metrics that vary by SLR (if they exist)
hazard_metrics <- hazard_metrics %>%
  safe_join(runup_df, c("parcel_id", "slr_m"), "runup") %>%
  safe_join(shore_df, c("parcel_id", "slr_m"), "shoreline change") %>%
  safe_join(squeeze_df, c("parcel_id", "slr_m"), "coastal squeeze")

# Join cliff data intelligently based on its structure
if (nrow(cliff_df) > 0) {
  # Check what type of temporal info cliff data has
  has_cliff_slr <- sum(!is.na(cliff_df$slr_m)) > 0
  has_cliff_year <- sum(!is.na(cliff_df$year)) > 0
  
  if (has_cliff_slr) {
    # Cliff data is SLR-based - join by parcel_id and slr_m
    message("  Joining cliff data by SLR level")
    hazard_metrics <- safe_join(hazard_metrics, cliff_df, c("parcel_id", "slr_m"), "cliff retreat")
  } else if (has_cliff_year) {
    # Cliff data is year-based - for now, just attach to all rows of each parcel
    # This will need special handling in interpolation
    message("  Cliff data is year-based (not SLR-based)")
    message("  Note: Cliff metrics will need separate interpolation - see cosmos_cliff_annual.csv")
    
    # Save cliff data separately for year-based analysis
    cliff_out_csv <- file.path(derived_dir, "cosmos_cliff_annual.csv")
    write_csv(cliff_df, cliff_out_csv)
    message("  Saved year-based cliff data: ", cliff_out_csv)
    
    # For the main output, join just the current/baseline cliff distance
    # (use the earliest year or average across years)
    if (has_cliff_year) {
      cliff_summary <- cliff_df %>%
        group_by(parcel_id, scenario) %>%
        summarise(
          cliff_dist_m = mean(cliff_dist_m, na.rm = TRUE),
          cliff_exposed_50m = any(cliff_exposed_50m, na.rm = TRUE),
          cliff_exposed_100m = any(cliff_exposed_100m, na.rm = TRUE),
          .groups = "drop"
        )
      hazard_metrics <- safe_join(hazard_metrics, cliff_summary, "parcel_id", "cliff retreat")
    }
  } else {
    # Cliff data has no temporal info - static projection
    message("  Joining cliff data (static projection)")
    
    # Check if we have multiple scenarios
    if ("scenario" %in% names(cliff_df)) {
      n_scenarios <- n_distinct(cliff_df$scenario)
      message("    Multiple cliff scenarios detected (", n_scenarios, " scenarios)")
      
      # For cliff-only sites with multiple scenarios, we need to expand the base
      # to accommodate each scenario
      if (nrow(raster_metrics) == 0) {
        # Expand base to include all scenarios
        scenarios <- unique(cliff_df$scenario)
        hazard_metrics <- expand_grid(
          parcel_id = parcels$parcel_id,
          scenario = scenarios,
          slr_m = NA_real_,
          storm = NA_character_
        )
        message("    Expanded base to ", nrow(hazard_metrics), " rows (parcels × scenarios)")
        
        # Join cliff data - handle potential column conflicts
        # Remove slr_m from cliff_df if it's all NA (it's a placeholder)
        cliff_to_join <- cliff_df
        if ("slr_m" %in% names(cliff_to_join) && all(is.na(cliff_to_join$slr_m))) {
          cliff_to_join <- cliff_to_join %>% select(-slr_m)
        }
        if ("storm" %in% names(cliff_to_join) && all(is.na(cliff_to_join$storm))) {
          cliff_to_join <- cliff_to_join %>% select(-storm)
        }
        if ("year" %in% names(cliff_to_join) && all(is.na(cliff_to_join$year))) {
          cliff_to_join <- cliff_to_join %>% select(-year)
        }
        
        hazard_metrics <- left_join(hazard_metrics, cliff_to_join, by = c("parcel_id", "scenario"))
      } else {
        # For flood sites with cliff data, just join by parcel_id
        # This will duplicate rows for each scenario - users can filter later
        message("    Note: Cliff scenarios will create duplicate parcel rows")
        hazard_metrics <- left_join(hazard_metrics, cliff_df, by = "parcel_id")
      }
    } else {
      # Single scenario - simple join
      hazard_metrics <- safe_join(hazard_metrics, cliff_df, "parcel_id", "cliff retreat")
    }
  }
}

# Final arrangement - handle both SLR-based and non-SLR-based data
if ("slr_m" %in% names(hazard_metrics)) {
  hazard_metrics <- hazard_metrics %>%
    arrange(parcel_id, slr_m)
} else if ("scenario" %in% names(hazard_metrics)) {
  hazard_metrics <- hazard_metrics %>%
    arrange(parcel_id, scenario)
} else {
  hazard_metrics <- hazard_metrics %>%
    arrange(parcel_id)
}

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
  paste("Site type:", site_type),
  paste("Date:", Sys.Date()),
  ""
)

# Add storm scenario info only if flood data exists
if (has_flood) {
  summary_lines <- c(summary_lines, "Storm scenarios: w000 (average) and w100 (extreme)", "")
}

summary_lines <- c(
  summary_lines,
  "DATA COVERAGE:",
  paste("  Total parcels:", n_distinct(hazard_metrics$parcel_id))
)

# Add SLR levels if they exist
if ("slr_m" %in% names(hazard_metrics) && sum(!is.na(hazard_metrics$slr_m)) > 0) {
  summary_lines <- c(
    summary_lines,
    paste("  SLR levels:", paste(sort(unique(hazard_metrics$slr_m[!is.na(hazard_metrics$slr_m)])), collapse = ", "), "m")
  )
}

summary_lines <- c(summary_lines, "")

# FLOODING METRICS (only if flood data exists)
if (has_flood && "depth_m" %in% names(hazard_metrics)) {
  summary_lines <- c(
    summary_lines,
    "FLOODING METRICS:",
    paste("  Parcel-SLR combos with depth data:", sum(!is.na(hazard_metrics$depth_m)))
  )
  
  if ("flooded" %in% names(hazard_metrics)) {
    summary_lines <- c(
      summary_lines,
      paste("  Parcel-SLR combos flooded (>", flood_threshold_m, "m):", 
            sum(hazard_metrics$flooded, na.rm = TRUE))
    )
  }
  
  if ("duration_hr" %in% names(hazard_metrics)) {
    summary_lines <- c(
      summary_lines,
      paste("  Parcel-SLR combos with duration data:", sum(!is.na(hazard_metrics$duration_hr)))
    )
  }
  
  if ("wave_ht_m" %in% names(hazard_metrics)) {
    summary_lines <- c(
      summary_lines,
      paste("  Parcel-SLR combos with wave data:", sum(!is.na(hazard_metrics$wave_ht_m)))
    )
  }
  
  summary_lines <- c(summary_lines, "")
}

# VECTOR METRICS (runup, shoreline, cliff)
vector_metrics_present <- FALSE

vector_summary <- c("VECTOR METRICS:")

if ("runup_dist_m" %in% names(hazard_metrics)) {
  vector_summary <- c(
    vector_summary,
    paste("  Parcels with runup data:", sum(!is.na(hazard_metrics$runup_dist_m)))
  )
  vector_metrics_present <- TRUE
}

if ("runup_exposed" %in% names(hazard_metrics)) {
  vector_summary <- c(
    vector_summary,
    paste("  Parcels exposed to runup:", sum(hazard_metrics$runup_exposed, na.rm = TRUE))
  )
}

if ("shore_dist_m" %in% names(hazard_metrics)) {
  vector_summary <- c(
    vector_summary,
    paste("  Parcels with shoreline data:", sum(!is.na(hazard_metrics$shore_dist_m)))
  )
  vector_metrics_present <- TRUE
}

if ("cliff_dist_m" %in% names(hazard_metrics)) {
  vector_summary <- c(
    vector_summary,
    paste("  Parcels with cliff data:", sum(!is.na(hazard_metrics$cliff_dist_m)))
  )
  vector_metrics_present <- TRUE
}

if ("cliff_exposed_50m" %in% names(hazard_metrics)) {
  vector_summary <- c(
    vector_summary,
    paste("  Parcels within 50m of cliff:", sum(hazard_metrics$cliff_exposed_50m, na.rm = TRUE))
  )
}

if ("cliff_exposed_100m" %in% names(hazard_metrics)) {
  vector_summary <- c(
    vector_summary,
    paste("  Parcels within 100m of cliff:", sum(hazard_metrics$cliff_exposed_100m, na.rm = TRUE))
  )
}

if ("in_squeeze_zone" %in% names(hazard_metrics)) {
  vector_summary <- c(
    vector_summary,
    paste("  Parcels in coastal squeeze zones:", sum(hazard_metrics$in_squeeze_zone, na.rm = TRUE))
  )
  vector_metrics_present <- TRUE
}

if (vector_metrics_present) {
  summary_lines <- c(summary_lines, vector_summary, "")
}

# Next steps guidance
summary_lines <- c(
  summary_lines,
  "NEXT STEPS:"
)

if (site_type == "CLIFF-ONLY" && file.exists(file.path(derived_dir, "cosmos_cliff_annual.csv"))) {
  summary_lines <- c(
    summary_lines,
    "  Note: Year-based cliff data saved separately to cosmos_cliff_annual.csv",
    "  1. Create interpolation script for cliff retreat timeline",
    "  2. Integrate cliff timeline with property economics",
    "  3. Run calculate_retreat_years.R to compute optimal retreat timing"
  )
} else if (site_type == "FLOOD-ONLY") {
  summary_lines <- c(
    summary_lines,
    "  1. Run interpolate_cosmos.R to create annual hazard timelines",
    "  2. Run calculate_retreat_years.R to compute optimal retreat timing"
  )
} else {
  summary_lines <- c(
    summary_lines,
    "  1. Run interpolate_cosmos.R for flood hazard timelines",
    "  2. Integrate cliff data (see cosmos_cliff_annual.csv if year-based)",
    "  3. Run calculate_retreat_years.R to compute optimal retreat timing"
  )
}

summary_lines <- c(summary_lines, "========================================")

summary_file <- file.path(derived_dir, "cosmos_hazard_metrics_summary.txt")
writeLines(summary_lines, summary_file)

cat("\n")
cat(paste(summary_lines, collapse = "\n"))
cat("\n\n")

message("✓ Summary saved: ", summary_file)