# CoSMoS HAZARD METRICS EXTRACTION - UNIFIED PIPELINE
#
# Purpose: Extract coastal flood and cliff retreat hazards from CoSMoS model data
#          for property-level managed retreat economic analysis
#
# Author: Will Dean
#
# WHAT THIS SCRIPT DOES
#
# This script processes USGS Coastal Storm Modeling System (CoSMoS) outputs to
# extract coastal hazard metrics at the parcel level across multiple sea level
# rise (SLR) scenarios and storm return periods. It handles two distinct coastal
# hazard types:
#
# 1. INUNDATION HAZARDS (for low-lying coastal areas):
#    - Flood depth (meters)
#    - Flood duration (hours)
#    - Wave height (meters) [optional]
#    - Storm scenarios: w000 (avg. conditions), w001 (1-yr), w020 (20-yr), w100 (100-yr)
#
# 2. CLIFF RETREAT HAZARDS (for bluff/cliff communities):
#    - Distance from present-day cliff edge (meters)
#    - Exposure flags (< 50m, < 100m from cliff)
#    - Management scenarios: "Let It Go" (natural retreat) vs. "Hold The Line" (armoring)
#
# The script auto-detects which hazard types are available in your data directories
# and extracts all present. This unified approach allows analysis of:
#   - Pure inundation sites (e.g., Carpinteria, King Salmon)
#   - Pure cliff retreat sites (e.g., Pacifica bluffs, Isla Vista)
#
# OUTPUT
#
# Creates: data/{case_name}/derived/cosmos_hazard_metrics.csv
#
# Structure: One row per parcel-SLR-scenario combination with columns:
#   - parcel_id: Unique property identifier
#   - slr_m: Sea level rise scenario (meters above MHHW)
#   - scenario: Management scenario ("LetItGo" or "HoldTheLine" for cliff sites)
#   - depth_m_w001, depth_m_w020, etc.: Flood depths for each storm
#   - duration_hr_w001, duration_hr_w020, etc.: Flood durations
#   - flooded_w001, flooded_w020, etc.: Boolean flags (depth >= 0.15m)
#   - cliff_dist_m: Distance to cliff edge at given SLR and management scenario
#   - cliff_exposed_50m, cliff_exposed_100m: Proximity flags
#   - [All original parcel attributes from redfin_df.csv]
#
# TECHNICAL NOTES
#
# CoSMoS Data Structure:
#   - Raster data organized by: cosmos/{metric}/{storm}/slrXXX_*.tif
#   - Vector cliff data: cosmos/cliff_retreat/*.gpkg with SLR scenarios in Name field
#
# SLR Baseline Fix (Critical for Cliff Data):
#   - CoSMoS cliff retreat lines are provided for multiple future SLR scenarios
#   - For economic modeling, we need CURRENT baseline cliff position
#   - Script **CURRENTLY(1/4)** identifies the LOWEST SLR scenario as the baseline (25cm)
#   - Distance calculations use this baseline to represent present-day exposure
#
# Flood Threshold:
#   - 0.15m (15cm) depth used to flag "flooded" status
#   - Filters out CoSMoS model noise and minor wet spots
#   - Represents actionable flooding that would affect property use/value
#
# Auto-Detection Logic:
#   - Script checks for existence of each metric directory
#   - Only extracts data that's actually present
#   - Warns user about missing datasets but continues processing
#   - Allows flexible application across sites with different hazard profiles
#

rm(list = ls())
library(terra)
library(sf)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(purrr)


# CONFIGURATION
case_name <- "king_salmon"  # Change for other sites
data_dir <- file.path("data", case_name)
cosmos_dir <- file.path(data_dir, "cosmos")
derived_dir <- file.path(data_dir, "derived")
dir.create(derived_dir, recursive = TRUE, showWarnings = FALSE)

# Storm scenarios: Extract ALL available return periods
# Script auto-detects which storms exist in your data directories
storm_scenarios <- c( "w001", "w020", "w100")

# Flood threshold for filtering model noise
flood_threshold_m <- 0.15


# STORM SCENARIO MODE
USE_STORM_SCENARIOS <- TRUE


if (USE_STORM_SCENARIOS) {
  message("MODE: Storm Scenarios (annual, 20yr, 100yr)")
} else {
  message("MODE: Average Conditions (legacy)")
}


# STORM HELPER FUNCTIONS

extract_slr_from_filename <- function(filename) {
  # Handle multiple filename formats:
  # - "flood_depth_slr025_..." (lowercase)
  # - "flood_depth_SLR025_..." (uppercase)  
  # - "flood_depth_SLR025_W100_flood_depth_ks.tif" (King Salmon format)
  slr_code <- str_extract(filename, "(?i)slr\\d{3}")
  if (is.na(slr_code)) return(NA_real_)
  slr_cm <- as.numeric(str_extract(slr_code, "\\d{3}"))
  slr_cm / 100
}

extract_at_points_storm <- function(raster_path, points_sf) {
  if (!file.exists(raster_path)) return(rep(NA_real_, nrow(points_sf)))
  tryCatch({
    r <- terra::rast(raster_path)
    
    # CRITICAL: Transform points to match raster CRS
    points_transformed <- st_transform(points_sf, crs(r))
    
    values <- terra::extract(r, vect(points_transformed), method = "simple")
    depths_raw <- if (ncol(values) > 1) values[[2]] else rep(NA_real_, nrow(points_sf))
    
    # Filter NoData values (negative or extremely large)
    depths_raw <- ifelse(is.na(depths_raw) | depths_raw < 0 | depths_raw > 1e6, NA_real_, depths_raw)
    
    # NOTE: Rasters from cosmos_process.R are ALREADY in meters (converted there)
    # No additional conversion needed
    depths_m <- depths_raw
    
    # Replace NA with 0 (no flooding)
    depths_m <- ifelse(is.na(depths_m), 0, depths_m)
    
    return(depths_m)
  }, error = function(e) {
    warning("Error extracting: ", e$message)
    rep(NA_real_, nrow(points_sf))
  })
}

process_storm_scenario <- function(storm_dir, storm_name, props_sf, parcel_ids) {
  message("\n  → ", storm_name)
  if (!dir.exists(storm_dir)) {
    warning("    Directory not found: ", storm_dir)
    return(NULL)
  }
  depth_files <- list.files(storm_dir, pattern = "flood_depth.*\\.tif$", full.names = TRUE)
  if (length(depth_files) == 0) {
    warning("    No rasters found")
    return(NULL)
  }
  message("    Found ", length(depth_files), " SLR scenarios")
  
  # Check CRS of first raster
  test_raster <- terra::rast(depth_files[1])
  message("    Raster CRS: ", crs(test_raster, describe = TRUE)$name)
  message("    Parcels CRS: ", st_crs(props_sf)$input)
  
  results_list <- list()
  n_extracted <- 0
  
  for (raster_path in depth_files) {
    filename <- basename(raster_path)
    slr_m <- extract_slr_from_filename(filename)
    if (is.na(slr_m)) {
      warning("    Could not parse SLR from: ", filename)
      next
    }
    message("      SLR ", sprintf("%.2f", slr_m), "m")
    depths <- extract_at_points_storm(raster_path, props_sf)
    
    # Check if extraction worked
    n_nonzero <- sum(depths > 0, na.rm = TRUE)
    n_na <- sum(is.na(depths))
    message("        Non-zero values: ", n_nonzero, " | NA values: ", n_na, " | Total: ", length(depths))
    
    results_list[[length(results_list) + 1]] <- tibble(
      parcel_id = parcel_ids,
      slr_m = slr_m,
      depth_m = depths
    )
    
    if (n_nonzero > 0) n_extracted <- n_extracted + 1
  }
  
  message("    Summary: ", n_extracted, "/", length(depth_files), " scenarios had non-zero data")
  
  if (length(results_list) == 0) {
    warning("    No data extracted - possible CRS mismatch")
    return(NULL)
  }
  
  bind_rows(results_list) %>%
    arrange(parcel_id, slr_m) %>%
    mutate(depth_m = if_else(is.na(depth_m), 0, depth_m))
}

# HELPER: DETECT AVAILABLE STORMS

detect_available_storms <- function(metric_name) {
  metric_base <- file.path(cosmos_dir, metric_name)
  if (!dir.exists(metric_base)) {
    message("  WARNING  Directory not found: ", metric_base)
    return(character(0))
  }
  
  all_subdirs <- list.dirs(metric_base, full.names = FALSE, recursive = FALSE)
  available <- intersect(storm_scenarios, all_subdirs)
  
  if (length(available) == 0) {
    message("  WARNING  No storm subdirectories found in ", metric_base)
  } else {
    message("  OK Found storms: ", paste(available, collapse = ", "))
  }
  
  return(available)
}


# LOAD PARCELS

parcel_path <- file.path(data_dir, "redfin_df.csv")
if (!file.exists(parcel_path)) {
  stop("ERROR Parcel file not found: ", parcel_path)
}

message("\n→ Loading parcels from: ", basename(parcel_path))
parcels_raw <- read_csv(parcel_path, show_col_types = FALSE)

# Robust column detection
lon_col <- intersect(names(parcels_raw), c("LONGITUDE", "longitude", "lon", "x", "X"))[1]
lat_col <- intersect(names(parcels_raw), c("LATITUDE", "latitude", "lat", "y", "Y"))[1]

stopifnot(!is.na(lon_col), !is.na(lat_col))

parcels <- parcels_raw %>%
  filter(!is.na(.data[[lon_col]]), !is.na(.data[[lat_col]])) %>%
  st_as_sf(coords = c(lon_col, lat_col), crs = 4326, remove = FALSE) %>%
  mutate(parcel_id = row_number())

message("  OK Loaded ", nrow(parcels), " parcels")

# STORM SCENARIO EXTRACTION

has_storm_data <- dir.exists(file.path(cosmos_dir, "storm_1yr_depth"))

if (USE_STORM_SCENARIOS && has_storm_data) {

  message("EXTRACTING STORM FLOOD SCENARIOS")
  
  # Annual storm
  annual_dir <- file.path(cosmos_dir, "storm_1yr_depth")
  storm_annual <- process_storm_scenario(annual_dir, "Annual Storm (1-year)", 
                                         parcels, parcels$parcel_id)
  
  # 20-year storm
  storm_20yr_dir <- file.path(cosmos_dir, "storm_20yr_depth")
  storm_20yr <- process_storm_scenario(storm_20yr_dir, "20-year Storm",
                                       parcels, parcels$parcel_id)
  
  # 100-year storm
  storm_100yr_dir <- file.path(cosmos_dir, "storm_100yr_depth")
  storm_100yr <- process_storm_scenario(storm_100yr_dir, "100-year Storm",
                                        parcels, parcels$parcel_id)
  
  # Save storm data
  message("SAVING STORM DATA")
  
  if (!is.null(storm_annual)) {
    out_path <- file.path(derived_dir, "storm_annual.csv")
    write_csv(storm_annual, out_path)
    message(" storm_annual.csv")
    message("  Rows: ", nrow(storm_annual), " | Parcels: ", n_distinct(storm_annual$parcel_id))
    message("  SLR: ", paste(sort(unique(storm_annual$slr_m)), collapse = ", "), " m\n")
  }
  
  if (!is.null(storm_20yr)) {
    out_path <- file.path(derived_dir, "storm_20yr.csv")
    write_csv(storm_20yr, out_path)
    message(" storm_20yr.csv")
    message("  Rows: ", nrow(storm_20yr), " | Parcels: ", n_distinct(storm_20yr$parcel_id))
    message("  SLR: ", paste(sort(unique(storm_20yr$slr_m)), collapse = ", "), " m\n")
  }
  
  if (!is.null(storm_100yr)) {
    out_path <- file.path(derived_dir, "storm_100yr.csv")
    write_csv(storm_100yr, out_path)
    message(" storm_100yr.csv")
    message("  Rows: ", nrow(storm_100yr), " | Parcels: ", n_distinct(storm_100yr$parcel_id))
    message("  SLR: ", paste(sort(unique(storm_100yr$slr_m)), collapse = ", "), " m\n")
  }
  
  message("✓ Storm extraction complete!")
  message("  Skipping average conditions flood extraction (using storms instead)\n")
  
  SKIP_AVERAGE_CONDITIONS <- TRUE
  raster_metrics <- tibble()  # Initialize empty so merge logic works
  
} else if (USE_STORM_SCENARIOS && !has_storm_data) {
  message("\n USE_STORM_SCENARIOS = TRUE but no storm data found")
  message("  Looking for: ", file.path(cosmos_dir, "storm_1yr_depth"))
  message("  Falling back to average conditions\n")
  SKIP_AVERAGE_CONDITIONS <- FALSE
  raster_metrics <- tibble()  # Will be filled by average conditions extraction
} else {
  message("\n→ Using average conditions (legacy mode)\n")
  SKIP_AVERAGE_CONDITIONS <- FALSE
  raster_metrics <- tibble()  # Will be filled by average conditions extraction
}


# SECTION A: RASTER EXTRACTION (ALL STORM SCENARIOS - AVERAGE CONDITIONS)
# Only runs if USE_STORM_SCENARIOS = FALSE or no storm data found

if (!exists("SKIP_AVERAGE_CONDITIONS") || !SKIP_AVERAGE_CONDITIONS) {
  
  message("EXTRACTING AVERAGE CONDITIONS (LEGACY)")
  
  # Helper function to extract one metric for one storm
  extract_raster_metric <- function(metric_name, storm) {
    metric_dir <- file.path(cosmos_dir, metric_name, storm)
    
    if (!dir.exists(metric_dir)) {
      return(tibble())
    }
    
    tif_files <- list.files(metric_dir, pattern = "\\.tif$", full.names = TRUE)
    
    if (length(tif_files) == 0) {
      return(tibble())
    }
    
    message("    Extracting ", metric_name, " (", storm, "): ", length(tif_files), " rasters")
    
    results <- map_dfr(tif_files, function(f) {
      # Parse SLR from filename
      fname <- basename(f)
      slr_match <- str_extract(fname, "(?i)slr\\d{3}")
      
      if (is.na(slr_match)) {
        message("      WARNING  Could not parse SLR from: ", fname)
        return(NULL)
      }
      
      slr_cm <- as.numeric(str_extract(slr_match, "\\d{3}"))
      slr_m <- slr_cm / 100
      
      # Extract values
      r <- rast(f)
      pts <- st_transform(parcels, crs(r))
      vals_raw <- terra::extract(r, vect(pts), ID = FALSE)[, 1]
      
      # Clean large NoData values
      vals_clean <- ifelse(is.finite(vals_raw) & vals_raw < 1e6, vals_raw, NA_real_)
      
      tibble(
        parcel_id = parcels$parcel_id,
        slr_m = slr_m,
        storm = storm,
        !!sym(metric_name) := vals_clean
      )
    })
    
    return(results)
  }
  
  # A1: FLOOD DEPTH - All storms
  message("\n→ FLOOD DEPTH - Extracting all available storms...")
  available_storms_depth <- detect_available_storms("flood_depth")
  
  if (length(available_storms_depth) > 0) {
    depth_list <- lapply(available_storms_depth, function(storm) {
      df <- extract_raster_metric("flood_depth", storm) %>%
        rename(!!paste0("depth_cm_", storm) := flood_depth) %>%
        mutate(
          !!paste0("depth_m_", storm) := .data[[paste0("depth_cm_", storm)]] / 100,
          !!paste0("depth_m_", storm) := pmax(.data[[paste0("depth_m_", storm)]], 0, na.rm = TRUE)
        ) %>%
        select(-!!paste0("depth_cm_", storm), -storm)
      return(df)
    })
    
    # Merge all storms
    depth_df <- reduce(depth_list, full_join, by = c("parcel_id", "slr_m"))
    message("  OK Depth extraction complete: ", nrow(depth_df), " rows")
  } else {
    depth_df <- tibble()
    message("  WARNING  No flood depth data found")
  }
  
  # A2: FLOOD DURATION - All storms
  if (nrow(depth_df) > 0) {
    message("\n→ FLOOD DURATION - Extracting all available storms...")
    available_storms_duration <- detect_available_storms("flood_duration")
    
    if (length(available_storms_duration) > 0) {
      duration_list <- lapply(available_storms_duration, function(storm) {
        df <- extract_raster_metric("flood_duration", storm) %>%
          rename(!!paste0("duration_hr_", storm) := flood_duration) %>%
          mutate(!!paste0("duration_hr_", storm) := pmax(.data[[paste0("duration_hr_", storm)]], 0, na.rm = TRUE)) %>%
          select(-storm)
        return(df)
      })
      
      duration_df <- reduce(duration_list, full_join, by = c("parcel_id", "slr_m"))
      message("  OK Duration extraction complete: ", nrow(duration_df), " rows")
    } else {
      duration_df <- tibble()
      message("  WARNING  No flood duration data found")
    }
    
    # A3: WAVE HEIGHT - All available storms (optional, not used in valuation)
    message("\n→ WAVE HEIGHT - Extracting all available storms...")
    available_storms_wave <- detect_available_storms("wave_ht")
    
    if (length(available_storms_wave) > 0) {
      wave_list <- lapply(available_storms_wave, function(storm) {
        df <- extract_raster_metric("wave_ht", storm) %>%
          rename(!!paste0("wave_ht_m_", storm) := wave_ht) %>%
          mutate(!!paste0("wave_ht_m_", storm) := pmax(.data[[paste0("wave_ht_m_", storm)]], 0, na.rm = TRUE)) %>%
          select(-storm)
        return(df)
      })
      
      wave_df <- reduce(wave_list, full_join, by = c("parcel_id", "slr_m"))
      message("  OK Wave height extraction complete: ", nrow(wave_df), " rows")
    } else {
      wave_df <- tibble()
      message("  WARNING  No wave height data found")
    }
    
    # Merge all raster metrics
    raster_metrics <- depth_df
    if (nrow(duration_df) > 0) {
      raster_metrics <- raster_metrics %>% full_join(duration_df, by = c("parcel_id", "slr_m"))
    }
    if (nrow(wave_df) > 0) {
      raster_metrics <- raster_metrics %>% full_join(wave_df, by = c("parcel_id", "slr_m"))
    }
    
    # Add flooding flags for each storm
    for (storm in available_storms_depth) {
      depth_col <- paste0("depth_m_", storm)
      if (depth_col %in% names(raster_metrics)) {
        raster_metrics <- raster_metrics %>%
          mutate(!!paste0("flooded_", storm) := .data[[depth_col]] >= flood_threshold_m)
      }
    }
    
    message("  OK Merged raster metrics: ", nrow(raster_metrics), " rows, ", ncol(raster_metrics), " columns")
    
  } else {
    raster_metrics <- tibble()
    message("  WARNING  No raster metrics available")
  }
  
} # End of average conditions extraction

# SECTION B: CLIFF RETREAT - EXTRACT ALL SLR SCENARIOS

message("\n→ CLIFF RETREAT - Extracting all SLR scenarios...")

cliff_dir <- file.path(cosmos_dir, "cliff_retreat")

if (!dir.exists(cliff_dir)) {
  message("  WARNING  Cliff directory not found")
  cliff_metrics <- tibble()
} else {
  gpkg_files <- list.files(cliff_dir, pattern = "\\.gpkg$", full.names = TRUE)
  
  if (length(gpkg_files) == 0) {
    message("  WARNING  No .gpkg files found")
    cliff_metrics <- tibble()
  } else {
    message("  Found ", length(gpkg_files), " cliff file(s)")
    
    # Look for BOTH management scenario files
    letitgo_files <- gpkg_files[grepl("LetItGo", basename(gpkg_files), ignore.case = TRUE)]
    letitgo_files <- letitgo_files[!grepl("Uncertainty", basename(letitgo_files), ignore.case = TRUE)]
    
    holdtheline_files <- gpkg_files[grepl("HoldTheLine", basename(gpkg_files), ignore.case = TRUE)]
    holdtheline_files <- holdtheline_files[!grepl("Uncertainty", basename(holdtheline_files), ignore.case = TRUE)]
    
    # Combine both scenarios for processing
    all_scenario_files <- c(letitgo_files, holdtheline_files)
    
    if (length(all_scenario_files) == 0) {
      message("  WARNING  No management scenario files found (LetItGo or HoldTheLine)")
      message("  Available files:")
      for (f in basename(gpkg_files)) {
        message("    - ", f)
      }
      cliff_metrics <- tibble()
    } else {
      
      message("  Found ", length(letitgo_files), " LetItGo file(s)")
      message("  Found ", length(holdtheline_files), " HoldTheLine file(s)")
      
      cliff_list <- list()  # Will store tibbles for each SLR scenario × management scenario
      
      # Process each management scenario file
      for (cliff_file in all_scenario_files) {
        
        # Determine which management scenario this file represents
        scenario_name <- if (grepl("LetItGo", basename(cliff_file), ignore.case = TRUE)) {
          "LetItGo"
        } else if (grepl("HoldTheLine", basename(cliff_file), ignore.case = TRUE)) {
          "HoldTheLine"  
        } else {
          "Unknown"
        }
        
        message("\n  Processing: ", basename(cliff_file), " [", scenario_name, "]")
        
        tryCatch({
          cliff_data <- st_read(cliff_file, quiet = TRUE)
          message("    Features: ", nrow(cliff_data))
          message("    Columns: ", paste(names(cliff_data), collapse = ", "))
          
          
          # STRATEGY 1: SoCal format - SLR encoded in Name field as text
          
          # Format: "Cliff position, 0.25 m SLR" or similar
          
          if ("Name" %in% names(cliff_data)) {
            message("    Detected SoCal format (SLR in Name field)")
            
            # Show sample Names
            sample_names <- head(unique(cliff_data$Name), 3)
            message("    Sample Names: ", paste(sample_names, collapse = " | "))
            
            # Extract SLR values from Name field
            # Pattern matches: "X.XX m" or "X.X m" or "X m"
            cliff_data$slr_m_parsed <- NA_real_
            
            for (i in 1:nrow(cliff_data)) {
              name_str <- as.character(cliff_data$Name[i])
              
              # Try to extract number before "m SLR" or "m slr"
              # Matches patterns like "0.25 m SLR", "1.0 m SLR", "2 m SLR"
              matches <- str_match(name_str, "([0-9]+\\.?[0-9]*) ?m")
              
              if (!is.na(matches[1, 2])) {
                cliff_data$slr_m_parsed[i] <- as.numeric(matches[1, 2])
              }
            }
            
            # Check if parsing succeeded
            parsed_slr <- unique(cliff_data$slr_m_parsed[!is.na(cliff_data$slr_m_parsed)])
            
            if (length(parsed_slr) > 0) {
              message("    Parsed SLR scenarios (m): ", paste(sort(parsed_slr), collapse = ", "))
              
              # Extract distances for each SLR scenario
              for (slr_val in sort(parsed_slr)) {
                # Get features for this SLR level
                cliff_subset <- cliff_data %>% filter(slr_m_parsed == slr_val)
                
                if (nrow(cliff_subset) > 0) {
                  # Transform parcels to match cliff data CRS
                  parcels_proj <- st_transform(parcels, st_crs(cliff_subset))
                  
                  # Calculate minimum distance from each parcel to cliff lines
                  distances <- st_distance(parcels_proj, cliff_subset)
                  min_dist_m <- as.numeric(apply(distances, 1, min))
                  
                  # Store results
                  cliff_list[[length(cliff_list) + 1]] <- tibble(
                    parcel_id = parcels$parcel_id,
                    slr_m = slr_val,
                    cliff_dist_m = min_dist_m,
                    scenario = scenario_name
                  )
                  
                  message("      ✓ SLR = ", slr_val, " m: ", 
                          sum(min_dist_m < 100), " parcels within 100m")
                }
              }
              
            } else {
              message("    WARNING  Could not parse SLR values from Name field")
              message("    Name field values:")
              print(head(cliff_data$Name, 5))
            }
            
            
            # STRATEGY 2: Files with explicit SLR_cm or SLR fields
          } else if (any(c("SLR_cm", "SLR", "slr_cm", "slr") %in% names(cliff_data))) {
            
            message("    Detected explicit SLR field")
            
            # Find the SLR field
            slr_field <- NA
            for (field in c("SLR_cm", "SLR", "slr_cm", "slr")) {
              if (field %in% names(cliff_data)) {
                slr_field <- field
                break
              }
            }
            
            # Get unique SLR values
            slr_values <- unique(cliff_data[[slr_field]])
            slr_values <- slr_values[!is.na(slr_values)]
            
            # Determine if values are in cm or m
            if (all(slr_values >= 10)) {
              # Values in cm
              slr_m_values <- slr_values / 100
              message("    Found SLR scenarios (cm): ", paste(sort(slr_values), collapse = ", "))
            } else {
              # Values already in m
              slr_m_values <- slr_values
              message("    Found SLR scenarios (m): ", paste(sort(slr_values), collapse = ", "))
            }
            
            # Extract for each SLR scenario
            for (i in seq_along(slr_values)) {
              slr_val <- slr_values[i]
              slr_m <- slr_m_values[i]
              
              cliff_subset <- cliff_data %>% filter(.data[[slr_field]] == slr_val)
              
              if (nrow(cliff_subset) > 0) {
                parcels_proj <- st_transform(parcels, st_crs(cliff_subset))
                distances <- st_distance(parcels_proj, cliff_subset)
                min_dist_m <- as.numeric(apply(distances, 1, min))
                
                cliff_list[[length(cliff_list) + 1]] <- tibble(
                  parcel_id = parcels$parcel_id,
                  slr_m = slr_m,
                  cliff_dist_m = min_dist_m,
                  scenario = scenario_name
                )
                
                message("      ✓ SLR = ", slr_m, " m: ", 
                        sum(min_dist_m < 100), " parcels within 100m")
              }
            }
            
            # ====================================================================
            # STRATEGY 3: CenCal format - numeric Name field
            # ====================================================================
          } else if ("Name" %in% names(cliff_data)) {
            
            # Try interpreting Name as numeric (CenCal uses cm values like 0, 25, 50, 100)
            name_numeric <- suppressWarnings(as.numeric(cliff_data$Name))
            
            if (any(!is.na(name_numeric))) {
              message("    Detected CenCal format (numeric Name field)")
              
              slr_cm_values <- unique(name_numeric[!is.na(name_numeric)])
              slr_m_values <- slr_cm_values / 100
              
              message("    Found SLR scenarios (cm): ", paste(sort(slr_cm_values), collapse = ", "))
              
              # Extract for each SLR scenario
              for (i in seq_along(slr_cm_values)) {
                slr_cm <- slr_cm_values[i]
                slr_m <- slr_m_values[i]
                
                cliff_subset <- cliff_data %>% 
                  filter(!is.na(suppressWarnings(as.numeric(Name)))) %>%
                  filter(suppressWarnings(as.numeric(Name)) == slr_cm)
                
                if (nrow(cliff_subset) > 0) {
                  parcels_proj <- st_transform(parcels, st_crs(cliff_subset))
                  distances <- st_distance(parcels_proj, cliff_subset)
                  min_dist_m <- as.numeric(apply(distances, 1, min))
                  
                  cliff_list[[length(cliff_list) + 1]] <- tibble(
                    parcel_id = parcels$parcel_id,
                    slr_m = slr_m,
                    cliff_dist_m = min_dist_m,
                    scenario = scenario_name
                  )
                  
                  message("      ✓ SLR = ", slr_m, " m: ", 
                          sum(min_dist_m < 100), " parcels within 100m")
                }
              }
            }
          } else {
            message("    WARNING  Cannot identify SLR encoding format")
            message("    Available fields: ", paste(names(cliff_data), collapse = ", "))
          }
          
        }, error = function(e) {
          message("    ERROR  ", e$message)
        })
      }
      
      # --------------------------------------------------------------------
      # Combine all SLR scenarios
      # --------------------------------------------------------------------
      
      if (length(cliff_list) > 0) {
        cliff_metrics <- bind_rows(cliff_list) %>%
          arrange(parcel_id, slr_m)
        
        # Add exposure flags based on California Coastal Commission setback guidelines
        cliff_metrics <- cliff_metrics %>%
          mutate(
            cliff_exposed_10m = cliff_dist_m < 10,   # Immediate danger - evacuate
            cliff_exposed_25m = cliff_dist_m < 25,   # Planned retreat - remove within 5-10 yrs
            cliff_exposed_50m = cliff_dist_m < 50,   # Warning zone - start planning
            cliff_exposed_100m = cliff_dist_m < 100  # Watch zone - long-term monitoring
          )
        
        message("\n  CLIFF EXTRACTION COMPLETE")
        message("    Total rows: ", nrow(cliff_metrics))
        message("    Parcels: ", n_distinct(cliff_metrics$parcel_id))
        message("    Management scenarios: ", paste(sort(unique(cliff_metrics$scenario)), collapse = ", "))
        message("    SLR scenarios: ", paste(sort(unique(cliff_metrics$slr_m)), collapse = ", "), " m")
        message("\n  Exposure thresholds (ever reached across all SLR scenarios):")
        message("    Properties within 10m (IMMEDIATE):  ", 
                n_distinct(cliff_metrics$parcel_id[cliff_metrics$cliff_exposed_10m]))
        message("    Properties within 25m (RETREAT):    ", 
                n_distinct(cliff_metrics$parcel_id[cliff_metrics$cliff_exposed_25m]))
        message("    Properties within 50m (WARNING):    ", 
                n_distinct(cliff_metrics$parcel_id[cliff_metrics$cliff_exposed_50m]))
        message("    Properties within 100m (WATCH):     ", 
                n_distinct(cliff_metrics$parcel_id[cliff_metrics$cliff_exposed_100m]))
        
      } else {
        message("\n  WARNING  Could not extract cliff data from any files")
        cliff_metrics <- tibble()
      }
    }
  }
}

# FINAL MERGE AND SAVE

message("MERGING AND SAVING FINAL OUTPUT")

# Determine what data we have
has_raster <- exists("raster_metrics") && nrow(raster_metrics) > 0
has_storm <- (exists("storm_annual") && !is.null(storm_annual)) ||
  (exists("storm_20yr") && !is.null(storm_20yr)) ||
  (exists("storm_100yr") && !is.null(storm_100yr))
has_cliff <- exists("cliff_metrics") && nrow(cliff_metrics) > 0

if (!has_raster && !has_storm && !has_cliff) {
  stop("ERROR: No hazard data extracted. Check your cosmos/ directory structure.")
}

# Build the base grid (parcel_id × slr_m combinations)
if (has_raster) {
  # Use average conditions data
  base_grid <- raster_metrics
  message("→ Using average conditions flood data")
  
} else if (has_storm) {
  # Use storm scenarios
  message("→ Using storm scenarios flood data")
  
  # Collect all unique SLR scenarios
  all_slr <- c()
  if (exists("storm_annual") && !is.null(storm_annual)) all_slr <- c(all_slr, storm_annual$slr_m)
  if (exists("storm_20yr") && !is.null(storm_20yr)) all_slr <- c(all_slr, storm_20yr$slr_m)
  if (exists("storm_100yr") && !is.null(storm_100yr)) all_slr <- c(all_slr, storm_100yr$slr_m)
  all_slr <- sort(unique(all_slr))
  
  # Create base grid
  base_grid <- expand_grid(
    parcel_id = 1:nrow(parcels),
    slr_m = all_slr
  )
  
  # Merge storm data
  if (exists("storm_annual") && !is.null(storm_annual)) {
    base_grid <- base_grid %>%
      left_join(storm_annual %>% select(parcel_id, slr_m, depth_m), 
                by = c("parcel_id", "slr_m"))
  }
  
  # Note: For King Salmon, interpolate_cosmos.R expects depth_m for average conditions
  # Storm-specific columns (depth_m_w001, etc.) are handled by monte_carlo_storms.R later
  
} else if (has_cliff) {
  # Cliff-only site
  message("→ Using cliff retreat data only (no flood data)")
  all_slr <- sort(unique(cliff_metrics$slr_m))
  base_grid <- expand_grid(
    parcel_id = 1:nrow(parcels),
    slr_m = all_slr
  )
}

# Merge cliff data if available
if (has_cliff) {
  message("→ Merging cliff retreat data")
  
  if ("scenario" %in% names(cliff_metrics)) {
    # Has management scenarios - expand grid
    scenarios <- sort(unique(cliff_metrics$scenario))
    base_grid <- map_dfr(scenarios, ~base_grid %>% mutate(scenario = .x))
    base_grid <- base_grid %>%
      left_join(cliff_metrics, by = c("parcel_id", "slr_m", "scenario"))
  } else {
    # No management scenarios
    base_grid <- base_grid %>%
      left_join(cliff_metrics, by = c("parcel_id", "slr_m"))
  }
}

# Add parcel attributes
parcels_clean <- parcels %>% 
  st_drop_geometry() %>% 
  mutate(parcel_id = row_number())

final_output <- base_grid %>%
  left_join(parcels_clean, by = "parcel_id")

# Save
output_path <- file.path(derived_dir, "cosmos_hazard_metrics.csv")
write_csv(final_output, output_path)

message("\n✓ SAVED: cosmos_hazard_metrics.csv")
message("  Path: ", output_path)
message("  Rows: ", format(nrow(final_output), big.mark = ","))
message("  Parcels: ", n_distinct(final_output$parcel_id))
message("  SLR scenarios: ", paste(sort(unique(final_output$slr_m)), collapse = ", "), " m")

if ("scenario" %in% names(final_output)) {
  message("  Management scenarios: ", paste(sort(unique(final_output$scenario)), collapse = ", "))
}

message("EXTRACTION COMPLETE!")
message("Next step: Run interpolate_cosmos.R to create annual timelines\n")
