# EXTRACT COSMOS METRICS
# This script extracts w000, w001, w020, w100 + SLR cliff distance fix
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
case_name <- "pacifica"  # Change for other sites
data_dir <- file.path("data", case_name)
cosmos_dir <- file.path(data_dir, "cosmos")
derived_dir <- file.path(data_dir, "derived")
dir.create(derived_dir, recursive = TRUE, showWarnings = FALSE)

# Storm scenarios: Extract ALL available return periods
# Script auto-detects which storms exist in your data directories
storm_scenarios <- c("w000", "w001", "w020", "w100")

# Flood threshold for filtering model noise
flood_threshold_m <- 0.15

message("=" * 80)
message("CoSMoS Metrics Extraction: ", toupper(case_name))
message("Configured storms: ", paste(storm_scenarios, collapse = ", "))
message("Script will extract all available storm scenarios")
message("=" * 80)

# ============================================================================
# HELPER: DETECT AVAILABLE STORMS
# ============================================================================

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

# ============================================================================
# LOAD PARCELS
# ============================================================================

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

# ============================================================================
# SECTION A: RASTER EXTRACTION (ALL STORM SCENARIOS)
# ============================================================================

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

# ============================================================================
# SECTION B: CLIFF RETREAT (WITH SLR FIX)
# ============================================================================

message("\n→ CLIFF RETREAT - Extracting with SLR baseline fix...")

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
    # Prioritize non-uncertainty, LetItGo files
    letitgo_files <- gpkg_files[grepl("LetItGo|let.*go", basename(gpkg_files), ignore.case = TRUE)]
    letitgo_main <- letitgo_files[!grepl("Uncertainty", basename(letitgo_files), ignore.case = TRUE)]
    
    if (length(letitgo_main) > 0) {
      file_path <- letitgo_main[1]
      message("  Processing: ", basename(file_path))
      
      cliff_lines <- st_read(file_path, quiet = TRUE)
      message("    Features: ", nrow(cliff_lines))
      message("    Columns: ", paste(head(names(cliff_lines), 5), collapse = ", "))
      
      # STRATEGY 2.5: Check for SLR values in Name field
      has_name <- "Name" %in% names(cliff_lines)
      
      if (has_name) {
        unique_names <- unique(cliff_lines$Name)
        slr_in_names <- suppressWarnings(as.numeric(unique_names))
        slr_in_names <- slr_in_names[!is.na(slr_in_names)]
        
        if (length(slr_in_names) > 0 && all(slr_in_names >= 0 & slr_in_names <= 1000)) {
          message("    Found SLR values (cm): ", paste(sort(slr_in_names), collapse = ", "))
          
          # Use LOWEST SLR as baseline
          baseline_slr <- min(slr_in_names)
          message("    Using SLR = ", baseline_slr, "cm as baseline")
          
          baseline_cliff <- cliff_lines %>% filter(Name == as.character(baseline_slr))
          
          if (nrow(baseline_cliff) > 0) {
            # Calculate distances
            parcels_proj <- st_transform(parcels, st_crs(baseline_cliff))
            distances <- st_distance(parcels_proj, baseline_cliff)
            min_dist_m <- as.numeric(apply(distances, 1, min))
            
            cliff_metrics <- tibble(
              parcel_id = parcels$parcel_id,
              scenario = "LetItGo",
              slr_m = baseline_slr / 100,
              cliff_dist_m = min_dist_m,
              cliff_exposed_50m = min_dist_m < 50,
              cliff_exposed_100m = min_dist_m < 100
            )
            
            message("    OK Calculated distances using baseline SLR = ", baseline_slr, "cm")
          } else {
            message("    WARNING  No features for baseline SLR")
            cliff_metrics <- tibble()
          }
        } else {
          message("    WARNING  Name field doesn't contain SLR values")
          cliff_metrics <- tibble()
        }
      } else {
        message("    WARNING  No 'Name' field found")
        cliff_metrics <- tibble()
      }
    } else {
      message("  WARNING  No LetItGo files found")
      cliff_metrics <- tibble()
    }
  }
}

# ============================================================================
# MERGE AND SAVE
# ============================================================================

message("\n→ Merging all metrics...")

if (nrow(raster_metrics) > 0 && nrow(cliff_metrics) > 0) {
  # Merge raster + cliff
  hazard_metrics <- raster_metrics %>%
    left_join(cliff_metrics %>% select(-scenario), by = "parcel_id")
} else if (nrow(raster_metrics) > 0) {
  hazard_metrics <- raster_metrics
} else if (nrow(cliff_metrics) > 0) {
  hazard_metrics <- cliff_metrics
} else {
  stop("ERROR No hazard metrics extracted!")
}

# Add parcel info
hazard_metrics <- parcels %>%
  st_drop_geometry() %>%
  select(parcel_id, everything()) %>%
  right_join(hazard_metrics, by = "parcel_id")

# Save
output_path <- file.path(derived_dir, "cosmos_hazard_metrics.csv")
write_csv(hazard_metrics, output_path)

message(" EXTRACTION COMPLETE")
message("   Output: ", output_path)
message("   Parcels: ", length(unique(hazard_metrics$parcel_id)))
message("   Columns: ", ncol(hazard_metrics))
message("")
message("Storm columns created:")
for (storm in storm_scenarios) {
  depth_col <- paste0("depth_m_", storm)
  if (depth_col %in% names(hazard_metrics)) {
    message("  OK ", storm, ": depth, duration, flooded flag")
  }
}
message("")
