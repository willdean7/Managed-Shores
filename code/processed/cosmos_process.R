# cosmos_process.R
# Purpose: Pre-process CoSMoS storm scenarios with selective SLR level filtering
# Author: Will Dean
#
# OPTIMIZATION: Only processes SLR levels needed for OPC 2024 interpolation
#
# For each storm scenario, processes flood depth and duration across strategic SLR levels

rm(list = ls())
library(sf)
library(terra)
library(stringr)
library(dplyr)
library(tools)


# CONFIGURATION
# Paths
external_base <- "/Volumes/TOSHIBA HD/Managed_Shores"
project_base  <- "~/Documents/Managed_Shores"
crs_work      <- 3310  # CA Albers (meters)

# Case study selection
case_name <- "king_salmon"  # Options: carpinteria, king_salmon, silver_strand, isla_vista, pacifica
case_abbr <- "ks"         # "carp", "ks", "ss", "iv", "pac"

# AOI bounding boxes (WGS84)
bbox_lookup <- list(
  carpinteria   = c(xmin = -119.55, ymin = 34.38, xmax = -119.52, ymax = 34.41),
  king_salmon   = c(xmin = -124.22463, ymin = 40.73482, xmax = -124.21277, ymax = 40.74378),
  silver_strand = c(xmin = -119.23, ymin = 34.14, xmax = -119.21, ymax = 34.16),
  isla_vista    = c(xmin = -119.88, ymin = 34.40, xmax = -119.84, ymax = 34.42),
  pacifica      = c(xmin = -122.52, ymin = 37.60, xmax = -122.48, ymax = 37.65)
)

bbox4326 <- st_bbox(bbox_lookup[[case_name]], crs = st_crs(4326))

# Output directory
local_output <- file.path(project_base, sprintf("data/%s/cosmos", case_name))
dir.create(local_output, recursive = TRUE, showWarnings = FALSE)

message("========================================")
message("CoSMoS Storm Processing: ", toupper(case_name))
message("========================================")
message("AOI: ", paste(names(bbox4326), round(bbox4326, 3), collapse = ", "))
message("Output: ", local_output)
message("========================================\n")

# Expand AOI
bbox_poly_work         <- st_transform(st_as_sfc(bbox4326), crs_work) |> st_buffer(500)
bbox_poly4326_expanded <- st_transform(bbox_poly_work, 4326)


# SLR LEVEL FILTERING - KEY OPTIMIZATION

# Only process SLR levels needed for OPC 2024 interpolation
# OPC High scenario goes up to ~2.0m, so we need good coverage from 0-2.5m

# STRATEGY: Process every other level up to 2.0m, then coarser beyond
SLR_LEVELS_TO_PROCESS <- c(
  "slr000",  # 0.0m - baseline
  "slr025",  # 0.25m
  "slr050",  # 0.5m
  "slr075",  # 0.75m
  "slr100",  # 1.0m
  "slr125",  # 1.25m
  "slr150",  # 1.5m
  "slr175",  # 1.75m
  "slr200",  # 2.0m
  "slr250",  # 2.5m (for extrapolation safety)
  "slr300"   # 3.0m (upper bound)
)

message("SLR OPTIMIZATION:")
message("  Processing ", length(SLR_LEVELS_TO_PROCESS), " SLR levels (instead of all ~15)")
message("  Levels: ", paste(SLR_LEVELS_TO_PROCESS, collapse = ", "))

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

crop_raster_to_bbox <- function(r, bbox_poly_4326) {
  b <- st_transform(bbox_poly_4326, crs(r))
  r_c <- terra::crop(r, vect(b))
  terra::mask(r_c, vect(b))
}

clip_vector_to_bbox <- function(v, bbox_poly_4326) {
  b <- st_transform(bbox_poly_4326, st_crs(v))
  suppressWarnings(st_intersection(v, b))
}

discover_scenarios <- function(parent_dir) {
  if (!dir.exists(parent_dir)) {
    warning("Directory not found: ", parent_dir)
    return(character(0))
  }
  
  dirs <- list.dirs(parent_dir, full.names = FALSE, recursive = FALSE)
  
  # MATCH both uppercase and lowercase patterns
  dirs <- dirs[grepl("^slr\\d{3}_w\\d{3}_", dirs, ignore.case = TRUE)]
  
  # FILTER to only desired SLR levels (case-insensitive)
  slr_codes <- tolower(str_extract(dirs, "(?i)^slr\\d{3}"))  # Extract and convert to lowercase
  dirs <- dirs[slr_codes %in% SLR_LEVELS_TO_PROCESS]
  
  sort(dirs)
}

merge_crop_save <- function(tile_dir, out_dir, out_stub, mosaic_fun = max) {
  
  tif_files <- list.files(tile_dir, pattern = "\\.tif$", full.names = TRUE)
  
  if (!length(tif_files)) { 
    message("  ⚠ No .tif files found in: ", tile_dir)
    return(invisible(NULL)) 
  }
  
  # FILTER to only HU07 and HU08 tiles for King Salmon case study
  if (case_name == "king_salmon") {
    tif_files <- tif_files[grepl("HU0[78]_", basename(tif_files))]
    
    if (!length(tif_files)) {
      message("  ⚠ No HU07 or HU08 tiles found")
      return(invisible(NULL))
    }
    message("  → Processing ", length(tif_files), " tiles (HU07-HU08 only for King Salmon)...")
  } else {
    message("  → Processing ", length(tif_files), " tiles...")
  }
  
  # Read and project rasters with error handling
  rs <- list()
  for (i in seq_along(tif_files)) {
    tryCatch({
      r <- terra::rast(tif_files[i])
      
      # CRITICAL: Convert to float and divide by 100 to get meters
      # Original data is in centimeters stored as INT4U
      r <- r / 100.0
      
      # Set values < 0.01 (1 cm) to NA (likely noise/nodata)
      r[r < 0.01] <- NA
      
      if (!is.null(r) && inherits(r, "SpatRaster")) {
        r_proj <- terra::project(r, paste0("EPSG:", crs_work), method = "bilinear")
        if (!is.null(r_proj) && inherits(r_proj, "SpatRaster")) {
          rs[[length(rs) + 1]] <- r_proj
        }
      }
    }, error = function(e) {
      warning("    Error reading ", basename(tif_files[i]), ": ", e$message)
    })
  }
  
  if (length(rs) == 0) {
    message("  ⚠ No valid rasters after projection")
    return(invisible(NULL))
  }
  
  # Align all rasters to first one
  template <- rs[[1]]
  rs_aligned <- lapply(rs, function(r) {
    if (!terra::compareGeom(r, template, stopOnError = FALSE)) {
      terra::resample(r, template, method = "bilinear")
    } else {
      r
    }
  })
  
  fun <- if (is.character(mosaic_fun)) match.fun(mosaic_fun) else mosaic_fun
  
  # Merge rasters
  message("  → Mosaicking ", length(rs_aligned), " tiles...")
  merged <- if (length(rs_aligned) == 1) {
    rs_aligned[[1]]
  } else {
    src <- terra::sprc(rs_aligned)
    terra::mosaic(src, fun = fun)
  }
  
  # Crop to AOI
  message("  → Cropping to AOI...")
  cropped <- crop_raster_to_bbox(merged, bbox_poly4326_expanded)
  
  # Save
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(out_dir, paste0(out_stub, ".tif"))
  terra::writeRaster(cropped, out_path, overwrite = TRUE,
                     filetype = "GTiff",
                     datatype = "FLT4S",
                     gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=IF_SAFER"))
  
  message("  ✓ Saved: ", basename(out_path))
  return(out_path)
}

process_storm_metric <- function(base_dir, out_dir, metric_name) {
  
  if (!dir.exists(base_dir)) {
    warning("Storm directory not found: ", base_dir)
    return(invisible(NULL))
  }
  
  scenarios <- discover_scenarios(base_dir)
  
  if (length(scenarios) == 0) {
    message("  ⚠ No scenarios found in: ", base_dir)
    return(invisible(NULL))
  }
  
  message("  Found ", length(scenarios), " scenarios (filtered from all available)")
  
  for (s in scenarios) {
    message("\n  Scenario: ", s)
    src_dir <- file.path(base_dir, s)
    out_stub <- paste0(metric_name, "_", s, "_", case_abbr)
    merge_crop_save(src_dir, out_dir, out_stub, mosaic_fun = max)
  }
  
  invisible(NULL)
}

# ============================================================================
# STORM SCENARIO PROCESSING
# change the folder names for each case study
# ============================================================================

storm_configs <- list(
  annual = list(
    name = "1-year storm (annual)",
    folder = "CoSMoS_1yr_storm_flood_depth_and_duration_king_salmon",
    abbrev = "1yr"
  ),
  storm_20yr = list(
    name = "20-year storm",
    folder = "CoSMoS_20yr_storm_flood_depth_and_duration_king_salmon",
    abbrev = "20yr"
  ),
  storm_100yr = list(
    name = "100-year storm",
    folder = "CoSMoS_100yr_storm_flood_depth_and_duration_king_salmon",
    abbrev = "100yr"
  )
)


for (storm_type in names(storm_configs)) {
  
  config <- storm_configs[[storm_type]]
  
  message("\n========================================")
  message("PROCESSING: ", toupper(config$name))
  message("========================================\n")
  
  storm_base <- file.path(external_base, config$folder)
  
  if (!dir.exists(storm_base)) {
    warning("Storm base directory not found: ", storm_base)
    warning("Skipping ", config$name)
    next
  }
  
  # --- FLOOD DEPTH ---
  message("Processing flood depth...")
  depth_base <- file.path(storm_base, "flood_depth")
  depth_out  <- file.path(local_output, paste0("storm_", config$abbrev, "_depth"))
  process_storm_metric(depth_base, depth_out, "flood_depth")
  
  # --- FLOOD DURATION ---
  message("\nProcessing flood duration...")
  duration_base <- file.path(storm_base, "flood_duration")
  duration_out  <- file.path(local_output, paste0("storm_", config$abbrev, "_duration"))
  process_storm_metric(duration_base, duration_out, "flood_duration")
  
  message("\n✓ Completed: ", config$name, "\n")
}

# ============================================================================
# SUMMARY
# ============================================================================

message("\n========================================")
message("PROCESSING COMPLETE")
message("========================================")

all_outputs <- list.files(local_output, pattern = "\\.tif$", 
                          recursive = TRUE, full.names = FALSE)

if (length(all_outputs) > 0) {
  message("\nCreated ", length(all_outputs), " raster files:")
  
  for (storm_type in names(storm_configs)) {
    config <- storm_configs[[storm_type]]
    storm_files <- all_outputs[grepl(config$abbrev, all_outputs)]
    
    if (length(storm_files) > 0) {
      message("\n", config$name, " (", length(storm_files), " files):")
      for (f in sort(storm_files)) {
        message("  - ", f)
      }
    }
  }
  
  message("\n✓ All outputs saved to: ", local_output)
  
} else {
  message("\n⚠ No output files created. Check warnings above.")
}

message("\n========================================")
message("Next steps:")
message("  1. Check output files in: ", local_output)
message("  2. Run extract_cosmos_metrics_unified.R")
message("  3. Results ready for Monte Carlo simulation")
message("========================================\n")