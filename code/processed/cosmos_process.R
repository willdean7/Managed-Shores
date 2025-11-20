#The purpose of the script is to pre-process the large CoSMoS datasets to case study locations before integrating them into the Managed Shores model.

rm(list = ls())
library(sf)
library(terra)
library(stringr)
library(dplyr)
library(tools)

# CONFIG 
external_base <- "/Volumes/TOSHIBA HD/Managed_Shores"   # external drive root
project_base  <- "~/Documents/Managed_Shores"            # project root
crs_work      <- 3310                                    # CA Albers (meters)

# Choose the case and AOI here (only place you edit)
case_name <- "carpinteria"  # change to "silver_strand" to run that site etc...
case_abbr <- "carp"  

# AOI bbox (4326) — set per case
bbox_lookup <- list(
  carpinteria   = c(xmin=-119.55, ymin=34.38, xmax=-119.52, ymax=34.41),
  silver_strand = c(xmin=-119.23, ymin=34.14, xmax=-119.21, ymax=34.16) # <- adjust if needed
)
bbox4326 <- st_bbox(bbox_lookup[[case_name]], crs = st_crs(4326))

# Output root
local_output <- file.path(project_base, sprintf("data/%s/cosmos", case_name))
dir.create(local_output, recursive = TRUE, showWarnings = FALSE)

# Expand AOI by 1000 m in working CRS
bbox_poly_work         <- st_transform(st_as_sfc(bbox4326), crs_work) |> st_buffer(1000)
bbox_poly4326_expanded <- st_transform(bbox_poly_work, 4326)

# helper functions
crop_raster_to_bbox <- function(r, bbox_poly_4326) {
  b <- st_transform(bbox_poly_4326, crs(r))
  r_c <- terra::crop(r, vect(b))
  terra::mask(r_c, vect(b))
}
clip_vector_to_bbox <- function(v, bbox_poly_4326) {
  b <- st_transform(bbox_poly_4326, st_crs(v))
  suppressWarnings(st_intersection(v, b))
}

# Auto-discover scenario folders like ".../slr100_w100_*"
discover_scenarios <- function(parent_dir) {
  # find scenario directories that already end with "_flood_depth" or "_flood_duration"
  dirs <- list.dirs(parent_dir, full.names = FALSE, recursive = FALSE)
  dirs <- dirs[grepl("^slr\\d{3}_w\\d{3}_", dirs)]
  sort(dirs)
}

# Generic raster pipeline (continuous fields)
merge_crop_save <- function(tile_dir, out_dir, out_stub, mosaic_fun = max) {
  tif_files <- list.files(tile_dir, pattern = "\\.tif$", full.names = TRUE)
  if (!length(tif_files)) { 
    message("No .tif files in: ", tile_dir)
    return(invisible(NULL)) 
  }
  message("Merging ", length(tif_files), " tiles in: ", tile_dir)
  
  # Read and project rasters with error handling
  rs <- list()
  for (i in seq_along(tif_files)) {
    tryCatch({
      r <- terra::rast(tif_files[i])
      if (!is.null(r) && inherits(r, "SpatRaster")) {
        r_proj <- terra::project(r, paste0("EPSG:", crs_work))
        if (!is.null(r_proj) && inherits(r_proj, "SpatRaster")) {
          rs[[length(rs) + 1]] <- r_proj
        } else {
          warning("Projection failed for: ", basename(tif_files[i]))
        }
      } else {
        warning("Skipping invalid raster: ", basename(tif_files[i]))
      }
    }, error = function(e) {
      warning("Error reading ", basename(tif_files[i]), ": ", e$message)
    })
  }
  
  if (length(rs) == 0) {
    message("No valid rasters found in: ", tile_dir)
    return(invisible(NULL))
  }
  
  message("Successfully processed ", length(rs), " out of ", length(tif_files), " tiles")
  
  # Align all rasters to the first one
  template   <- rs[[1]]
  rs_aligned <- lapply(rs, function(r) {
    if (!terra::compareGeom(r, template, stopOnError = FALSE)) {
      terra::resample(r, template, method = "bilinear")
    } else r
  })
  
  fun <- if (is.character(mosaic_fun)) match.fun(mosaic_fun) else mosaic_fun
  
  # Merge rasters using a safer approach
  message("Mosaicking ", length(rs_aligned), " aligned rasters...")
  merged <- if (length(rs_aligned) == 1) {
    rs_aligned[[1]]
  } else {
    # Use terra's SpatRasterCollection instead of Reduce
    src <- terra::sprc(rs_aligned)
    terra::mosaic(src, fun = fun)
  }
  
  message("Mosaic complete, cropping to AOI...")
  cropped <- crop_raster_to_bbox(merged, bbox_poly4326_expanded)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(out_dir, paste0(out_stub, ".tif"))
  terra::writeRaster(cropped, out_path, overwrite = TRUE)
  message("  → saved: ", out_path)
  out_path
}

# SOURCE ROOTS
flood_depth_base <- file.path(external_base, "CoSMoS_v3_Phase2_average_conditions_flood_depth_and_duration_sb", "flood_depth")
flood_dur_base   <- file.path(external_base, "CoSMoS_v3_Phase2_average_conditions_flood_depth_and_duration_sb", "flood_duration")
wave_ht_base     <- file.path(external_base, "wave_ht_sb")
runup_base       <- file.path(external_base, "CoSMoS_runup_socal")
cliff_base       <- file.path(external_base, "CoSMoS_cliff_retreat_projections_socal")
squeeze_base     <- file.path(external_base, "CoSMoS_coastal_squeeze_socal")
shore_base       <- file.path(external_base, "CoSMoS_shoreline_change_projections_socal")

# FLOOD DEPTH 
flood_depth_out  <- file.path(local_output, "flood_depth")
dir.create(flood_depth_out, recursive = TRUE, showWarnings = FALSE)
slr_steps <- discover_scenarios(flood_depth_base)

for (s in slr_steps) {
  src_dir <- file.path(flood_depth_base, s)
  merge_crop_save(src_dir, flood_depth_out, paste0("flood_depth_", s, "_", case_abbr), mosaic_fun = max)
}

# FLOOD DURATION
flood_dur_out <- file.path(local_output, "flood_duration")
dir.create(flood_dur_out, recursive = TRUE, showWarnings = FALSE)
slr_steps_dur <- discover_scenarios(flood_dur_base)

for (s in slr_steps_dur) {
  src_dir <- file.path(flood_dur_base, s)
  merge_crop_save(src_dir, flood_dur_out, paste0("flood_duration_", s, "_", case_abbr), mosaic_fun = max)
}

# WAVE HEIGHT 
wave_ht_out <- file.path(local_output, "wave_ht")
dir.create(wave_ht_out, recursive = TRUE, showWarnings = FALSE)


process_wave_ht <- function(slr_step) {
  cat("Processing wave height:", slr_step, "\n")
  scenario_folder <- file.path(wave_ht_base, paste0(slr_step, "_waves"))
  tif_files <- list.files(scenario_folder, pattern = "\\.tif$", full.names = TRUE)
  if (!length(tif_files)) { cat("  No TIFFs in ", scenario_folder, "\n\n"); return(NULL) }
  
  # read + project
  rs <- lapply(tif_files, function(p) terra::project(terra::rast(p), paste0("EPSG:", crs_work)))
  template <- rs[[1]]
  rs_aligned <- lapply(rs, function(r) {
    if (!isTRUE(terra::compareGeom(r, template, stopOnError = FALSE))) {
      terra::resample(r, template, method = "bilinear")
    } else r
  })
  
  if (length(rs_aligned) == 1) {
    merged <- rs_aligned[[1]]
  } else {
    src <- terra::sprc(rs_aligned)
    merged <- terra::mosaic(src, fun = max)
  }
  
  cropped <- crop_raster_to_bbox(merged, bbox_poly4326_expanded)
  
  out_file <- file.path(wave_ht_out, paste0("wave_ht_", slr_step, "_", case_abbr, ".tif"))
  terra::writeRaster(
    cropped, out_file, overwrite = TRUE,
    filetype = "GTiff",
    gdal = c("COMPRESS=LZW","TILED=YES","BIGTIFF=IF_SAFER")
  )
  cat("  Saved:", out_file, "\n\n")
  out_file
}

slr_steps <- sub("_waves$", "", discover_scenarios(wave_ht_base))
invisible(lapply(slr_steps, process_wave_ht))

# RUNUP (vectors)
runup_out <- file.path(local_output, "runup"); dir.create(runup_out, TRUE, FALSE)
runup_shps <- list.files(runup_base, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)

# Filter to only process w000 and w100 scenarios
runup_storms_to_process <- c("w000", "w100")

if (length(runup_shps)) {
  for (shp in runup_shps) {
    nm   <- file_path_sans_ext(basename(shp))
    scen <- str_to_lower(str_extract(nm, "SLR\\d{3}_W\\d{3}"))
    
    # Extract storm code to check if we should process this file
    storm_code <- str_extract(scen, "w\\d{3}")
    
    if (is.na(scen) || is.na(storm_code) || !storm_code %in% runup_storms_to_process) {
      message("Skipping runup scenario (not w000/w100): ", nm)
      next
    }
    
    out_dir <- file.path(runup_out, scen); dir.create(out_dir, TRUE, FALSE)
    
    v <- suppressMessages(st_read(shp, quiet = TRUE))
    v_clip <- clip_vector_to_bbox(v, bbox_poly4326_expanded)
    
    if (nrow(v_clip)) {
      out_path <- file.path(out_dir, paste0(nm, "_", case_abbr, ".shp"))
      suppressMessages(st_write(v_clip, out_path, delete_dsn = TRUE, quiet = TRUE))
      message("Runup → ", out_path)
    } else message("Runup (no features in AOI): ", nm)
  }
} else message("No runup shapefiles found at: ", runup_base)