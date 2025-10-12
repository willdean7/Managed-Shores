#The purpose of the script is to pre-process the large CoSMoS datasets before integrating

library(terra)
library(sf)
library(stringr)
library(ggplot2)


# Paths
external_base <- "/Volumes/TOSHIBA HD/Managed_Shores"   # Will's external drive
project_base  <- "~/Documents/Managed_Shores"           
local_output  <- file.path(project_base, "data/carpinteria/cosmos")

dir.create(local_output, recursive = TRUE, showWarnings = FALSE)

# set to Area of interest
bbox4326 <- st_bbox(c(xmin=-119.55, ymin=34.38, xmax=-119.52, ymax=34.41), crs = st_crs(4326))
bbox_poly4326 <- st_as_sfc(bbox4326)
carp_ext <- ext(bbox4326$xmin, bbox4326$xmax, bbox4326$ymin, bbox4326$ymax)
# Transform bbox to the same CRS as the raster
bbox_poly_utm <- st_transform(bbox_poly4326, 3718)
# Convert to terra extent
carp_ext_utm <- ext(vect(bbox_poly_utm))

# Add a 1000-meter buffer
bbox_utm_expanded <- st_buffer(bbox_poly_utm, 1000)

# Transform back to WGS84 for plotting if needed
bbox_poly4326_expanded <- st_transform(bbox_utm_expanded, 4326)


# Helpers to crop safely when CRS differs
crop_raster_to_bbox <- function(r, bbox_poly) {
  # reproject bbox to raster CRS, then crop & mask
  b <- st_transform(bbox_poly, crs(r))
  r_c <- crop(r, vect(b))
  # mask keeps only inside polygon (nicer edges than pure crop)
  mask(r_c, vect(b))
}

clip_vector_to_bbox <- function(v, bbox_poly) {
  b <- st_transform(bbox_poly, st_crs(v))
  suppressWarnings(st_intersection(v, b))
}

# Scenarios we care about
slr_steps <- c("slr000_w100","slr025_w100","slr050_w100","slr075_w100",
               "slr100_w100","slr125_w100","slr150_w100","slr175_w100",
               "slr200_w100","slr500_w100")

# Generic: merge (mosaic) a folder of tiles, crop, save
merge_crop_save <- function(tile_dir, out_dir, out_stub) {
  tif_files <- list.files(tile_dir, pattern = "\\.tif$", full.names = TRUE)
  if (length(tif_files) == 0) {
    message("No .tif files in: ", tile_dir)
    return(invisible(NULL))
  }
  message("Merging ", length(tif_files), " tiles in: ", tile_dir)
  
  # Load all rasters into a list
  rasters <- lapply(tif_files, rast)
  
  # Mosaic handles different extents and resolutions
  merged <- do.call(mosaic, c(rasters, fun = "max"))  # or "mean"
  
  # Crop to Carpinteria bbox
  cropped <- crop_raster_to_bbox(merged, bbox_poly4326)
  
  # Save
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(out_dir, paste0(out_stub, ".tif"))
  writeRaster(cropped, out_path, overwrite = TRUE)
  
  message("  → saved: ", out_path)
  return(out_path)
}

# FLOOD DEPTH RASTERS
flood_depth_base <- file.path(external_base, "CoSMoS_100yr_storm_flood_depth_duration_sb", "flood_depth")
flood_depth_out  <- file.path(local_output, "flood_depth")
dir.create(flood_depth_out, recursive = TRUE, showWarnings = FALSE)

for (s in slr_steps) {
  # external folder names are like "slr100_w100_flood_depth"
  src_dir <- file.path(flood_depth_base, paste0(s, "_flood_depth"))
  merge_crop_save(src_dir, flood_depth_out, paste0("flood_depth_", s, "_carp")) # change _carp as needed for area
}


# FLOOD DURATION RASTERS
flood_dur_base <- file.path(external_base, "CoSMoS_100yr_storm_flood_depth_duration_sb", "flood_duration")
flood_dur_out  <- file.path(local_output, "flood_duration")
dir.create(flood_dur_out, recursive = TRUE, showWarnings = FALSE)

for (s in slr_steps) {
  # external folder names are like "slr100_w100_flood_duration"
  src_dir <- file.path(flood_dur_base, paste0(s, "_flood_duration"))
  merge_crop_save(src_dir, flood_dur_out, paste0("flood_duration_", s, "_carp")) # change _carp as needed for area
}

# WAVE HEIGHT RASTERS

wave_ht_base <- file.path(external_base, "wave_ht_sb")
wave_ht_out  <- file.path(local_output, "wave_ht")
dir.create(wave_ht_out, recursive = TRUE, showWarnings = FALSE)

# Function to merge and crop wave height tiles
process_wave_ht <- function(slr_step) {
  cat("Processing scenario:", slr_step, "\n")
  
  # Path to folder like "slr500_w100_waves"
  scenario_folder <- file.path(wave_ht_base, paste0(slr_step, "_waves"))
  
  # Get all .tif files inside SB01-SB10
  tif_files <- list.files(scenario_folder, pattern = "\\.tif$", full.names = TRUE)
  
  if (length(tif_files) == 0) {
    cat("  No TIFF files found in", scenario_folder, "\n\n")
    return(NULL)
  }
  
  cat("  Found", length(tif_files), "tiles\n")
  
  # Load all tiles
  rasters <- lapply(tif_files, rast)
  
  # Align all rasters to the first tile's grid
  base_raster <- rasters[[1]]
  aligned_rasters <- lapply(rasters, function(r) {
    if (!compareGeom(r, base_raster, stopOnError = FALSE)) {
      cat("    Aligning", names(r), "to match base raster\n")
      r <- resample(r, base_raster, method = "near") # nearest neighbor
    }
    return(r)
  })
  
  # Merge tiles now that they're aligned
  merged <- do.call(mosaic, c(aligned_rasters, fun = "max"))
  
  # Crop to Carpinteria
  cropped <- crop(merged, carp_ext_utm)
  
  # Save output
  out_file <- file.path(wave_ht_out, paste0("wave_ht_", slr_step, "_carp.tif")) # change _carp as needed
  writeRaster(cropped, out_file, overwrite = TRUE)
  cat("  Saved cropped raster to:", out_file, "\n\n")
  
  return(out_file)
}

# Loop through each scenario
all_wave_ht_outputs <- lapply(slr_steps, process_wave_ht)

cat("Wave height processing complete! Files saved to:", wave_ht_out, "\n")

# RUNUP SHAPEFILES

# Base folder locations
runup_base <- file.path(external_base, "CoSMoS_runup_socal")
runup_out  <- file.path(local_output, "runup")

# Create root output folder
dir.create(runup_out, recursive = TRUE, showWarnings = FALSE)

# List shapefiles
runup_shps <- list.files(runup_base, pattern = "\\.shp$", full.names = TRUE)

if (length(runup_shps) > 0) {
  for (shp in runup_shps) {
    nm <- tools::file_path_sans_ext(basename(shp))  
    
    # Extract scenario name like slr000_w000
    scen <- str_to_lower(str_extract(nm, "SLR\\d{3}_W\\d{3}"))
    scen <- ifelse(
      is.na(scen),
      "unknown",
      str_replace_all(scen, "SLR", "slr") |> str_replace("_W", "_w")
    )
    
    # Create a subfolder for each scenario
    out_dir <- file.path(runup_out, scen)
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    
    # Check that the directory now exists
    if (!dir.exists(out_dir)) {
      stop("Output folder was not created correctly: ", out_dir)
    }
    
    # Read the shapefile
    v <- st_read(shp, quiet = TRUE)
    
    # Clip to bounding box
    v_clip <- clip_vector_to_bbox(v, bbox_poly4326)
    
    # Create a safe output path
    out_path <- file.path(out_dir, paste0(nm, "_carp.shp")) # change _carp as needed
    
    # Save only if there are features inside the bounding box
    if (nrow(v_clip) > 0) {
      st_write(v_clip, out_path, delete_dsn = TRUE, quiet = TRUE)
      message("Runup → ", out_path)
    } else {
      message("No features in bbox for ", nm)
    }
  }
} else {
  message("No runup shapefiles found at: ", runup_base)
}

# CLIFF RETREAT SHAPEFILES
# Base input folder on external drive
cliff_base <- file.path(external_base, "CoSMoS_cliff_retreat_projections_socal")

# Output folder on local project directory
cliff_out  <- file.path(local_output, "cliff_retreat")
dir.create(cliff_out, recursive = TRUE, showWarnings = FALSE)

# List all cliff retreat shapefiles
cliff_shps <- list.files(cliff_base, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)

for (shp in cliff_shps) {
  nm <- tools::file_path_sans_ext(basename(shp))  # file name without extension
  message("\nProcessing cliff file: ", nm)
  
  # Read shapefile
  v <- st_read(shp, quiet = TRUE)
  
  # Ensure both are in the same CRS (UTM)
  if (st_crs(v) != st_crs(bbox_poly_utm)) {
    v <- st_transform(v, st_crs(bbox_poly_utm))
  }
  
  # Clip to Carpinteria area
  v_clip <- clip_vector_to_bbox(v, bbox_poly4326)
  
  if (nrow(v_clip) > 0) {
    # Save as GeoPackage to preserve elevation (Z)
    out_path <- file.path(cliff_out, paste0(nm, "_carp.gpkg")) # change _carp as needed
    st_write(v_clip, out_path, delete_dsn = TRUE, quiet = TRUE)
    message("  → Saved: ", out_path)
  } else {
    message("  → No features found within bbox for: ", nm)
  }
}

# COASTAL SQUEEZE SHAPEFILES
squeeze_base <- file.path(external_base, "CoSMoS_coastal_squeeze_socal")
squeeze_out  <- file.path(local_output, "coastal_squeeze")
dir.create(squeeze_out, recursive = TRUE, showWarnings = FALSE)

sq_shps <- list.files(squeeze_base, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)

for (shp in sq_shps) {
  nm <- tools::file_path_sans_ext(basename(shp))
  message("\nProcessing coastal squeeze file: ", nm)
  
  # Read the input shapefile
  v <- st_read(shp, quiet = TRUE)
  
  # Clip to bounding box
  v_clip <- clip_vector_to_bbox(v, bbox_poly4326)
  
  # Define output path (use GeoPackage for reliability)
  out_path <- file.path(squeeze_out, paste0(nm, "_carp.gpkg")) # change _carp as needed
  
  # Write only if there are features inside the bounding box
  if (nrow(v_clip) > 0) {
    st_write(v_clip, out_path, delete_dsn = TRUE, quiet = TRUE)
    message("  → Saved clipped file: ", out_path)
  } else {
    message("  → No features found within bbox for: ", nm)
  }
}


# SHORELINE CHANGE PROJECTIONS SHAPEFILES
shore_base <- file.path(external_base, "CoSMoS_shoreline_change_projections_socal")
shore_out  <- file.path(local_output, "shoreline_change")
dir.create(shore_out, recursive = TRUE, showWarnings = FALSE)

shore_shps <- list.files(shore_base, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)

for (shp in shore_shps) {
  nm <- tools::file_path_sans_ext(basename(shp))
  message("Processing: ", nm)
  
  v <- st_read(shp, quiet = TRUE)
  v_clip <- clip_vector_to_bbox(v, bbox_poly4326)
  
  if (nrow(v_clip) > 0) {
    out_path <- file.path(shore_out, paste0(nm, "_carp.gpkg")) # change _carp as needed
    st_write(v_clip, out_path, delete_dsn = TRUE, quiet = TRUE)
    message("  → Saved as: ", out_path)
  } else {
    message("  → No features in bbox for ", nm)
  }
}
