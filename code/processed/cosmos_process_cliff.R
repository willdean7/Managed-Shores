# cosmos_process_cliff.R
# Purpose: Pre-process CoSMoS cliff retreat data for bluff sites (Pacifica, Isla Vista)
# Author: Will Dean (Managed Shores)
#
# This is a CLIFF-ONLY version - no flood depth, duration, wave height, or runup
# For use with bluff retreat sites where cliff erosion is the primary hazard

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
case_name <- "pacifica"  # pacifica, isla_vista, etc.
case_abbr <- "paci"      # Short abbreviation for filenames

# AOI bbox (4326) — set per case
bbox_lookup <- list(
  pacifica      = c(xmin=-122.52, ymin=37.59, xmax=-122.48, ymax=37.65),
  isla_vista    = c(xmin=-119.87, ymin=34.40, xmax=-119.84, ymax=34.42),
  # Add more bluff sites here
  carpinteria   = c(xmin=-119.55, ymin=34.38, xmax=-119.52, ymax=34.41)  # For reference
)
bbox4326 <- st_bbox(bbox_lookup[[case_name]], crs = st_crs(4326))

# Output root
local_output <- file.path(project_base, sprintf("data/%s/cosmos", case_name))
dir.create(local_output, recursive = TRUE, showWarnings = FALSE)

# Expand AOI by 1000 m in working CRS
bbox_poly_work         <- st_transform(st_as_sfc(bbox4326), crs_work) |> st_buffer(1000)
bbox_poly4326_expanded <- st_transform(bbox_poly_work, 4326)

# Helper function for vector data
clip_vector_to_bbox <- function(v, bbox_poly_4326) {
  b <- st_transform(bbox_poly_4326, st_crs(v))
  suppressWarnings(st_intersection(v, b))
}

# SOURCE ROOTS FOR CLIFF DATA
cliff_base <- file.path(external_base, "CoastalCliff_projections_CenCal_CoSMoS_3.1")

# CoSMoS 3.1 has two management scenarios
letitgo_path <- file.path(cliff_base, "Management_Scenario-LetItGo")
holdtheline_path <- file.path(cliff_base, "Management_Scenario-HoldTheLine")


message("CoSMoS CLIFF DATA PROCESSING: ", toupper(case_name))
message("Extracting BOTH management scenarios")



# CLIFF RETREAT - BOTH SCENARIOS

cliff_out <- file.path(local_output, "cliff_retreat")
dir.create(cliff_out, recursive = TRUE, showWarnings = FALSE)

# Function to process one scenario
process_cliff_scenario <- function(scenario_path, scenario_name) {
  
  message("\n→ Processing ", scenario_name, " scenario...")
  message("  Path: ", scenario_path)
  
  if (!dir.exists(scenario_path)) {
    message("  ⚠ Scenario folder not found, skipping")
    return(invisible(NULL))
  }
  
  # Look for KMZ files in the KMZs subfolder
  kmz_folder <- file.path(scenario_path, "KMZs")
  if (!dir.exists(kmz_folder)) {
    kmz_folder <- scenario_path
  }
  
  # Find cliff projection KMZ files (lines preferred, then points)
  kmz_files <- list.files(kmz_folder, pattern = "cliff_projections.*lines.*\\.kmz$", 
                          full.names = TRUE, ignore.case = TRUE)
  
  # Also get uncertainty files (separate pattern to keep them organized)
  uncertainty_files <- list.files(kmz_folder, pattern = "uncertainty.*\\.kmz$", 
                                  full.names = TRUE, ignore.case = TRUE)
  
  if (length(kmz_files) == 0) {
    kmz_files <- list.files(kmz_folder, pattern = "cliff_projections.*\\.kmz$", 
                            full.names = TRUE, ignore.case = TRUE)
    
  }
  
  # Combine main projections and uncertainty files
  all_kmz_files <- c(kmz_files, uncertainty_files)
  
  if (length(all_kmz_files) == 0) {
    message("  ⚠ No cliff projection KMZ files found")
    return(invisible(NULL))
  }
  
  message("  Found ", length(kmz_files), " projection file(s) + ", 
          length(uncertainty_files), " uncertainty file(s)")
  
  # Process each KMZ file (projections + uncertainty)
  for (kmz_file in all_kmz_files) {
    nm <- file_path_sans_ext(basename(kmz_file))
    
    message("\n    Processing: ", nm)
    
    # KMZ is just a zipped KML - extract it first
    temp_dir <- tempdir()
    temp_kml <- file.path(temp_dir, paste0(nm, ".kml"))
    
    # Try to extract KML from KMZ
    tryCatch({
      # Unzip the KMZ
      unzip(kmz_file, exdir = temp_dir, overwrite = TRUE)
      
      # Find the extracted KML file (usually doc.kml)
      kml_files <- list.files(temp_dir, pattern = "\\.kml$", full.names = TRUE)
      
      if (length(kml_files) == 0) {
        message("      ⚠ No KML found inside KMZ")
        next
      }
      
      # Use the first KML file found
      kml_to_read <- kml_files[1]
      message("      Extracted KML: ", basename(kml_to_read))
      
      # Read the KML
      v <- suppressMessages(st_read(kml_to_read, quiet = TRUE))
      
      if (is.null(v) || nrow(v) == 0) {
        message("      ⚠ KML contains no features")
        next
      }
      
      message("      Total features: ", nrow(v))
      message("      Geometry: ", as.character(st_geometry_type(v, by_geometry = FALSE)))
      
      # Print attributes
      attr_names <- names(v)
      message("      Attributes: ", paste(attr_names[1:min(8, length(attr_names))], collapse = ", "))
      
      # Clip to study area
      v_clip <- clip_vector_to_bbox(v, bbox_poly4326_expanded)
      
      if (nrow(v_clip) > 0) {
        # Save as GeoPackage
        out_path <- file.path(cliff_out, paste0(nm, "_", case_abbr, ".gpkg"))
        suppressMessages(st_write(v_clip, out_path, delete_dsn = TRUE, quiet = TRUE))
        
        message("      ✓ Saved: ", basename(out_path))
        message("      Features in AOI: ", nrow(v_clip))
        
        # Show sample of key fields
        if ("Name" %in% names(v_clip)) {
          unique_vals <- unique(v_clip$Name)
          message("      Sample names: ", paste(head(unique_vals, 5), collapse = ", "))
        }
        if ("Description" %in% names(v_clip)) {
          # KML descriptions often contain the actual data
          message("      Description field available (may contain year/SLR data)")
        }
        
      } else {
        message("      ⚠ No features in study area")
        message("      Check bbox: ", paste(names(bbox4326), "=", bbox4326, collapse = ", "))
      }
      
      # Clean up temp KML
      unlink(kml_to_read)
      
    }, error = function(e) {
      message("      ⚠ Error processing KMZ: ", e$message)
      message("      Try extracting manually with: unzip '", basename(kmz_file), "'")
    })
  }
  
  invisible(NULL)
}

# Process Let It Go (natural retreat - PRIMARY for retreat analysis)
process_cliff_scenario(letitgo_path, "Let It Go")

# Process Hold the Line (with armoring - for cost comparison)
process_cliff_scenario(holdtheline_path, "Hold the Line")


# SUMMARY & VALIDATION

message("CLIFF DATA EXTRACTION COMPLETE")
message("Output location: ", local_output)
message("\nExtracted data:")

# Count files
cliff_files_saved <- list.files(cliff_out, pattern = "\\.gpkg$")
message("  Cliff retreat: ", length(cliff_files_saved), " file(s)")

message("\nNEXT STEPS:")
message("  1. Check extracted files in: ", cliff_out)
message("  2. Verify cliff data covers your study area")
message("  3. Run redfin_data_code_ms.R to process property data")


# Quick validation: Load and preview one file
if (length(cliff_files_saved) > 0) {
  message("PREVIEW: First cliff retreat file")
  test_file <- file.path(cliff_out, cliff_files_saved[1])
  test_data <- st_read(test_file, quiet = TRUE)
  
  message("  File: ", cliff_files_saved[1])
  message("  Features: ", nrow(test_data))
  message("  Geometry type: ", as.character(st_geometry_type(test_data, by_geometry = FALSE)))
  message("  CRS: ", st_crs(test_data)$input)
  message("  Columns: ", paste(names(test_data), collapse = ", "))
  
  # Print first few values of key columns (if they exist)
  if ("YEAR" %in% names(test_data)) {
    message("  Years: ", paste(unique(test_data$YEAR)[1:min(5, length(unique(test_data$YEAR)))], collapse = ", "))
  }
  if ("SLR_cm" %in% names(test_data)) {
    message("  SLR levels: ", paste(unique(test_data$SLR_cm), collapse = ", "), " cm")
  }
}
