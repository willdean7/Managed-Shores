# interpolate_cosmos.R
# Purpose: Convert CoSMoS static SLR scenarios to annual hazard timelines using OPC 2024 projections
# Author: Will Dean
#
# IMPORTANT TIMELINE NOTE:
#   OPC 2024 report baseline = Year 2020
#   Current analysis year = 2026 (6 years after OPC baseline)
#   This script starts projections at OPC Year 6 to represent present day (2026)
#   Planning horizon: 2026-2100 (74 years from present, stays within OPC data range)
#
# Input:
#   - data/{site}/derived/cosmos_hazard_metrics.csv (from extract_cosmos_metrics_unified.R)
#   - OPC 2024 SLR scenarios (Intermediate, Intermediate-High, High)
#
# Output:
#   - data/{site}/derived/cosmos_annual_hazards.csv
#   - data/{site}/derived/cosmos_annual_hazards_summary.txt
#
# What this does:
#   Takes hazard metrics at discrete SLR levels (e.g., 0, 0.25, 0.5, 1.0, 1.5, 2.0m)
#   and interpolates them to create smooth annual timelines for years 0-80
#   using OPC sea level rise projections, starting from Year 6 in OPC timeline (2026).

rm(list = ls())
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(stringr)


# CONFIGURATION
case_name <- "carpinteria"  # Change for other sites
data_dir <- file.path("data", case_name)
derived_dir <- file.path(data_dir, "derived")

# Planning horizon
bigT <- 74  # Years from present (2026-2100, stays within OPC data range)

# OPC timeline adjustment
# OPC 2024 baseline = Year 2020
# Current year = 2026
# Years since OPC baseline = 6
OPC_BASELINE_YEAR <- 2020
CURRENT_YEAR <- 2026
YEARS_SINCE_OPC_BASELINE <- CURRENT_YEAR - OPC_BASELINE_YEAR


message("CoSMoS Temporal Interpolation: ", toupper(case_name))
message("Timeline: 2026-2100 (", bigT, " years)")
message("OPC 2024 baseline: ", OPC_BASELINE_YEAR, " (", YEARS_SINCE_OPC_BASELINE, " years ago)")
message("Starting projections at OPC Year ", YEARS_SINCE_OPC_BASELINE, " (present day)")


# LOAD HAZARD METRICS
hazards_path <- file.path(derived_dir, "cosmos_hazard_metrics.csv")
stopifnot(file.exists(hazards_path))

hazards <- read_csv(hazards_path, show_col_types = FALSE)

message("\n✓ Loaded hazard metrics: ", nrow(hazards), " rows (parcel × SLR combinations)")
message("  - Parcels: ", n_distinct(hazards$parcel_id))
message("  - SLR levels: ", paste(sort(unique(hazards$slr_m)), collapse = ", "), " m\n")


# OPC 2024 SLR SCENARIOS

# OPC 2024 sea level rise projections (in feet, converted to meters)
# These are relative to OPC's 2020 baseline
ft_to_m <- 0.3048

# Decadal data points from OPC 2024 report
# Time 0 = Year 2020
sea_level_data <- list(
  "Intermediate" = data.frame(
    time = c(0, 10, 20, 30, 40, 50, 60, 70, 80),
    sea_level_rise = c(0, 0.4, 0.6, 0.8, 1.1, 1.4, 1.8, 2.4, 3.1) * ft_to_m
  ),
  "Intermediate_High" = data.frame(
    time = c(0, 10, 20, 30, 40, 50, 60, 70, 80),
    sea_level_rise = c(0, 0.4, 0.7, 1.0, 1.5, 2.2, 3.0, 3.9, 4.9) * ft_to_m
  ),
  "High" = data.frame(
    time = c(0, 10, 20, 30, 40, 50, 60, 70, 80),
    sea_level_rise = c(0, 0.4, 0.8, 1.2, 2.0, 3.0, 4.1, 5.4, 6.6) * ft_to_m
  )
)

# Fit quadratic models to get smooth annual SLR(t) curves
# We use quadratic because SLR accelerates over time
# Start at OPC Year 6 (2026) and project forward 74 years
fit_slr_model <- function(df, start_year = YEARS_SINCE_OPC_BASELINE, n_years = bigT) {
  df$timesquared <- df$time^2
  m <- lm(sea_level_rise ~ time + timesquared, data = df)
  
  # Predict from OPC Year 6 through Year 6+80
  years_opc <- start_year:(start_year + n_years)
  nd <- data.frame(time = years_opc, timesquared = years_opc^2)
  slr_vec <- as.numeric(predict(m, newdata = nd))
  
  # Ensure monotonic increase (no backtracking due to extrapolation)
  slr_vec <- cummax(slr_vec)
  
  slr_vec
}

# Create smooth annual SLR vectors for each scenario
# These represent Years 0-74 from present (2026-2100)
# But are calculated from OPC Years 6-80 (2026-2100)
slr_scenarios <- lapply(sea_level_data, function(df) fit_slr_model(df))

# Preview
message("OPC 2024 SLR Scenarios (starting from present day = 2026):")
for (scen in names(slr_scenarios)) {
  slr_vec <- slr_scenarios[[scen]]
  message(sprintf("  %s: Year 0 (2026) = %.3fm, Year 40 (2066) = %.2fm, Year %d (%d) = %.2fm", 
                  scen, slr_vec[1], slr_vec[41], bigT, CURRENT_YEAR + bigT, slr_vec[bigT + 1]))
}

# SAVE SLR PROJECTION CURVES FOR VISUALIZATION
message("\n→ Saving SLR projection curves for methods figures...")
slr_curves <- bind_rows(
  tibble(scenario = "Intermediate", 
         year = 0:bigT, 
         calendar_year = CURRENT_YEAR + (0:bigT),
         slr_m = slr_scenarios[["Intermediate"]]),
  tibble(scenario = "Intermediate_High", 
         year = 0:bigT, 
         calendar_year = CURRENT_YEAR + (0:bigT),
         slr_m = slr_scenarios[["Intermediate_High"]]),
  tibble(scenario = "High", 
         year = 0:bigT, 
         calendar_year = CURRENT_YEAR + (0:bigT),
         slr_m = slr_scenarios[["High"]])
)

slr_curves_path <- file.path(derived_dir, "slr_projection_curves.csv")
write_csv(slr_curves, slr_curves_path)
message("✓ Saved SLR curves: ", basename(slr_curves_path))

# INTERPOLATION FUNCTIONS

# For a single parcel, interpolate hazard metrics at each year's SLR level
interpolate_annual_hazards <- function(parcel_hazards, slr_annual_vec, parcel_num) {
  # parcel_hazards: df with columns slr_m, depth_m, duration_hr, etc. for ONE parcel
  # slr_annual_vec: vector of SLR(t) for years 0:bigT (present through planning horizon)
  
  years <- 0:bigT  # Changed from 1:length() to 0:bigT for clarity
  results <- tibble(year = years, slr_m = slr_annual_vec)
  
  # Metrics to interpolate (average conditions only)
  # Storm scenarios (w100) removed - will be handled by Monte Carlo simulation later
  continuous_metrics <- c(
    "depth_m",
    "duration_hr",
    "wave_ht_m",
    "runup_dist_m",
    "shore_dist_m", "cliff_dist_m"
  )
  
  # Baseline metrics (constant, don't interpolate)
  baseline_metrics <- c(
    "baseline_cliff_dist_m"  # Current cliff distance - same for all years
  )
  
  # Add baseline columns (constant values)
  for (metric in baseline_metrics) {
    if (metric %in% names(parcel_hazards)) {
      # Use first value (should be same for all SLR within a parcel-scenario)
      baseline_value <- parcel_hazards[[metric]][1]
      results[[metric]] <- rep(baseline_value, length(years))
    }
  }
  
  for (metric in continuous_metrics) {
    if (metric %in% names(parcel_hazards)) {
      # Remove NA rows for interpolation
      clean_data <- parcel_hazards %>%
        filter(!is.na(.data[[metric]]), !is.na(slr_m)) %>%
        arrange(slr_m) %>%
        distinct(slr_m, .keep_all = TRUE)  # Remove duplicate SLR values
      
      if (nrow(clean_data) > 1) {
        # Linear interpolation with flat extrapolation at ends
        interp_fn <- approxfun(
          x = clean_data$slr_m,
          y = clean_data[[metric]],
          method = "linear",
          rule = 2  # Constant extrapolation beyond range (use endpoint values)
        )
        results[[metric]] <- pmax(interp_fn(slr_annual_vec), 0)  # Floor at 0
      } else if (nrow(clean_data) == 1) {
        # Only one data point - use constant value
        results[[metric]] <- clean_data[[metric]][1]
      } else {
        # No data - set to NA
        results[[metric]] <- NA_real_
      }
    }
  }
  
  # Binary flags: becomes TRUE when SLR exceeds threshold where flag=TRUE in static data
  # Storm scenarios (w100) removed - will be handled by Monte Carlo simulation later
  binary_metrics <- c(
    "flooded", 
    "runup_exposed",
    "cliff_exposed", "in_squeeze_zone"
  )
  
  for (metric in binary_metrics) {
    if (metric %in% names(parcel_hazards)) {
      # Find minimum SLR where exposure occurs
      exposed_data <- parcel_hazards %>%
        filter(!is.na(.data[[metric]]), .data[[metric]] == TRUE)
      
      if (nrow(exposed_data) > 0) {
        exposed_slr <- min(exposed_data$slr_m, na.rm = TRUE)
        results[[metric]] <- slr_annual_vec >= exposed_slr
      } else {
        # Never exposed in our SLR range
        results[[metric]] <- FALSE
      }
    }
  }
  
  results
}


# RUN INTERPOLATION FOR ALL PARCELS × SCENARIOS
message("\n→ Interpolating hazard timelines for all parcels × scenarios...")
message("  This will take a minute...\n")

annual_hazards_list <- list()

for (scen_name in names(slr_scenarios)) {
  message("  Processing: ", scen_name, "...")
  
  slr_vec <- slr_scenarios[[scen_name]]
  
  # Get unique cliff management scenarios from input data
  cliff_scenarios <- unique(hazards$scenario)
  if (length(cliff_scenarios) == 0 || all(is.na(cliff_scenarios))) {
    cliff_scenarios <- c(NA)  # No cliff scenarios in data
  }
  
  message("    Cliff management scenarios: ", paste(cliff_scenarios, collapse = ", "))
  
  # Process each parcel
  parcel_ids <- unique(hazards$parcel_id)
  
  parcel_annual <- map_dfr(parcel_ids, function(pid) {
    # Process each cliff management scenario separately
    parcel_results <- map_dfr(cliff_scenarios, function(cliff_scen) {
      # Filter for this parcel and cliff scenario
      if (is.na(cliff_scen)) {
        parcel_data <- hazards %>% filter(parcel_id == pid)
      } else {
        parcel_data <- hazards %>% 
          filter(parcel_id == pid, scenario == cliff_scen)
      }
      
      if (nrow(parcel_data) == 0) return(NULL)
      
      annual <- interpolate_annual_hazards(parcel_data, slr_vec, pid)
      annual$parcel_id <- pid
      annual$scenario <- scen_name
      
      # Preserve cliff management scenario if it exists
      if (!is.na(cliff_scen)) {
        annual$cliff_scenario <- cliff_scen
      }
      
      annual
    })
    
    parcel_results
  })
  
  annual_hazards_list[[scen_name]] <- parcel_annual
}

# Combine all scenarios
annual_hazards <- bind_rows(annual_hazards_list)

message("\n✓ Interpolation complete!")
message("  - Total rows: ", nrow(annual_hazards))
message("  - Parcels: ", n_distinct(annual_hazards$parcel_id))
message("  - SLR scenarios: ", paste(unique(annual_hazards$scenario), collapse = ", "))

if ("cliff_scenario" %in% names(annual_hazards)) {
  message("  - Cliff management scenarios: ", paste(unique(annual_hazards$cliff_scenario), collapse = ", "))
}

message("  - Years: ", min(annual_hazards$year), "-", max(annual_hazards$year), 
        " (", CURRENT_YEAR, "-", CURRENT_YEAR + bigT, ")\n")

# ADD FLOODING FLAG BASED ON THRESHOLD
# Only apply if depth_m column exists (flood sites)
if ("depth_m" %in% names(annual_hazards)) {
  # Apply flooding threshold to annual data (0.15m as in extraction)
  # IMPORTANT: This threshold must match extract_cosmos_metrics_unified.R
  # for consistent flood classification across the pipeline
  flood_threshold_m <- 0.15
  
  annual_hazards <- annual_hazards %>%
    mutate(
      flooded = !is.na(depth_m) & depth_m >= flood_threshold_m
    )
  
  has_flood_data <- TRUE
} else {
  message("  Note: No flood data (cliff-only site)")
  has_flood_data <- FALSE
}


# SAVE OUTPUTS

out_csv <- file.path(derived_dir, "cosmos_annual_hazards.csv")
write_csv(annual_hazards, out_csv)

message("✓ Saved: ", out_csv)


# SAVE EXAMPLE INTERPOLATIONS FOR METHODS VISUALIZATION
if (has_flood_data) {
  message("\n→ Saving example depth interpolations for methods figures...")
  
  # Select 5 example properties (or fewer if less available)
  n_examples <- min(5, n_distinct(annual_hazards$parcel_id))
  example_parcels <- sample(unique(annual_hazards$parcel_id), n_examples)
  
  # Get their interpolated depths (just Intermediate scenario)
  example_depths <- annual_hazards %>%
    filter(parcel_id %in% example_parcels,
           scenario == "Intermediate") %>%
    select(parcel_id, year, slr_m, depth_m, flooded) %>%
    mutate(calendar_year = CURRENT_YEAR + year)
  
  # Also get their original static CoSMoS data for comparison
  example_static <- hazards %>%
    filter(parcel_id %in% example_parcels) %>%
    select(parcel_id, slr_m, depth_m) %>%
    mutate(data_type = "static_cosmos")
  
  # Mark the interpolated data
  example_interp <- example_depths %>%
    select(parcel_id, slr_m, depth_m) %>%
    mutate(data_type = "interpolated")
  
  # Combine for easy plotting
  example_combined <- bind_rows(example_static, example_interp)
  
  example_path <- file.path(derived_dir, "example_depth_interpolations.csv")
  write_csv(example_combined, example_path)
  message("✓ Saved example interpolations: ", basename(example_path))
  message("  Properties: ", paste(example_parcels, collapse = ", "))
  
} else {
  message("\n→ Skipping example interpolations (no flood data)")
}


# SUMMARY REPORT

# Create summary text
summary_lines <- c(
  "========================================",
  "CoSMoS Annual Hazards Summary",
  "========================================",
  paste("Site:", case_name),
  paste("Date:", Sys.Date()),
  paste("Timeline: ", CURRENT_YEAR, "-", CURRENT_YEAR + bigT, " (", bigT, " years from present)", sep = ""),
  paste("OPC 2024 baseline: ", OPC_BASELINE_YEAR, " (started ", YEARS_SINCE_OPC_BASELINE, " years ago)", sep = ""),
  "",
  "SCENARIOS:",
  paste("  ", names(slr_scenarios)),
  ""
)

# FLOOD SUMMARY (only if flood data exists)
if (has_flood_data) {
  # Count flooding events per scenario
  flood_summary <- annual_hazards %>%
    group_by(scenario) %>%
    summarise(
      total_parcel_years = n(),
      flooded_count = sum(flooded, na.rm = TRUE),
      pct_flooded = 100 * mean(flooded, na.rm = TRUE),
      .groups = "drop"
    )
  
  summary_lines <- c(
    summary_lines,
    "FLOODING SUMMARY (average annual conditions):",
    ""
  )
  
  for (i in 1:nrow(flood_summary)) {
    s <- flood_summary[i, ]
    summary_lines <- c(
      summary_lines,
      paste0("  ", s$scenario, ":"),
      paste0("    Total (parcel × year) combinations: ", s$total_parcel_years),
      paste0("    Flooded instances: ", s$flooded_count, 
             " (", round(s$pct_flooded, 1), "%)"),
      ""
    )
  }
  
  # First year of flooding for each parcel × scenario
  first_flood <- annual_hazards %>%
    filter(flooded) %>%
    group_by(parcel_id, scenario) %>%
    summarise(first_flood_year = min(year, na.rm = TRUE), .groups = "drop")
  
  first_flood_summary <- first_flood %>%
    group_by(scenario) %>%
    summarise(
      parcels_flood = n(),
      median_first_flood = median(first_flood_year, na.rm = TRUE),
      min_first_flood = min(first_flood_year, na.rm = TRUE),
      max_first_flood = max(first_flood_year, na.rm = TRUE),
      .groups = "drop"
    )
  
  summary_lines <- c(
    summary_lines,
    "",
    "FIRST FLOODING YEAR (average conditions):",
    ""
  )
  
  for (i in 1:nrow(first_flood_summary)) {
    s <- first_flood_summary[i, ]
    calendar_median <- CURRENT_YEAR + s$median_first_flood
    calendar_min <- CURRENT_YEAR + s$min_first_flood
    calendar_max <- CURRENT_YEAR + s$max_first_flood
    
    summary_lines <- c(
      summary_lines,
      paste0("  ", s$scenario, ":"),
      paste0("    Parcels that flood: ", s$parcels_flood),
      paste0("    Median first flood: Year ", round(s$median_first_flood), " (", calendar_median, ")"),
      paste0("    Range: Year ", s$min_first_flood, " (", calendar_min, ") to Year ", 
             s$max_first_flood, " (", calendar_max, ")"),
      ""
    )
  }
}

# CLIFF SUMMARY (if cliff data exists)
if ("cliff_dist_m" %in% names(annual_hazards)) {
  summary_lines <- c(
    summary_lines,
    "",
    "CLIFF RETREAT SUMMARY:",
    ""
  )
  
  # Calculate cliff statistics
  for (slr_scen in unique(annual_hazards$scenario)) {
    scen_data <- annual_hazards %>% filter(scenario == slr_scen)
    
    # Count properties in danger zones at different time points
    year_0 <- scen_data %>% filter(year == 0)  # Present day
    year_40 <- scen_data %>% filter(year == 40)  # Mid-century
    year_80 <- scen_data %>% filter(year == bigT)  # End of planning horizon
    
    summary_lines <- c(
      summary_lines,
      paste0("  ", slr_scen, ":"),
      paste0("    Present day (", CURRENT_YEAR, "): ", 
             sum(year_0$cliff_dist_m < 50, na.rm = TRUE), " parcels within 50m"),
      paste0("    Year 40 (", CURRENT_YEAR + 40, "): ",
             sum(year_40$cliff_dist_m < 50, na.rm = TRUE), " parcels within 50m"),
      paste0("    Year ", bigT, " (", CURRENT_YEAR + bigT, "): ",
             sum(year_80$cliff_dist_m < 50, na.rm = TRUE), " parcels within 50m"),
      ""
    )
  }
}

summary_lines <- c(
  summary_lines,
  "DATA STRUCTURE:",
  paste("  Columns:", paste(names(annual_hazards), collapse = ", ")),
  "",
  paste0("EXAMPLE (Parcel 1, Intermediate scenario, first 5 years from ", CURRENT_YEAR, "):"),
  ""
)

# Add example rows
if ("depth_m" %in% names(annual_hazards)) {
  # Flood site example
  example <- annual_hazards %>%
    filter(parcel_id == 1, scenario == "Intermediate", year <= 4) %>%
    select(year, slr_m, depth_m, flooded)
  
  for (i in 1:nrow(example)) {
    row <- example[i, ]
    calendar_year <- CURRENT_YEAR + row$year
    summary_lines <- c(
      summary_lines,
      sprintf("  Year %d (%d): SLR=%.3fm, depth=%.2fm, flooded=%s",
              row$year, calendar_year, row$slr_m, row$depth_m, row$flooded)
    )
  }
} else if ("cliff_dist_m" %in% names(annual_hazards)) {
  # Cliff site example
  example <- annual_hazards %>%
    filter(parcel_id == 1, scenario == "Intermediate", year <= 4)
  
  if ("cliff_scenario" %in% names(example)) {
    example <- example %>% filter(cliff_scenario == unique(cliff_scenario)[1])
  }
  
  example <- example %>% select(year, slr_m, cliff_dist_m)
  
  for (i in 1:nrow(example)) {
    row <- example[i, ]
    calendar_year <- CURRENT_YEAR + row$year
    in_50m <- row$cliff_dist_m < 50
    in_100m <- row$cliff_dist_m < 100
    summary_lines <- c(
      summary_lines,
      sprintf("  Year %d (%d): SLR=%.3fm, cliff_dist=%.1fm, <50m=%s, <100m=%s",
              row$year, calendar_year, row$slr_m, row$cliff_dist_m, in_50m, in_100m)
    )
  }
}

summary_lines <- c(
  summary_lines,
  "",
  "NEXT STEPS:",
  "  1. Review this summary for sanity checks",
  "  2. Run cosmos_valuation_v3.R to compute optimal retreat timing",
  "  3. Analyze retreat schedule by scenario",
  "========================================"
)

summary_file <- file.path(derived_dir, "cosmos_annual_hazards_summary.txt")
writeLines(summary_lines, summary_file)

cat("\n")
cat(paste(summary_lines, collapse = "\n"))
cat("\n\n")
