# interpolate_cosmos.R
# Purpose: Convert CoSMoS static SLR scenarios to annual hazard timelines using OPC 2024 projections
# Author: Will Dean
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
#   Takes hazard metrics at discrete SLR levels (e.g., 0, 0.5, 1.0, 1.5, 2.0, 5.0m)
#   and interpolates them to create smooth annual timelines for years 1-100
#   using OPC sea level rise projections.

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

bigT <- 100  # Planning horizon (years from present)


message("CoSMoS Temporal Interpolation: ", toupper(case_name))
message("Converting SLR levels → annual timelines (years 1-", bigT, ")")


# LOAD HAZARD METRICS
hazards_path <- file.path(derived_dir, "cosmos_hazard_metrics.csv")
stopifnot(file.exists(hazards_path))

hazards <- read_csv(hazards_path, show_col_types = FALSE)

message("✓ Loaded hazard metrics: ", nrow(hazards), " rows (parcel × SLR combinations)")
message("  - Parcels: ", n_distinct(hazards$parcel_id))
message("  - SLR levels: ", paste(sort(unique(hazards$slr_m)), collapse = ", "), " m\n")


# OPC 2024 SLR SCENARIOS

# OPC 2024 sea level rise projections (in feet, converted to meters)
ft_to_m <- 0.3048

# Decadal data points from OPC 2024 report
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
fit_slr_model <- function(df, years = 1:100) {
  df$timesquared <- df$time^2
  m <- lm(sea_level_rise ~ time + timesquared, data = df)
  nd <- data.frame(time = years, timesquared = years^2)
  slr_vec <- as.numeric(predict(m, newdata = nd))
  # Ensure monotonic increase (no backtracking due to extrapolation)
  slr_vec <- cummax(slr_vec)
  slr_vec
}

# Create smooth annual SLR vectors for each scenario
slr_scenarios <- lapply(sea_level_data, fit_slr_model)

# Preview
message("OPC 2024 SLR Scenarios (annual interpolation):")
for (scen in names(slr_scenarios)) {
  slr_vec <- slr_scenarios[[scen]]
  message(sprintf("  %s: Year 1 = %.2fm, Year 50 = %.2fm, Year 100 = %.2fm", 
                  scen, slr_vec[1], slr_vec[50], slr_vec[100]))
}

# INTERPOLATION FUNCTIONS

# For a single parcel, interpolate hazard metrics at each year's SLR level
interpolate_annual_hazards <- function(parcel_hazards, slr_annual_vec, parcel_num) {
  # parcel_hazards: df with columns slr_m, depth_m, duration_hr, etc. for ONE parcel
  # slr_annual_vec: vector of SLR(t) for years 1:100
  
  years <- 1:length(slr_annual_vec)
  results <- tibble(year = years, slr_m = slr_annual_vec)
  
  # Metrics to interpolate (both w000 and w100)
  continuous_metrics <- c(
    "depth_m", "depth_m_w100",
    "duration_hr", "duration_hr_w100",
    "wave_ht_m", "wave_ht_m_w100",
    "runup_dist_m", "runup_dist_m_w100",
    "shore_dist_m", "cliff_dist_m"
  )
  
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
  binary_metrics <- c(
    "flooded", 
    "runup_exposed", "runup_exposed_w100",
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
message("→ Interpolating hazard timelines for all parcels × scenarios...")
message("  This will take a minute...\n")

annual_hazards_list <- list()

for (scen_name in names(slr_scenarios)) {
  message("  Processing: ", scen_name, "...")
  
  slr_vec <- slr_scenarios[[scen_name]]
  
  # Process each parcel
  parcel_ids <- unique(hazards$parcel_id)
  
  parcel_annual <- map_dfr(parcel_ids, function(pid) {
    parcel_data <- hazards %>% filter(parcel_id == pid)
    
    annual <- interpolate_annual_hazards(parcel_data, slr_vec, pid)
    annual$parcel_id <- pid
    annual$scenario <- scen_name
    
    annual
  })
  
  annual_hazards_list[[scen_name]] <- parcel_annual
}

# Combine all scenarios
annual_hazards <- bind_rows(annual_hazards_list)

message("\n✓ Interpolation complete!")
message("  - Total rows: ", nrow(annual_hazards))
message("  - Parcels: ", n_distinct(annual_hazards$parcel_id))
message("  - Scenarios: ", paste(unique(annual_hazards$scenario), collapse = ", "))
message("  - Years: ", min(annual_hazards$year), "-", max(annual_hazards$year), "\n")

# ADD FLOODING FLAGS BASED ON THRESHOLD

# Apply flooding threshold to annual data (0.15m as in extraction)
# IMPORTANT: This threshold must match extract_cosmos_metrics_unified.R
# for consistent flood classification across the pipeline
flood_threshold_m <- 0.15

annual_hazards <- annual_hazards %>%
  mutate(
    flooded_w000 = !is.na(depth_m) & depth_m >= flood_threshold_m,
    flooded_w100 = !is.na(depth_m_w100) & depth_m_w100 >= flood_threshold_m
  )


# SAVE OUTPUTS

out_csv <- file.path(derived_dir, "cosmos_annual_hazards.csv")
write_csv(annual_hazards, out_csv)

message("✓ Saved: ", out_csv)


# SUMMARY REPORT

# Count flooding events per scenario
flood_summary <- annual_hazards %>%
  group_by(scenario) %>%
  summarise(
    total_parcel_years = n(),
    flooded_w000_count = sum(flooded_w000, na.rm = TRUE),
    flooded_w100_count = sum(flooded_w100, na.rm = TRUE),
    pct_flooded_w000 = 100 * mean(flooded_w000, na.rm = TRUE),
    pct_flooded_w100 = 100 * mean(flooded_w100, na.rm = TRUE),
    .groups = "drop"
  )

# First year of flooding for each parcel × scenario (w000)
first_flood <- annual_hazards %>%
  filter(flooded_w000) %>%
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

# Create summary text
summary_lines <- c(
  "========================================",
  "CoSMoS Annual Hazards Summary",
  "========================================",
  paste("Site:", case_name),
  paste("Date:", Sys.Date()),
  paste("Planning horizon:", bigT, "years"),
  "",
  "SCENARIOS:",
  paste("  ", names(slr_scenarios)),
  "",
  "FLOODING SUMMARY (w000 = average conditions):",
  ""
)

for (i in 1:nrow(flood_summary)) {
  s <- flood_summary[i, ]
  summary_lines <- c(
    summary_lines,
    paste0("  ", s$scenario, ":"),
    paste0("    Total (parcel × year) combinations: ", s$total_parcel_years),
    paste0("    Flooded instances (w000): ", s$flooded_w000_count, 
           " (", round(s$pct_flooded_w000, 1), "%)"),
    paste0("    Flooded instances (w100): ", s$flooded_w100_count,
           " (", round(s$pct_flooded_w100, 1), "%)"),
    ""
  )
}

summary_lines <- c(
  summary_lines,
  "",
  "FIRST FLOODING YEAR (w000):",
  ""
)

for (i in 1:nrow(first_flood_summary)) {
  s <- first_flood_summary[i, ]
  summary_lines <- c(
    summary_lines,
    paste0("  ", s$scenario, ":"),
    paste0("    Parcels that flood: ", s$parcels_flood),
    paste0("    Median first flood year: ", round(s$median_first_flood)),
    paste0("    Range: year ", s$min_first_flood, " to ", s$max_first_flood),
    ""
  )
}

summary_lines <- c(
  summary_lines,
  "DATA STRUCTURE:",
  paste("  Columns:", paste(names(annual_hazards), collapse = ", ")),
  "",
  "EXAMPLE (Parcel 1, Intermediate scenario, first 5 years):",
  ""
)

# Add example rows
example <- annual_hazards %>%
  filter(parcel_id == 1, scenario == "Intermediate", year <= 5) %>%
  select(year, slr_m, depth_m, depth_m_w100, flooded_w000, flooded_w100)

for (i in 1:nrow(example)) {
  row <- example[i, ]
  summary_lines <- c(
    summary_lines,
    sprintf("  Year %d: SLR=%.3fm, depth_w000=%.2fm, depth_w100=%.2fm, flooded_w000=%s, flooded_w100=%s",
            row$year, row$slr_m, row$depth_m, row$depth_m_w100, row$flooded_w000, row$flooded_w100)
  )
}

summary_lines <- c(
  summary_lines,
  "",
  "NEXT STEPS:",
  "  1. Review this summary for sanity checks",
  "  2. Run calculate_retreat_years.R to compute optimal retreat timing",
  "  3. Analyze retreat schedule by scenario",
  "========================================"
)

summary_file <- file.path(derived_dir, "cosmos_annual_hazards_summary.txt")
writeLines(summary_lines, summary_file)

cat("\n")
cat(paste(summary_lines, collapse = "\n"))
cat("\n\n")