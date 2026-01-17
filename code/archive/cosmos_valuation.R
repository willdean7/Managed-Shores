# cosmos_valuation.R
# Purpose: Calculate optimal retreat year T* using CoSMoS annual hazards and property economics
# Author: Will Dean (Managed Shores)
#
# Input:
#   - data/{site}/derived/cosmos_annual_hazards.csv (from interpolate_cosmos_to_years.R)
#   - data/{site}/redfin_df.csv (property data with prices, rents, locations)
#   - data/landprices_CA_ms.csv (California land values by ZIP code)
#
# Output:
#   - data/{site}/derived/retreat_schedule_baseline.csv (baseline scenario)
#   - data/{site}/derived/retreat_schedule_sensitivity.csv (all sensitivity scenarios)
#   - data/{site}/derived/sensitivity_summary.csv (comparison across parameters)
#   - data/{site}/derived/retreat_schedule_summary.txt (QA report)
#
# Economic Model:
#   For each property and year t, compare:
#     NPV(staying) = NPV of rent from t to T + NPV of owner utility - NPV of flood damages from t to T
#   
#   Retreat year T* = first year where NPV(staying) <= 0
#   (i.e., expected damages exceed rental income + owner utility)

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
bigT <- 100    # Years

# Depth-damage function parameters (fitted logistic curve)
# Unchanged from MSGRP_valuationcode_ms.R
depth_params <- c(a = 0.4, b = 1, d = 2)

# BASELINE PARAMETERS
# These are the "best estimate" parameters for the primary analysis

BASELINE <- list(
  rental_yield = 0.05,      # 5% annual yield (industry benchmark)
  discount_rate = 0.02,     # 2% (appropriate for government planning)
  damage_threshold = 0.15,  # 15cm (accounts for nuisance flooding)
  owner_utility = 0,        # $0/year (conservative baseline)
  storm_scenario = "w000"   # Average annual conditions (w000) vs extreme (w100)
)

# SENSITIVITY ANALYSIS 
# Toggle sensitivity analysis on/off
RUN_SENSITIVITY <- TRUE  # Set FALSE for faster baseline-only runs

# Define parameter ranges for sensitivity analysis
SENSITIVITY_PARAMS <- list(
  rental_yield = c(0.04, 0.05, 0.06),           # 4%, 5%, 6%
  discount_rate = c(0.015, 0.02, 0.03),         # 1.5%, 2%, 3%
  damage_threshold = c(0.0, 0.15, 0.30),        # 0cm, 15cm, 30cm
  owner_utility = c(0, 25000, 50000),           # $0, $25K, $50K per year
  storm_scenario = c("w000", "w100")            # Average vs 100-year storm
)

# ============================================================

message("CoSMoS Retreat Year Calculation: ", toupper(case_name))
message("Planning horizon: ", bigT, " years")
message("\nBASELINE PARAMETERS:")
message("  Rental yield: ", BASELINE$rental_yield * 100, "%")
message("  Discount rate: ", BASELINE$discount_rate * 100, "%")
message("  Damage threshold: ", BASELINE$damage_threshold, "m")
message("  Owner utility: $", format(BASELINE$owner_utility, big.mark = ","), "/year")
message("  Storm scenario: ", BASELINE$storm_scenario, " (", 
        ifelse(BASELINE$storm_scenario == "w000", "average conditions", "100-year storm"), ")")

if (RUN_SENSITIVITY) {
  message("\nSENSITIVITY ANALYSIS: ENABLED")
  message("  Rental yields: ", paste0(SENSITIVITY_PARAMS$rental_yield * 100, "%", collapse = ", "))
  message("  Discount rates: ", paste0(SENSITIVITY_PARAMS$discount_rate * 100, "%", collapse = ", "))
  message("  Damage thresholds: ", paste0(SENSITIVITY_PARAMS$damage_threshold, "m", collapse = ", "))
  message("  Owner utilities: $", paste(format(SENSITIVITY_PARAMS$owner_utility, big.mark = ","), collapse = ", "))
  message("  Storm scenarios: ", paste(SENSITIVITY_PARAMS$storm_scenario, collapse = ", "))
} else {
  message("\nSENSITIVITY ANALYSIS: DISABLED (baseline only)")
}
message("========================================\n")


# LOAD DATA
# 1. Annual hazards (from interpolation script)
hazards_path <- file.path(derived_dir, "cosmos_annual_hazards.csv")
stopifnot(file.exists(hazards_path))

hazards <- read_csv(hazards_path, show_col_types = FALSE)

message("✓ Loaded annual hazards: ", nrow(hazards), " rows")
message("  - Parcels: ", n_distinct(hazards$parcel_id))
message("  - Scenarios: ", paste(unique(hazards$scenario), collapse = ", "))
message("  - Years: ", min(hazards$year), "-", max(hazards$year), "\n")

# 2. Property data (Redfin)
props_path <- file.path(data_dir, "redfin_df.csv")
stopifnot(file.exists(props_path))

props_raw <- read_csv(props_path, show_col_types = FALSE)

# Create parcel_id to match hazards
props <- props_raw %>%
  mutate(parcel_id = row_number()) %>%
  filter(!is.na(PRICE))

message("✓ Loaded property data: ", nrow(props), " properties")
message("  - Mean price: $", format(round(mean(props$PRICE, na.rm = TRUE)), big.mark = ","), "\n")

# 3. Land values (California ZIP code data)
land_path <- "data/landprices_CA_ms.csv"
stopifnot(file.exists(land_path))

landvals <- read_csv(land_path, skip = 1, show_col_types = FALSE) %>%
  filter(Year == 2022) %>%
  select(
    landval_acre = `Land Value\n(Per Acre, As-Is)`,
    landshare = `Land Share of Property Value`,
    `ZIP Code`
  ) %>%
  rename(`ZIP OR POSTAL CODE` = `ZIP Code`) %>%
  mutate(`ZIP OR POSTAL CODE` = as.character(`ZIP OR POSTAL CODE`))

# Calculate average land share for properties without ZIP match
avg_share <- mean(landvals$landshare, na.rm = TRUE)

# Join land values and calculate structure value
props <- props %>%
  mutate(`ZIP OR POSTAL CODE` = as.character(`ZIP OR POSTAL CODE`)) %>%
  left_join(landvals, by = "ZIP OR POSTAL CODE") %>%
  mutate(
    landval = PRICE * landshare,
    landval = if_else(is.na(landval), PRICE * avg_share, landval),
    strval = PRICE - landval
  )

message("✓ Calculated land and structure values")
message("  - Mean structure value: $", format(round(mean(props$strval, na.rm = TRUE)), big.mark = ","))
message("  - Mean land value: $", format(round(mean(props$landval, na.rm = TRUE)), big.mark = ","), "\n")

# ECONOMIC FUNCTIONS

# NPV of rent stream from year t to T
npv_rent <- function(t, T, rent_annual, beta) {
  if (t > T) return(0)
  years <- t:T
  n <- length(years)
  sum(rent_annual * beta^(1:n))
}

# Depth-damage function (logistic curve)
damage_from_depth <- function(depth_m, strval, params = depth_params) {
  a <- params["a"]
  b <- params["b"]
  d <- params["d"]
  
  # Fraction of structure value damaged
  frac <- a / (1 + exp(-b * (depth_m - d)))
  
  # Total damage (dollars)
  pmax(frac, 0) * strval
}

# NPV of expected damages from year t to T
npv_damages <- function(t, T, depth_vec, strval, params, beta, threshold) {
  if (t > T) return(0)
  years <- t:T
  
  # Get depths for these years
  depths <- depth_vec[years]
  
  # Apply damage threshold - ignore flooding below this depth
  depths[depths < threshold] <- 0
  
  # Calculate damage each year
  damages <- damage_from_depth(depths, strval, params)
  
  # Discount and sum
  n <- length(years)
  sum(damages * beta^(1:n))
}


# CORE RETREAT CALCULATION FUNCTION

# Function to calculate T* for one property × one scenario × one parameter set
calc_tstar_one <- function(prop_id, scenario_name, hazards_df, props_df, 
                           rental_yield, discount_rate, damage_threshold, owner_utility, storm_scenario) {
  
  # Get hazard timeline for this property × scenario
  prop_hazards <- hazards_df %>%
    filter(parcel_id == prop_id, scenario == scenario_name) %>%
    arrange(year)
  
  # Get property economics
  prop <- props_df %>% filter(parcel_id == prop_id)
  
  if (nrow(prop_hazards) == 0 || nrow(prop) == 0) {
    return(tibble(
      parcel_id = prop_id,
      scenario = scenario_name,
      T_star = NA_real_,
      reason = "no_data"
    ))
  }
  
  # Calculate rental income based on yield
  price_val <- prop$PRICE[1]
  rent_val <- price_val * rental_yield
  str_val <- prop$strval[1]
  
  # Convert discount rate to discount factor
  beta <- 1 / (1 + discount_rate)
  
  # Time series of flood depths (meters) - select based on storm scenario
  # depth_m = w000 (average conditions), depth_m_w100 = 100-year storm
  if (storm_scenario == "w100") {
    depth_ts <- prop_hazards$depth_m_w100
  } else {
    depth_ts <- prop_hazards$depth_m  # Default to w000
  }
  
  if (all(is.na(depth_ts)) || all(depth_ts == 0, na.rm = TRUE)) {
    # Never floods → never retreat
    return(tibble(
      parcel_id = prop_id,
      scenario = scenario_name,
      T_star = bigT + 1,  # Beyond planning horizon
      reason = "never_floods"
    ))
  }
  
  # Find optimal T*: first year where NPV(staying) <= 0
  # NPV(staying) = NPV(rent) + NPV(owner_utility) - NPV(damages)
  
  t_star <- bigT + 1  # Default: never retreat within planning horizon
  
  for (t in 1:bigT) {
    npv_r <- npv_rent(t, bigT, rent_val, beta)
    npv_utility <- npv_rent(t, bigT, owner_utility, beta)
    npv_d <- npv_damages(t, bigT, depth_ts, str_val, depth_params, beta, damage_threshold)
    
    # Check if staying is no longer economically viable
    npv_staying <- npv_r + npv_utility - npv_d
    if (is.finite(npv_staying) && npv_staying <= 0) {
      t_star <- t
      break
    }
  }
  
  # Determine reason for retreat
  reason <- if (t_star > bigT) {
    "no_retreat_needed"
  } else if (t_star <= 10) {
    "immediate"
  } else if (t_star <= 25) {
    "early"
  } else if (t_star <= 50) {
    "mid_term"
  } else {
    "late"
  }
  
  tibble(
    parcel_id = prop_id,
    scenario = scenario_name,
    T_star = t_star,
    reason = reason
  )
}

# Wrapper function to run all properties × scenarios for given parameters
run_scenario <- function(rental_yield, discount_rate, damage_threshold, owner_utility, storm_scenario,
                         scenario_label = "custom") {
  
  message("\n→ Running: ", scenario_label)
  message("  Rental yield: ", rental_yield * 100, "%, ",
          "Discount rate: ", discount_rate * 100, "%, ",
          "Threshold: ", damage_threshold, "m, ",
          "Owner utility: $", format(owner_utility, big.mark = ","), ", ",
          "Storm: ", storm_scenario)
  
  scenarios <- unique(hazards$scenario)
  parcel_ids <- unique(hazards$parcel_id)
  
  results_list <- list()
  
  for (scen in scenarios) {
    scenario_results <- map_dfr(
      parcel_ids,
      ~ calc_tstar_one(.x, scen, hazards, props,
                       rental_yield, discount_rate, damage_threshold, owner_utility, storm_scenario)
    )
    results_list[[scen]] <- scenario_results
  }
  
  retreat_schedule <- bind_rows(results_list) %>%
    rename(retreat_year = T_star) %>%
    mutate(
      parameter_set = scenario_label,
      rental_yield = rental_yield,
      discount_rate = discount_rate,
      damage_threshold = damage_threshold,
      owner_utility = owner_utility,
      storm_scenario = storm_scenario
    )
  
  # Add property details
  retreat_schedule <- retreat_schedule %>%
    left_join(
      props %>% select(
        parcel_id, ADDRESS, PRICE, strval, landval, elevation,
        LATITUDE, LONGITUDE
      ),
      by = "parcel_id"
    ) %>%
    mutate(rent = PRICE * rental_yield)
  
  return(retreat_schedule)
}


# RUN BASELINE SCENARIO
baseline_results <- run_scenario(
  rental_yield = BASELINE$rental_yield,
  discount_rate = BASELINE$discount_rate,
  damage_threshold = BASELINE$damage_threshold,
  owner_utility = BASELINE$owner_utility,
  storm_scenario = BASELINE$storm_scenario,
  scenario_label = "baseline"
)

# Save baseline results
baseline_path <- file.path(derived_dir, "retreat_schedule_baseline.csv")
write_csv(baseline_results, baseline_path)
message("\n✓ Saved baseline: ", basename(baseline_path))

# RUN SENSITIVITY ANALYSIS

if (RUN_SENSITIVITY) {
  
  message("\n========================================")
  message("RUNNING SENSITIVITY ANALYSIS")
  message("========================================")
  
  # Create all combinations of parameters (one-at-a-time sensitivity)
  # We'll vary one parameter at a time while holding others at baseline
  
  sensitivity_scenarios <- list()
  scenario_counter <- 1
  
  # 1. Rental yield sensitivity
  for (ry in SENSITIVITY_PARAMS$rental_yield) {
    if (ry != BASELINE$rental_yield) {  # Skip baseline (already ran)
      label <- sprintf("rental_yield_%.0fpct", ry * 100)
      sensitivity_scenarios[[scenario_counter]] <- list(
        label = label,
        rental_yield = ry,
        discount_rate = BASELINE$discount_rate,
        damage_threshold = BASELINE$damage_threshold,
        owner_utility = BASELINE$owner_utility,
        storm_scenario = BASELINE$storm_scenario
      )
      scenario_counter <- scenario_counter + 1
    }
  }
  
  # 2. Discount rate sensitivity
  for (dr in SENSITIVITY_PARAMS$discount_rate) {
    if (dr != BASELINE$discount_rate) {
      label <- sprintf("discount_rate_%.1fpct", dr * 100)
      sensitivity_scenarios[[scenario_counter]] <- list(
        label = label,
        rental_yield = BASELINE$rental_yield,
        discount_rate = dr,
        damage_threshold = BASELINE$damage_threshold,
        owner_utility = BASELINE$owner_utility,
        storm_scenario = BASELINE$storm_scenario
      )
      scenario_counter <- scenario_counter + 1
    }
  }
  
  # 3. Damage threshold sensitivity
  for (dt in SENSITIVITY_PARAMS$damage_threshold) {
    if (dt != BASELINE$damage_threshold) {
      label <- sprintf("threshold_%.0fcm", dt * 100)
      sensitivity_scenarios[[scenario_counter]] <- list(
        label = label,
        rental_yield = BASELINE$rental_yield,
        discount_rate = BASELINE$discount_rate,
        damage_threshold = dt,
        owner_utility = BASELINE$owner_utility,
        storm_scenario = BASELINE$storm_scenario
      )
      scenario_counter <- scenario_counter + 1
    }
  }
  
  # 4. Owner utility sensitivity
  for (ou in SENSITIVITY_PARAMS$owner_utility) {
    if (ou != BASELINE$owner_utility) {
      label <- sprintf("owner_utility_%dk", ou / 1000)
      sensitivity_scenarios[[scenario_counter]] <- list(
        label = label,
        rental_yield = BASELINE$rental_yield,
        discount_rate = BASELINE$discount_rate,
        damage_threshold = BASELINE$damage_threshold,
        owner_utility = ou,
        storm_scenario = BASELINE$storm_scenario
      )
      scenario_counter <- scenario_counter + 1
    }
  }
  
  # 5. Storm scenario sensitivity
  for (ss in SENSITIVITY_PARAMS$storm_scenario) {
    if (ss != BASELINE$storm_scenario) {
      label <- sprintf("storm_%s", ss)
      sensitivity_scenarios[[scenario_counter]] <- list(
        label = label,
        rental_yield = BASELINE$rental_yield,
        discount_rate = BASELINE$discount_rate,
        damage_threshold = BASELINE$damage_threshold,
        owner_utility = BASELINE$owner_utility,
        storm_scenario = ss
      )
      scenario_counter <- scenario_counter + 1
    }
  }
  
  # Run all sensitivity scenarios
  all_sensitivity_results <- list()
  
  for (i in seq_along(sensitivity_scenarios)) {
    s <- sensitivity_scenarios[[i]]
    
    result <- run_scenario(
      rental_yield = s$rental_yield,
      discount_rate = s$discount_rate,
      damage_threshold = s$damage_threshold,
      owner_utility = s$owner_utility,
      storm_scenario = s$storm_scenario,
      scenario_label = s$label
    )
    
    all_sensitivity_results[[i]] <- result
  }
  
  # Combine all sensitivity results
  sensitivity_combined <- bind_rows(all_sensitivity_results)
  
  # Save sensitivity results
  sensitivity_path <- file.path(derived_dir, "retreat_schedule_sensitivity.csv")
  write_csv(sensitivity_combined, sensitivity_path)
  message("\n✓ Saved sensitivity analysis: ", basename(sensitivity_path))
  
  # CREATE SENSITIVITY SUMMARY
  
  message("\n→ Creating sensitivity summary...")
  
  # Combine baseline + sensitivity for comparison
  all_results <- bind_rows(baseline_results, sensitivity_combined)
  
  # Summary statistics by parameter set and SLR scenario
  sensitivity_summary <- all_results %>%
    group_by(parameter_set, scenario, rental_yield, discount_rate, damage_threshold, owner_utility, storm_scenario) %>%
    summarise(
      total_parcels = n(),
      immediate_retreat = sum(retreat_year <= 10, na.rm = TRUE),
      immediate_pct = round(100 * immediate_retreat / n(), 1),
      early_retreat = sum(retreat_year > 10 & retreat_year <= 25, na.rm = TRUE),
      early_pct = round(100 * early_retreat / n(), 1),
      mid_retreat = sum(retreat_year > 25 & retreat_year <= 50, na.rm = TRUE),
      mid_pct = round(100 * mid_retreat / n(), 1),
      late_retreat = sum(retreat_year > 50 & retreat_year <= 100, na.rm = TRUE),
      late_pct = round(100 * late_retreat / n(), 1),
      no_retreat = sum(retreat_year > 100 | is.na(retreat_year), na.rm = TRUE),
      no_retreat_pct = round(100 * no_retreat / n(), 1),
      median_retreat = median(retreat_year[retreat_year <= 100], na.rm = TRUE),
      mean_retreat = mean(retreat_year[retreat_year <= 100], na.rm = TRUE),
      properties_retreat = sum(retreat_year <= 100, na.rm = TRUE),
      total_value_at_risk_millions = round(sum(PRICE[retreat_year <= 100], na.rm = TRUE) / 1e6, 1),
      .groups = "drop"
    ) %>%
    arrange(parameter_set, scenario)
  
  # Save summary
  summary_path <- file.path(derived_dir, "sensitivity_summary.csv")
  write_csv(sensitivity_summary, summary_path)
  message("✓ Saved sensitivity summary: ", basename(summary_path))
  
} else {
  message("\nSensitivity analysis skipped (RUN_SENSITIVITY = FALSE)")
  all_results <- baseline_results
  sensitivity_summary <- baseline_results %>%
    group_by(parameter_set = "baseline", scenario) %>%
    summarise(
      total_parcels = n(),
      immediate_retreat = sum(retreat_year <= 10, na.rm = TRUE),
      immediate_pct = round(100 * immediate_retreat / n(), 1),
      median_retreat = median(retreat_year[retreat_year <= 100], na.rm = TRUE),
      .groups = "drop"
    )
}


# CREATE SUMMARY TEXT REPORT
# Baseline summary by scenario
baseline_scenario_summary <- baseline_results %>%
  group_by(scenario) %>%
  summarise(
    total_parcels = n(),
    immediate_retreat = sum(retreat_year <= 10, na.rm = TRUE),
    early_retreat = sum(retreat_year > 10 & retreat_year <= 25, na.rm = TRUE),
    mid_retreat = sum(retreat_year > 25 & retreat_year <= 50, na.rm = TRUE),
    late_retreat = sum(retreat_year > 50 & retreat_year <= 100, na.rm = TRUE),
    no_retreat = sum(retreat_year > 100 | is.na(retreat_year), na.rm = TRUE),
    median_retreat = median(retreat_year[retreat_year <= 100], na.rm = TRUE),
    mean_retreat = mean(retreat_year[retreat_year <= 100], na.rm = TRUE),
    .groups = "drop"
  )

# Economic summary
economic_summary <- baseline_results %>%
  filter(retreat_year <= 100) %>%
  group_by(scenario) %>%
  summarise(
    properties_retreat = n(),
    total_property_value = sum(PRICE, na.rm = TRUE),
    total_structure_value = sum(strval, na.rm = TRUE),
    mean_property_value = mean(PRICE, na.rm = TRUE),
    .groups = "drop"
  )

# Create summary text
summary_lines <- c(
  "========================================",
  "CoSMoS Retreat Schedule Summary",
  "========================================",
  paste("Site:", case_name),
  paste("Date:", Sys.Date()),
  paste("Planning horizon:", bigT, "years"),
  "",
  "BASELINE PARAMETERS:",
  paste("  Rental yield:", BASELINE$rental_yield * 100, "%"),
  paste("  Discount rate:", BASELINE$discount_rate * 100, "%"),
  paste("  Damage threshold:", BASELINE$damage_threshold, "m"),
  paste("  Owner utility: $", format(BASELINE$owner_utility, big.mark=","), "/year"),
  paste("  Storm scenario:", BASELINE$storm_scenario, 
        ifelse(BASELINE$storm_scenario == "w000", "(average conditions)", "(100-year storm)")),
  "",
  "BASELINE RETREAT TIMING BY SCENARIO:",
  ""
)

for (i in 1:nrow(baseline_scenario_summary)) {
  s <- baseline_scenario_summary[i, ]
  summary_lines <- c(
    summary_lines,
    paste0("  ", s$scenario, ":"),
    paste0("    Total parcels: ", s$total_parcels),
    paste0("    Immediate retreat (≤10 yrs): ", s$immediate_retreat, 
           " (", round(100*s$immediate_retreat/s$total_parcels, 1), "%)"),
    paste0("    Early retreat (11-25 yrs): ", s$early_retreat,
           " (", round(100*s$early_retreat/s$total_parcels, 1), "%)"),
    paste0("    Mid-term retreat (26-50 yrs): ", s$mid_retreat,
           " (", round(100*s$mid_retreat/s$total_parcels, 1), "%)"),
    paste0("    Late retreat (51-100 yrs): ", s$late_retreat,
           " (", round(100*s$late_retreat/s$total_parcels, 1), "%)"),
    paste0("    No retreat needed: ", s$no_retreat,
           " (", round(100*s$no_retreat/s$total_parcels, 1), "%)"),
    paste0("    Median retreat year: ", round(s$median_retreat, 1)),
    paste0("    Mean retreat year: ", round(s$mean_retreat, 1)),
    ""
  )
}

summary_lines <- c(
  summary_lines,
  "",
  "BASELINE ECONOMIC IMPACT:",
  ""
)

for (i in 1:nrow(economic_summary)) {
  s <- economic_summary[i, ]
  summary_lines <- c(
    summary_lines,
    paste0("  ", s$scenario, ":"),
    paste0("    Properties requiring retreat: ", s$properties_retreat),
    paste0("    Total property value at risk: $", 
           format(round(s$total_property_value/1e6, 1), big.mark = ","), "M"),
    paste0("    Total structure value at risk: $",
           format(round(s$total_structure_value/1e6, 1), big.mark = ","), "M"),
    paste0("    Mean property value: $",
           format(round(s$mean_property_value/1e3), big.mark = ","), "K"),
    ""
  )
}

if (RUN_SENSITIVITY) {
  summary_lines <- c(
    summary_lines,
    "",
    "========================================",
    "SENSITIVITY ANALYSIS RESULTS",
    "========================================",
    "",
    "Key finding: How sensitive is retreat timing to parameter assumptions?",
    "",
    "Summary by parameter variation (Intermediate scenario only):",
    ""
  )
  
  # Show immediate retreat % for each parameter variation
  sens_intermediate <- sensitivity_summary %>%
    filter(scenario == "Intermediate") %>%
    arrange(parameter_set)
  
  summary_lines <- c(
    summary_lines,
    "Parameter Set                    | Immediate (<10yr) | Median Retreat Year",
    "--------------------------------|-------------------|--------------------"
  )
  
  # Add baseline first
  baseline_int <- sens_intermediate %>% filter(parameter_set == "baseline")
  if (nrow(baseline_int) > 0) {
    summary_lines <- c(
      summary_lines,
      sprintf("%-31s | %6.1f%%          | %.1f years",
              "BASELINE (5%, 2%, 15cm, $0)",
              baseline_int$immediate_pct,
              baseline_int$median_retreat)
    )
  }
  
  # Add sensitivity scenarios
  sens_nonbaseline <- sens_intermediate %>% filter(parameter_set != "baseline")
  for (i in 1:nrow(sens_nonbaseline)) {
    s <- sens_nonbaseline[i, ]
    summary_lines <- c(
      summary_lines,
      sprintf("%-31s | %6.1f%%          | %.1f years",
              s$parameter_set,
              s$immediate_pct,
              s$median_retreat)
    )
  }
  
  summary_lines <- c(
    summary_lines,
    "",
    "See sensitivity_summary.csv for complete results across all scenarios.",
    ""
  )
}

summary_lines <- c(
  summary_lines,
  "",
  "OUTPUT FILES:",
  "  - retreat_schedule_baseline.csv: Baseline scenario property-level results",
  if (RUN_SENSITIVITY) "  - retreat_schedule_sensitivity.csv: All sensitivity scenarios" else NULL,
  if (RUN_SENSITIVITY) "  - sensitivity_summary.csv: Summary statistics for comparison" else NULL,
  "",
  "OUTPUT COLUMNS:",
  "  - parcel_id: Property identifier",
  "  - scenario: SLR scenario (Intermediate, Intermediate_High, High)",
  "  - retreat_year: Optimal year to retreat (T*)",
  "  - reason: Timing category (immediate, early, mid_term, late, no_retreat_needed)",
  "  - parameter_set: Label for parameter combination",
  "  - rental_yield, discount_rate, damage_threshold, owner_utility, storm_scenario: Parameters used",
  "  - ADDRESS, PRICE, rent, strval, elevation, etc.: Property details",
  "",
  "NEXT STEPS:",
  "  1. Review retreat_schedule_baseline.csv for detailed property-level results",
  "  2. Compare sensitivity_summary.csv to understand parameter importance",
  "  3. Create visualizations comparing scenarios and parameter variations",
  "  4. Identify priority properties for buyout programs",
  "========================================"
)

summary_file <- file.path(derived_dir, "retreat_schedule_summary.txt")
writeLines(summary_lines, summary_file)
