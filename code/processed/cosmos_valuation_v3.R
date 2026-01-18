# cosmos_valuation_v3.R
# Purpose: Calculate optimal retreat year T* using flood NPV and cliff retreat distance
# Author: Will Dean (Managed Shores)
#
# MODEL FRAMING:
#   This model calculates economically optimal retreat timing (T*) based on NPV analysis.
#   
#   NPV Calculation:
#     NPV(staying) = Rental Income - Flood Damages
#     T* = year when NPV(staying) ≤ 0 (economically unsustainable)
#     
#     Note: Tenant (former owner) covers normal property maintenance and wear-and-tear
#     as part of standard lease agreement. Government only pays for flood damages.
#   
#   Buyout-Leaseback Pricing:
#     Buyout Price = NPV(future rental income from year 1 to T*) + Land Value
#     - Compensates owner for lost rental income stream and land
#     - Does NOT pay for structure value (economically obsolete at T*)
#     - Government leases property back to owner, receives rental income
#     - Eventually acquires land when structure fails
#   
#   Leaseback Structure:
#     - Tenant (former owner) pays market rent
#     - Tenant covers normal property maintenance (standard lease terms)
#     - Government (landlord) responsible only for major structural issues from flooding
#     - This is the standard landlord-tenant model used throughout rental markets
#   
#   Results provide retreat timing estimates and buyout costs for various scenarios.
#   Governments can use these to inform buyout-leaseback program design.
#
# Key Features:
#   - Literature-based cliff thresholds (10m minimum safety, 25m planned retreat)
#   - Handles properties already within threshold
#   - Unified script for flood-only, cliff-only, and hybrid sites
#   - Storm scenarios - ONLY affect flood sites, not cliff sites
#   - Monte Carlo simulation for stochastic storm modeling
#   - NPV-based buyout pricing (economic value vs. market fiction)
#
# Input:
#   - data/{site}/derived/cosmos_annual_hazards.csv (from interpolate_cosmos.R)
#   - data/{site}/redfin_df.csv (property data with prices, rents, locations)
#   - data/landprices_CA_ms.csv (California land values by ZIP code)
#
# Output:
#   - data/{site}/derived/retreat_schedule_baseline.csv
#   - data/{site}/derived/retreat_schedule_sensitivity.csv
#   - data/{site}/derived/sensitivity_summary.csv
#   - data/{site}/derived/retreat_schedule_summary.txt
#   - data/{site}/derived/mc_distributions_baseline.csv (if Monte Carlo enabled)
#   - data/{site}/derived/community_stats_baseline.json

rm(list = ls())
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(stringr)
library(jsonlite)  # For saving community stats to JSON

source("code/processed/monte_carlo_storms.R") 

# CONFIGURATION
case_name <- "isla_vista"  # Change for other sites
data_dir <- file.path("data", case_name)
derived_dir <- file.path(data_dir, "derived")

# Planning horizon
bigT <- 80    # Years (matches OPC 2024 projections)

# Depth-damage function parameters (fitted logistic curve)
depth_params <- c(a = 0.4, b = 1, d = 2)

# CLIFF RETREAT PARAMETERS (Literature-Based)
# Citations: CCC guidance, Lindstrom v. CCC (2019), Martin v. CCC (2021)
CLIFF_THRESHOLD_RETREAT <- 25  # Planned retreat (25m = ~82ft per CCC standards)
CLIFF_THRESHOLD_IMMEDIATE <- 10  # Immediate safety (10m = ~33ft minimum setback)

# For properties already within retreat threshold, calculate T* based on 
# time to reach immediate threshold rather than forcing immediate retreat
USE_EROSION_RATE_FOR_EXPOSED <- TRUE

# BASELINE PARAMETERS
BASELINE <- list(
  rental_yield = 0.05,      # 5% annual yield (industry benchmark)
  discount_rate = 0.02,     # 2% (appropriate for government planning)
  damage_threshold = 0.15,  # 15cm (accounts for nuisance flooding)
  cliff_scenario = "LetItGo"  # Natural retreat (vs HoldTheLine for sensitivity)
)

# MONTE CARLO CONFIGURATION
RUN_MONTE_CARLO <- TRUE   # Set FALSE to use deterministic baseline
N_MC_SIMS <- 1000        # Number of simulations (10,000 for publication)

if (RUN_MONTE_CARLO) {
  message("\nMONTE CARLO: ENABLED")
  message("  Simulations per property: ", N_MC_SIMS)
} else {
  message("\nMONTE CARLO: DISABLED (using deterministic baseline)")
}

# SENSITIVITY ANALYSIS 
RUN_SENSITIVITY <- TRUE  # can toggle for quicker testing

# SMART SENSITIVITY DESIGN:
# Instead of full factorial (324 combinations), use targeted comparisons
# Provides all data for thesis tables/figures in ~1.5 hours instead of 15 hours

# Create smart parameter grid
create_smart_sensitivity_grid <- function() {
  
  # Helper to create parameter set
  make_params <- function(disc, thresh, yield = 0.05) {
    tibble(
      rental_yield = yield,
      discount_rate = disc,
      damage_threshold = thresh
    )
  }
  
  # TIER 1: Discount rate sensitivity
  tier1 <- bind_rows(
    make_params(disc = 0.015, thresh = 0.15, yield = 0.05),
    make_params(disc = 0.020, thresh = 0.15, yield = 0.05),
    make_params(disc = 0.030, thresh = 0.15, yield = 0.05)
  )
  
  # TIER 2: Damage threshold sensitivity
  tier2 <- bind_rows(
    make_params(disc = 0.02, thresh = 0.15, yield = 0.05),
    make_params(disc = 0.02, thresh = 0.30, yield = 0.05)
  )
  
  # TIER 3: Rental yield sensitivity
  tier3 <- bind_rows(
    make_params(disc = 0.02, thresh = 0.15, yield = 0.03),
    make_params(disc = 0.02, thresh = 0.15, yield = 0.04),
    make_params(disc = 0.02, thresh = 0.15, yield = 0.05),
    make_params(disc = 0.02, thresh = 0.15, yield = 0.06)
  )
  
  # Combine and remove duplicates
  bind_rows(tier1, tier2, tier3) %>% distinct()
}

SENSITIVITY_GRID <- create_smart_sensitivity_grid()

message("\n=== STREAMLINED SENSITIVITY DESIGN ===")
message("Total combinations (per cliff scenario): ", nrow(SENSITIVITY_GRID))
message("\nTiers:")
message("  1. Discount rates: 1.5%, 2%, 3%")  
message("  2. Damage thresholds: 0.15m (baseline), 0.3m (high tolerance)")
message("  3. Rental yields: 3%, 4%, 5%, 6%")
message("\nFor sites WITH cliffs (Isla Vista, Pacifica):")
message("  Each combination tested with LetItGo AND HoldTheLine")
message("  Total: ", nrow(SENSITIVITY_GRID), " × 2 cliff scenarios = ", nrow(SENSITIVITY_GRID) * 2)
message("\nFor sites WITHOUT cliffs")
message("  Cliff scenario fixed to LetItGo")
message("  Total: ", nrow(SENSITIVITY_GRID), " combinations")
message("\nEstimated runtime:")
message("  No cliffs: ~", round(nrow(SENSITIVITY_GRID) * 3 / 60, 1), " hours")
message("  Pacifica/Isla Vista (with cliffs): ~", round(nrow(SENSITIVITY_GRID) * 2 * 3 / 60, 1), " hours")


message("CoSMoS Retreat Year Calculation: ", toupper(case_name))
message("Planning horizon: ", bigT, " years")
message("\nCLIFF RETREAT PARAMETERS:")
message("  Planned retreat threshold: ", CLIFF_THRESHOLD_RETREAT, "m (based on CCC standards)")
message("  Immediate safety threshold: ", CLIFF_THRESHOLD_IMMEDIATE, "m")
message("  Properties already exposed: ", ifelse(USE_EROSION_RATE_FOR_EXPOSED, 
                                                 "Calculate T* using erosion rate", "Force immediate retreat"))
message("\nBASELINE PARAMETERS:")
message("  Rental yield: ", BASELINE$rental_yield * 100, "%")
message("  Discount rate: ", BASELINE$discount_rate * 100, "%")
message("  Damage threshold: ", BASELINE$damage_threshold, "m (for flood sites)")
message("  Cliff scenario: ", BASELINE$cliff_scenario)

if (RUN_SENSITIVITY) {
  message("\nSENSITIVITY ANALYSIS: ENABLED")
} else {
  message("\nSENSITIVITY ANALYSIS: DISABLED")
}

# LOAD DATA
hazards_path <- file.path(derived_dir, "cosmos_annual_hazards.csv")
stopifnot(file.exists(hazards_path))

hazards <- read_csv(hazards_path, show_col_types = FALSE)

# Detect site type based on available data
has_flood <- "depth_m" %in% names(hazards) && sum(!is.na(hazards$depth_m)) > 0
has_cliff <- "cliff_dist_m" %in% names(hazards) && sum(!is.na(hazards$cliff_dist_m)) > 0
has_cliff_scenario <- "cliff_scenario" %in% names(hazards)

site_type <- if (has_flood && !has_cliff) {
  "FLOOD-ONLY"
} else if (has_cliff && !has_flood) {
  "CLIFF-ONLY"
} else if (has_flood && has_cliff) {
  "HYBRID (flood + cliff)"
} else {
  "UNKNOWN"
}

message("✓ Loaded annual hazards: ", nrow(hazards), " rows")
message("  Site type: ", site_type)
message("  Parcels: ", n_distinct(hazards$parcel_id))
message("  Scenarios: ", paste(unique(hazards$scenario), collapse = ", "))
if (has_cliff_scenario) {
  message("  Cliff scenarios: ", paste(unique(hazards$cliff_scenario), collapse = ", "))
}
message("  Years: ", min(hazards$year), "-", max(hazards$year), "\n")

# Rename scenario column for consistency
if ("slr_scenario" %in% names(hazards) && !"scenario" %in% names(hazards)) {
  hazards <- hazards %>% rename(scenario = slr_scenario)
}

# LOAD STORM DATA FOR MONTE CARLO
if (RUN_MONTE_CARLO && site_type %in% c("FLOOD-ONLY", "HYBRID (flood + cliff)")) {
  
  message("\n→ Loading storm scenario data...")
  
  storm_annual_path <- file.path(derived_dir, "storm_annual.csv")
  storm_20yr_path <- file.path(derived_dir, "storm_20yr.csv")
  storm_100yr_path <- file.path(derived_dir, "storm_100yr.csv")
  
  if (all(file.exists(c(storm_annual_path, storm_20yr_path, storm_100yr_path)))) {
    
    storm_data_list <- list(
      annual = read_csv(storm_annual_path, show_col_types = FALSE),
      storm_20yr = read_csv(storm_20yr_path, show_col_types = FALSE),
      storm_100yr = read_csv(storm_100yr_path, show_col_types = FALSE)
    )
    
    message("✓ Loaded storm data:")
    message("  Annual storms: ", nrow(storm_data_list$annual), " rows")
    message("  20-year storms: ", nrow(storm_data_list$storm_20yr), " rows")
    message("  100-year storms: ", nrow(storm_data_list$storm_100yr), " rows")
    
  } else {
    warning("Monte Carlo enabled but storm data files not found:")
    warning("  Expected: ", derived_dir, "/storm_*.csv")
    warning("  Falling back to deterministic mode")
    storm_data_list <- NULL
    RUN_MONTE_CARLO <- FALSE
  }
  
} else {
  
  if (RUN_MONTE_CARLO) {
    message("\n→ Monte Carlo not applicable (cliff-only site)")
  }
  storm_data_list <- NULL
  
}


# Load property data
props_path <- file.path(data_dir, "redfin_df.csv")
stopifnot(file.exists(props_path))

props_raw <- read_csv(props_path, show_col_types = FALSE)

props <- props_raw %>%
  mutate(parcel_id = row_number()) %>%
  filter(!is.na(PRICE))

message("✓ Loaded property data: ", nrow(props), " properties")
message("  Mean price: $", format(round(mean(props$PRICE, na.rm = TRUE)), big.mark = ","), "\n")

# Load land values
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

avg_share <- mean(landvals$landshare, na.rm = TRUE)

props <- props %>%
  mutate(`ZIP OR POSTAL CODE` = as.character(`ZIP OR POSTAL CODE`)) %>%
  left_join(landvals, by = "ZIP OR POSTAL CODE") %>%
  mutate(
    landval = PRICE * landshare,
    landval = if_else(is.na(landval), PRICE * avg_share, landval),
    strval = PRICE - landval
  )

message("✓ Calculated land and structure values")
message("  Mean structure value: $", format(round(mean(props$strval, na.rm = TRUE)), big.mark = ","))
message("  Mean land value: $", format(round(mean(props$landval, na.rm = TRUE)), big.mark = ","), "\n")


# ECONOMIC FUNCTIONS

npv_rent <- function(t, T, rent_annual, beta) {
  if (t > T) return(0)
  years <- t:T
  n <- length(years)
  sum(rent_annual * beta^(1:n))
}

damage_from_depth <- function(depth_m, strval, params = depth_params) {
  a <- params["a"]
  b <- params["b"]
  d <- params["d"]
  frac <- a / (1 + exp(-b * (depth_m - d)))
  pmax(frac, 0) * strval
}

npv_damages <- function(t, T, depth_vec, strval, params, beta, threshold) {
  if (t > T) return(0)
  years <- t:T
  depths <- depth_vec[years]
  depths[depths < threshold] <- 0
  damages <- damage_from_depth(depths, strval, params)
  n <- length(years)
  sum(damages * beta^(1:n))
}


# CORE RETREAT CALCULATION FUNCTION

calc_tstar_one <- function(prop_id, scenario_name, cliff_scen_name, hazards_df, props_df, 
                           rental_yield, discount_rate, damage_threshold, 
                           site_type) {
  
  # Get hazard timeline for this property × scenario
  if (is.na(cliff_scen_name) || !has_cliff_scenario) {
    prop_hazards <- hazards_df %>%
      filter(parcel_id == prop_id, scenario == scenario_name) %>%
      arrange(year)
  } else {
    prop_hazards <- hazards_df %>%
      filter(parcel_id == prop_id, scenario == scenario_name, cliff_scenario == cliff_scen_name) %>%
      arrange(year)
  }
  
  prop <- props_df %>% filter(parcel_id == prop_id)
  
  if (nrow(prop_hazards) == 0 || nrow(prop) == 0) {
    return(tibble(
      parcel_id = prop_id,
      scenario = scenario_name,
      cliff_scenario = cliff_scen_name,
      T_star = NA_real_,
      T_flood = NA_real_,
      T_cliff = NA_real_,
      initial_cliff_dist = NA_real_,
      retreat_trigger = "no_data",
      cliff_exposure_status = NA_character_
    ))
  }
  
  # Property economics
  price_val <- prop$PRICE[1]
  rent_val <- price_val * rental_yield
  str_val <- prop$strval[1]
  beta <- 1 / (1 + discount_rate)
  
  # Initialize retreat years
  t_flood <- bigT + 1
  t_cliff <- bigT + 1
  initial_cliff_dist <- NA_real_
  cliff_exposure_status <- "not_applicable"
  
  
  # ===== CLIFF TRIGGER =====
  # For cliff sites: Based purely on distance thresholds
  # NOTE: CoSMoS cliff retreat already incorporates mean annual wave power
  #       No storm scenario variation needed (unlike flood sites)
  
  if (site_type %in% c("CLIFF-ONLY", "HYBRID (flood + cliff)")) {
    
    if ("cliff_dist_m" %in% names(prop_hazards)) {
      cliff_dist_ts <- prop_hazards$cliff_dist_m
      initial_cliff_dist <- cliff_dist_ts[1]  # Year 1 distance
      
      if (!all(is.na(cliff_dist_ts))) {
        
        # Determine current exposure status
        if (initial_cliff_dist < CLIFF_THRESHOLD_IMMEDIATE) {
          cliff_exposure_status <- "immediate_hazard"
        } else if (initial_cliff_dist < CLIFF_THRESHOLD_RETREAT) {
          cliff_exposure_status <- "within_retreat_threshold"
        } else {
          cliff_exposure_status <- "safe"
        }
        
        # Calculate retreat year based on exposure status
        if (USE_EROSION_RATE_FOR_EXPOSED && initial_cliff_dist < CLIFF_THRESHOLD_RETREAT) {
          # Property already within planned retreat threshold
          # Calculate T* as time to reach immediate safety threshold (10m)
          
          years_immediate <- which(cliff_dist_ts < CLIFF_THRESHOLD_IMMEDIATE)
          if (length(years_immediate) > 0) {
            t_cliff <- min(years_immediate)
          } else {
            # Never reaches immediate threshold within horizon
            t_cliff <- bigT + 1
          }
          
        } else {
          # Standard case: property currently safe (>25m)
          # Retreat when cliff reaches planned retreat threshold (25m)
          
          years_retreat <- which(cliff_dist_ts < CLIFF_THRESHOLD_RETREAT)
          if (length(years_retreat) > 0) {
            t_cliff <- min(years_retreat)
          }
        }
      }
    }
  }
  
  # ===== FINAL RETREAT YEAR =====
  # T* = earlier of flood trigger or cliff trigger
  t_star <- min(t_flood, t_cliff)
  
  # Determine trigger type
  retreat_trigger <- if (t_star > bigT) {
    "no_retreat_needed"
  } else if (t_star == t_flood && t_star == t_cliff) {
    "both_simultaneous"
  } else if (t_star == t_flood) {
    "flood_npv"
  } else if (t_star == t_cliff) {
    if (cliff_exposure_status == "immediate_hazard") {
      "cliff_immediate_safety"
    } else if (cliff_exposure_status == "within_retreat_threshold") {
      "cliff_erosion_ongoing"
    } else {
      "cliff_planned_retreat"
    }
  } else {
    "unknown"
  }
  
  # Timing category
  timing <- if (t_star > bigT) {
    "no_retreat"
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
    cliff_scenario = cliff_scen_name,
    T_star = t_star,
    T_flood = t_flood,
    T_cliff = t_cliff,
    initial_cliff_dist = initial_cliff_dist,
    cliff_exposure_status = cliff_exposure_status,
    retreat_trigger = retreat_trigger,
    timing_category = timing
  )
}

# Wrapper function
run_scenario <- function(rental_yield, discount_rate, damage_threshold,  
                         cliff_scenario, scenario_label = "custom") {
  
  message("\n→ Running: ", scenario_label)
  message("  Parameters: yield=", rental_yield*100, "%, disc=", discount_rate*100, 
          "%, thresh=", damage_threshold, "m, cliff=", cliff_scenario)
  
  slr_scenarios <- unique(hazards$scenario)
  parcel_ids <- unique(hazards$parcel_id)
  
  if (has_cliff_scenario) {
    cliff_scenarios <- c(cliff_scenario)
  } else {
    cliff_scenarios <- c(NA)
  }
  
  results_list <- list()
  mc_distributions_list <- list()  # NEW: Collect MC distributions separately
  
  for (slr_scen in slr_scenarios) {
    for (cliff_scen in cliff_scenarios) {
      
      # Choose Monte Carlo or Deterministic based on configuration
      if (RUN_MONTE_CARLO && !is.null(storm_data_list)) {
        
        # MONTE CARLO MODE
        scenario_results_with_attrs <- map(
          parcel_ids,
          ~ calc_tstar_monte_carlo(
            prop_id = .x,
            scenario_name = slr_scen,
            cliff_scen_name = cliff_scen,
            hazards_df = hazards,
            props_df = props,
            storm_data_list = storm_data_list,
            n_sims = N_MC_SIMS,
            rental_yield = rental_yield,
            discount_rate = discount_rate,
            damage_threshold = damage_threshold,
            site_type = site_type,
            bigT = bigT,
            depth_params = depth_params
          )
        )
        
        # Extract distributions before binding rows
        for (i in seq_along(scenario_results_with_attrs)) {
          result_row <- scenario_results_with_attrs[[i]]
          dist <- attr(result_row, "mc_distribution")
          
          if (!is.null(dist) && length(dist) > 0) {
            mc_distributions_list[[length(mc_distributions_list) + 1]] <- tibble(
              parcel_id = result_row$parcel_id,
              scenario = slr_scen,
              cliff_scenario = cliff_scen,
              sim_num = 1:length(dist),
              retreat_year = dist
            )
          }
        }
        
        # Bind rows (this loses attributes but we've saved them)
        scenario_results <- bind_rows(scenario_results_with_attrs)
        
      } else {
        
        # DETERMINISTIC MODE
        scenario_results <- map_dfr(
          parcel_ids,
          ~ calc_tstar_one(.x, slr_scen, cliff_scen, hazards, props,
                           rental_yield, discount_rate, damage_threshold, 
                           site_type)
        )
        
        # Add placeholder MC columns for consistency
        scenario_results <- scenario_results %>%
          mutate(
            mc_enabled = FALSE,
            mc_n_sims = 0,
            mc_mean_year = T_star,
            mc_median_year = T_star,
            mc_sd_year = 0,
            mc_q05_year = T_star,
            mc_q25_year = T_star,
            mc_q75_year = T_star,
            mc_q95_year = T_star,
            mc_range_years = 0
          )
      }
      
      key <- paste(slr_scen, cliff_scen, sep = "_")
      results_list[[key]] <- scenario_results
    }
  }
  
  retreat_schedule <- bind_rows(results_list)
  
  # Handle column naming (Monte Carlo returns retreat_year, deterministic returns T_star)
  if ("T_star" %in% names(retreat_schedule) && !"retreat_year" %in% names(retreat_schedule)) {
    retreat_schedule <- retreat_schedule %>% rename(retreat_year = T_star)
  }
  
  retreat_schedule <- retreat_schedule %>%
    mutate(
      parameter_set = scenario_label,
      rental_yield = rental_yield,
      discount_rate = discount_rate,
      damage_threshold = damage_threshold
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
  
  # Combine MC distributions if they exist
  mc_distributions_df <- NULL
  if (length(mc_distributions_list) > 0) {
    mc_distributions_df <- bind_rows(mc_distributions_list)
    mc_distributions_df <- mc_distributions_df %>%
      mutate(
        parameter_set = scenario_label,
        rental_yield = rental_yield,
        discount_rate = discount_rate,
        damage_threshold = damage_threshold
      )
  }
  
  # Return both results and distributions
  return(list(
    results = retreat_schedule,
    mc_distributions = mc_distributions_df
  ))
}


# ==============================================================================
# BUYOUT ECONOMICS CALCULATION (reusable for baseline and sensitivity)
# ==============================================================================

calculate_buyout_economics <- function(df) {
  
  message("  Calculating buyout prices for ", nrow(df), " rows...")
  
  df %>%
    mutate(
      # Calculate NPV of rental income from year 1 to T*
      # Uses the discount_rate column already in the dataframe
      npv_rent_to_tstar = mapply(
        function(rent, T, discount_rate) {
          if (T <= 0) return(0)
          if (T > bigT) return(NA_real_)
          beta <- 1 / (1 + discount_rate)
          rent * (1 - beta^T) / (1 - beta)
        },
        rent, retreat_year, discount_rate
      ),
      
      # BUYOUT PRICE: NPV of future rent stream + land value, CAPPED at market price
      buyout_price_uncapped = npv_rent_to_tstar + landval,
      
      buyout_price = if_else(
        retreat_year <= bigT,
        pmin(npv_rent_to_tstar + landval, PRICE),  # Cap at market
        NA_real_
      ),
      
      # Track which properties were capped
      buyout_capped = if_else(
        retreat_year <= bigT,
        buyout_price_uncapped > PRICE,
        NA
      ),
      
      # Comparison metrics
      buyout_market_100pct = if_else(retreat_year <= bigT, PRICE, NA_real_),
      
      buyout_savings_pct = if_else(
        retreat_year <= bigT,
        100 * (1 - buyout_price / PRICE),
        NA_real_
      ),
      
      buyout_savings_dollars = if_else(
        retreat_year <= bigT,
        PRICE - buyout_price,
        NA_real_
      ),
      
      # GOVERNMENT ECONOMICS
      gov_rent_collected = npv_rent_to_tstar,
      gov_land_recovered = landval,
      gov_net_cost = if_else(
        retreat_year <= bigT,
        buyout_price - gov_rent_collected - gov_land_recovered,
        NA_real_
      ),
      
      # Government outcome category
      gov_outcome = case_when(
        is.na(gov_net_cost) ~ NA_character_,
        gov_net_cost < 0 ~ "profit",
        gov_net_cost == 0 ~ "break_even",
        gov_net_cost <= landval * 0.1 ~ "near_break_even",
        TRUE ~ "net_cost"
      )
    )
}


# RUN BASELINE SCENARIO
baseline_output <- run_scenario(
  rental_yield = BASELINE$rental_yield,
  discount_rate = BASELINE$discount_rate,
  damage_threshold = BASELINE$damage_threshold,
  cliff_scenario = BASELINE$cliff_scenario,
  scenario_label = "baseline"
)

# Extract results and distributions
baseline_results <- baseline_output$results
baseline_mc_distributions <- baseline_output$mc_distributions

# Note: baseline_results will be saved AFTER buyout calculation

# BASELINE SUMMARY

message("Total properties: ", n_distinct(baseline_results$parcel_id))

# Monte Carlo summary if enabled
if (RUN_MONTE_CARLO && sum(baseline_results$mc_enabled, na.rm = TRUE) > 0) {
  print_mc_summary(baseline_results)
}

# Trigger summary
trigger_summary <- baseline_results %>%
  group_by(retreat_trigger) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(desc(count))

message("\nRetreat triggers:")
for (i in 1:nrow(trigger_summary)) {
  message("  ", trigger_summary$retreat_trigger[i], ": ", trigger_summary$count[i])
}

# Cliff exposure status (if applicable)
if (has_cliff) {
  exposure_summary <- baseline_results %>%
    filter(!is.na(cliff_exposure_status)) %>%
    group_by(cliff_exposure_status) %>%
    summarise(count = n(), .groups = "drop")
  
  message("\nCliff exposure status:")
  for (i in 1:nrow(exposure_summary)) {
    message("  ", exposure_summary$cliff_exposure_status[i], ": ", exposure_summary$count[i])
  }
}

# Timing summary
timing_summary <- baseline_results %>%
  group_by(timing_category) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(desc(count))

message("\nRetreat timing:")
for (i in 1:nrow(timing_summary)) {
  message("  ", timing_summary$timing_category[i], ": ", timing_summary$count[i])
}

# Statistics for retreating properties
retreating <- baseline_results %>% filter(retreat_year <= bigT)

if (nrow(retreating) > 0) {
  message("\nRetreating properties (", nrow(retreating), "):")
  message("  Mean retreat year: ", round(mean(retreating$retreat_year, na.rm = TRUE), 1))
  message("  Median retreat year: ", median(retreating$retreat_year, na.rm = TRUE))
}


# RUN SENSITIVITY ANALYSIS
if (RUN_SENSITIVITY) {
  
  message("RUNNING SENSITIVITY ANALYSIS")
  if (RUN_MONTE_CARLO) {
    message("NOTE: Monte Carlo is enabled - each scenario takes ~2-3 min")
  } else {
    message("Using deterministic mode")
  }
  
  # Use the smart grid
  param_grid <- SENSITIVITY_GRID
  
  # Add cliff_scenario column
  # For sites WITH cliffs: test both LetItGo and HoldTheLine
  # For sites WITHOUT cliffs: only LetItGo (cliffs not relevant)
  if (has_cliff_scenario) {
    # Expand grid to include both cliff scenarios
    param_grid <- bind_rows(
      param_grid %>% mutate(cliff_scenario = "LetItGo"),
      param_grid %>% mutate(cliff_scenario = "HoldTheLine")
    )
    message("Site has cliff properties - testing both cliff scenarios")
  } else {
    param_grid$cliff_scenario <- "LetItGo"
    message("Site has no cliff properties - cliff scenario fixed to LetItGo")
  }
  
  message("Total combinations: ", nrow(param_grid), "\n")
  
  sensitivity_results_list <- list()
  
  for (i in 1:nrow(param_grid)) {
    params <- param_grid[i, ]
    
    label <- sprintf(
      "y%.2f_d%.3f_t%.2f_%s",
      params$rental_yield,
      params$discount_rate,
      params$damage_threshold,
      ifelse(is.na(params$cliff_scenario), "NA", params$cliff_scenario)
    )
    
    output <- run_scenario(
      rental_yield = params$rental_yield,
      discount_rate = params$discount_rate,
      damage_threshold = params$damage_threshold,
      cliff_scenario = params$cliff_scenario,
      scenario_label = label
    )
    
    sensitivity_results_list[[i]] <- output$results
    
    # Optionally save MC distributions for sensitivity scenarios
    # (commented out by default to save space - can enable if needed)
    # if (!is.null(output$mc_distributions)) {
    #   mc_path <- file.path(derived_dir, paste0("mc_distributions_", label, ".csv"))
    #   write_csv(output$mc_distributions, mc_path)
    # }
  }
  
  sensitivity_results <- bind_rows(sensitivity_results_list)
  
  # CALCULATE BUYOUT ECONOMICS FOR SENSITIVITY
  message("\n→ Calculating buyout economics for SENSITIVITY...")
  sensitivity_results <- calculate_buyout_economics(sensitivity_results)
  
  sensitivity_path <- file.path(derived_dir, "retreat_schedule_sensitivity.csv")
  write_csv(sensitivity_results, sensitivity_path)
  message("\n✓ Saved sensitivity: ", basename(sensitivity_path))
  message("  Rows: ", format(nrow(sensitivity_results), big.mark = ","))
  message("  Includes: retreat timing + buyout economics + government economics")
  
  # Summary
  sensitivity_summary <- sensitivity_results %>%
    group_by(parameter_set, scenario, cliff_scenario, 
             rental_yield, discount_rate, damage_threshold) %>%
    summarise(
      n_properties = n(),
      mean_retreat_year = mean(retreat_year[retreat_year <= bigT], na.rm = TRUE),
      median_retreat_year = median(retreat_year[retreat_year <= bigT], na.rm = TRUE),
      n_immediate = sum(timing_category == "immediate", na.rm = TRUE),
      n_early = sum(timing_category == "early", na.rm = TRUE),
      n_mid = sum(timing_category == "mid_term", na.rm = TRUE),
      n_late = sum(timing_category == "late", na.rm = TRUE),
      n_no_retreat = sum(timing_category == "no_retreat", na.rm = TRUE),
      # Add buyout economics summaries
      total_buyout_M = sum(buyout_price, na.rm = TRUE) / 1e6,
      total_market_M = sum(buyout_market_100pct, na.rm = TRUE) / 1e6,
      gov_net_cost_M = sum(gov_net_cost, na.rm = TRUE) / 1e6,
      n_capped = sum(buyout_capped, na.rm = TRUE),
      n_gov_profit = sum(gov_outcome == "profit", na.rm = TRUE),
      .groups = "drop"
    )
  
  summary_path <- file.path(derived_dir, "sensitivity_summary.csv")
  write_csv(sensitivity_summary, summary_path)
  message("✓ Saved summary: ", basename(summary_path))
}

# CALCULATE BUYOUT ECONOMICS FOR BASELINE

message("\n→ Calculating buyout economics for BASELINE...")
baseline_results <- calculate_buyout_economics(baseline_results)

# Buyout summary
retreating_for_buyout <- baseline_results %>% filter(!is.na(buyout_price))
total_buyout <- sum(retreating_for_buyout$buyout_price, na.rm = TRUE)
total_market <- sum(retreating_for_buyout$PRICE, na.rm = TRUE)
total_savings <- sum(retreating_for_buyout$buyout_savings_dollars, na.rm = TRUE)
n_capped <- sum(retreating_for_buyout$buyout_capped, na.rm = TRUE)

message("✓ Buyout prices calculated:")
message("  Total buyout cost (NPV-based, capped): $", format(round(total_buyout / 1e6, 1), big.mark = ","), "M")
message("  Total market value: $", format(round(total_market / 1e6, 1), big.mark = ","), "M")
message("  Government savings vs market: $", format(round(total_savings / 1e6, 1), big.mark = ","), "M (",
        round(100 * total_savings / total_market, 1), "% discount)")
message("  Properties capped at market price: ", n_capped, " (", 
        round(100 * n_capped / nrow(retreating_for_buyout), 1), "%)")

# Government economics
gov_outcomes <- retreating_for_buyout %>%
  count(gov_outcome) %>%
  arrange(desc(n))

total_gov_net_cost <- sum(retreating_for_buyout$gov_net_cost, na.rm = TRUE)
avg_gov_net_cost <- mean(retreating_for_buyout$gov_net_cost, na.rm = TRUE)

message("\n→ Government Economics (Buyout-Leaseback Program):")
message("  Government net cost after rent collection: $", 
        format(round(total_gov_net_cost / 1e6, 1), big.mark = ","), "M")
message("  Average net cost per property: $", 
        format(round(avg_gov_net_cost / 1e3), big.mark = ","), "k")

message("\n  Outcome breakdown:")
for (i in 1:nrow(gov_outcomes)) {
  outcome_label <- switch(gov_outcomes$gov_outcome[i],
                          "profit" = "Properties where gov PROFITS",
                          "break_even" = "Properties where gov breaks even",
                          "near_break_even" = "Properties near break-even",
                          "net_cost" = "Properties with net cost to gov"
  )
  message("    ", outcome_label, ": ", gov_outcomes$n[i])
}

message("\n  Pricing model: NPV(future rent) + land value, capped at market price")

# Show average buyout price
avg_buyout <- mean(retreating_for_buyout$buyout_price, na.rm = TRUE)
avg_market <- mean(retreating_for_buyout$PRICE, na.rm = TRUE)
message("  Average buyout: $", format(round(avg_buyout / 1e3), big.mark = ","), "k")
message("  Average market: $", format(round(avg_market / 1e3), big.mark = ","), "k")

# OPTIONAL: Add sensitivity to test different pricing models
# Uncomment to test 90% or 110% scenarios
# baseline_results <- baseline_results %>%
#   mutate(
#     buyout_price_90pct = if_else(retreat_year <= bigT, PRICE * 0.90, NA_real_),
#     buyout_price_110pct = if_else(retreat_year <= bigT, PRICE * 1.10, NA_real_)
#   )

# SAVE BASELINE RESULTS (with buyout prices)
baseline_path <- file.path(derived_dir, "retreat_schedule_baseline.csv")
write_csv(baseline_results, baseline_path)
message("✓ Saved baseline with buyout prices: ", basename(baseline_path))

# SAVE MONTE CARLO DISTRIBUTIONS (for Shiny visualization)
if (!is.null(baseline_mc_distributions) && nrow(baseline_mc_distributions) > 0) {
  mc_dist_path <- file.path(derived_dir, "mc_distributions_baseline.csv")
  write_csv(baseline_mc_distributions, mc_dist_path)
  message("✓ Saved Monte Carlo distributions: ", basename(mc_dist_path))
  message("  Total rows: ", format(nrow(baseline_mc_distributions), big.mark = ","))
  message("  Properties: ", n_distinct(baseline_mc_distributions$parcel_id))
  message("  Simulations per property: ", N_MC_SIMS)
}

# CALCULATE AND SAVE COMMUNITY-LEVEL STATISTICS
message("\n→ Calculating community-level statistics...")
community_stats <- calculate_community_stats(baseline_results, planning_horizon = bigT)
print_community_stats(community_stats)

# Save community stats to JSON for Shiny
community_stats_path <- file.path(derived_dir, "community_stats_baseline.json")
jsonlite::write_json(community_stats, community_stats_path, pretty = TRUE, auto_unbox = TRUE)
message("✓ Saved community statistics: ", basename(community_stats_path))


# GENERATE QA REPORT
report_path <- file.path(derived_dir, "retreat_schedule_summary.txt")

sink(report_path)
cat("=================================================\n")
cat("MANAGED SHORES - RETREAT SCHEDULE SUMMARY\n")
cat("Site:", toupper(case_name), "\n")
cat("Site type:", site_type, "\n")
cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("=================================================\n\n")

cat("CLIFF RETREAT PARAMETERS:\n")
cat("  Planned retreat threshold:", CLIFF_THRESHOLD_RETREAT, "m\n")
cat("  Immediate safety threshold:", CLIFF_THRESHOLD_IMMEDIATE, "m\n")
cat("  Already exposed properties:", ifelse(USE_EROSION_RATE_FOR_EXPOSED,
                                            "Use erosion rate to calculate T*", "Force immediate retreat"), "\n\n")

cat("BASELINE PARAMETERS:\n")
cat("  Rental yield:", BASELINE$rental_yield * 100, "%\n")
cat("  Discount rate:", BASELINE$discount_rate * 100, "%\n")
cat("  Damage threshold:", BASELINE$damage_threshold, "m\n")
cat("  Cliff scenario:", BASELINE$cliff_scenario, "\n\n")

cat("BASELINE RESULTS:\n")
cat("  Total properties:", n_distinct(baseline_results$parcel_id), "\n\n")

cat("  Retreat triggers:\n")
for (i in 1:nrow(trigger_summary)) {
  cat("    ", trigger_summary$retreat_trigger[i], ":", trigger_summary$count[i], "\n")
}

if (has_cliff) {
  cat("\n  Cliff exposure status:\n")
  for (i in 1:nrow(exposure_summary)) {
    cat("    ", exposure_summary$cliff_exposure_status[i], ":", exposure_summary$count[i], "\n")
  }
}

cat("\n  Retreat timing:\n")
for (i in 1:nrow(timing_summary)) {
  cat("    ", timing_summary$timing_category[i], ":", timing_summary$count[i], "\n")
}

if (nrow(retreating) > 0) {
  cat("\n  Retreating properties:\n")
  cat("    Count:", nrow(retreating), "\n")
  cat("    Mean year:", round(mean(retreating$retreat_year, na.rm = TRUE), 1), "\n")
  cat("    Median year:", median(retreating$retreat_year, na.rm = TRUE), "\n")
  
  # Add buyout information
  if ("buyout_price" %in% names(baseline_results)) {
    buyout_props <- baseline_results %>% filter(!is.na(buyout_price))
    if (nrow(buyout_props) > 0) {
      cat("\n  Buyout costs:\n")
      cat("    Total buyout: $", format(round(sum(buyout_props$buyout_price) / 1e6, 1), big.mark = ","), "M\n")
      cat("    Total market: $", format(round(sum(buyout_props$PRICE) / 1e6, 1), big.mark = ","), "M\n")
      cat("    Savings: $", format(round((sum(buyout_props$PRICE) - sum(buyout_props$buyout_price)) / 1e6, 1), big.mark = ","), "M (",
          round(100 * (sum(buyout_props$PRICE) - sum(buyout_props$buyout_price)) / sum(buyout_props$PRICE), 1), "% discount)\n")
      cat("    Mean buyout: $", format(round(mean(buyout_props$buyout_price) / 1e3), big.mark = ","), "k\n")
    }
  }
}

cat("\n=================================================\n")
sink()
