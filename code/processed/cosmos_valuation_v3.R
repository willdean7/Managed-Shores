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
#     Buyout Price = NPV(rental income from year 0 to T*)
#     - Compensates owner for lost rental income stream
#     - Rental income already reflects both structure and land value
#     - At T*, land has no residual value (flooded or cliff failure imminent)
#     - Government leases property back to owner, receives rental income
#     - Eventually demolishes structure; land has no further economic value
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
#   - Single cliff threshold: retreat triggered at 10m minimum setback
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
#   - data/{site}/derived/mc_damage_distributions_baseline.csv (if Monte Carlo enabled)
#     Format: parcel_id, scenario, sim_num, year, annual_damage
#   - data/{site}/derived/community_stats_baseline.json

rm(list = ls())
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(stringr)
library(jsonlite)

source("code/processed/monte_carlo_storms.R") 

# CONFIGURATION
case_name <- "stinson"  # Change for other sites
data_dir <- file.path("data", case_name)
derived_dir <- file.path(data_dir, "derived")

# Planning horizon
bigT <- 74    # Years (matches OPC 2024 projections, shifted to start from 2026 through 2100)

# Depth-damage function parameters (fitted logistic curve)
depth_params <- c(a = 0.4, b = 1, d = 2)

# CLIFF RETREAT PARAMETERS (Literature-Based)
# Citations: CCC guidance, Lindstrom v. CCC (2019), Martin v. CCC (2021)
CLIFF_THRESHOLD <- 10  # Single safety threshold (10m = ~33ft minimum setback)
# Retreat is triggered when projected cliff distance drops below 10m

# BASELINE PARAMETERS
BASELINE <- list(
  rental_yield = 0.05 / (1 + 0.05),      # 5% annual yield (industry benchmark)
  discount_rate = 0.05,     # 5% market/rental yield rate (homeowner perspective for T*)
  discount_rate_gov = 0.02, # 2% government discount rate (for leaseback/repairs)
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

# SIMPLIFIED SENSITIVITY DESIGN:
# Only test government discount rate (δ_g) for fiscal analysis
# T* is calculated with fixed market rate (δ = 5%)
# Government economics vary with δ_g: 2%, 3%, 4%

# Create simplified parameter grid
create_smart_sensitivity_grid <- function() {
  # Only vary government discount rate for leaseback/repair NPV calculations
  # Note: discount_rate here refers to GOVERNMENT rate for sensitivity
  # T* calculation always uses market rate (0.05) in monte_carlo_storms.R
  tibble(
    rental_yield = 0.05/(1+0.05),           # Fixed
    discount_rate = 0.05,          # Fixed (market rate for T*)
    discount_rate_gov = c(0.02, 0.03, 0.04),  # Variable (government financing cost)
    damage_threshold = 0.15        # Fixed
  )
}

SENSITIVITY_GRID <- create_smart_sensitivity_grid()

message("\n=== SIMPLIFIED SENSITIVITY DESIGN ===")
message("Total combinations: ", nrow(SENSITIVITY_GRID))
message("\nFixed parameters:")
message("  Market discount rate (T* calc): 5%")
message("  Rental yield: 5%")
message("  Damage threshold: 0.15m")
message("\nSensitivity dimension:")
message("  Government discount rates (δ_g): 2%, 3%, 4%")
message("  - Affects: Leaseback revenue, Repair costs")
message("  - Does NOT affect: T* calculation (uses fixed 5%)")
message("  Total: ", nrow(SENSITIVITY_GRID), " combinations")
message("\nEstimated runtime:")
message("  No cliffs: ~", round(nrow(SENSITIVITY_GRID) * 3 / 60, 1), " hours")
message("  Pacifica/Isla Vista (with cliffs): ~", round(nrow(SENSITIVITY_GRID) * 2 * 3 / 60, 1), " hours")


message("CoSMoS Retreat Year Calculation: ", toupper(case_name))
message("Planning horizon: ", bigT, " years")
message("\nCLIFF RETREAT PARAMETERS:")
message("  Cliff threshold: ", CLIFF_THRESHOLD, "m (single safety threshold)")
message("  Retreat triggered when projected cliff distance < ", CLIFF_THRESHOLD, "m")
message("\nBASELINE PARAMETERS:")
message("  Rental yield: ", BASELINE$rental_yield * 100, "%")
message("  Market discount rate (T* calc): ", BASELINE$discount_rate * 100, "%")
message("  Government discount rate (LB/Repairs): ", BASELINE$discount_rate_gov * 100, "%")
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
  
  # Pre-compute NPV components
  beta_powers <- beta^(1:bigT)
  
  # NPV of rent from each starting year
  npv_r_vec <- numeric(bigT)
  for (t in 1:bigT) {
    years_remaining <- bigT - t + 1
    npv_r_vec[t] <- rent_val * sum(beta_powers[1:years_remaining])
  }
  
  # NPV of damages from each starting year (for flood sites only)
  if (site_type %in% c("FLOOD-ONLY", "HYBRID (flood + cliff)")) {
    
    # Get flood depth timeline
    if ("depth_m" %in% names(prop_hazards)) {
      depth_vec <- prop_hazards$depth_m
      depth_vec[is.na(depth_vec)] <- 0
      depth_vec[depth_vec < damage_threshold] <- 0
      
      # Calculate annual damages
      a <- depth_params["a"]
      b <- depth_params["b"]
      d <- depth_params["d"]
      damage_fraction <- a / (1 + exp(-b * (depth_vec - d)))
      damage_fraction <- pmax(damage_fraction, 0)
      annual_damages <- damage_fraction * str_val
      
      # NPV of damages from each starting year
      npv_d_vec <- numeric(bigT)
      for (t in 1:bigT) {
        if (t <= length(annual_damages)) {
          years_remaining <- min(bigT - t + 1, length(annual_damages) - t + 1)
          if (years_remaining > 0) {
            npv_d_vec[t] <- sum(annual_damages[t:min(length(annual_damages), bigT)] * 
                                  beta_powers[1:years_remaining])
          }
        }
      }
    } else {
      npv_d_vec <- rep(0, bigT)
    }
    
  } else {
    # Cliff-only site: no flood damages
    npv_d_vec <- rep(0, bigT)
  }
  
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
      
      # Use BASELINE (current) cliff distance for initial exposure
      if ("baseline_cliff_dist_m" %in% names(prop_hazards)) {
        initial_cliff_dist <- prop_hazards$baseline_cliff_dist_m[1]  # Current (2026) distance
      } else {
        # Fallback for old data without baseline
        initial_cliff_dist <- cliff_dist_ts[1]  # Year 1 distance
        warning("No baseline_cliff_dist_m found, using cliff_dist_m[1] as fallback")
      }
      
      if (!all(is.na(cliff_dist_ts))) {
        
        # Determine current exposure status using single 10m threshold
        if (initial_cliff_dist <= CLIFF_THRESHOLD) {
          cliff_exposure_status <- "immediate_hazard"
        } else {
          cliff_exposure_status <- "safe"
        }
        
        # Calculate retreat year: triggered when cliff distance reaches or drops below 10m
        years_retreat <- which(cliff_dist_ts <= CLIFF_THRESHOLD)
        if (length(years_retreat) > 0) {
          t_cliff <- min(years_retreat)
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
    } else {
      "cliff_10m_threshold"
    }
  } else {
    "unknown"
  }
  
  # Timing category
  timing <- if (t_star > bigT) {
    "beyond_horizon"
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
    T_star = if (t_star > bigT) NA_real_ else t_star,
    npv_rent_annual = list(npv_r_vec),
    npv_damages_annual = list(npv_d_vec),
    npv_net_annual = list(npv_r_vec - npv_d_vec),
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
  message("  Parameters: yield=", rental_yield*100, "%, δ_market=", discount_rate*100, 
          "%, thresh=", damage_threshold, "m, cliff=", cliff_scenario)
  message("  (δ_gov will be applied in buyout economics calculation)")
  
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

calculate_buyout_economics <- function(df, discount_rate_gov = NULL) {
  
  # If discount_rate_gov not provided, try to get from dataframe
  # Otherwise use baseline
  if (is.null(discount_rate_gov)) {
    if ("discount_rate_gov" %in% names(df)) {
      # Will use row-specific values
      use_row_specific <- TRUE
      message("  Using row-specific government discount rates from dataframe")
    } else {
      discount_rate_gov <- BASELINE$discount_rate_gov
      use_row_specific <- FALSE
      message("  Using default government discount rate: ", discount_rate_gov * 100, "%")
    }
  } else {
    use_row_specific <- FALSE
    message("  Using provided government discount rate: ", discount_rate_gov * 100, "%")
  }
  
  message("  Calculating buyout economics for ", nrow(df), " rows...")
  message("    Market discount rate (for BOP): ", df$discount_rate[1] * 100, "%")
  
  df %>%
    mutate(
      # ===== BUYOUT PRICE (BOP) =====
      # Uses MARKET discount rate (δ = 5%)
      # BOP = Σ(t=0 to T*) [rent / (1+δ)^t]
      # Formula: rent × [(1+δ)^T* - 1] / [δ(1+δ)^T*]
      buyout_price = mapply(
        function(rent, T, disc_market) {
          if (is.na(T)) return(NA_real_)
          if (T <= 0) return(0)
          if (T > bigT) return(NA_real_)
          
          # Beginning-of-period: sum from t=0 to t=T (T+1 terms)
          # Rent payments start immediately upon buyout
          beta <- 1 / (1 + disc_market)
          rent * (1 - beta^(T+1)) / (1 - beta)
        },
        rent, retreat_year, discount_rate # Uses market rate (5%)
      ),
      
      # ===== GOVERNMENT LEASEBACK REVENUE (LB) =====
      # Uses GOVERNMENT discount rate (δ_g = 2%, 3%, or 4%)
      # LB = Σ(t=0 to T*) [rent / (1+δ_g)^t]
      leaseback_revenue = if (use_row_specific) {
        mapply(
          function(rent, T, disc_gov_row) {
            if (is.na(T)) return(NA_real_)
            if (T <= 0) return(0)
            if (T > bigT) return(NA_real_)
            
            beta_gov <- 1 / (1 + disc_gov_row)
            rent * (1 - beta_gov^(T+1)) / (1 - beta_gov)
          },
          rent, retreat_year, discount_rate_gov
        )
      } else {
        mapply(
          function(rent, T) {
            if (is.na(T)) return(NA_real_)
            if (T <= 0) return(0)
            if (T > bigT) return(NA_real_)
            
            beta_gov <- 1 / (1 + discount_rate_gov)
            rent * (1 - beta_gov^(T+1)) / (1 - beta_gov)
          },
          rent, retreat_year
        )
      },
      
      # ===== GOVERNMENT REPAIR COSTS (NPV_Repairs) =====
      # Uses GOVERNMENT discount rate (δ_g = 2%, 3%, or 4%)
      # NPV_Repairs = Σ(t=0 to T*) [D_k × SV / (1+δ_g)^t]
      # Note: damages are in list column format (flood sites only)
      # Cliff-only sites (no mean_annual_damages column) get npv_repairs = 0
      npv_repairs = if (!"mean_annual_damages" %in% names(df)) {
        # Cliff-only site: no flood repair costs
        rep(0, nrow(df))
      } else if (use_row_specific) {
        mapply(
          function(damages_list, T, disc_gov_row) {
            if (is.na(T)) return(NA_real_)
            if (T <= 0) return(0)
            if (T > bigT) return(NA_real_)
            
            # Extract damages vector from list column
            if (is.null(damages_list) || length(damages_list) == 0) return(NA_real_)
            damages_vec <- unlist(damages_list)
            
            # Only sum damages from year 1 to T*
            damages_to_tstar <- damages_vec[1:min(T, length(damages_vec))]
            
            # Calculate NPV with government discount rate
            beta_gov <- 1 / (1 + disc_gov_row)
            beta_powers_gov <- beta_gov^(1:length(damages_to_tstar))
            
            sum(damages_to_tstar * beta_powers_gov)
          },
          mean_annual_damages, retreat_year, discount_rate_gov
        )
      } else {
        mapply(
          function(damages_list, T) {
            if (is.na(T)) return(NA_real_)
            if (T <= 0) return(0)
            if (T > bigT) return(NA_real_)
            
            # Extract damages vector from list column
            if (is.null(damages_list) || length(damages_list) == 0) return(NA_real_)
            damages_vec <- unlist(damages_list)
            
            # Only sum damages from year 1 to T*
            damages_to_tstar <- damages_vec[1:min(T, length(damages_vec))]
            
            # Calculate NPV with government discount rate
            beta_gov <- 1 / (1 + discount_rate_gov)
            beta_powers_gov <- beta_gov^(1:length(damages_to_tstar))
            
            sum(damages_to_tstar * beta_powers_gov)
          },
          mean_annual_damages, retreat_year
        )
      },
      
      # ===== GOVERNMENT NET OUTCOME =====
      # Net Cost = BOP + NPV_Repairs - LB
      gov_net_cost = buyout_price + npv_repairs - leaseback_revenue,
      
      # Government outcome category
      gov_outcome = case_when(
        is.na(gov_net_cost) ~ NA_character_,
        gov_net_cost < 0 ~ "profit",  # Revenue > Costs
        gov_net_cost <= PRICE * 0.05 ~ "low_cost",  # < 5% of property value
        gov_net_cost <= PRICE * 0.25 ~ "moderate_cost",  # 5-25% of property value
        TRUE ~ "high_cost"  # > 25% of property value
      ),
      
      # ===== COMPARISON METRICS =====
      # Track if buyout price would exceed market value (unlikely with new formula)
      buyout_capped = if_else(
        !is.na(retreat_year) & retreat_year <= bigT,
        buyout_price > PRICE,
        NA
      ),
      
      buyout_savings_pct = if_else(
        !is.na(retreat_year) & retreat_year <= bigT,
        100 * (1 - buyout_price / PRICE),
        NA_real_
      ),
      
      buyout_savings_dollars = if_else(
        !is.na(retreat_year) & retreat_year <= bigT,
        PRICE - buyout_price,
        NA_real_
      ),
      
      # Leaseback advantage (LB uses lower discount rate than BOP)
      leaseback_advantage = leaseback_revenue - buyout_price,
      leaseback_advantage_pct = 100 * (leaseback_revenue / buyout_price - 1)
    )
}

# More error sensitivity calculations (e.g., T* off by ±5 years)
# ==============================================================================
# TIMING SENSITIVITY ANALYSIS
# ==============================================================================
# Calculate impacts if optimal retreat year (T*) is mis-estimated
# Shows how costs shift between public and private parties

calculate_timing_sensitivity <- function(df, planning_horizon = bigT) {
  
  message("\n→ Calculating timing sensitivity analysis...")
  message("  (What happens if T* is off by ±5 or ±10 years?)")
  
  # Only analyze properties that retreat within planning horizon
  retreating <- df %>% 
    filter(!is.na(retreat_year), retreat_year <= planning_horizon)
  
  if (nrow(retreating) == 0) {
    message("  No properties retreating - skipping analysis")
    return(NULL)
  }
  
  # Helper function to calculate NPV of rent stream
  calc_npv_rent <- function(rent, years, disc_rate) {
    if (years <= 0) return(0)
    if (years > planning_horizon) years <- planning_horizon
    beta <- 1 / (1 + disc_rate)
    rent * (1 - beta^years) / (1 - beta)
  }
  
  # For each property, calculate costs under different timing scenarios
  sensitivity_results <- retreating %>%
    mutate(
      # Baseline (T* is correct)
      baseline_buyout = buyout_price,
      baseline_leaseback = leaseback_revenue,
      baseline_repairs = npv_repairs,
      baseline_gov_cost = gov_net_cost,
      baseline_owner_loss = PRICE - buyout_price,
      
      # Scenario 1: Retreat 5 years too early
      t_minus5 = pmax(retreat_year - 5, 1),
      buyout_minus5 = mapply(calc_npv_rent, rent, t_minus5, discount_rate),
      # Note: For simplified analysis, assume same leaseback/repairs ratio
      # In reality would need to recalculate with actual damage data
      leaseback_minus5 = buyout_minus5 * (baseline_leaseback / baseline_buyout),
      repairs_minus5 = baseline_repairs * (t_minus5 / retreat_year),  # Proportional
      gov_cost_minus5 = buyout_minus5 + repairs_minus5 - leaseback_minus5,
      owner_loss_minus5 = PRICE - buyout_minus5,
      
      # Scenario 2: Retreat 5 years too late
      t_plus5 = pmin(retreat_year + 5, planning_horizon),
      buyout_plus5 = mapply(calc_npv_rent, rent, t_plus5, discount_rate),
      leaseback_plus5 = buyout_plus5 * (baseline_leaseback / baseline_buyout),
      repairs_plus5 = baseline_repairs * (t_plus5 / retreat_year),  # Proportional
      gov_cost_plus5 = buyout_plus5 + repairs_plus5 - leaseback_plus5,
      owner_loss_plus5 = PRICE - buyout_plus5,
      
      # Scenario 3: Retreat 10 years too early
      t_minus10 = pmax(retreat_year - 10, 1),
      buyout_minus10 = mapply(calc_npv_rent, rent, t_minus10, discount_rate),
      leaseback_minus10 = buyout_minus10 * (baseline_leaseback / baseline_buyout),
      repairs_minus10 = baseline_repairs * (t_minus10 / retreat_year),
      gov_cost_minus10 = buyout_minus10 + repairs_minus10 - leaseback_minus10,
      owner_loss_minus10 = PRICE - buyout_minus10,
      
      # Scenario 4: Retreat 10 years too late  
      t_plus10 = pmin(retreat_year + 10, planning_horizon),
      buyout_plus10 = mapply(calc_npv_rent, rent, t_plus10, discount_rate),
      leaseback_plus10 = buyout_plus10 * (baseline_leaseback / baseline_buyout),
      repairs_plus10 = baseline_repairs * (t_plus10 / retreat_year),
      gov_cost_plus10 = buyout_plus10 + repairs_plus10 - leaseback_plus10,
      owner_loss_plus10 = PRICE - buyout_plus10,
      
      # Calculate deltas from baseline
      gov_cost_delta_minus5 = gov_cost_minus5 - baseline_gov_cost,
      gov_cost_delta_plus5 = gov_cost_plus5 - baseline_gov_cost,
      gov_cost_delta_minus10 = gov_cost_minus10 - baseline_gov_cost,
      gov_cost_delta_plus10 = gov_cost_plus10 - baseline_gov_cost,
      
      owner_loss_delta_minus5 = owner_loss_minus5 - baseline_owner_loss,
      owner_loss_delta_plus5 = owner_loss_plus5 - baseline_owner_loss,
      owner_loss_delta_minus10 = owner_loss_minus10 - baseline_owner_loss,
      owner_loss_delta_plus10 = owner_loss_plus10 - baseline_owner_loss
    )
  
  # Summary statistics
  summary_stats <- sensitivity_results %>%
    summarise(
      n_properties = n(),
      
      # Government costs
      mean_gov_cost_baseline = mean(baseline_gov_cost, na.rm = TRUE),
      mean_gov_cost_minus5 = mean(gov_cost_minus5, na.rm = TRUE),
      mean_gov_cost_plus5 = mean(gov_cost_plus5, na.rm = TRUE),
      mean_gov_cost_minus10 = mean(gov_cost_minus10, na.rm = TRUE),
      mean_gov_cost_plus10 = mean(gov_cost_plus10, na.rm = TRUE),
      
      # Owner losses
      mean_owner_loss_baseline = mean(baseline_owner_loss, na.rm = TRUE),
      mean_owner_loss_minus5 = mean(owner_loss_minus5, na.rm = TRUE),
      mean_owner_loss_plus5 = mean(owner_loss_plus5, na.rm = TRUE),
      mean_owner_loss_minus10 = mean(owner_loss_minus10, na.rm = TRUE),
      mean_owner_loss_plus10 = mean(owner_loss_plus10, na.rm = TRUE),
      
      # Total community costs
      total_gov_delta_minus5 = sum(gov_cost_delta_minus5, na.rm = TRUE),
      total_gov_delta_plus5 = sum(gov_cost_delta_plus5, na.rm = TRUE),
      total_gov_delta_minus10 = sum(gov_cost_delta_minus10, na.rm = TRUE),
      total_gov_delta_plus10 = sum(gov_cost_delta_plus10, na.rm = TRUE),
      
      total_owner_delta_minus5 = sum(owner_loss_delta_minus5, na.rm = TRUE),
      total_owner_delta_plus5 = sum(owner_loss_delta_plus5, na.rm = TRUE),
      total_owner_delta_minus10 = sum(owner_loss_delta_minus10, na.rm = TRUE),
      total_owner_delta_plus10 = sum(owner_loss_delta_plus10, na.rm = TRUE)
    )
  
  message("✓ Timing sensitivity calculated for ", nrow(sensitivity_results), " properties")
  
  return(list(
    property_level = sensitivity_results,
    summary = summary_stats
  ))
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
  #print_mc_summary(baseline_results)
  message("✓ Monte Carlo completed for ", sum(baseline_results$mc_enabled, na.rm = TRUE), " properties")
}

# Trigger summary
if ("retreat_trigger" %in% names(baseline_results)) {
  trigger_summary <- baseline_results %>%
    group_by(retreat_trigger) %>%
    summarise(count = n(), .groups = "drop") %>%
    arrange(desc(count))
  
  message("\nRetreat triggers:")
  for (i in 1:nrow(trigger_summary)) {
    message("  ", trigger_summary$retreat_trigger[i], ": ", trigger_summary$count[i])
  }
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
retreating <- baseline_results %>% filter(!is.na(retreat_year) & retreat_year <= bigT)

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
      "y%.2f_d%.3f_dg%.3f_t%.2f_%s",
      params$rental_yield,
      params$discount_rate,
      params$discount_rate_gov,  # Add to label
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
    
    # Add discount_rate_gov to results
    output$results <- output$results %>%
      mutate(discount_rate_gov = params$discount_rate_gov)
    
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
  
  # Save as RDS (preserves list columns)
  sensitivity_rds_path <- file.path(derived_dir, "retreat_schedule_sensitivity.rds")
  saveRDS(sensitivity_results, sensitivity_rds_path)
  message("✓ Saved sensitivity as RDS: ", basename(sensitivity_rds_path))
  
  # Flatten list columns for CSV
  sensitivity_for_csv <- sensitivity_results %>%
    mutate(
      mean_annual_damages_str = if ("mean_annual_damages" %in% names(.)) {
        sapply(mean_annual_damages, function(x) {
          if (is.null(x) || length(x) == 0) return("")
          paste(round(x, 2), collapse = ",")
        })
      } else {
        NA_character_
      },
      sd_annual_damages_str = if ("sd_annual_damages" %in% names(.)) {
        sapply(sd_annual_damages, function(x) {
          if (is.null(x) || length(x) == 0) return("")
          paste(round(x, 2), collapse = ",")
        })
      } else {
        NA_character_
      },
      q05_annual_damages_str = if ("q05_annual_damages" %in% names(.)) {
        sapply(q05_annual_damages, function(x) {
          if (is.null(x) || length(x) == 0) return("")
          paste(round(x, 2), collapse = ",")
        })
      } else {
        NA_character_
      },
      q95_annual_damages_str = if ("q95_annual_damages" %in% names(.)) {
        sapply(q95_annual_damages, function(x) {
          if (is.null(x) || length(x) == 0) return("")
          paste(round(x, 2), collapse = ",")
        })
      } else {
        NA_character_
      }
    ) %>%
    select(-any_of(c("mean_annual_damages", "sd_annual_damages", 
                     "q05_annual_damages", "q95_annual_damages")))
  
  sensitivity_path <- file.path(derived_dir, "retreat_schedule_sensitivity.csv")
  write_csv(sensitivity_for_csv, sensitivity_path)
  message("✓ Saved sensitivity as CSV: ", basename(sensitivity_path))
  message("  Rows: ", format(nrow(sensitivity_results), big.mark = ","))
  message("  Includes: retreat timing + buyout economics + government economics")
  
  # Summary
  sensitivity_summary <- sensitivity_results %>%
    group_by(scenario, cliff_scenario, 
             rental_yield, discount_rate, discount_rate_gov, damage_threshold) %>%
    summarise(
      n_properties = n(),
      mean_retreat_year = mean(retreat_year[retreat_year <= bigT], na.rm = TRUE),
      median_retreat_year = median(retreat_year[retreat_year <= bigT], na.rm = TRUE),
      n_immediate = sum(timing_category == "immediate", na.rm = TRUE),
      n_early = sum(timing_category == "early", na.rm = TRUE),
      n_mid = sum(timing_category == "mid_term", na.rm = TRUE),
      n_late = sum(timing_category == "late", na.rm = TRUE),
      n_beyond_horizon = sum(timing_category == "beyond_horizon", na.rm = TRUE),
      # Add buyout economics summaries
      total_buyout_M = sum(buyout_price, na.rm = TRUE) / 1e6,
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
baseline_results <- calculate_buyout_economics(baseline_results, 
                                               discount_rate_gov = BASELINE$discount_rate_gov)

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

timing_sensitivity <- calculate_timing_sensitivity(baseline_results)

if (!is.null(timing_sensitivity)) {
  # Save property-level results
  timing_path <- file.path(derived_dir, "timing_sensitivity_baseline.csv")
  write_csv(timing_sensitivity$property_level, timing_path)
  message("✓ Saved timing sensitivity: ", basename(timing_path))
  
  # Save summary
  timing_summary_path <- file.path(derived_dir, "timing_sensitivity_summary.csv")
  write_csv(timing_sensitivity$summary, timing_summary_path)
  message("✓ Saved timing summary: ", basename(timing_summary_path))
}


# SAVE BASELINE RESULTS (with buyout prices)

# Save full data as RDS (preserves list columns)
rds_path <- file.path(derived_dir, "retreat_schedule_baseline.rds")
saveRDS(baseline_results, rds_path)
message("✓ Saved baseline as RDS (with list columns): ", basename(rds_path))

# Flatten list columns to strings for CSV export
baseline_for_csv <- baseline_results %>%
  mutate(
    # Format mean annual damages (in dollars) if MC was used
    mean_annual_damages_str = if ("mean_annual_damages" %in% names(.)) {
      sapply(mean_annual_damages, function(x) {
        if (is.null(x) || length(x) == 0) return("")
        paste(round(x, 2), collapse = ",")
      })
    } else {
      NA_character_
    },
    # Format damage uncertainty bounds if available
    sd_annual_damages_str = if ("sd_annual_damages" %in% names(.)) {
      sapply(sd_annual_damages, function(x) {
        if (is.null(x) || length(x) == 0) return("")
        paste(round(x, 2), collapse = ",")
      })
    } else {
      NA_character_
    },
    q05_annual_damages_str = if ("q05_annual_damages" %in% names(.)) {
      sapply(q05_annual_damages, function(x) {
        if (is.null(x) || length(x) == 0) return("")
        paste(round(x, 2), collapse = ",")
      })
    } else {
      NA_character_
    },
    q95_annual_damages_str = if ("q95_annual_damages" %in% names(.)) {
      sapply(q95_annual_damages, function(x) {
        if (is.null(x) || length(x) == 0) return("")
        paste(round(x, 2), collapse = ",")
      })
    } else {
      NA_character_
    }
  ) %>%
  # Remove list columns (keep string versions)
  select(-any_of(c("mean_annual_damages", "sd_annual_damages", 
                   "q05_annual_damages", "q95_annual_damages")))

baseline_path <- file.path(derived_dir, "retreat_schedule_baseline.csv")
write_csv(baseline_for_csv, baseline_path)
message("✓ Saved baseline as CSV (damages as comma-separated): ", basename(baseline_path))
message("  → For visualization in R: use readRDS('", basename(rds_path), "')")
message("  → Annual damages in mean_annual_damages_str (dollars per year)")
message("  → Rent is constant in 'rent' column")

# Save MC damage distributions (detailed annual damages per simulation)
if (RUN_MONTE_CARLO && nrow(baseline_results) > 0) {
  
  # Filter to MC results only (those with damage_matrix attributes)
  mc_results <- baseline_results %>% filter(mc_enabled == TRUE)
  
  if (nrow(mc_results) > 0) {
    mc_dist_path <- file.path(derived_dir, "mc_damage_distributions_baseline.csv")
    mc_damage_dist <- save_mc_damage_distributions(mc_results, mc_dist_path)
    
    if (!is.null(mc_damage_dist)) {
      message("  Properties: ", n_distinct(mc_damage_dist$parcel_id))
      message("  Simulations per property: ", N_MC_SIMS)
      message("  Years per simulation: ", max(mc_damage_dist$year))
    }
  }
}

# Save MC T* distributions (retreat year per simulation) for Shiny histogram
if (RUN_MONTE_CARLO && nrow(baseline_results) > 0) {
  
  mc_results <- baseline_results %>% filter(mc_enabled == TRUE)
  
  if (nrow(mc_results) > 0 && "tstar_distribution" %in% names(mc_results)) {
    
    message("\n→ Saving MC T* distributions for Shiny visualization...")
    
    # Extract T* distribution for each property
    tstar_dist_list <- list()
    
    for (i in 1:nrow(mc_results)) {
      row <- mc_results[i, ]
      
      if (!is.null(row$tstar_distribution[[1]])) {
        tstar_vec <- row$tstar_distribution[[1]]
        
        tstar_dist_list[[i]] <- tibble(
          parcel_id = row$parcel_id,
          scenario = row$scenario,
          cliff_scenario = row$cliff_scenario,
          sim_num = 1:length(tstar_vec),
          retreat_year = tstar_vec
        )
      }
    }
    
    if (length(tstar_dist_list) > 0) {
      tstar_dist_df <- bind_rows(tstar_dist_list)
      
      tstar_dist_path <- file.path(derived_dir, "mc_tstar_distributions_baseline.csv")
      write_csv(tstar_dist_df, tstar_dist_path)
      
      message("✓ Saved MC T* distributions: ", basename(tstar_dist_path))
      message("  Properties: ", n_distinct(tstar_dist_df$parcel_id))
      message("  Simulations per property: ", N_MC_SIMS)
      message("  Total rows: ", nrow(tstar_dist_df))
    }
  }
}

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
cat("  Cliff threshold:", CLIFF_THRESHOLD, "m (single safety threshold)\n\n")

cat("BASELINE PARAMETERS:\n")
cat("  Rental yield:", BASELINE$rental_yield * 100, "%\n")
cat("  Discount rate:", BASELINE$discount_rate * 100, "%\n")
cat("  Damage threshold:", BASELINE$damage_threshold, "m\n")
cat("  Cliff scenario:", BASELINE$cliff_scenario, "\n\n")

cat("BASELINE RESULTS:\n")
cat("  Total properties:", n_distinct(baseline_results$parcel_id), "\n\n")

if ("retreat_trigger" %in% names(baseline_results)) {
  cat("  Retreat triggers:\n")
  for (i in 1:nrow(trigger_summary)) {
    cat("    ", trigger_summary$retreat_trigger[i], ":", trigger_summary$count[i], "\n")
  }
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
sink()