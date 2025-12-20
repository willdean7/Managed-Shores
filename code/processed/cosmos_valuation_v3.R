# cosmos_valuation_v3.R
# Purpose: Calculate optimal retreat year T* using flood NPV and cliff retreat distance
# Author: Will Dean (Managed Shores)
#
# Key Features:
#   - Literature-based cliff thresholds (10m minimum safety, 25m planned retreat)
#   - Handles properties already within threshold
#   - Unified script for flood-only, cliff-only, and hybrid sites
#   - Storm scenarios - ONLY affect flood sites, not cliff sites
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

rm(list = ls())
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(stringr)


# CONFIGURATION
case_name <- "pacifica"  # Change for other sites
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
  owner_utility = 0,        # $0/year (conservative baseline)
  cliff_scenario = "LetItGo"  # Natural retreat (vs HoldTheLine for sensitivity)
)

# SENSITIVITY ANALYSIS 
RUN_SENSITIVITY <- TRUE  # Set FALSE for faster baseline-only runs

SENSITIVITY_PARAMS <- list(
  rental_yield = c(0.04, 0.05, 0.06),           # 4%, 5%, 6%
  discount_rate = c(0.015, 0.02, 0.03),         # 1.5%, 2%, 3%
  damage_threshold = c(0.0, 0.15, 0.30),        # 0cm, 15cm, 30cm (for flood sites)
  owner_utility = c(0, 25000, 50000),           # $0, $25K, $50K per year
  cliff_scenario = c("LetItGo", "HoldTheLine")  # Natural retreat vs armoring
)


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
message("  Owner utility: $", format(BASELINE$owner_utility, big.mark = ","), "/year")
message("  Cliff scenario: ", BASELINE$cliff_scenario)

if (RUN_SENSITIVITY) {
  message("\nSENSITIVITY ANALYSIS: ENABLED")
} else {
  message("\nSENSITIVITY ANALYSIS: DISABLED")
}
message("========================================\n")


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
                           rental_yield, discount_rate, damage_threshold, owner_utility, 
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
  
  # ===== FLOOD TRIGGER =====
  # For flood sites: Use average annual conditions (depth_m)
  # NOTE: Storm scenarios (w000 vs w100) removed from baseline
  #       CoSMoS depth_m already represents expected annual flooding
  #       For future Monte Carlo work, storm draws will happen per-year
  
  if (site_type %in% c("FLOOD-ONLY", "HYBRID (flood + cliff)")) {
    
    # Use annual average flood depth (depth_m column)
    # If depth_m_w100 exists, could use for sensitivity analysis
    depth_ts <- prop_hazards$depth_m
    
    if (!all(is.na(depth_ts)) && !all(depth_ts == 0, na.rm = TRUE)) {
      for (t in 1:bigT) {
        npv_r <- npv_rent(t, bigT, rent_val, beta)
        npv_utility <- npv_rent(t, bigT, owner_utility, beta)
        npv_d <- npv_damages(t, bigT, depth_ts, str_val, depth_params, beta, damage_threshold)
        
        npv_staying <- npv_r + npv_utility - npv_d
        if (is.finite(npv_staying) && npv_staying <= 0) {
          t_flood <- t
          break
        }
      }
    }
  }
  
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
run_scenario <- function(rental_yield, discount_rate, damage_threshold, owner_utility, 
                         cliff_scenario, scenario_label = "custom") {
  
  message("\n→ Running: ", scenario_label)
  message("  Parameters: yield=", rental_yield*100, "%, disc=", discount_rate*100, 
          "%, thresh=", damage_threshold, "m, util=$", owner_utility/1000, "K, ",
          "cliff=", cliff_scenario)
  
  slr_scenarios <- unique(hazards$scenario)
  parcel_ids <- unique(hazards$parcel_id)
  
  if (has_cliff_scenario) {
    cliff_scenarios <- c(cliff_scenario)
  } else {
    cliff_scenarios <- c(NA)
  }
  
  results_list <- list()
  
  for (slr_scen in slr_scenarios) {
    for (cliff_scen in cliff_scenarios) {
      
      scenario_results <- map_dfr(
        parcel_ids,
        ~ calc_tstar_one(.x, slr_scen, cliff_scen, hazards, props,
                         rental_yield, discount_rate, damage_threshold, 
                         owner_utility, site_type)
      )
      
      key <- paste(slr_scen, cliff_scen, sep = "_")
      results_list[[key]] <- scenario_results
    }
  }
  
  retreat_schedule <- bind_rows(results_list) %>%
    rename(retreat_year = T_star) %>%
    mutate(
      parameter_set = scenario_label,
      rental_yield = rental_yield,
      discount_rate = discount_rate,
      damage_threshold = damage_threshold,
      owner_utility = owner_utility
    )
  
  # Add property details
  retreat_schedule <- retreat_schedule %>%
    left_join(
      props %>% select(
        parcel_id, ADDRESS, PRICE, strval, landval, 
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
  cliff_scenario = BASELINE$cliff_scenario,
  scenario_label = "baseline"
)

# Note: baseline_results will be saved AFTER buyout calculation

# BASELINE SUMMARY
message("\n========================================")
message("BASELINE RESULTS SUMMARY")
message("========================================\n")

message("Total properties: ", n_distinct(baseline_results$parcel_id))

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
  
  message("\n========================================")
  message("RUNNING SENSITIVITY ANALYSIS")
  message("========================================\n")
  
  param_grid <- expand.grid(
    rental_yield = SENSITIVITY_PARAMS$rental_yield,
    discount_rate = SENSITIVITY_PARAMS$discount_rate,
    damage_threshold = SENSITIVITY_PARAMS$damage_threshold,
    owner_utility = SENSITIVITY_PARAMS$owner_utility,
    cliff_scenario = if (has_cliff_scenario) SENSITIVITY_PARAMS$cliff_scenario else NA,
    stringsAsFactors = FALSE
  )
  
  if (!has_cliff_scenario) {
    param_grid <- param_grid %>% select(-cliff_scenario)
    param_grid$cliff_scenario <- NA
  }
  
  message("Total combinations: ", nrow(param_grid), "\n")
  
  sensitivity_results_list <- list()
  
  for (i in 1:nrow(param_grid)) {
    params <- param_grid[i, ]
    
    label <- sprintf(
      "y%.2f_d%.3f_t%.2f_u%d_%s",
      params$rental_yield,
      params$discount_rate,
      params$damage_threshold,
      params$owner_utility,
      ifelse(is.na(params$cliff_scenario), "NA", params$cliff_scenario)
    )
    
    results <- run_scenario(
      rental_yield = params$rental_yield,
      discount_rate = params$discount_rate,
      damage_threshold = params$damage_threshold,
      owner_utility = params$owner_utility,
      cliff_scenario = params$cliff_scenario,
      scenario_label = label
    )
    
    sensitivity_results_list[[i]] <- results
  }
  
  sensitivity_results <- bind_rows(sensitivity_results_list)
  
  sensitivity_path <- file.path(derived_dir, "retreat_schedule_sensitivity.csv")
  write_csv(sensitivity_results, sensitivity_path)
  message("\n✓ Saved sensitivity: ", basename(sensitivity_path))
  
  # Summary
  sensitivity_summary <- sensitivity_results %>%
    group_by(parameter_set, scenario, cliff_scenario, 
             rental_yield, discount_rate, damage_threshold, 
             owner_utility) %>%
    summarise(
      n_properties = n(),
      mean_retreat_year = mean(retreat_year[retreat_year <= bigT], na.rm = TRUE),
      median_retreat_year = median(retreat_year[retreat_year <= bigT], na.rm = TRUE),
      n_immediate = sum(timing_category == "immediate", na.rm = TRUE),
      n_early = sum(timing_category == "early", na.rm = TRUE),
      n_mid = sum(timing_category == "mid_term", na.rm = TRUE),
      n_late = sum(timing_category == "late", na.rm = TRUE),
      n_no_retreat = sum(timing_category == "no_retreat", na.rm = TRUE),
      .groups = "drop"
    )
  
  summary_path <- file.path(derived_dir, "sensitivity_summary.csv")
  write_csv(sensitivity_summary, summary_path)
  message("✓ Saved summary: ", basename(summary_path))
}

# CALCULATE BUYOUT PRICES
message("\n→ Calculating buyout prices...")

# Function to calculate NPV of rental income
calculate_npv_rent_buyout <- function(rent, T, discount_rate) {
  if (T <= 0) return(0)
  if (T > bigT) return(NA_real_)  # No retreat = no buyout
  
  beta <- 1 / (1 + discount_rate)
  rent * (1 - beta^T) / (1 - beta)
}

# Add buyout columns to baseline results
baseline_results <- baseline_results %>%
  mutate(
    # NPV of rental income through retreat year
    npv_rent = mapply(calculate_npv_rent_buyout, rent, retreat_year, 
                      MoreArgs = list(discount_rate = BASELINE$discount_rate)),
    
    # Uncapped buyout = NPV(rent) + land value
    buyout_uncapped = npv_rent + landval,
    
    # Final buyout = capped at market price
    buyout_price = pmin(buyout_uncapped, PRICE, na.rm = TRUE),
    
    # For non-retreating properties, set buyout to NA
    buyout_price = if_else(retreat_year > bigT, NA_real_, buyout_price),
    buyout_uncapped = if_else(retreat_year > bigT, NA_real_, buyout_uncapped),
    npv_rent = if_else(retreat_year > bigT, NA_real_, npv_rent)
  )

# Buyout summary
retreating_for_buyout <- baseline_results %>% filter(!is.na(buyout_price))
total_buyout <- sum(retreating_for_buyout$buyout_price, na.rm = TRUE)
total_market <- sum(retreating_for_buyout$PRICE, na.rm = TRUE)
buyout_savings <- total_market - total_buyout

message("✓ Buyout prices calculated:")
message("  Total buyout cost: $", format(round(total_buyout / 1e6, 1), big.mark = ","), "M")
message("  Total market value: $", format(round(total_market / 1e6, 1), big.mark = ","), "M")
message("  Savings: $", format(round(buyout_savings / 1e6, 1), big.mark = ","), "M (", 
        round(100 * buyout_savings / total_market, 1), "% discount)")

# Properties capped at market
capped_count <- sum(retreating_for_buyout$buyout_uncapped > retreating_for_buyout$PRICE, na.rm = TRUE)
message("  Properties capped at market: ", capped_count, " / ", nrow(retreating_for_buyout), 
        " (", round(100 * capped_count / nrow(retreating_for_buyout), 1), "%)\n")

# SAVE BASELINE RESULTS (with buyout prices)
baseline_path <- file.path(derived_dir, "retreat_schedule_baseline.csv")
write_csv(baseline_results, baseline_path)
message("✓ Saved baseline with buyout prices: ", basename(baseline_path))


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
cat("  Owner utility: $", format(BASELINE$owner_utility, big.mark = ","), "/year\n")
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

