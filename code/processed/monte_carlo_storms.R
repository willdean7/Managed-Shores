# monte_carlo_storms.R
# Monte Carlo Storm Simulation Module
# Author: Will Dean
#

library(dplyr)
library(tidyr)


# STORM SAMPLING FUNCTIONS

#' Pre-create interpolation functions for one property
#'
#' Instead of filtering storm data 24,000 times, filter ONCE and create
#' three interpolation functions that can be called quickly
#'
#' @param storm_data_list List with $annual, $storm_20yr, $storm_100yr
#' @param parcel_id Property identifier
#' @return List with three interpolation functions
create_interpolation_functions <- function(storm_data_list, parcel_id) {
  
  # Filter each storm dataset ONCE for this property
  annual_data <- storm_data_list$annual %>% 
    filter(parcel_id == !!parcel_id) %>%
    filter(!is.na(slr_m), !is.na(depth_m)) %>%
    arrange(slr_m) %>%
    distinct(slr_m, .keep_all = TRUE)
  
  storm20_data <- storm_data_list$storm_20yr %>%
    filter(parcel_id == !!parcel_id) %>%
    filter(!is.na(slr_m), !is.na(depth_m)) %>%
    arrange(slr_m) %>%
    distinct(slr_m, .keep_all = TRUE)
  
  storm100_data <- storm_data_list$storm_100yr %>%
    filter(parcel_id == !!parcel_id) %>%
    filter(!is.na(slr_m), !is.na(depth_m)) %>%
    arrange(slr_m) %>%
    distinct(slr_m, .keep_all = TRUE)
  
  # Create interpolation functions (or constant functions if no data)
  if (nrow(annual_data) >= 2) {
    interp_annual <- approxfun(annual_data$slr_m, annual_data$depth_m, rule = 2)
  } else {
    interp_annual <- function(x) rep(0, length(x))
  }
  
  if (nrow(storm20_data) >= 2) {
    interp_20yr <- approxfun(storm20_data$slr_m, storm20_data$depth_m, rule = 2)
  } else {
    interp_20yr <- function(x) rep(0, length(x))
  }
  
  if (nrow(storm100_data) >= 2) {
    interp_100yr <- approxfun(storm100_data$slr_m, storm100_data$depth_m, rule = 2)
  } else {
    interp_100yr <- function(x) rep(0, length(x))
  }
  
  return(list(
    annual = interp_annual,
    storm_20yr = interp_20yr,
    storm_100yr = interp_100yr
  ))
}


#' Generate complete storm depth timeline for one Monte Carlo simulation (OPTIMIZED)
#' 
#' Uses pre-created interpolation functions for massive speedup
#' 
#' @param prop_hazards Data frame with year, slr_m columns (one row per year)
#' @param interp_funcs List with three interpolation functions
#' @return Numeric vector of depths (one per year, length = nrow(prop_hazards))
generate_storm_timeline_fast <- function(prop_hazards, interp_funcs) {
  
  n_years <- nrow(prop_hazards)
  slr_vec <- prop_hazards$slr_m
  
  # Get depths for all years from each storm type (VECTORIZED!)
  annual_depths <- interp_funcs$annual(slr_vec)
  storm20_depths <- interp_funcs$storm_20yr(slr_vec)
  storm100_depths <- interp_funcs$storm_100yr(slr_vec)
  
  # Random draws for all years at once (VECTORIZED!)
  rand_vec <- runif(n_years)
  
  # Initialize with annual storm (94% probability)
  depths <- annual_depths
  
  # Replace with 20-year storm where appropriate (5% probability)
  storm20_mask <- rand_vec >= 0.01 & rand_vec < 0.06
  depths[storm20_mask] <- storm20_depths[storm20_mask]
  
  # Replace with 100-year storm where appropriate (1% probability)
  storm100_mask <- rand_vec < 0.01
  depths[storm100_mask] <- storm100_depths[storm100_mask]
  
  return(depths)
}



# MONTE CARLO RETREAT YEAR CALCULATION

#' Calculate retreat year with Monte Carlo storm simulation
#' 
#' FULLY OPTIMIZED VERSION - Uses pre-filtering and vectorization throughout
#' 
#' @param prop_id Property identifier
#' @param scenario_name SLR scenario (Intermediate, Intermediate_High, High)
#' @param cliff_scen_name Cliff management scenario (for hybrid sites)
#' @param hazards_df Annual hazards data frame (from interpolate_cosmos.R)
#' @param props_df Property data frame
#' @param storm_data_list List with storm CSVs: $annual, $storm_20yr, $storm_100yr
#' @param n_sims Number of Monte Carlo simulations (default 10000)
#' @param rental_yield Annual rental yield (fraction of property value)
#' @param discount_rate Social discount rate
#' @param damage_threshold Flood depth threshold (meters)
#' @param site_type "FLOOD-ONLY", "CLIFF-ONLY", or "HYBRID (flood + cliff)"
#' @return Tibble with retreat year statistics
calc_tstar_monte_carlo <- function(prop_id, scenario_name, cliff_scen_name, 
                                   hazards_df, props_df, storm_data_list,
                                   n_sims = 10000,
                                   rental_yield, discount_rate, damage_threshold, 
                                   site_type,
                                   bigT, depth_params) {
  
  # ===== GET PROPERTY DATA =====
  
  # Get hazard timeline
  has_cliff_scenario <- "cliff_scenario" %in% names(hazards_df)
  
  if (is.na(cliff_scen_name) || !has_cliff_scenario) {
    prop_hazards <- hazards_df %>%
      filter(parcel_id == prop_id, scenario == scenario_name) %>%
      arrange(year)
  } else {
    prop_hazards <- hazards_df %>%
      filter(parcel_id == prop_id, scenario == scenario_name, 
             cliff_scenario == cliff_scen_name) %>%
      arrange(year)
  }
  
  prop <- props_df %>% filter(parcel_id == prop_id)
  
  # Return NA if no data
  if (nrow(prop_hazards) == 0 || nrow(prop) == 0) {
    return(tibble(
      parcel_id = prop_id,
      scenario = scenario_name,
      cliff_scenario = cliff_scen_name,
      retreat_year = NA_real_,
      mc_enabled = FALSE,
      mc_n_sims = 0,
      mc_mean_year = NA_real_,
      mc_median_year = NA_real_,
      mc_sd_year = NA_real_,
      mc_q05_year = NA_real_,
      mc_q25_year = NA_real_,
      mc_q75_year = NA_real_,
      mc_q95_year = NA_real_,
      mc_range_years = NA_real_,
      retreat_trigger = "no_data",
      timing_category = NA_character_,
      cliff_exposure_status = NA_character_,
      initial_cliff_dist_m = NA_real_
    ))
  }
  
  # Check if this is a flood site
  is_flood_site <- site_type %in% c("FLOOD-ONLY", "HYBRID (flood + cliff)")
  
  # Check for cliff data and calculate exposure status
  cliff_exposure_status <- "not_applicable"
  initial_cliff_dist <- NA_real_
  
  if (site_type %in% c("CLIFF-ONLY", "HYBRID (flood + cliff)")) {
    if ("cliff_dist_m" %in% names(prop_hazards)) {
      initial_cliff_dist <- prop_hazards$cliff_dist_m[1]  # Year 1 distance
      
      if (!is.na(initial_cliff_dist)) {
        CLIFF_THRESHOLD_IMMEDIATE <- 10  # From valuation script
        CLIFF_THRESHOLD_RETREAT <- 25     # From valuation script
        
        if (initial_cliff_dist < CLIFF_THRESHOLD_IMMEDIATE) {
          cliff_exposure_status <- "immediate_hazard"
        } else if (initial_cliff_dist < CLIFF_THRESHOLD_RETREAT) {
          cliff_exposure_status <- "within_retreat_threshold"
        } else {
          cliff_exposure_status <- "safe"
        }
      }
    }
  }
  
  # If not flood site or no storm data, return deterministic result
  if (!is_flood_site || is.null(storm_data_list)) {
    return(tibble(
      parcel_id = prop_id,
      scenario = scenario_name,
      cliff_scenario = cliff_scen_name,
      retreat_year = NA_real_,
      mc_enabled = FALSE,
      mc_n_sims = 0,
      mc_mean_year = NA_real_,
      mc_median_year = NA_real_,
      mc_sd_year = NA_real_,
      mc_q05_year = NA_real_,
      mc_q25_year = NA_real_,
      mc_q75_year = NA_real_,
      mc_q95_year = NA_real_,
      mc_range_years = NA_real_,
      retreat_trigger = "not_flood_site",
      timing_category = NA_character_,
      cliff_exposure_status = cliff_exposure_status,
      initial_cliff_dist_m = initial_cliff_dist
    ))
  }
  
  # ===== PROPERTY ECONOMICS =====
  
  price_val <- prop$PRICE[1]
  rent_val <- price_val * rental_yield
  str_val <- prop$strval[1]
  beta <- 1 / (1 + discount_rate)
  
  # ===== PRE-COMPUTE NPV COMPONENTS (OPTIMIZATION 1) =====
  
  # Pre-compute beta powers (used repeatedly)
  beta_powers <- beta^(1:bigT)
  
  # Pre-compute rental NPV for each starting year
  npv_r_vec <- numeric(bigT)
  
  for (t in 1:bigT) {
    years_remaining <- bigT - t + 1
    npv_r_vec[t] <- rent_val * sum(beta_powers[1:years_remaining])
  }
  
  # ===== PRE-FILTER STORM DATA (OPTIMIZATION 2 - CRITICAL!) =====
  
  # Create interpolation functions ONCE instead of 24,000 times
  interp_funcs <- create_interpolation_functions(storm_data_list, prop_id)
  
  # ===== MONTE CARLO SIMULATIONS =====
  
  retreat_years <- numeric(n_sims)
  
  for (sim in 1:n_sims) {
    
    # Generate stochastic storm timeline (FAST NOW!)
    storm_depths <- generate_storm_timeline_fast(
      prop_hazards = prop_hazards,
      interp_funcs = interp_funcs
    )
    
    # Apply damage threshold
    storm_depths[storm_depths < damage_threshold] <- 0
    
    # Calculate damages for all years (vectorized)
    a <- depth_params["a"]
    b <- depth_params["b"]
    d <- depth_params["d"]
    damage_fraction <- a / (1 + exp(-b * (storm_depths - d)))
    damage_fraction <- pmax(damage_fraction, 0)
    annual_damages <- damage_fraction * str_val
    
    # Calculate NPV of damages from each potential starting year
    # NOTE: This loop is hard to vectorize without loss of accuracy
    # Performance: ~80 iterations is negligible compared to other operations
    npv_d_vec <- numeric(bigT)
    for (t in 1:bigT) {
      years_remaining <- bigT - t + 1
      npv_d_vec[t] <- sum(annual_damages[t:bigT] * beta_powers[1:years_remaining])
    }
    
    # Find retreat year for this timeline (vectorized comparison)
    # NPV(staying) = NPV(net rent) - NPV(damages)
    npv_staying <- npv_r_vec - npv_d_vec
    
    # Find first year where staying is not profitable
    retreat_candidates <- which(npv_staying <= 0)
    
    if (length(retreat_candidates) > 0) {
      t_star <- min(retreat_candidates)
    } else {
      t_star <- bigT + 1  # Never retreat
    }
    
    retreat_years[sim] <- t_star
  }
  
  # ===== AGGREGATE STATISTICS =====
  
  tstar_mean <- mean(retreat_years)
  tstar_median <- median(retreat_years)
  tstar_sd <- sd(retreat_years)
  tstar_q05 <- quantile(retreat_years, 0.05)
  tstar_q25 <- quantile(retreat_years, 0.25)
  tstar_q75 <- quantile(retreat_years, 0.75)
  tstar_q95 <- quantile(retreat_years, 0.95)
  
  # Determine retreat trigger (based on mean T*)
  retreat_trigger <- if (tstar_mean <= bigT) {
    "flood_storm_mc"
  } else {
    "beyond_horizon"
  }
  
  # Calculate timing category (based on mean T*)
  timing_category <- if (tstar_mean > bigT) {
    "beyond_horizon"
  } else if (tstar_mean <= 10) {
    "immediate"
  } else if (tstar_mean <= 25) {
    "early"
  } else if (tstar_mean <= 50) {
    "mid_term"
  } else {
    "late"
  }
  
  # ===== RETURN RESULTS =====
  
  # NOTE: Don't include PRICE, strval, landval here - valuation script will add them
  # via left_join to avoid duplicate columns
  
  results_summary <- tibble(
    parcel_id = prop_id,
    scenario = scenario_name,
    cliff_scenario = cliff_scen_name,
    retreat_year = if (tstar_mean > bigT) NA_real_ else tstar_mean,  # NA if beyond horizon
    mc_enabled = TRUE,
    mc_n_sims = n_sims,
    mc_mean_year = tstar_mean,
    mc_median_year = tstar_median,
    mc_sd_year = tstar_sd,
    mc_q05_year = tstar_q05,
    mc_q25_year = tstar_q25,
    mc_q75_year = tstar_q75,
    mc_q95_year = tstar_q95,
    mc_range_years = tstar_q95 - tstar_q05,
    retreat_trigger = retreat_trigger,
    timing_category = timing_category,
    cliff_exposure_status = cliff_exposure_status,
    initial_cliff_dist_m = initial_cliff_dist
  )
  
  # Optionally save full distribution for Shiny visualization
  # This creates a list attribute that can be extracted later
  attr(results_summary, "mc_distribution") <- retreat_years
  
  return(results_summary)
}


# UTILITY: EXTRACT AND SAVE MONTE CARLO DISTRIBUTIONS

#' Extract Monte Carlo distributions from results and save to CSV
#' 
#' This extracts the full retreat_year distribution for each property
#' and saves it in long format for Shiny visualization
#' 
#' @param mc_results Data frame with Monte Carlo results (from calc_tstar_monte_carlo)
#' @param output_path File path to save distributions CSV
#' @return Data frame in long format: parcel_id, scenario, sim_num, retreat_year
save_mc_distributions <- function(mc_results, output_path) {
  
  # Extract distributions from attributes
  distributions_list <- list()
  
  for (i in 1:nrow(mc_results)) {
    row_data <- mc_results[i, ]
    dist <- attr(row_data, "mc_distribution")
    
    if (!is.null(dist)) {
      distributions_list[[i]] <- tibble(
        parcel_id = row_data$parcel_id,
        scenario = row_data$scenario,
        cliff_scenario = row_data$cliff_scenario,
        sim_num = 1:length(dist),
        retreat_year = dist
      )
    }
  }
  
  if (length(distributions_list) > 0) {
    distributions_df <- bind_rows(distributions_list)
    write_csv(distributions_df, output_path)
    message("✓ Saved MC distributions: ", basename(output_path))
    message("  Total rows: ", format(nrow(distributions_df), big.mark = ","))
    return(distributions_df)
  } else {
    message("⚠ No MC distributions found in results")
    return(NULL)
  }
}



# UTILITY: CALCULATE COMMUNITY-LEVEL STATISTICS

#' Calculate community-level retreat statistics
#' 
#' @param results Data frame with retreat_year column
#' @param planning_horizon Planning horizon (default 80 years)
#' @return List with community statistics
calculate_community_stats <- function(results, planning_horizon = 80) {
  
  total_props <- nrow(results)
  
  # Properties that retreat within horizon
  retreating <- results %>% filter(!is.na(retreat_year) & retreat_year <= planning_horizon)
  n_retreating <- nrow(retreating)
  pct_retreating <- 100 * n_retreating / total_props
  
  # Properties by timing category
  timing_breakdown <- results %>%
    count(timing_category) %>%
    mutate(
      percent = 100 * n / total_props,
      label = paste0(timing_category, ": ", n, " (", round(percent, 1), "%)")
    )
  
  # Properties by retreat trigger
  trigger_breakdown <- results %>%
    count(retreat_trigger) %>%
    mutate(
      percent = 100 * n / total_props,
      label = paste0(retreat_trigger, ": ", n, " (", round(percent, 1), "%)")
    )
  
  # Retreat year statistics (for those that retreat)
  if (n_retreating > 0) {
    retreat_stats <- list(
      mean_year = mean(retreating$retreat_year, na.rm = TRUE),
      median_year = median(retreating$retreat_year, na.rm = TRUE),
      sd_year = sd(retreating$retreat_year, na.rm = TRUE),
      min_year = min(retreating$retreat_year, na.rm = TRUE),
      max_year = max(retreating$retreat_year, na.rm = TRUE)
    )
  } else {
    retreat_stats <- list(
      mean_year = NA, median_year = NA, sd_year = NA,
      min_year = NA, max_year = NA
    )
  }
  
  # Monte Carlo uncertainty (if available)
  mc_props <- results %>% filter(mc_enabled == TRUE)
  if (nrow(mc_props) > 0) {
    mc_stats <- list(
      n_properties_mc = nrow(mc_props),
      mean_uncertainty_years = mean(mc_props$mc_sd_year, na.rm = TRUE),
      mean_range_years = mean(mc_props$mc_range_years, na.rm = TRUE)
    )
  } else {
    mc_stats <- list(
      n_properties_mc = 0,
      mean_uncertainty_years = NA,
      mean_range_years = NA
    )
  }
  
  # Economic viability (properties economically viable for managed retreat)
  # Definition: Properties with retreat_year <= planning_horizon
  # These are candidates for proactive buyout-leaseback
  economic_viability <- list(
    total_properties = total_props,
    n_viable_retreat = n_retreating,
    pct_viable_retreat = pct_retreating,
    n_beyond_horizon = total_props - n_retreating,
    pct_beyond_horizon = 100 * (total_props - n_retreating) / total_props
  )
  
  return(list(
    economic_viability = economic_viability,
    retreat_stats = retreat_stats,
    timing_breakdown = timing_breakdown,
    trigger_breakdown = trigger_breakdown,
    mc_stats = mc_stats
  ))
}


#' Print community statistics summary
#' 
#' @param community_stats Output from calculate_community_stats()
print_community_stats <- function(community_stats) {
  
  cat("COMMUNITY-LEVEL STATISTICS\n")
  
  # Economic viability
  ev <- community_stats$economic_viability
  cat("ECONOMIC VIABILITY FOR MANAGED RETREAT:\n")
  cat("  Total properties:", ev$total_properties, "\n")
  cat("  Economically viable for retreat:", ev$n_viable_retreat, 
      "(", round(ev$pct_viable_retreat, 1), "%)\n")
  cat("  Beyond planning horizon:", ev$n_beyond_horizon, "(", round(ev$pct_beyond_horizon, 1), "%)\n\n")
  
  # Retreat timing
  rs <- community_stats$retreat_stats
  if (!is.na(rs$mean_year)) {
    cat("RETREAT TIMING (for viable properties):\n")
    cat("  Mean retreat year:", round(rs$mean_year, 1), "\n")
    cat("  Median retreat year:", round(rs$median_year, 1), "\n")
    cat("  Range:", rs$min_year, "-", rs$max_year, "years\n")
    cat("  Standard deviation:", round(rs$sd_year, 1), "years\n\n")
  }
  
  # Timing categories
  cat("RETREAT TIMING CATEGORIES:\n")
  for (i in 1:nrow(community_stats$timing_breakdown)) {
    cat("  ", community_stats$timing_breakdown$label[i], "\n")
  }
  cat("\n")
  
  # Monte Carlo uncertainty
  mc <- community_stats$mc_stats
  if (mc$n_properties_mc > 0) {
    cat("MONTE CARLO UNCERTAINTY:\n")
    cat("  Properties with MC:", mc$n_properties_mc, "\n")
    cat("  Mean uncertainty (SD):", round(mc$mean_uncertainty_years, 1), "years\n")
    cat("  Mean 90% CI width:", round(mc$mean_range_years, 1), "years\n")
  }
  
  cat("\n========================================\n\n")
}


# UTILITY: PRINT MONTE CARLO SUMMARY

#' Print summary statistics for Monte Carlo results
#' 
#' @param mc_results Data frame with Monte Carlo output
print_mc_summary <- function(mc_results) {
  
  mc_props <- mc_results %>% filter(mc_enabled == TRUE)
  
  if (nrow(mc_props) == 0) {
    message("No Monte Carlo results to summarize")
    return(invisible(NULL))
  }
  
  retreating <- mc_props %>% filter(retreat_year <= max(retreat_year[is.finite(retreat_year)]))
  
  message("MONTE CARLO SUMMARY")
  message("Properties analyzed: ", nrow(mc_props))
  message("Simulations per property: ", unique(mc_props$mc_n_sims)[1])
  message("\nRETREAT TIMING (mean across simulations - expected value):")
  
  if (nrow(retreating) > 0) {
    message("  Properties retreating: ", nrow(retreating))
    message("  Mean retreat year: ", round(mean(retreating$retreat_year, na.rm = TRUE), 1))
    message("  Median retreat year: ", median(retreating$retreat_year, na.rm = TRUE))
    
    message("\nUNCERTAINTY METRICS:")
    message("  Mean uncertainty (SD): ", round(mean(retreating$mc_sd_year, na.rm = TRUE), 1), " years")
    message("  Mean 90% CI width: ", round(mean(retreating$mc_range_years, na.rm = TRUE), 1), " years")
    
    # Show distribution of retreat years
    decade_breaks <- seq(0, max(retreating$retreat_year, na.rm = TRUE) + 10, by = 10)
    decade_counts <- table(cut(retreating$retreat_year, breaks = decade_breaks, include.lowest = TRUE))
    
    message("\nRETREAT TIMING DISTRIBUTION (by decade):")
    for (i in seq_along(decade_counts)) {
      if (decade_counts[i] > 0) {
        message("  ", names(decade_counts)[i], ": ", decade_counts[i], " properties")
      }
    }
  } else {
    message("  No properties retreat within planning horizon")
  }
  
  message("========================================\n")
}