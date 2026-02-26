# managed_shores_app_enhanced.R
# Purpose: Interactive dashboard for coastal managed retreat economics
# Author: Will Dean (Managed Shores)
#
# Enhanced Features:
# 1. Government economics (profit/loss from buyout-leaseback)
# 2. Monte Carlo uncertainty visualization
# 3. SLR scenario comparison
# 4. Buyout cap analysis
# 5. Property-level ROI breakdown
#
# To run: shiny::runApp("cosmos_shiny_enhanced.R")

rm(list = ls())
library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(leaflet)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(sf)
library(scales)
library(jsonlite)


# DATA LOADING
# Configuration
CASE_NAME <- "stinson"  # Change to isla_vista, pacifica as needed
DATA_DIR <- file.path("data", CASE_NAME)
DERIVED_DIR <- file.path(DATA_DIR, "derived")

# ==============================================================================
# LOAD DATA - Handle both RDS and CSV formats
# ==============================================================================

# Try to load RDS first (preferred - has list columns intact)
rds_baseline <- file.path(DERIVED_DIR, "retreat_schedule_baseline.rds")
rds_sensitivity <- file.path(DERIVED_DIR, "retreat_schedule_sensitivity.rds")

if (file.exists(rds_baseline)) {
  message("Loading baseline from RDS (preserves list columns)")
  baseline_results <- readRDS(rds_baseline)
} else {
  message("Loading baseline from CSV")
  baseline_results <- read_csv(
    file.path(DERIVED_DIR, "retreat_schedule_baseline.csv"),
    show_col_types = FALSE
  )
  
  # Drop the *_str columns that can't be used in Shiny
  # (NPV plot calculates these on-the-fly anyway)
  baseline_results <- baseline_results %>%
    select(-matches("_str$"))
}

# Load sensitivity results (if available)
if (file.exists(rds_sensitivity)) {
  message("Loading sensitivity from RDS")
  sensitivity_results <- readRDS(rds_sensitivity)
  has_sensitivity <- TRUE
} else {
  sensitivity_file <- file.path(DERIVED_DIR, "retreat_schedule_sensitivity.csv")
  if (file.exists(sensitivity_file)) {
    message("Loading sensitivity from CSV")
    sensitivity_results <- read_csv(sensitivity_file, show_col_types = FALSE)
    # Drop *_str columns
    sensitivity_results <- sensitivity_results %>%
      select(-matches("_str$"))
    has_sensitivity <- TRUE
  } else {
    has_sensitivity <- FALSE
  }
}

# Combine baseline and sensitivity
if (has_sensitivity) {
  # Add result_type indicator
  baseline_results <- baseline_results %>% mutate(result_type = "baseline")
  sensitivity_results <- sensitivity_results %>% mutate(result_type = "sensitivity")
  
  all_results <- bind_rows(baseline_results, sensitivity_results)
} else {
  all_results <- baseline_results %>% mutate(result_type = "baseline")
}

# Load Monte Carlo distributions (if available)
# NOTE: This requires T* distribution data (retreat_year per simulation)
mc_file <- file.path(DERIVED_DIR, "mc_tstar_distributions_baseline.csv")
if (file.exists(mc_file)) {
  mc_distributions <- read_csv(mc_file, show_col_types = FALSE)
  has_mc <- TRUE
  message("✓ Loaded MC T* distributions: ", nrow(mc_distributions), " rows")
} else {
  has_mc <- FALSE
  message("MC T* distributions not found - histogram will be disabled")
}

# Load community stats (if available)
stats_file <- file.path(DERIVED_DIR, "community_stats_baseline.json")
if (file.exists(stats_file)) {
  community_stats <- fromJSON(stats_file)
  has_stats <- TRUE
} else {
  has_stats <- FALSE
}

# Detect site type
has_flood <- any(grepl("flood", all_results$retreat_trigger, ignore.case = TRUE))
has_cliff <- any(grepl("cliff", all_results$retreat_trigger, ignore.case = TRUE))

site_type <- case_when(
  has_flood & !has_cliff ~ "FLOOD",
  has_cliff & !has_flood ~ "CLIFF",
  has_flood & has_cliff ~ "HYBRID",
  TRUE ~ "UNKNOWN"
)

# Extract parameter ranges - order correctly
scenarios_raw <- unique(all_results$scenario) %>% na.omit()
# Correct order: Intermediate, Intermediate_High, High
scenario_order <- c("Intermediate", "Intermediate_High", "High")
scenarios <- scenario_order[scenario_order %in% scenarios_raw]

#Function to calculate NPV components for a single property
calculate_npv_components_property <- function(parcel_id, scenario, hazards_data, baseline_data, bigT = 75) {
  
  tryCatch({
    # Get property parameters
    prop_params <- baseline_data %>%
      filter(parcel_id == !!parcel_id, scenario == !!scenario) %>%
      slice(1)
    
    if (nrow(prop_params) == 0) {
      message("No property found: parcel_id=", parcel_id, ", scenario=", scenario)
      return(NULL)
    }
    
    rent <- prop_params$rent
    discount_rate <- 0.05  # Fixed market rate (used for T* calculation)
    damage_threshold <- 0.15  # Fixed threshold
    strval <- prop_params$strval
    
    if (any(is.na(c(rent, strval)))) {
      message("Missing parameters for parcel ", parcel_id)
      return(NULL)
    }
    
    beta <- 1 / (1 + discount_rate)
    depth_params <- c(a = 0.4, b = 1, d = 2)
    
    # Get flood timeline
    flood_timeline <- hazards_data %>%
      filter(parcel_id == !!parcel_id, scenario == !!scenario) %>%
      arrange(year) %>%
      mutate(
        depth_clean = if_else(depth_m >= damage_threshold, depth_m, 0),
        damage_frac = depth_params["a"] / (1 + exp(-depth_params["b"] * (depth_clean - depth_params["d"]))),
        damage_frac = pmax(damage_frac, 0),
        annual_damage = damage_frac * strval
      )
    
    if (nrow(flood_timeline) == 0) {
      message("No hazards data for parcel ", parcel_id, ", scenario=", scenario)
      return(NULL)
    }
    
    n_years <- min(bigT, nrow(flood_timeline))
    beta_powers <- beta^(1:n_years)
    
    # Calculate NPV vectors - UPDATED TO MATCH NEW MODEL
    # npv_r_vec[t] = NPV of perpetual rent (constant)
    # npv_d_vec[t] = NPV of damages from year t to 2100 + perpetual tail
    
    # NPV(Rents) - Perpetuity (constant for all years)
    npv_r_perpetuity <- rent * (1 + discount_rate) / discount_rate
    npv_r_vec <- rep(npv_r_perpetuity, n_years)
    
    # NPV(Damages) - Finite + Perpetual tail
    npv_d_vec <- numeric(n_years)
    annual_damages <- flood_timeline$annual_damage
    
    for (t in 1:n_years) {
      years_remaining <- n_years - t + 1
      
      # Part 1: Known damages from t to 2100
      if (t <= length(annual_damages)) {
        end_idx <- min(length(annual_damages), n_years)
        damage_vals <- annual_damages[t:end_idx]
        npv_known <- sum(damage_vals * beta_powers[1:length(damage_vals)])
      } else {
        npv_known <- 0
      }
      
      # Part 2: Perpetual damages after 2100 (constant at year 2100 level)
      final_damage <- annual_damages[min(length(annual_damages), n_years)]
      discount_to_T_hat <- beta^years_remaining
      npv_perpetual <- (final_damage / discount_rate) * discount_to_T_hat
      
      npv_d_vec[t] <- npv_known + npv_perpetual
    }
    
    # Create output
    npv_data <- tibble(
      year = 1:n_years,
      npv_rent = npv_r_vec,
      npv_damage = npv_d_vec,
      npv_net = npv_r_vec - npv_d_vec
    )
    
    return(npv_data)
    
  }, error = function(e) {
    message("Error calculating NPV: ", e$message)
    message("  parcel_id=", parcel_id, ", scenario=", scenario)
    return(NULL)
  })
}
# Extract cliff scenario options (for cliff-only sites)
cliff_scenarios_raw <- unique(all_results$cliff_scenario) %>% na.omit()
cliff_scenarios <- if(length(cliff_scenarios_raw) > 0) {
  cliff_scenarios_raw
} else {
  NULL
}
has_cliff_scenarios <- !is.null(cliff_scenarios) && length(cliff_scenarios) > 1

# Check for government economics columns
has_gov_econ <- all(c("gov_net_cost", "gov_outcome", "leaseback_revenue", "npv_repairs") %in% names(baseline_results))
has_buyout_cap <- "buyout_capped" %in% names(baseline_results)



# =============================================================================
# UI
# =============================================================================

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = paste0("Managed Shores: ", toupper(CASE_NAME)),
    titleWidth = 400
  ),
  
  dashboardSidebar(
    width = 280,
    
    sidebarMenu(
      id = "tabs",
      menuItem("🏛️ Government Economics", tabName = "gov_econ", icon = icon("landmark")),
      menuItem("💰 Property Buyouts", tabName = "buyouts", icon = icon("dollar-sign")),
      menuItem("🌊 SLR Scenarios", tabName = "scenarios", icon = icon("water")),
      menuItem("📊 Monte Carlo", tabName = "mc", icon = icon("dice")),
      menuItem("🗺️ Spatial View", tabName = "map", icon = icon("map")),
      menuItem("ℹ️ About", tabName = "about", icon = icon("info-circle"))
    ),
    
    hr(),
    
    # SCENARIO SELECTION
    h4("Scenario", style = "padding-left: 15px; font-weight: bold;"),
    
    selectInput(
      "slr_scenario",
      "SLR Scenario:",
      choices = scenarios,
      selected = scenarios[1]  # Default to Intermediate
    ),
    
    # Cliff scenario selector (conditional on site type)
    conditionalPanel(
      condition = paste0("'", site_type, "' == 'CLIFF' && ", has_cliff_scenarios),
      selectInput(
        "cliff_scenario",
        "Cliff Management:",
        choices = if(has_cliff_scenarios) cliff_scenarios else NULL,
        selected = if(has_cliff_scenarios) cliff_scenarios[1] else NULL
      ),
      helpText("LetItGo: Natural erosion | HoldTheLine: Armoring/seawalls")
    ),
    
    hr(),
    h4("Parameters", style = "padding-left: 15px; font-weight: bold;"),
    
    div(style = "padding: 10px 15px; background-color: #f8f9fa; margin: 10px; border-radius: 5px; border: 1px solid #dee2e6;",
        p(strong("Fixed Parameters:"), style = "margin-bottom: 8px; color: #212529; font-size: 14px;"),
        p("• Market Discount Rate (T*): 5%", style = "margin: 2px 0; font-size: 13px; color: #495057;"),
        p("• Rental Yield: 5%", style = "margin: 2px 0; font-size: 13px; color: #495057;"),
        p("• Damage Threshold: 15cm", style = "margin: 2px 0; font-size: 13px; color: #495057;")
    ),
    
    sliderInput(
      "discount_rate_gov",
      "Government Discount Rate (δ_g %):",
      min = 2,
      max = 4,
      value = 2,
      step = 1,
      post = "%"
    ),
    
    div(style = "padding: 10px 15px; background-color: #d1ecf1; margin: 10px; border-radius: 5px; border: 1px solid #bee5eb;",
        p(strong("δ_g affects:"), style = "margin-bottom: 8px; color: #0c5460; font-size: 14px;"),
        p("• Leaseback Revenue", style = "margin: 2px 0; font-size: 13px; color: #0c5460;"),
        p("• NPV Repair Costs", style = "margin: 2px 0; font-size: 13px; color: #0c5460;"),
        p("• Government Net Cost", style = "margin: 2px 0; font-size: 13px; color: #0c5460;")
    ),
    
    hr(),
    actionButton("reset", "Reset to Baseline", icon = icon("undo"), 
                 style = "width: 90%; margin-left: 15px;")
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .value-box { height: 110px; }
        .info-box { min-height: 90px; }
        .small-box .icon-large { font-size: 60px; }
        .bg-profit { background-color: #00a65a !important; }
        .bg-loss { background-color: #dd4b39 !important; }
        .bg-neutral { background-color: #f39c12 !important; }
      "))
    ),
    
    tabItems(
      
      
      # TAB 1: GOVERNMENT ECONOMICS
      tabItem(
        tabName = "gov_econ",
        
        h2("Government Economics: Buyout-Leaseback Program"),
        p("Analyze the net cost/profit of the managed retreat program from the government's perspective."),
        
        # Show current parameters
        fluidRow(
          box(
            title = NULL,
            status = "info",
            width = 12,
            solidHeader = FALSE,
            background = "light-blue",
            htmlOutput("current_params_display")
          )
        ),
        
        # Top-level metrics
        fluidRow(
          valueBoxOutput("total_buyout_box", width = 3),
          valueBoxOutput("total_revenue_box", width = 3),
          valueBoxOutput("gov_net_box", width = 3),
          valueBoxOutput("roi_pct_box", width = 3)
        ),
        
        fluidRow(
          valueBoxOutput("rent_collected_box", width = 6),
          valueBoxOutput("props_capped_box", width = 6)
        ),
        
        # Government economics breakdown
        fluidRow(
          box(
            title = "Government Cash Flow (Buyout-Leaseback Model)",
            status = "primary",
            solidHeader = TRUE,
            width = 8,
            plotlyOutput("gov_cashflow", height = "400px"),
            hr(),
            htmlOutput("cashflow_explanation")
          ),
          
          box(
            title = "Property Outcome Breakdown",
            status = "success",
            solidHeader = TRUE,
            width = 4,
            plotlyOutput("outcome_pie", height = "250px"),
            hr(),
            htmlOutput("outcome_stats")
          )
        ),
        
        # By retreat timing
        fluidRow(
          box(
            title = "Government Net Cost by Retreat Timing",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("net_by_timing", height = "350px"),
            htmlOutput("timing_explanation")
          )
        )
      ),
      
      
      # TAB 2: PROPERTY BUYOUTS
      tabItem(
        tabName = "buyouts",
        
        h2("Property-Level Buyout Analysis"),
        p("Detailed breakdown of buyout pricing and economics for each property."),
        
        # Summary metrics
        fluidRow(
          infoBoxOutput("total_props_box", width = 3),
          infoBoxOutput("avg_buyout_box", width = 3),
          infoBoxOutput("avg_market_box", width = 3),
          infoBoxOutput("total_savings_box", width = 3)
        ),
        
        # Buyout vs market comparison
        fluidRow(
          box(
            title = "Buyout Price vs Market Value",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("buyout_scatter", height = "400px")
          ),
          
          box(
            title = "Buyout Price Distribution",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("buyout_hist", height = "400px")
          )
        ),
        
        # Property table
        fluidRow(
          box(
            title = "Property-Level Details",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            p("Properties highlighted in green generate government profit. Red indicates net cost."),
            DT::dataTableOutput("buyout_table")
          )
        )
      ),
      
      
      # TAB 3: SLR SCENARIO COMPARISON
      tabItem(
        tabName = "scenarios",
        
        h2("Sea Level Rise Scenario Comparison"),
        p("Compare retreat timing and costs across different SLR projections."),
        
        # Scenario comparison
        fluidRow(
          box(
            title = "Retreat Timing by SLR Scenario",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("scenario_retreat_years", height = "400px")
          ),
          
          box(
            title = "Government Economics by Scenario",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("scenario_gov_econ", height = "400px")
          )
        ),
        
        fluidRow(
          box(
            title = "Properties Retreating Over Time",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("cumulative_retreat", height = "400px"),
            htmlOutput("scenario_interpretation")
          )
        )
      ),
      
      
      # TAB 4: MONTE CARLO UNCERTAINTY
      tabItem(
        tabName = "mc",
        
        h2("Monte Carlo Uncertainty Analysis"),
        p("Visualize uncertainty in retreat timing from storm variability."),
        
        conditionalPanel(
          condition = paste0("'", has_mc, "' == 'TRUE'"),
          
          # MC summary metrics
          fluidRow(
            infoBoxOutput("mc_sims_box", width = 3),
            infoBoxOutput("mc_uncertainty_box", width = 3),
            infoBoxOutput("mc_range_box", width = 3),
            infoBoxOutput("mc_confidence_box", width = 3)
          ),
          
          # Property selector for MC visualization
          fluidRow(
            box(
              title = "Select Property for Detailed MC Analysis",
              status = "info",
              solidHeader = TRUE,
              width = 12,
              selectInput(
                "mc_property",
                "Property:",
                choices = NULL  # Will be populated dynamically
              )
            )
          ),
          
          # MC distribution plots
          fluidRow(
            box(
              title = "Monte Carlo Distribution (Selected Property)",
              status = "primary",
              solidHeader = TRUE,
              width = 6,
              plotlyOutput("mc_histogram", height = "400px"),
              htmlOutput("mc_property_stats")
            ),
            
            box(
              title = "Community-Wide MC Uncertainty",
              status = "warning",
              solidHeader = TRUE,
              width = 6,
              plotlyOutput("mc_uncertainty_scatter", height = "400px"),
              htmlOutput("mc_community_stats")
            )
          )
        ),
        
        conditionalPanel(
          condition = paste0("'", has_mc, "' == 'FALSE'"),
          box(
            title = "Monte Carlo Data Not Available",
            status = "danger",
            solidHeader = TRUE,
            width = 12,
            p("Monte Carlo distributions were not found. Make sure 'mc_tstar_distributions_baseline.csv' exists in the derived data directory."),
            p("Run the valuation script with Monte Carlo enabled to generate this data.")
          )
        )
      ),
      
      
      # TAB 5: SPATIAL MAP
      tabItem(
        tabName = "map",
        
        h2("Spatial Distribution of Retreat Timing"),
        p("Interactive map showing when properties should retreat based on economic analysis."),
        
        fluidRow(
          box(
            title = "Retreat Timing Map",
            status = "primary",
            solidHeader = TRUE,
            width = 9,
            leafletOutput("retreat_map", height = "600px")
          ),
          
          box(
            title = "Selected Property",
            status = "info",
            solidHeader = TRUE,
            width = 3,
            uiOutput("selected_property_info"),
            hr(),
            uiOutput("selected_property_econ"),
            hr(),
            h4("NPV Timeline", style = "font-weight: bold;"),
            plotlyOutput("selected_property_npv", height = "300px")
          ),
        )
      ),
      
      # TAB 6: ABOUT
      tabItem(
        tabName = "about",
        
        h2("About This Dashboard"),
        
        box(
          title = "Managed Shores: Buyout-Leaseback Economic Model",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          
          h4("Overview"),
          p("This dashboard analyzes the economics of proactive managed retreat through a buyout-leaseback program.",
            "The government purchases coastal properties before they become uninhabitable, leases them back to",
            "former owners, and eventually recovers the land when flooding makes continued occupation unviable."),
          
          h4("Model Parameters"),
          tags$ul(
            tags$li(strong("Rental Yield:"), "Annual rent as % of property value (3-6%)"),
            tags$li(strong("Discount Rate:"), "Social discount rate for NPV calculations (1.5-3%)"),
            tags$li(strong("Damage Threshold:"), "Minimum flood depth triggering economic damages (15-30cm)")
          ),
          
          h4("Government Economics"),
          p("The model tracks government cash flows:"),
          tags$ul(
            tags$li(strong("Buyout Cost:"), "MIN(NPV(future rent) + land value, market price)"),
            tags$li(strong("Revenue:"), "Rent collected + land value recovered at retreat"),
            tags$li(strong("Net Cost:"), "Buyout - Rent - Land (negative = profit!)")
          ),
          
          h4("Data Sources"),
          tags$ul(
            tags$li("Sea level rise: CoSMoS (USGS)"),
            tags$li("Property values: Redfin"),
            tags$li("Storm scenarios: Monte Carlo simulation")
          ),
          
          h4("Site Information"),
          p(paste0("Site Type: ", site_type)),
          p(paste0("Total Properties: ", nrow(baseline_results))),
          p(paste0("SLR Scenarios: ", paste(scenarios, collapse = ", "))),
          p(paste0("Monte Carlo: ", ifelse(has_mc, "Enabled", "Not available"))),
          p(paste0("Government Economics: ", ifelse(has_gov_econ, "Available", "Not available")))
        )
      )
    )
  )
)

# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {
  
  # REACTIVE DATA FILTERS
  
  baseline_data <- reactive({
    all_results %>%
      filter(
        result_type == "baseline",
        scenario == input$slr_scenario
      )
  })
  
  filtered_data <- reactive({
    # Start with all data for this scenario
    df <- all_results %>%
      filter(scenario == input$slr_scenario)
    
    # Filter by cliff scenario if applicable
    if (site_type == "CLIFF" && has_cliff_scenarios && !is.null(input$cliff_scenario)) {
      df <- df %>% filter(cliff_scenario == input$cliff_scenario)
    }
    
    n_before <- nrow(df)
    
    # Apply parameter filters based on sliders
    # Note: Data has parameters as decimals (e.g., 0.05 = 5%), slider shows percentages
    # Filter by government discount rate (only varying parameter)
    if ("discount_rate_gov" %in% names(df)) {
      target_rate_gov <- input$discount_rate_gov / 100  # Convert 2% -> 0.02
      df <- df %>% filter(abs(discount_rate_gov - target_rate_gov) < 0.001)
    }
    
    n_after <- nrow(df)
    
    # Debug: print what we filtered to
    if (n_after > 0 && n_after != n_before) {
      cliff_msg <- if(site_type == "CLIFF" && !is.null(input$cliff_scenario)) {
        paste0(", cliff=", input$cliff_scenario)
      } else {
        ""
      }
      message(sprintf("Filtered from %d to %d rows for scenario=%s%s, δ_gov=%.0f%%",
                      n_before, n_after, input$slr_scenario, cliff_msg,
                      input$discount_rate_gov))
    }
    
    # If no matches found, use baseline
    if (nrow(df) == 0) {
      message("No sensitivity data found for these parameters, using baseline")
      df <- all_results %>%
        filter(scenario == input$slr_scenario, result_type == "baseline")
    }
    
    df
  })
  
  # Update MC property selector
  observe({
    df <- baseline_data()
    props_with_retreat <- df %>% 
      filter(!is.na(buyout_price)) %>%
      arrange(retreat_year) %>%
      mutate(label = paste0(ADDRESS, " (T*=", round(retreat_year, 1), ")"))
    
    updateSelectInput(session, "mc_property", 
                      choices = setNames(props_with_retreat$parcel_id, props_with_retreat$label),
                      selected = props_with_retreat$parcel_id[1])
  })
  
  # TAB 1: GOVERNMENT ECONOMICS OUTPUTS
  
  output$current_params_display <- renderUI({
    df <- filtered_data()
    n_props <- sum(!is.na(df$buyout_price))
    
    cliff_display <- if(site_type == "CLIFF" && has_cliff_scenarios && !is.null(input$cliff_scenario)) {
      paste0("Cliff: ", input$cliff_scenario, " | ")
    } else {
      ""
    }
    
    threshold_display <- if(site_type != "CLIFF") {
      paste0("Threshold: ", input$damage_threshold, "cm | ")
    } else {
      ""
    }
    
    HTML(paste0(
      "<strong>Current Parameters:</strong> ",
      "SLR: ", input$slr_scenario, " | ",
      cliff_display,
      "Market δ: 5% (fixed) | ",
      "Gov δ_g: ", input$discount_rate_gov, "% | ",
      "Rental Yield: 5% (fixed) | ",
      threshold_display,
      "<span style='color: #2ecc71;'><strong>", n_props, " properties analyzed</strong></span>"
    ))
  })
  
  output$total_buyout_box <- renderValueBox({
    df <- filtered_data() %>% filter(!is.na(buyout_price))
    total <- sum(df$buyout_price, na.rm = TRUE)
    
    valueBox(
      paste0("$", round(total / 1e6, 1), "M"),
      "Total Buyout Cost",
      icon = icon("hand-holding-usd"),
      color = "orange"
    )
  })
  
  output$total_revenue_box <- renderValueBox({
    if (!has_gov_econ) {
      valueBox("N/A", "Revenue data not available", icon = icon("question"), color = "red")
    } else {
      df <- filtered_data() %>% filter(!is.na(buyout_price))
      rent <- sum(df$leaseback_revenue, na.rm = TRUE)
      # Land has no value at T* (removed from model)
      total <- rent
      
      valueBox(
        paste0("$", round(total / 1e6, 1), "M"),
        "Leaseback Revenue",
        icon = icon("coins"),
        color = "green"
      )
    }
  })
  
  output$gov_net_box <- renderValueBox({
    if (!has_gov_econ) {
      valueBox("N/A", "Gov economics not available", icon = icon("question"), color = "red")
    } else {
      df <- filtered_data() %>% filter(!is.na(buyout_price))
      net <- sum(df$gov_net_cost, na.rm = TRUE)
      
      if (net < 0) {
        valueBox(
          paste0("$", round(abs(net) / 1e6, 1), "M"),
          "Government PROFIT",
          icon = icon("chart-line"),
          color = "green"
        )
      } else if (net > 0) {
        valueBox(
          paste0("$", round(net / 1e6, 1), "M"),
          "Government Cost",
          icon = icon("exclamation-triangle"),
          color = "red"
        )
      } else {
        valueBox(
          "$0",
          "Break Even",
          icon = icon("balance-scale"),
          color = "yellow"
        )
      }
    }
  })
  
  output$roi_pct_box <- renderValueBox({
    if (!has_gov_econ) {
      valueBox("N/A", "ROI not available", icon = icon("question"), color = "red")
    } else {
      df <- filtered_data() %>% filter(!is.na(buyout_price))
      buyout <- sum(df$buyout_price, na.rm = TRUE)
      net <- sum(df$gov_net_cost, na.rm = TRUE)
      
      # ROI = (Revenue - Cost) / Cost = -Net / Buyout
      roi_pct <- -100 * net / buyout
      
      color <- if(roi_pct > 20) "green" else if(roi_pct > 0) "yellow" else "red"
      
      valueBox(
        paste0(round(roi_pct, 1), "%"),
        "Return on Investment",
        icon = icon("percentage"),
        color = color
      )
    }
  })
  
  output$rent_collected_box <- renderValueBox({
    if (!has_gov_econ) {
      valueBox("N/A", "Data not available", icon = icon("question"), color = "red")
    } else {
      df <- filtered_data() %>% filter(!is.na(buyout_price))
      repairs <- sum(df$npv_repairs, na.rm = TRUE)
      
      valueBox(
        paste0("$", round(repairs / 1e6, 1), "M"),
        "NPV Repair Costs",
        icon = icon("tools"),
        color = "orange"
      )
    }
  })
  
  # Land recovered box removed - land has no value at T* in updated model
  
  output$props_capped_box <- renderValueBox({
    if (!has_buyout_cap) {
      valueBox("N/A", "Cap data not available", icon = icon("question"), color = "red")
    } else {
      df <- filtered_data() %>% filter(!is.na(buyout_price))
      n_capped <- sum(df$buyout_capped, na.rm = TRUE)
      pct <- 100 * n_capped / nrow(df)
      
      valueBox(
        paste0(n_capped, " (", round(pct, 1), "%)"),
        "Properties Capped at Market",
        icon = icon("hand-paper"),
        color = "yellow"
      )
    }
  })
  
  output$gov_cashflow <- renderPlotly({
    if (!has_gov_econ) {
      plot_ly() %>% layout(title = "Government economics data not available")
    } else {
      df <- filtered_data() %>% filter(!is.na(buyout_price))
      
      buyout_total <- sum(df$buyout_price, na.rm = TRUE) / 1e6
      leaseback_total <- sum(df$leaseback_revenue, na.rm = TRUE) / 1e6
      repairs_total <- sum(df$npv_repairs, na.rm = TRUE) / 1e6
      net_total <- sum(df$gov_net_cost, na.rm = TRUE) / 1e6
      
      data <- data.frame(
        category = c("Buyout Paid", "Leaseback Revenue", "Repair Costs", "Net Cost/Profit"),
        value = c(-buyout_total, leaseback_total, -repairs_total, -net_total),
        type = c("cost", "revenue", "cost", if(net_total < 0) "profit" else "cost")
      )
      
      colors <- c("cost" = "#e74c3c", "revenue" = "#3498db", "profit" = "#2ecc71")
      
      plot_ly(data, x = ~category, y = ~value, type = "bar",
              marker = list(color = ~colors[type])) %>%
        layout(
          title = "Government Cash Flow (Millions $)",
          xaxis = list(title = ""),
          yaxis = list(title = "Amount ($M)"),
          hovermode = "x"
        )
    }
  })
  
  output$cashflow_explanation <- renderUI({
    if (!has_gov_econ) {
      HTML("<p>Government economics data not available.</p>")
    } else {
      df <- baseline_data() %>% filter(!is.na(buyout_price))
      net <- sum(df$gov_net_cost, na.rm = TRUE)
      
      if (net < 0) {
        HTML(paste0(
          "<p><strong>Government PROFITS $", round(abs(net)/1e6, 1), "M from this program!</strong></p>",
          "<p>The buyout-leaseback model generates positive returns by:</p>",
          "<ul>",
          "<li>Collecting rental income during the leaseback period</li>",
          "<li>Recovering land value when properties retreat</li>",
          "<li>Capping buyouts at market price (prevents overpayment)</li>",
          "</ul>"
        ))
      } else {
        HTML(paste0(
          "<p><strong>Government has net cost of $", round(net/1e6, 1), "M</strong></p>",
          "<p>This could indicate:</p>",
          "<ul>",
          "<li>Rental yield too low for cost recovery</li>",
          "<li>Properties retreating too early (less rent collected)</li>",
          "<li>Land values too low</li>",
          "</ul>"
        ))
      }
    }
  })
  
  output$outcome_pie <- renderPlotly({
    if (!has_gov_econ) {
      plot_ly() %>% layout(title = "Data not available")
    } else {
      df <- filtered_data() %>% filter(!is.na(buyout_price))
      
      outcome_counts <- df %>%
        count(gov_outcome) %>%
        mutate(label = paste0(gov_outcome, ": ", n))
      
      colors <- c(
        "profit" = "#2ecc71",
        "break_even" = "#f39c12",
        "near_break_even" = "#f1c40f",
        "net_cost" = "#e74c3c"
      )
      
      plot_ly(outcome_counts, labels = ~gov_outcome, values = ~n, type = "pie",
              marker = list(colors = colors[outcome_counts$gov_outcome]),
              textinfo = "label+percent") %>%
        layout(showlegend = TRUE)
    }
  })
  
  output$outcome_stats <- renderUI({
    if (!has_gov_econ) {
      HTML("<p>Data not available</p>")
    } else {
      df <- filtered_data() %>% filter(!is.na(buyout_price))
      
      outcomes <- df %>%
        count(gov_outcome) %>%
        mutate(pct = 100 * n / nrow(df))
      
      html_text <- "<p><strong>Property Outcomes:</strong></p><ul>"
      
      for (i in 1:nrow(outcomes)) {
        html_text <- paste0(html_text, 
                            "<li>", outcomes$gov_outcome[i], ": ", outcomes$n[i], 
                            " (", round(outcomes$pct[i], 1), "%)</li>")
      }
      
      html_text <- paste0(html_text, "</ul>")
      HTML(html_text)
    }
  })
  
  output$net_by_timing <- renderPlotly({
    if (!has_gov_econ) {
      plot_ly() %>% layout(title = "Data not available")
    } else {
      df <- filtered_data() %>% filter(!is.na(buyout_price))
      
      timing_summary <- df %>%
        group_by(timing_category) %>%
        summarise(
          n = n(),
          mean_net = mean(gov_net_cost, na.rm = TRUE) / 1000,
          .groups = "drop"
        ) %>%
        arrange(match(timing_category, c("immediate", "early", "mid_term", "late")))
      
      plot_ly(timing_summary, x = ~timing_category, y = ~mean_net, type = "bar",
              marker = list(color = ifelse(timing_summary$mean_net < 0, "#2ecc71", "#e74c3c")),
              text = ~paste0("n=", n), textposition = "outside") %>%
        layout(
          title = "Average Government Net Cost by Retreat Timing",
          xaxis = list(title = "Timing Category"),
          yaxis = list(title = "Avg Net Cost per Property ($1000s)<br>(negative = profit)")
        )
    }
  })
  
  output$timing_explanation <- renderUI({
    if (site_type == "CLIFF") {
      HTML("
        <p><strong>Key Pattern (Cliff Site):</strong></p>
        <ul>
          <li><strong>Immediate retreat:</strong> Already within 10m — retreat year = 1, minimal leaseback revenue</li>
          <li><strong>Mid/Late retreat:</strong> Government profits strongly — decades of rent collected before cliff reaches 10m</li>
        </ul>
        <p>Cliff sites have no flood repair costs (NPV Repairs = 0), so the program is especially profitable.
        Government profit grows with retreat horizon since buyout is capped at market price but full rent is collected.</p>
      ")
    } else {
      HTML("
        <p><strong>Key Pattern:</strong></p>
        <ul>
          <li><strong>Immediate retreat:</strong> Government breaks even (buyout ≈ rent + land)</li>
          <li><strong>Late retreat:</strong> Government profits (capped buyout < total revenue)</li>
        </ul>
        <p>Properties with longer time horizons generate more profit because the buyout is capped at market price,
        but government collects decades of rental income.</p>
      ")
    }
  })
  
  # TAB 2: PROPERTY BUYOUTS OUTPUTS
  
  output$total_props_box <- renderInfoBox({
    df <- baseline_data() %>% filter(!is.na(buyout_price))
    
    infoBox(
      "Properties Retreating",
      paste0(nrow(df), " of ", nrow(baseline_data())),
      icon = icon("home"),
      color = "blue"
    )
  })
  
  output$avg_buyout_box <- renderInfoBox({
    df <- baseline_data() %>% filter(!is.na(buyout_price))
    avg <- mean(df$buyout_price, na.rm = TRUE)
    
    infoBox(
      "Avg Buyout",
      paste0("$", round(avg / 1000, 0), "k"),
      icon = icon("dollar-sign"),
      color = "orange"
    )
  })
  
  output$avg_market_box <- renderInfoBox({
    df <- baseline_data() %>% filter(!is.na(buyout_price))
    avg <- mean(df$PRICE, na.rm = TRUE)
    
    infoBox(
      "Avg Market Value",
      paste0("$", round(avg / 1000, 0), "k"),
      icon = icon("tag"),
      color = "purple"
    )
  })
  
  output$total_savings_box <- renderInfoBox({
    df <- baseline_data() %>% filter(!is.na(buyout_price))
    savings <- sum(df$buyout_savings_dollars, na.rm = TRUE)
    
    color <- if(savings > 0) "green" else "red"
    
    infoBox(
      "Total Savings vs Market",
      paste0("$", round(savings / 1e6, 1), "M"),
      icon = icon("piggy-bank"),
      color = color
    )
  })
  
  output$buyout_scatter <- renderPlotly({
    df <- baseline_data() %>% filter(!is.na(buyout_price))
    
    # Add diagonal line data
    max_val <- max(c(df$PRICE, df$buyout_price), na.rm = TRUE)
    
    p <- plot_ly() %>%
      # Main scatter
      add_trace(
        data = df,
        x = ~PRICE/1000, 
        y = ~buyout_price/1000,
        type = "scatter", 
        mode = "markers",
        marker = list(
          size = 8,
          color = ~retreat_year,
          colorscale = "Viridis",
          showscale = TRUE,
          colorbar = list(title = "Retreat<br>Year")
        ),
        text = ~paste0(ADDRESS, "<br>Market: $", round(PRICE/1000), "k<br>Buyout: $", 
                       round(buyout_price/1000), "k<br>T*: ", round(retreat_year, 1)),
        hoverinfo = "text",
        name = "Properties"
      ) %>%
      # Diagonal line
      add_trace(
        x = c(0, max_val/1000), 
        y = c(0, max_val/1000),
        type = "scatter", 
        mode = "lines",
        line = list(dash = "dash", color = "red", width = 2),
        name = "Buyout = Market",
        hoverinfo = "skip",
        showlegend = TRUE
      ) %>%
      layout(
        title = "Buyout Price vs Market Value",
        xaxis = list(title = "Market Value ($1000s)"),
        yaxis = list(title = "Buyout Price ($1000s)"),
        showlegend = TRUE,
        hovermode = "closest"
      )
    
    p
  })
  
  output$buyout_hist <- renderPlotly({
    df <- baseline_data() %>% filter(!is.na(buyout_price))
    
    plot_ly(df, x = ~buyout_price/1000, type = "histogram",
            marker = list(color = "#3498db")) %>%
      layout(
        title = "Distribution of Buyout Prices",
        xaxis = list(title = "Buyout Price ($1000s)"),
        yaxis = list(title = "Number of Properties")
      )
  })
  
  output$buyout_table <- DT::renderDataTable({
    df <- baseline_data() %>% 
      filter(!is.na(buyout_price)) %>%
      arrange(retreat_year) %>%
      select(ADDRESS, PRICE, buyout_price, retreat_year, timing_category,
             gov_net_cost, gov_outcome) %>%
      mutate(
        PRICE = round(PRICE),
        buyout_price = round(buyout_price),
        retreat_year = round(retreat_year, 1),
        gov_net_cost = round(gov_net_cost)
      )
    
    datatable(
      df,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        columnDefs = list(
          list(className = 'dt-right', targets = 1:6)
        )
      ),
      rownames = FALSE
    ) %>%
      formatCurrency(c("PRICE", "buyout_price", "gov_net_cost"), digits = 0) %>%
      formatStyle(
        'gov_outcome',
        backgroundColor = styleEqual(
          c("profit", "break_even", "near_break_even", "net_cost"),
          c("#d4edda", "#fff3cd", "#fff3cd", "#f8d7da")
        )
      )
  })
  
  # TAB 3: SLR SCENARIOS OUTPUTS
  
  output$scenario_retreat_years <- renderPlotly({
    scenario_summary <- baseline_results %>%
      group_by(scenario) %>%
      summarise(
        mean_year = mean(retreat_year, na.rm = TRUE),
        median_year = median(retreat_year, na.rm = TRUE),
        n_retreat = sum(!is.na(retreat_year)),
        .groups = "drop"
      ) %>%
      # Ensure correct ordering
      mutate(scenario = factor(scenario, levels = c("Intermediate", "Intermediate_High", "High"))) %>%
      arrange(scenario)
    
    plot_ly(scenario_summary, x = ~scenario, y = ~median_year, type = "bar",
            marker = list(color = "#3498db"),
            text = ~paste0("n=", n_retreat), textposition = "outside") %>%
      layout(
        title = "Median Retreat Year by SLR Scenario",
        xaxis = list(title = "SLR Scenario", categoryorder = "array",
                     categoryarray = c("Intermediate", "Intermediate_High", "High")),
        yaxis = list(title = "Median Retreat Year")
      )
  })
  
  output$scenario_gov_econ <- renderPlotly({
    if (!has_gov_econ) {
      plot_ly() %>% layout(title = "Government economics not available")
    } else {
      # For cliff sites with multiple scenarios, group by both SLR and cliff scenario
      if (site_type == "CLIFF" && has_cliff_scenarios) {
        scenario_econ <- baseline_results %>%
          filter(!is.na(buyout_price)) %>%
          group_by(scenario, cliff_scenario) %>%
          summarise(
            buyout = sum(buyout_price, na.rm = TRUE) / 1e6,
            rent = sum(leaseback_revenue, na.rm = TRUE) / 1e6,
            # Land has no value at T* (removed from model)
            net = sum(gov_net_cost, na.rm = TRUE) / 1e6,
            .groups = "drop"
          ) %>%
          mutate(
            scenario = factor(scenario, levels = c("Intermediate", "Intermediate_High", "High")),
            x_label = paste0(scenario, "\n", cliff_scenario)
          ) %>%
          arrange(scenario, cliff_scenario)
        
        # Create ordered x-axis labels
        expected_order <- expand.grid(
          scenario = c("Intermediate", "Intermediate_High", "High"),
          cliff_scenario = sort(unique(scenario_econ$cliff_scenario))  # Alphabetical: HoldTheLine, LetItGo
        ) %>%
          mutate(x_label = paste0(scenario, "\n", cliff_scenario)) %>%
          arrange(scenario, cliff_scenario) %>%
          pull(x_label)
        
        plot_ly(scenario_econ, x = ~x_label, color = ~cliff_scenario) %>%
          add_trace(y = ~buyout, name = "Buyout (cost)", type = "bar",
                    marker = list(color = "#e74c3c")) %>%
          add_trace(y = ~rent, name = "Rent (revenue)", type = "bar",
                    marker = list(color = "#3498db")) %>%
          add_trace(y = ~-net, name = "Net Profit", type = "scatter", mode = "lines+markers",
                    line = list(color = "#2ecc71", width = 3),
                    marker = list(size = 10)) %>%
          layout(
            title = "Government Economics by Scenario",
            xaxis = list(
              title = "SLR Scenario / Cliff Management",
              categoryorder = "array",
              categoryarray = expected_order
            ),
            yaxis = list(title = "Amount ($M)"),
            barmode = "group"
          )
      } else {
        # Original logic for non-cliff sites
        scenario_econ <- baseline_results %>%
          filter(!is.na(buyout_price)) %>%
          group_by(scenario) %>%
          summarise(
            buyout = sum(buyout_price, na.rm = TRUE) / 1e6,
            rent = sum(leaseback_revenue, na.rm = TRUE) / 1e6,
            # Land has no value at T* (removed from model)
            net = sum(gov_net_cost, na.rm = TRUE) / 1e6,
            .groups = "drop"
          ) %>%
          mutate(scenario = factor(scenario, levels = c("Intermediate", "Intermediate_High", "High"))) %>%
          arrange(scenario)
        
        plot_ly(scenario_econ, x = ~scenario) %>%
          add_trace(y = ~buyout, name = "Buyout (cost)", type = "bar",
                    marker = list(color = "#e74c3c")) %>%
          add_trace(y = ~rent, name = "Rent (revenue)", type = "bar",
                    marker = list(color = "#3498db")) %>%
          add_trace(y = ~-net, name = "Net Profit", type = "scatter", mode = "lines+markers",
                    line = list(color = "#2ecc71", width = 3),
                    marker = list(size = 10)) %>%
          layout(
            title = "Government Economics by SLR Scenario",
            xaxis = list(title = "SLR Scenario", categoryorder = "array",
                         categoryarray = c("Intermediate", "Intermediate_High", "High")),
            yaxis = list(title = "Amount ($M)"),
            barmode = "group"
          )
      }
    }
  })
  
  output$cumulative_retreat <- renderPlotly({
    # Get baseline only, all scenarios
    cumulative_data <- baseline_results %>%
      filter(!is.na(retreat_year)) %>%
      group_by(scenario) %>%
      arrange(retreat_year) %>%
      mutate(cumulative = row_number()) %>%
      ungroup()
    
    plot_ly(cumulative_data, x = ~retreat_year, y = ~cumulative, color = ~scenario,
            type = "scatter", mode = "lines",
            line = list(width = 3)) %>%
      layout(
        title = "Cumulative Properties Retreating Over Time",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Cumulative Properties")
      )
  })
  
  output$scenario_interpretation <- renderUI({
    if (site_type == "CLIFF") {
      HTML("
        <p><strong>Key Insights (Cliff Site):</strong></p>
        <ul>
          <li><strong>Higher SLR → Faster erosion:</strong> Cliff retreat rates accelerate under higher scenarios</li>
          <li><strong>HoldTheLine vs LetItGo:</strong> Armoring delays but does not prevent eventual retreat</li>
          <li><strong>Government profit decreases with higher SLR:</strong> Less time to collect rent before 10m threshold</li>
        </ul>
        <p>Unlike flood sites, cliff retreat timing is deterministic — there is no storm variability. Uncertainty
        comes from differences in SLR scenario and management approach (armoring vs. natural erosion).</p>
      ")
    } else {
      HTML("
        <p><strong>Key Insights:</strong></p>
        <ul>
          <li><strong>Higher SLR → Earlier retreat:</strong> Properties become unviable sooner</li>
          <li><strong>Higher SLR → More properties retreat:</strong> 100% retreat under High scenario</li>
          <li><strong>Government profit decreases with higher SLR:</strong> Less time to collect rent</li>
        </ul>
        <p>The program remains profitable across all SLR scenarios, but returns diminish as sea level rise accelerates.</p>
      ")
    }
  })
  
  # TAB 4: MONTE CARLO OUTPUTS
  
  output$mc_sims_box <- renderInfoBox({
    if (!has_mc) {
      infoBox("MC Disabled", "No data", icon = icon("times"), color = "red")
    } else {
      df <- baseline_data()
      n_sims <- df$mc_n_sims[1]
      
      infoBox(
        "MC Simulations",
        paste0(n_sims, " per property"),
        icon = icon("dice"),
        color = "blue"
      )
    }
  })
  
  output$mc_uncertainty_box <- renderInfoBox({
    if (!has_mc || site_type == "CLIFF") {
      infoBox("Uncertainty", if(site_type == "CLIFF") "Not applicable (cliff site)" else "N/A", 
              icon = icon("question"), color = if(site_type == "CLIFF") "blue" else "red")
    } else {
      df <- baseline_data() %>% filter(mc_enabled == TRUE)
      avg_sd <- mean(df$mc_sd_year, na.rm = TRUE)
      
      infoBox(
        "Avg Uncertainty (SD)",
        paste0("±", round(avg_sd, 1), " years"),
        icon = icon("chart-bar"),
        color = "yellow"
      )
    }
  })
  
  output$mc_range_box <- renderInfoBox({
    if (!has_mc || site_type == "CLIFF") {
      infoBox("Range", if(site_type == "CLIFF") "Not applicable" else "N/A", 
              icon = icon("question"), color = if(site_type == "CLIFF") "blue" else "red")
    } else {
      df <- baseline_data() %>% filter(mc_enabled == TRUE)
      avg_range <- mean(df$mc_range_years, na.rm = TRUE)
      
      infoBox(
        "Avg 90% CI Width",
        paste0(round(avg_range, 1), " years"),
        icon = icon("arrows-alt-h"),
        color = "purple"
      )
    }
  })
  
  output$mc_confidence_box <- renderInfoBox({
    if (!has_mc || site_type == "CLIFF") {
      infoBox("Confidence", if(site_type == "CLIFF") "Cliff retreat is deterministic" else "N/A",
              icon = icon("info-circle"), color = if(site_type == "CLIFF") "blue" else "red")
    } else {
      df <- baseline_data() %>% filter(mc_enabled == TRUE)
      avg_sd <- mean(df$mc_sd_year, na.rm = TRUE)
      
      # Determine confidence level
      if (avg_sd < 1) {
        level <- "Very High"
        color <- "green"
      } else if (avg_sd < 3) {
        level <- "High"
        color <- "green"
      } else if (avg_sd < 5) {
        level <- "Moderate"
        color <- "yellow"
      } else {
        level <- "Low"
        color <- "red"
      }
      
      infoBox(
        "Confidence Level",
        level,
        icon = icon("check-circle"),
        color = color
      )
    }
  })
  
  output$mc_histogram <- renderPlotly({
    if (!has_mc || is.null(input$mc_property)) {
      plot_ly() %>% layout(title = "Select a property")
    } else {
      df <- baseline_data()
      prop <- df %>% filter(parcel_id == input$mc_property) %>% slice(1)
      
      # Get MC distributions for this property
      mc_prop <- mc_distributions %>%
        filter(parcel_id == input$mc_property,
               scenario == input$slr_scenario)
      
      if (nrow(mc_prop) == 0) {
        plot_ly() %>% layout(title = "No MC data for this property/scenario")
      } else {
        # Calculate statistics directly from the distribution
        mean_val <- mean(mc_prop$retreat_year, na.rm = TRUE)
        median_val <- median(mc_prop$retreat_year, na.rm = TRUE)
        
        plot_ly(mc_prop, x = ~retreat_year, type = "histogram",
                marker = list(color = "#3498db"),
                nbinsx = 30) %>%
          layout(
            title = paste0("MC Distribution: ", prop$ADDRESS),
            xaxis = list(title = "Retreat Year"),
            yaxis = list(title = "Frequency"),
            shapes = list(
              # Mean line
              list(
                type = "line",
                x0 = mean_val,
                x1 = mean_val,
                y0 = 0,
                y1 = 1,
                yref = "paper",
                line = list(color = "red", width = 2, dash = "dash")
              ),
              # Median line
              list(
                type = "line",
                x0 = median_val,
                x1 = median_val,
                y0 = 0,
                y1 = 1,
                yref = "paper",
                line = list(color = "green", width = 2, dash = "dash")
              )
            ),
            annotations = list(
              list(x = mean_val, y = 1, yref = "paper",
                   text = paste0("Mean (", round(mean_val, 1), ")"), 
                   showarrow = FALSE, yshift = 10,
                   font = list(color = "red", size = 10)),
              list(x = median_val, y = 0.9, yref = "paper",
                   text = paste0("Median (", round(median_val, 0), ")"), 
                   showarrow = FALSE, yshift = 10,
                   font = list(color = "green", size = 10))
            )
          )
      }
    }
  })
  
  output$mc_property_stats <- renderUI({
    if (!has_mc || is.null(input$mc_property)) {
      HTML("<p>Select a property to see MC statistics.</p>")
    } else {
      # Get MC distributions for this property
      mc_prop <- mc_distributions %>%
        filter(parcel_id == input$mc_property,
               scenario == input$slr_scenario)
      
      if (nrow(mc_prop) == 0) {
        HTML("<p>No MC data available for this property/scenario.</p>")
      } else {
        # Calculate from actual distribution
        mean_val <- mean(mc_prop$retreat_year, na.rm = TRUE)
        median_val <- median(mc_prop$retreat_year, na.rm = TRUE)
        sd_val <- sd(mc_prop$retreat_year, na.rm = TRUE)
        q05_val <- quantile(mc_prop$retreat_year, 0.05, na.rm = TRUE)
        q95_val <- quantile(mc_prop$retreat_year, 0.95, na.rm = TRUE)
        range_val <- q95_val - q05_val
        
        HTML(paste0(
          "<p><strong>Monte Carlo Statistics:</strong></p>",
          "<ul>",
          "<li>Mean: ", round(mean_val, 1), " years</li>",
          "<li>Median: ", round(median_val, 0), " years</li>",
          "<li>SD: ±", round(sd_val, 1), " years</li>",
          "<li>90% CI: [", round(q05_val, 0), ", ", round(q95_val, 0), "]</li>",
          "<li>Range: ", round(range_val, 0), " years</li>",
          "</ul>"
        ))
      }
    }
  })
  
  output$mc_uncertainty_scatter <- renderPlotly({
    if (!has_mc || site_type == "CLIFF") {
      plot_ly() %>% layout(title = if(site_type == "CLIFF") 
        "Monte Carlo not applicable: cliff retreat timing is deterministic given erosion rates" 
        else "MC data not available")
    } else {
      df <- baseline_data() %>% filter(mc_enabled == TRUE)
      
      plot_ly(df, x = ~retreat_year, y = ~mc_sd_year,
              type = "scatter", mode = "markers",
              marker = list(
                size = 8,
                color = ~timing_category,
                colorscale = "Viridis"
              ),
              text = ~paste0(ADDRESS, "<br>T*: ", round(retreat_year, 1), 
                             "<br>SD: ", round(mc_sd_year, 1)),
              hoverinfo = "text") %>%
        layout(
          title = "Uncertainty vs Retreat Timing",
          xaxis = list(title = "Mean Retreat Year"),
          yaxis = list(title = "Uncertainty (SD in years)")
        )
    }
  })
  
  output$mc_community_stats <- renderUI({
    if (!has_mc || site_type == "CLIFF") {
      HTML(if(site_type == "CLIFF") 
        "<p>Monte Carlo is not applicable for cliff sites. Retreat timing is determined deterministically by the cliff erosion rate reaching the 10m safety threshold.</p>"
        else "<p>MC data not available.</p>")
    } else {
      df <- baseline_data() %>% filter(mc_enabled == TRUE)
      
      HTML(paste0(
        "<p><strong>Community-Wide Statistics:</strong></p>",
        "<ul>",
        "<li>Properties analyzed: ", nrow(df), "</li>",
        "<li>Mean uncertainty: ±", round(mean(df$mc_sd_year, na.rm = TRUE), 2), " years</li>",
        "<li>Mean 90% CI width: ", round(mean(df$mc_range_years, na.rm = TRUE), 2), " years</li>",
        "<li>Max uncertainty: ±", round(max(df$mc_sd_year, na.rm = TRUE), 1), " years</li>",
        "</ul>",
        "<p>Low uncertainty indicates that flood risk signal is strong and consistent across different storm scenarios.</p>"
      ))
    }
  })
  
  # TAB 5: MAP OUTPUTS
  
  output$retreat_map <- renderLeaflet({
    df <- filtered_data()  # Changed from baseline_data() to filtered_data()
    
    # Ensure timing_category is a factor with correct chronological order
    df <- df %>%
      mutate(
        timing_category = factor(
          timing_category,
          levels = c("immediate", "early", "mid_term", "late", "beyond_horizon"),
          ordered = TRUE
        )
      )
    
    # Create color palette - ORDERED BY TIMING (chronological)!
    # immediate (red) → early (orange) → mid_term (yellow) → late (blue) → beyond_horizon (gray)
    pal <- colorFactor(
      palette = c(
        "immediate" = "#d73027",        # Red (urgent!)
        "early" = "#fc8d59",            # Orange  
        "mid_term" = "#fee08b",         # Yellow
        "late" = "#91bfdb",             # Light blue
        "beyond_horizon" = "#999999"    # Gray
      ),
      levels = c("immediate", "early", "mid_term", "late", "beyond_horizon")  # Explicit chronological order
    )
    
    # Create map
    leaflet(df) %>%
      addTiles() %>%
      addCircleMarkers(
        ~LONGITUDE, ~LATITUDE,
        radius = 8,
        color = ~pal(timing_category),
        fillOpacity = 0.8,
        stroke = TRUE,
        weight = 1,
        popup = ~paste0(
          "<strong>", ADDRESS, "</strong><br>",
          "Retreat Year: ", round(retreat_year, 1), "<br>",
          "Category: ", timing_category, "<br>",
          "Market Value: $", format(PRICE, big.mark = ","), "<br>",
          "Buyout: $", format(buyout_price, big.mark = ",")
        ),
        layerId = ~parcel_id
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = ~timing_category,
        title = "Retreat Timing",
        opacity = 1
      )
  })
  
  output$selected_property_info <- renderUI({
    click <- input$retreat_map_marker_click
    
    if (is.null(click)) {
      HTML("<p>Click on a property to see details.</p>")
    } else {
      df <- filtered_data()  # Changed from baseline_data() to filtered_data()
      prop <- df %>% filter(parcel_id == click$id) %>% slice(1)
      
      if (nrow(prop) == 0) {
        HTML("<p>Property not found.</p>")
      } else {
        HTML(paste0(
          "<h4>", prop$ADDRESS, "</h4>",
          "<p><strong>Property Details:</strong></p>",
          "<ul>",
          "<li>Market Value: $", format(prop$PRICE, big.mark = ","), "</li>",
          "<li>Retreat Year: ", round(prop$retreat_year, 1), "</li>",
          "<li>Timing: ", prop$timing_category, "</li>",
          "<li>Trigger: ", prop$retreat_trigger, "</li>",
          "</ul>"
        ))
      }
    }
  })
  
  output$selected_property_econ <- renderUI({
    click <- input$retreat_map_marker_click
    
    if (is.null(click) || !has_gov_econ) {
      HTML("")
    } else {
      df <- filtered_data()  # Changed from baseline_data() to filtered_data()
      prop <- df %>% filter(parcel_id == click$id) %>% slice(1)
      
      if (nrow(prop) == 0 || is.na(prop$buyout_price)) {
        HTML("")
      } else {
        HTML(paste0(
          "<p><strong>Economics:</strong></p>",
          "<ul>",
          "<li>Buyout: $", format(round(prop$buyout_price), big.mark = ","), "</li>",
          "<li>Rent NPV: $", format(round(prop$leaseback_revenue), big.mark = ","), "</li>",
          "<li>Gov Net: $", format(round(prop$gov_net_cost), big.mark = ","), "</li>",
          "<li>Outcome: <strong>", prop$gov_outcome, "</strong></li>",
          "</ul>"
        ))
      }
    }
  })
  
  # NPV timeline / cliff distance plot for selected property
  output$selected_property_npv <- renderPlotly({
    req(input$retreat_map_marker_click)
    
    click <- input$retreat_map_marker_click
    selected_prop <- filtered_data() %>%
      filter(parcel_id == click$id) %>%
      slice(1)
    
    if (nrow(selected_prop) == 0) {
      return(plotly_empty() %>%
               layout(title = list(text = "No property selected", font = list(size = 10))))
    }
    
    # ── CLIFF SITES: show projected cliff distance over time ──────────────────
    if (site_type == "CLIFF") {
      # Load annual hazards to get cliff_dist_m trajectory
      hazards_file <- file.path(DERIVED_DIR, "cosmos_annual_hazards.csv")
      if (!file.exists(hazards_file)) {
        return(plotly_empty() %>%
                 layout(title = list(text = "Annual hazards file not found", font = list(size = 10))))
      }
      
      hazards <- read_csv(hazards_file, show_col_types = FALSE)
      
      cliff_scen <- if (!is.null(input$cliff_scenario)) input$cliff_scenario else selected_prop$cliff_scenario
      
      cliff_ts <- hazards %>%
        filter(
          parcel_id == click$id,
          scenario == input$slr_scenario,
          if ("cliff_scenario" %in% names(hazards)) cliff_scenario == cliff_scen else TRUE
        ) %>%
        arrange(year)
      
      if (nrow(cliff_ts) == 0 || !"cliff_dist_m" %in% names(cliff_ts)) {
        return(plotly_empty() %>%
                 layout(title = list(text = "No cliff distance data available", font = list(size = 10))))
      }
      
      tstar <- selected_prop$retreat_year
      
      p <- plot_ly(cliff_ts, x = ~year, y = ~cliff_dist_m, type = "scatter", mode = "lines",
                   line = list(color = "#e67e22", width = 2),
                   name = "Cliff Distance") %>%
        add_lines(x = range(cliff_ts$year), y = c(10, 10),
                  line = list(color = "red", dash = "dash", width = 1.5),
                  name = "10m Threshold") %>%
        layout(
          title = list(text = "Cliff Distance to Property", font = list(size = 10)),
          xaxis = list(title = "Year", tickfont = list(size = 8)),
          yaxis = list(title = "Distance (m)", tickfont = list(size = 8)),
          legend = list(orientation = "h", y = -0.3, font = list(size = 8)),
          margin = list(l = 50, r = 20, t = 25, b = 60)
        )
      
      # Add T* vertical line if property retreats
      if (!is.na(tstar)) {
        p <- p %>% add_lines(
          x = c(tstar, tstar), y = c(0, max(cliff_ts$cliff_dist_m, na.rm = TRUE)),
          line = list(color = "black", dash = "dot", width = 1.5),
          name = paste0("T*=", round(tstar))
        )
      }
      
      return(p)
    }
    
    # ── FLOOD SITES: NPV curve intersection plot ──────────────────────────────
    hazards_file <- file.path(DERIVED_DIR, "cosmos_annual_hazards.csv")
    if (!file.exists(hazards_file)) {
      return(plotly_empty() %>% 
               layout(title = list(text = "Hazards data not available", font = list(size = 10))))
    }
    
    hazards <- read_csv(hazards_file, show_col_types = FALSE)
    
    if ("mean_annual_damages" %in% names(selected_prop) && 
        !is.null(selected_prop$mean_annual_damages[[1]])) {
      
      annual_damages <- selected_prop$mean_annual_damages[[1]]
      rent <- selected_prop$rent
      discount_rate <- 0.05
      bigT_calc <- length(annual_damages)
      beta <- 1 / (1 + discount_rate)
      beta_powers <- beta^(1:bigT_calc)
      
      npv_r_perpetuity <- rent * (1 + discount_rate) / discount_rate
      npv_r_vec <- rep(npv_r_perpetuity, bigT_calc)
      npv_d_vec <- numeric(bigT_calc)
      
      for (t in 1:bigT_calc) {
        years_remaining <- bigT_calc - t + 1
        npv_known <- sum(annual_damages[t:bigT_calc] * beta_powers[1:years_remaining])
        final_damage <- annual_damages[bigT_calc]
        discount_to_T_hat <- beta^years_remaining
        npv_perpetual <- (final_damage / discount_rate) * discount_to_T_hat
        npv_d_vec[t] <- npv_known + npv_perpetual
      }
      
      npv_data <- tibble(year = 1:bigT_calc, npv_rent = npv_r_vec, npv_damage = npv_d_vec)
      
    } else {
      npv_data <- calculate_npv_components_property(
        parcel_id = selected_prop$parcel_id,
        scenario = selected_prop$scenario,
        hazards_data = hazards,
        baseline_data = filtered_data(),
        bigT = 75
      )
    }
    
    if (is.null(npv_data)) {
      return(plotly_empty() %>% 
               layout(title = list(text = "NPV data not available", font = list(size = 10))))
    }
    
    npv_long <- npv_data %>%
      pivot_longer(cols = c(npv_rent, npv_damage), names_to = "component", values_to = "value") %>%
      mutate(component = recode(component,
                                npv_rent = "NPV(Perpetual Rents)",
                                npv_damage = "NPV(Damages + Tail)"))
    
    p <- ggplot(npv_long, aes(x = year, y = value, color = component)) +
      geom_line(size = 1.2) +
      labs(x = "Year", y = "NPV ($)", title = "T* = Intersection of NPV Curves") +
      scale_y_continuous(labels = dollar_format()) +
      scale_color_manual(values = c("NPV(Perpetual Rents)" = "#27ae60", "NPV(Damages + Tail)" = "#c0392b")) +
      theme_minimal(base_size = 9) +
      theme(legend.position = "bottom", legend.text = element_text(size = 8),
            axis.title = element_text(size = 9), axis.text = element_text(size = 8),
            plot.title = element_text(size = 10, hjust = 0.5))
    
    if (!is.na(selected_prop$retreat_year)) {
      p <- p + geom_vline(xintercept = selected_prop$retreat_year,
                          linetype = "dashed", color = "black", alpha = 0.6)
      tstar <- selected_prop$retreat_year
      if (tstar > 50) {
        p <- p + coord_cartesian(xlim = c(max(0, tstar - 20), min(74, tstar + 10)))
      }
    }
    
    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(legend = list(orientation = "h", y = -0.25, x = 0.5, xanchor = "center"),
             margin = list(l = 50, r = 20, t = 20, b = 60))
  })
  
  # RESET BUTTON
  
  observeEvent(input$reset, {
    updateSliderInput(session, "discount_rate_gov", value = 2)
    updateSelectInput(session, "slr_scenario", selected = scenarios[1])  # Intermediate
  })
}


# RUN APP
shinyApp(ui, server)
