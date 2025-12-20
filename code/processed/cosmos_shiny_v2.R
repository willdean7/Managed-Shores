# cosmos_shiny_v2.R
# Purpose: Interactive Shiny dashboard for exploring retreat timing under different parameters
# Author: Will Dean (Managed Shores)
#
# This app allows users to:
# - Adjust model parameters (rental yield, discount rate, storm scenario, etc.)
# - See real-time updates to retreat timing and property-level results
# - Compare baseline vs custom parameter scenarios
# - Visualize spatial distribution of retreat timing
#
# To run: source("cosmos_interactive_app.R")

rm(list = ls())
library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)
library(plotly)
library(dplyr)
library(ggplot2)
library(scales)
library(readr)
library(sf)


# LOAD DATA
case_name <- "pacifica"
data_dir <- file.path("data", case_name)
derived_dir <- file.path(data_dir, "derived")

# Load baseline and sensitivity results
baseline_results <- read_csv(
  file.path(derived_dir, "retreat_schedule_baseline.csv"),
  show_col_types = FALSE
)

sensitivity_results <- read_csv(
  file.path(derived_dir, "retreat_schedule_sensitivity.csv"),
  show_col_types = FALSE
)

# Combine for easy filtering
all_results <- bind_rows(
  baseline_results,
  sensitivity_results
)

# Add elevation if missing (for backwards compatibility)
if (!"elevation" %in% names(all_results)) {
  all_results$elevation <- NA_real_
  message("Note: 'elevation' column not found in data - using NA")
}

# Create spatial version for mapping
results_sf <- all_results %>%
  filter(!is.na(LATITUDE), !is.na(LONGITUDE)) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326, remove = FALSE)

# Parameter ranges
rental_yields <- c(0.04, 0.05, 0.06)
discount_rates <- c(0.015, 0.02, 0.03)
damage_thresholds <- c(0.0, 0.15, 0.30)
owner_utilities <- c(0, 25000, 50000)
slr_scenarios <- c("Intermediate", "Intermediate_High", "High")

# Check if cliff scenarios exist in data
cliff_scenarios <- unique(all_results$cliff_scenario)
has_cliff_scenarios <- !all(is.na(cliff_scenarios))
if (has_cliff_scenarios) {
  cliff_scenarios <- cliff_scenarios[!is.na(cliff_scenarios)]
} else {
  cliff_scenarios <- c("Not Applicable")
}


# UI
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = "Managed Shores: Retreat Parameter Explorer",
    titleWidth = 400
  ),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Parameter Explorer", tabName = "explorer", icon = icon("sliders")),
      menuItem("Spatial Comparison", tabName = "map", icon = icon("map")),
      menuItem("Summary Statistics", tabName = "stats", icon = icon("chart-bar")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    ),
    hr(),
    h4("Model Parameters", style = "padding-left: 15px; font-weight: bold;"),
    
    selectInput(
      "slr_scenario",
      "SLR Scenario:",
      choices = slr_scenarios,
      selected = "High"
    ),
    
    sliderInput(
      "rental_yield",
      "Rental Yield (%):",
      min = 4,
      max = 6,
      value = 5,
      step = 1,
      post = "%"
    ),
    
    sliderInput(
      "discount_rate",
      "Discount Rate (%):",
      min = 1.5,
      max = 3.0,
      value = 2.0,
      step = 0.5,
      post = "%"
    ),
    
    sliderInput(
      "damage_threshold",
      "Damage Threshold (cm):",
      min = 0,
      max = 30,
      value = 15,
      step = 15
    ),
    
    sliderInput(
      "owner_utility",
      "Owner Utility ($/year):",
      min = 0,
      max = 50000,
      value = 0,
      step = 25000,
      pre = "$"
    ),
    
    # Only show cliff scenario selector if cliff scenarios exist
    conditionalPanel(
      condition = "output.has_cliff_scenarios",
      selectInput(
        "cliff_scenario",
        "Cliff Management:",
        choices = NULL,  # Will be updated in server
        selected = NULL
      )
    ),
    
    hr(),
    actionButton("reset", "Reset to Baseline", icon = icon("undo"), 
                 style = "width: 90%; margin-left: 15px;")
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .value-box { height: 100px; }
        .small-box .icon-large { font-size: 60px; }
      "))
    ),
    
    tabItems(
      # TAB 1: Parameter Explorer
      tabItem(
        tabName = "explorer",
        fluidRow(
          valueBoxOutput("median_retreat_box", width = 3),
          valueBoxOutput("immediate_retreat_box", width = 3),
          valueBoxOutput("total_properties_box", width = 3),
          valueBoxOutput("buyout_cost_box", width = 3)
        ),
        
        fluidRow(
          box(
            title = "Retreat Timeline Distribution",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("timing_dist_plot", height = "350px")
          ),
          box(
            title = "Comparison with Baseline",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("baseline_comparison_plot", height = "350px")
          )
        ),
        
        fluidRow(
          box(
            title = "Property-Level Results",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            DT::dataTableOutput("property_table")
          )
        )
      ),
      
      # TAB 2: Spatial Map
      tabItem(
        tabName = "map",
        fluidRow(
          box(
            title = "Spatial Distribution of Retreat Timing",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            leafletOutput("retreat_map", height = "600px")
          )
        ),
        fluidRow(
          box(
            title = "Selected Parameters",
            status = "info",
            width = 6,
            verbatimTextOutput("param_summary")
          ),
          box(
            title = "Map Legend",
            status = "info",
            width = 6,
            HTML("
              <p><strong>Retreat Timing Categories:</strong></p>
              <ul>
                <li><span style='color: #d73027;'>●</span> Immediate (0-10 years)</li>
                <li><span style='color: #fc8d59;'>●</span> Early (11-25 years)</li>
                <li><span style='color: #fee08b;'>●</span> Mid-term (26-50 years)</li>
                <li><span style='color: #91bfdb;'>●</span> Late (51-100 years)</li>
                <li><span style='color: #4575b4;'>●</span> No Retreat (>100 years)</li>
              </ul>
            ")
          )
        )
      ),
      
      # TAB 3: Summary Statistics
      tabItem(
        tabName = "stats",
        fluidRow(
          box(
            title = "Summary by Timing Category",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            DT::dataTableOutput("summary_table")
          )
        ),
        fluidRow(
          box(
            title = "Histogram: Retreat Year Distribution",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("retreat_histogram", height = "350px")
          ),
          box(
            title = "Economic Impact by Category",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("economic_impact_plot", height = "350px")
          )
        ),
        fluidRow(
          box(
            title = "Buyout Cost Analysis",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("buyout_comparison_plot", height = "350px")
          )
        )
      ),
      
      # TAB 4: About
      tabItem(
        tabName = "about",
        fluidRow(
          box(
            title = "About This Tool",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            h3("Managed Shores Retreat Parameter Explorer"),
            p("This interactive tool allows you to explore how different model parameters affect 
              optimal retreat timing for coastal properties in Carpinteria, California."),
            
            h4("Model Overview"),
            p("The retreat timing model calculates the optimal year T* for each property by comparing:"),
            tags$ul(
              tags$li("NPV of rental income from staying"),
              tags$li("NPV of expected flood damages"),
              tags$li("Owner utility (non-financial value of coastal living)")
            ),
            p("Properties should retreat when NPV(staying) ≤ 0."),
            
            h4("Parameters"),
            tags$ul(
              tags$li(strong("SLR Scenario:"), " OPC 2024 projections (Intermediate, Intermediate-High, High)"),
              tags$li(strong("Rental Yield:"), " Annual rent as % of property value (4-6%)"),
              tags$li(strong("Discount Rate:"), " Rate for calculating present value (1.5-3%)"),
              tags$li(strong("Damage Threshold:"), " Minimum flood depth to count as damage (0-30cm)"),
              tags$li(strong("Owner Utility:"), " Annual non-financial value of living there ($0-50K)")
            ),
            
            conditionalPanel(
              condition = "output.has_cliff_scenarios",
              tags$ul(
                tags$li(strong("Cliff Management:"), " Let It Go (natural erosion) vs Hold The Line (armoring)")
              )
            ),
            
            tags$div(
              class = "alert alert-info",
              style = "background-color: #d9edf7; border-color: #bce8f1; padding: 15px; margin: 15px 0; border-radius: 4px;",
              h4(icon("info-circle"), " Parameter Exploration", style = "margin-top: 0;"),
              p(strong("This tool supports complete parameter exploration:"), 
                " You can adjust ANY combination of parameters and see the results immediately."),
              p(strong("How it works:"), " The model has been run for multiple parameter combinations ",
                "(3 rental yields × 3 discount rates × 3 thresholds × 3 utilities × 3 SLR scenarios), ",
                "allowing you to explore the parameter space interactively."),
              p(strong("Example combinations to try:"), br(),
                "• Conservative: 4% yield + 3% discount (lower retreat likelihood)", br(),
                "• Aggressive: 6% yield + 1.5% discount (higher retreat likelihood)", br(),
                "• With utility: Any combo + $50K utility (delays retreat)")
            ),
            
            h4("Data Sources"),
            tags$ul(
              tags$li("CoSMoS flood projections (USGS)"),
              tags$li("OPC 2024 sea level rise scenarios"),
              tags$li("Redfin property data"),
              tags$li("California land values by ZIP code")
            ),
            
            h4("Citation"),
            p("Dean, W. (2025). Managed Shores: Economic Analysis of Coastal Retreat Timing. 
              [Working Paper]")
          )
        )
      )
    )
  )
)

# SERVER

server <- function(input, output, session) {
  
  # Initialize cliff scenario selector
  if (has_cliff_scenarios) {
    updateSelectInput(session, "cliff_scenario", 
                      choices = cliff_scenarios,
                      selected = cliff_scenarios[1])
  }
  
  # Output to control conditional panel
  output$has_cliff_scenarios <- reactive({ has_cliff_scenarios })
  outputOptions(output, "has_cliff_scenarios", suspendWhenHidden = FALSE)
  
  # Reactive: Get filtered data based on current parameters
  current_data <- reactive({
    data <- all_results %>%
      filter(
        scenario == input$slr_scenario,
        rental_yield == input$rental_yield / 100,
        discount_rate == input$discount_rate / 100,
        damage_threshold == input$damage_threshold / 100,
        owner_utility == input$owner_utility
      )
    
    # Filter by cliff scenario if applicable
    if (has_cliff_scenarios && !is.null(input$cliff_scenario)) {
      data <- data %>% filter(cliff_scenario == input$cliff_scenario)
    }
    
    data
  })
  
  # Reactive: Track current parameter combination for display
  current_param_label <- reactive({
    if (has_cliff_scenarios && !is.null(input$cliff_scenario)) {
      sprintf(
        "%.0f%% yield, %.1f%% discount, %.0fcm threshold, $%s utility, %s",
        input$rental_yield,
        input$discount_rate,
        input$damage_threshold,
        format(input$owner_utility, big.mark = ","),
        input$cliff_scenario
      )
    } else {
      sprintf(
        "%.0f%% yield, %.1f%% discount, %.0fcm threshold, $%s utility",
        input$rental_yield,
        input$discount_rate,
        input$damage_threshold,
        format(input$owner_utility, big.mark = ",")
      )
    }
  })
  
  # Reactive: Baseline data for comparison
  baseline_data <- reactive({
    data <- baseline_results %>%
      filter(scenario == input$slr_scenario)
    
    # Filter by cliff scenario if applicable
    if (has_cliff_scenarios && !is.null(input$cliff_scenario)) {
      data <- data %>% filter(cliff_scenario == input$cliff_scenario)
    }
    
    data
  })
  
  # Reset button
  observeEvent(input$reset, {
    updateSliderInput(session, "rental_yield", value = 5)
    updateSliderInput(session, "discount_rate", value = 2.0)
    updateSliderInput(session, "damage_threshold", value = 15)
    updateSliderInput(session, "owner_utility", value = 0)
    
    # Reset cliff scenario to first option if applicable
    if (has_cliff_scenarios) {
      updateSelectInput(session, "cliff_scenario", selected = cliff_scenarios[1])
    }
  })
  
  # Value Boxes
  output$median_retreat_box <- renderValueBox({
    med_year <- median(current_data()$retreat_year[current_data()$retreat_year <= 100], 
                       na.rm = TRUE)
    valueBox(
      value = ifelse(is.na(med_year), "N/A", round(med_year)),
      subtitle = "Median Retreat Year",
      icon = icon("calendar"),
      color = "blue"
    )
  })
  
  output$immediate_retreat_box <- renderValueBox({
    immediate_pct <- mean(current_data()$retreat_year <= 10, na.rm = TRUE) * 100
    valueBox(
      value = paste0(round(immediate_pct, 1), "%"),
      subtitle = "Immediate Retreat (≤10 yrs)",
      icon = icon("exclamation-triangle"),
      color = if(immediate_pct > 5) "red" else if(immediate_pct > 0) "yellow" else "green"
    )
  })
  
  output$total_properties_box <- renderValueBox({
    n_retreat <- sum(current_data()$retreat_year <= 100, na.rm = TRUE)
    valueBox(
      value = n_retreat,
      subtitle = "Properties Requiring Retreat",
      icon = icon("home"),
      color = "purple"
    )
  })
  
  output$buyout_cost_box <- renderValueBox({
    data <- current_data() %>% filter(retreat_year <= 100)
    
    # Check if buyout_price column exists
    if ("buyout_price" %in% names(data) && nrow(data) > 0) {
      total_buyout <- sum(data$buyout_price, na.rm = TRUE)
      total_market <- sum(data$PRICE, na.rm = TRUE)
      
      if (total_market > 0) {
        savings <- total_market - total_buyout
        discount_pct <- round(100 * savings / total_market, 1)
        
        valueBox(
          value = paste0("$", round(total_buyout / 1e6, 1), "M"),
          subtitle = paste0("Buyout Cost (", discount_pct, "% discount)"),
          icon = icon("money-bill-wave"),
          color = if(discount_pct > 10) "green" else if(discount_pct > 0) "light-blue" else "orange"
        )
      } else {
        valueBox(
          value = "$0M",
          subtitle = "Buyout Cost",
          icon = icon("money-bill-wave"),
          color = "gray"
        )
      }
    } else {
      # Fallback to market value if buyout not calculated
      total_value <- sum(data$PRICE, na.rm = TRUE)
      valueBox(
        value = paste0("$", round(total_value / 1e6), "M"),
        subtitle = "Property Value at Risk",
        icon = icon("dollar-sign"),
        color = "orange"
      )
    }
  })
  
  # Timing Distribution Plot
  output$timing_dist_plot <- renderPlotly({
    data <- current_data() %>%
      mutate(
        category = case_when(
          retreat_year <= 10 ~ "Immediate (0-10)",
          retreat_year <= 25 ~ "Early (11-25)",
          retreat_year <= 50 ~ "Mid-term (26-50)",
          retreat_year <= 100 ~ "Late (51-100)",
          TRUE ~ "No Retreat (>100)"
        ),
        category = factor(category, levels = c(
          "Immediate (0-10)", "Early (11-25)", "Mid-term (26-50)", 
          "Late (51-100)", "No Retreat (>100)"
        ))
      ) %>%
      count(category) %>%
      mutate(percentage = n / sum(n) * 100)
    
    p <- ggplot(data, aes(x = category, y = percentage, fill = category)) +
      geom_col() +
      scale_fill_manual(values = c(
        "Immediate (0-10)" = "#d73027",
        "Early (11-25)" = "#fc8d59",
        "Mid-term (26-50)" = "#fee08b",
        "Late (51-100)" = "#91bfdb",
        "No Retreat (>100)" = "#4575b4"
      ), guide = "none") +
      labs(x = NULL, y = "Percentage of Properties (%)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # Baseline Comparison Plot
  output$baseline_comparison_plot <- renderPlotly({
    current_med <- median(current_data()$retreat_year[current_data()$retreat_year <= 100], 
                          na.rm = TRUE)
    baseline_med <- median(baseline_data()$retreat_year[baseline_data()$retreat_year <= 100], 
                           na.rm = TRUE)
    
    comparison <- data.frame(
      scenario = c("Baseline", "Current"),
      median_year = c(baseline_med, current_med)
    )
    
    p <- ggplot(comparison, aes(x = scenario, y = median_year, fill = scenario)) +
      geom_col(width = 0.6) +
      scale_fill_manual(values = c("Baseline" = "#1f77b4", "Current" = "#ff7f0e"), guide = "none") +
      geom_text(aes(label = round(median_year, 1)), vjust = -0.5, size = 5) +
      labs(x = NULL, y = "Median Retreat Year") +
      ylim(0, max(c(baseline_med, current_med), na.rm = TRUE) * 1.15) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Property Table
  output$property_table <- DT::renderDataTable({
    data <- current_data()
    
    # Check if elevation column exists and has values
    has_elevation <- "elevation" %in% names(data) && any(!is.na(data$elevation))
    
    # Check if cliff distance exists
    has_cliff_dist <- "initial_cliff_dist" %in% names(data) && any(!is.na(data$initial_cliff_dist))
    
    # Check if buyout columns exist
    if ("buyout_price" %in% names(data) && any(!is.na(data$buyout_price))) {
      result <- data %>%
        filter(retreat_year <= 100) %>%  # Only show properties that retreat
        mutate(
          # Calculate discount BEFORE formatting
          discount_pct = ifelse(!is.na(buyout_price) & !is.na(PRICE) & PRICE > 0,
                                100 * (PRICE - buyout_price) / PRICE,
                                NA),
          # Now format for display
          PRICE_display = scales::dollar(PRICE),
          buyout_display = scales::dollar(buyout_price),
          discount_display = ifelse(!is.na(discount_pct),
                                    paste0(round(discount_pct, 1), "%"),
                                    "N/A")
        )
      
      # Build column selection based on available data
      select_cols <- c("ADDRESS", "PRICE_display", "retreat_year", "buyout_display", 
                       "discount_display", "retreat_trigger")
      rename_map <- c(
        Address = "ADDRESS",
        `Market Price` = "PRICE_display",
        `Retreat Year` = "retreat_year",
        `Buyout Price` = "buyout_display",
        `Discount` = "discount_display",
        `Trigger` = "retreat_trigger"
      )
      
      if (has_cliff_dist) {
        result <- result %>% mutate(cliff_dist_display = round(initial_cliff_dist, 1))
        select_cols <- c(select_cols, "cliff_dist_display")
        rename_map <- c(rename_map, `Cliff Dist (m)` = "cliff_dist_display")
      }
      
      if (has_elevation) {
        result <- result %>% mutate(elevation_display = round(elevation, 2))
        select_cols <- c(select_cols, "elevation_display")
        rename_map <- c(rename_map, `Elevation (m)` = "elevation_display")
      }
      
      result %>%
        select(all_of(select_cols)) %>%
        rename(all_of(rename_map))
    } else {
      result <- data %>%
        filter(retreat_year <= 100) %>%
        mutate(PRICE_display = scales::dollar(PRICE))
      
      select_cols <- c("ADDRESS", "PRICE_display", "retreat_year", "retreat_trigger")
      rename_map <- c(
        Address = "ADDRESS",
        Price = "PRICE_display",
        `Retreat Year` = "retreat_year",
        `Trigger` = "retreat_trigger"
      )
      
      if (has_cliff_dist) {
        result <- result %>% mutate(cliff_dist_display = round(initial_cliff_dist, 1))
        select_cols <- c(select_cols, "cliff_dist_display")
        rename_map <- c(rename_map, `Cliff Dist (m)` = "cliff_dist_display")
      }
      
      if (has_elevation) {
        result <- result %>% mutate(elevation_display = round(elevation, 2))
        select_cols <- c(select_cols, "elevation_display")
        rename_map <- c(rename_map, `Elevation (m)` = "elevation_display")
      }
      
      result %>%
        select(all_of(select_cols)) %>%
        rename(all_of(rename_map))
    }
  }, options = list(pageLength = 10, scrollX = TRUE))
  
  # Map
  output$retreat_map <- renderLeaflet({
    # Use current_data() which already has correct filtering
    data_sf <- current_data() %>%
      filter(!is.na(LATITUDE), !is.na(LONGITUDE)) %>%
      st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326, remove = FALSE) %>%
      mutate(
        color = case_when(
          retreat_year <= 10 ~ "#d73027",
          retreat_year <= 25 ~ "#fc8d59",
          retreat_year <= 50 ~ "#fee08b",
          retreat_year <= 100 ~ "#91bfdb",
          TRUE ~ "#4575b4"
        )
      )
    
    # Build popup with conditional buyout info
    if ("buyout_price" %in% names(data_sf) && any(!is.na(data_sf$buyout_price))) {
      # Create buyout info for each property
      buyout_info <- sapply(1:nrow(data_sf), function(i) {
        if (!is.na(data_sf$buyout_price[i]) && !is.na(data_sf$PRICE[i])) {
          savings <- data_sf$PRICE[i] - data_sf$buyout_price[i]
          savings_pct <- 100 * savings / data_sf$PRICE[i]
          paste0("Buyout Cost: $", format(round(data_sf$buyout_price[i]), big.mark = ","), "<br>",
                 "Savings: $", format(round(savings), big.mark = ","), 
                 " (", round(savings_pct, 1), "%)<br>")
        } else {
          ""
        }
      })
      
      # Add cliff distance if available
      cliff_info <- if ("initial_cliff_dist" %in% names(data_sf) && any(!is.na(data_sf$initial_cliff_dist))) {
        paste0("Cliff Distance: ", round(data_sf$initial_cliff_dist, 1), "m<br>")
      } else {
        ""
      }
      
      # Add elevation if available
      elev_info <- if ("elevation" %in% names(data_sf) && any(!is.na(data_sf$elevation))) {
        paste0("Elevation: ", round(data_sf$elevation, 2), "m")
      } else {
        ""
      }
      
      popup_text <- paste0(
        "<b>", data_sf$ADDRESS, "</b><br>",
        "Retreat Year: ", data_sf$retreat_year, "<br>",
        "Market Price: $", format(data_sf$PRICE, big.mark = ","), "<br>",
        buyout_info,
        cliff_info,
        elev_info
      )
    } else {
      # Add cliff distance if available
      cliff_info <- if ("initial_cliff_dist" %in% names(data_sf) && any(!is.na(data_sf$initial_cliff_dist))) {
        paste0("Cliff Distance: ", round(data_sf$initial_cliff_dist, 1), "m<br>")
      } else {
        ""
      }
      
      # Add elevation if available
      elev_info <- if ("elevation" %in% names(data_sf) && any(!is.na(data_sf$elevation))) {
        paste0("Elevation: ", round(data_sf$elevation, 2), "m")
      } else {
        ""
      }
      
      popup_text <- paste0(
        "<b>", data_sf$ADDRESS, "</b><br>",
        "Retreat Year: ", data_sf$retreat_year, "<br>",
        "Price: $", format(data_sf$PRICE, big.mark = ","), "<br>",
        cliff_info,
        elev_info
      )
    }
    
    leaflet(data_sf) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        radius = 6,
        color = ~color,
        fillColor = ~color,
        fillOpacity = 0.7,
        weight = 1,
        popup = popup_text
      ) %>%
      addLegend(
        position = "bottomright",
        colors = c("#d73027", "#fc8d59", "#fee08b", "#91bfdb", "#4575b4"),
        labels = c("Immediate", "Early", "Mid-term", "Late", "No Retreat"),
        title = "Retreat Timing"
      )
  })
  
  # Parameter Summary
  output$param_summary <- renderText({
    paste0(
      "SLR Scenario: ", input$slr_scenario, "\n",
      "Rental Yield: ", input$rental_yield, "%\n",
      "Discount Rate: ", input$discount_rate, "%\n",
      "Damage Threshold: ", input$damage_threshold, " cm\n",
      "Owner Utility: $", format(input$owner_utility, big.mark = ","), "/year\n",
      "Storm Scenario: ", input$storm_scenario
    )
  })
  
  # Summary Table
  output$summary_table <- DT::renderDataTable({
    data <- current_data() %>%
      filter(retreat_year <= 100) %>%  # Only include properties that retreat
      mutate(
        category = case_when(
          retreat_year <= 10 ~ "Immediate (0-10)",
          retreat_year <= 25 ~ "Early (11-25)",
          retreat_year <= 50 ~ "Mid-term (26-50)",
          retreat_year <= 100 ~ "Late (51-100)",
          TRUE ~ "No Retreat"
        ),
        category = factor(category, levels = c(
          "Immediate (0-10)", "Early (11-25)", "Mid-term (26-50)", "Late (51-100)"
        ))
      )
    
    # Check if we have any data
    if (nrow(data) == 0) {
      return(data.frame(
        `Retreat Category` = "No properties retreat",
        `Number of Properties` = 0,
        check.names = FALSE
      ))
    }
    
    # Check for elevation
    has_elevation <- "elevation" %in% names(data) && any(!is.na(data$elevation))
    
    # Check if buyout columns exist
    if ("buyout_price" %in% names(data) && any(!is.na(data$buyout_price))) {
      if (has_elevation) {
        summary_df <- data %>%
          group_by(category, .drop = FALSE) %>%
          summarise(
            `Number of Properties` = n(),
            `Avg Property Value` = mean(PRICE, na.rm = TRUE),
            `Total Market Value` = sum(PRICE, na.rm = TRUE),
            `Total Buyout Cost` = sum(buyout_price, na.rm = TRUE),
            `Total Savings` = sum(PRICE - buyout_price, na.rm = TRUE),
            `Avg Discount %` = 100 * mean((PRICE - buyout_price) / PRICE, na.rm = TRUE),
            `Avg Elevation (m)` = mean(elevation, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          filter(`Number of Properties` > 0) %>%
          mutate(
            `Avg Property Value` = scales::dollar(`Avg Property Value`),
            `Total Market Value` = scales::dollar(`Total Market Value`),
            `Total Buyout Cost` = scales::dollar(`Total Buyout Cost`),
            `Total Savings` = scales::dollar(`Total Savings`),
            `Avg Discount %` = paste0(round(`Avg Discount %`, 1), "%"),
            `Avg Elevation (m)` = round(`Avg Elevation (m)`, 2)
          ) %>%
          rename(`Retreat Category` = category)
      } else {
        summary_df <- data %>%
          group_by(category, .drop = FALSE) %>%
          summarise(
            `Number of Properties` = n(),
            `Avg Property Value` = mean(PRICE, na.rm = TRUE),
            `Total Market Value` = sum(PRICE, na.rm = TRUE),
            `Total Buyout Cost` = sum(buyout_price, na.rm = TRUE),
            `Total Savings` = sum(PRICE - buyout_price, na.rm = TRUE),
            `Avg Discount %` = 100 * mean((PRICE - buyout_price) / PRICE, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          filter(`Number of Properties` > 0) %>%
          mutate(
            `Avg Property Value` = scales::dollar(`Avg Property Value`),
            `Total Market Value` = scales::dollar(`Total Market Value`),
            `Total Buyout Cost` = scales::dollar(`Total Buyout Cost`),
            `Total Savings` = scales::dollar(`Total Savings`),
            `Avg Discount %` = paste0(round(`Avg Discount %`, 1), "%")
          ) %>%
          rename(`Retreat Category` = category)
      }
      
      return(summary_df)
    } else {
      if (has_elevation) {
        summary_df <- data %>%
          group_by(category, .drop = FALSE) %>%
          summarise(
            `Number of Properties` = n(),
            `Avg Property Value` = mean(PRICE, na.rm = TRUE),
            `Total Value at Risk` = sum(PRICE, na.rm = TRUE),
            `Avg Elevation (m)` = mean(elevation, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          filter(`Number of Properties` > 0) %>%
          mutate(
            `Avg Property Value` = scales::dollar(`Avg Property Value`),
            `Total Value at Risk` = scales::dollar(`Total Value at Risk`),
            `Avg Elevation (m)` = round(`Avg Elevation (m)`, 2)
          ) %>%
          rename(`Retreat Category` = category)
      } else {
        summary_df <- data %>%
          group_by(category, .drop = FALSE) %>%
          summarise(
            `Number of Properties` = n(),
            `Avg Property Value` = mean(PRICE, na.rm = TRUE),
            `Total Value at Risk` = sum(PRICE, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          filter(`Number of Properties` > 0) %>%
          mutate(
            `Avg Property Value` = scales::dollar(`Avg Property Value`),
            `Total Value at Risk` = scales::dollar(`Total Value at Risk`)
          ) %>%
          rename(`Retreat Category` = category)
      }
      
      return(summary_df)
    }
  }, options = list(pageLength = 10, dom = 't', ordering = FALSE))
  
  # Retreat Histogram
  output$retreat_histogram <- renderPlotly({
    p <- ggplot(current_data() %>% filter(retreat_year <= 100), 
                aes(x = retreat_year)) +
      geom_histogram(binwidth = 5, fill = "#1f77b4", alpha = 0.7) +
      labs(x = "Retreat Year", y = "Number of Properties") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Economic Impact Plot
  output$economic_impact_plot <- renderPlotly({
    data <- current_data() %>%
      mutate(
        category = case_when(
          retreat_year <= 10 ~ "Immediate (0-10)",
          retreat_year <= 25 ~ "Early (11-25)",
          retreat_year <= 50 ~ "Mid-term (26-50)",
          retreat_year <= 100 ~ "Late (51-100)",
          TRUE ~ "No Retreat (>100)"
        ),
        category = factor(category, levels = c(
          "Immediate (0-10)", "Early (11-25)", "Mid-term (26-50)", 
          "Late (51-100)", "No Retreat (>100)"
        ))
      ) %>%
      group_by(category) %>%
      summarise(total_value = sum(PRICE, na.rm = TRUE) / 1e6, .groups = "drop")
    
    p <- ggplot(data, aes(x = category, y = total_value, fill = category)) +
      geom_col() +
      scale_fill_manual(values = c(
        "Immediate (0-10)" = "#d73027",
        "Early (11-25)" = "#fc8d59",
        "Mid-term (26-50)" = "#fee08b",
        "Late (51-100)" = "#91bfdb",
        "No Retreat (>100)" = "#4575b4"
      ), guide = "none") +
      labs(x = NULL, y = "Total Property Value ($ Millions)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # Buyout Comparison Plot
  output$buyout_comparison_plot <- renderPlotly({
    data <- current_data() %>%
      filter(retreat_year <= 100) %>%
      mutate(
        category = case_when(
          retreat_year <= 10 ~ "Immediate\n(0-10)",
          retreat_year <= 25 ~ "Early\n(11-25)",
          retreat_year <= 50 ~ "Mid-term\n(26-50)",
          retreat_year <= 100 ~ "Late\n(51-100)",
          TRUE ~ "No Retreat"
        ),
        category = factor(category, levels = c(
          "Immediate\n(0-10)", "Early\n(11-25)", "Mid-term\n(26-50)", 
          "Late\n(51-100)"
        ))
      )
    
    # Check if buyout columns exist
    if ("buyout_price" %in% names(data) && nrow(data) > 0) {
      summary_data <- data %>%
        group_by(category) %>%
        summarise(
          n_properties = n(),
          market_value = sum(PRICE, na.rm = TRUE) / 1e6,
          buyout_cost = sum(buyout_price, na.rm = TRUE) / 1e6,
          savings = market_value - buyout_cost,
          .groups = "drop"
        ) %>%
        tidyr::pivot_longer(
          cols = c(market_value, buyout_cost),
          names_to = "type",
          values_to = "value"
        ) %>%
        mutate(
          type = factor(type, 
                        levels = c("market_value", "buyout_cost"),
                        labels = c("Market Value", "Buyout Cost"))
        )
      
      p <- ggplot(summary_data, aes(x = category, y = value, fill = type)) +
        geom_col(position = "dodge", alpha = 0.8) +
        scale_fill_manual(values = c("Market Value" = "#e41a1c", "Buyout Cost" = "#377eb8")) +
        labs(
          x = "Retreat Timing Category",
          y = "Total Value ($ Millions)",
          fill = NULL,
          title = "Buyout Cost vs Market Value by Timing"
        ) +
        theme_minimal() +
        theme(
          legend.position = "top",
          axis.text.x = element_text(size = 9)
        )
    } else {
      # Fallback if no buyout data
      summary_data <- data %>%
        group_by(category) %>%
        summarise(
          market_value = sum(PRICE, na.rm = TRUE) / 1e6,
          .groups = "drop"
        )
      
      p <- ggplot(summary_data, aes(x = category, y = market_value)) +
        geom_col(fill = "#e41a1c", alpha = 0.8) +
        labs(
          x = "Retreat Timing Category",
          y = "Total Market Value ($ Millions)",
          title = "Market Value at Risk (Buyout data not available)"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(size = 9))
    }
    
    ggplotly(p)
  })
}


# RUN APP

shinyApp(ui, server)
