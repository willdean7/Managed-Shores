# cosmos_shiny.R
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

# ============================================================================
# LOAD DATA
# ============================================================================

case_name <- "carpinteria"
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

# Create spatial version for mapping
results_sf <- all_results %>%
  filter(!is.na(LATITUDE), !is.na(LONGITUDE)) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326, remove = FALSE)

# Parameter ranges
rental_yields <- c(0.04, 0.05, 0.06)
discount_rates <- c(0.015, 0.02, 0.03)
damage_thresholds <- c(0.0, 0.15, 0.30)
owner_utilities <- c(0, 25000, 50000)
storm_scenarios <- c("w000", "w100")
slr_scenarios <- c("Intermediate", "Intermediate_High", "High")

# ============================================================================
# UI
# ============================================================================

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
    
    selectInput(
      "storm_scenario",
      "Storm Scenario:",
      choices = c("w000 (Average)" = "w000", "w100 (100-year storm)" = "w100"),
      selected = "w000"
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
          valueBoxOutput("value_at_risk_box", width = 3)
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
              tags$li(strong("Rental Yield:"), " Annual rent as % of property value (4-6%)"),
              tags$li(strong("Discount Rate:"), " Rate for calculating present value (1.5-3%)"),
              tags$li(strong("Damage Threshold:"), " Minimum flood depth to count as damage (0-30cm)"),
              tags$li(strong("Owner Utility:"), " Annual non-financial value of living there ($0-50K)"),
              tags$li(strong("Storm Scenario:"), " w000 (average) vs w100 (100-year storm)")
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

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {
  
  # Reactive: Get filtered data based on current parameters
  current_data <- reactive({
    all_results %>%
      filter(
        scenario == input$slr_scenario,
        rental_yield == input$rental_yield / 100,
        discount_rate == input$discount_rate / 100,
        damage_threshold == input$damage_threshold / 100,
        owner_utility == input$owner_utility,
        storm_scenario == input$storm_scenario
      )
  })
  
  # Reactive: Baseline data for comparison
  baseline_data <- reactive({
    baseline_results %>%
      filter(scenario == input$slr_scenario)
  })
  
  # Reset button
  observeEvent(input$reset, {
    updateSliderInput(session, "rental_yield", value = 5)
    updateSliderInput(session, "discount_rate", value = 2.0)
    updateSliderInput(session, "damage_threshold", value = 15)
    updateSliderInput(session, "owner_utility", value = 0)
    updateSelectInput(session, "storm_scenario", selected = "w000")
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
  
  output$value_at_risk_box <- renderValueBox({
    total_value <- sum(current_data()$PRICE[current_data()$retreat_year <= 100], na.rm = TRUE)
    valueBox(
      value = paste0("$", round(total_value / 1e6), "M"),
      subtitle = "Property Value at Risk",
      icon = icon("dollar-sign"),
      color = "orange"
    )
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
    current_data() %>%
      select(ADDRESS, PRICE, retreat_year, reason, elevation) %>%
      mutate(
        PRICE = scales::dollar(PRICE),
        elevation = round(elevation, 2)
      ) %>%
      rename(
        Address = ADDRESS,
        Price = PRICE,
        `Retreat Year` = retreat_year,
        Category = reason,
        `Elevation (m)` = elevation
      )
  }, options = list(pageLength = 10, scrollX = TRUE))
  
  # Map
  output$retreat_map <- renderLeaflet({
    data_sf <- results_sf %>%
      filter(
        scenario == input$slr_scenario,
        rental_yield == input$rental_yield / 100,
        discount_rate == input$discount_rate / 100,
        damage_threshold == input$damage_threshold / 100,
        owner_utility == input$owner_utility,
        storm_scenario == input$storm_scenario
      ) %>%
      mutate(
        color = case_when(
          retreat_year <= 10 ~ "#d73027",
          retreat_year <= 25 ~ "#fc8d59",
          retreat_year <= 50 ~ "#fee08b",
          retreat_year <= 100 ~ "#91bfdb",
          TRUE ~ "#4575b4"
        )
      )
    
    leaflet(data_sf) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        radius = 6,
        color = ~color,
        fillColor = ~color,
        fillOpacity = 0.7,
        weight = 1,
        popup = ~paste0(
          "<b>", ADDRESS, "</b><br>",
          "Retreat Year: ", retreat_year, "<br>",
          "Price: $", format(PRICE, big.mark = ","), "<br>",
          "Elevation: ", round(elevation, 2), "m"
        )
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
    current_data() %>%
      mutate(
        category = case_when(
          retreat_year <= 10 ~ "Immediate (0-10)",
          retreat_year <= 25 ~ "Early (11-25)",
          retreat_year <= 50 ~ "Mid-term (26-50)",
          retreat_year <= 100 ~ "Late (51-100)",
          TRUE ~ "No Retreat (>100)"
        )
      ) %>%
      group_by(category) %>%
      summarise(
        `Number of Properties` = n(),
        `Avg Property Value` = scales::dollar(mean(PRICE, na.rm = TRUE)),
        `Total Value at Risk` = scales::dollar(sum(PRICE, na.rm = TRUE)),
        `Avg Elevation (m)` = round(mean(elevation, na.rm = TRUE), 2),
        .groups = "drop"
      ) %>%
      rename(`Retreat Category` = category)
  }, options = list(pageLength = 10, dom = 't'))
  
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
}

# ============================================================================
# RUN APP
# ============================================================================

shinyApp(ui, server)
