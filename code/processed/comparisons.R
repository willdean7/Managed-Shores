###This script makes an interactive dashboard for neighborhood comparisons
#Building on the sf created in 'MSGRP_valuationcode_ms.R'
#It starts by preparing your property data—calculating vulnerability metrics, categorizing properties based 
#on their optimal retreat year, and identifying the main factors driving those decisions (such as elevation, structure value, and rental income). 
#The dashboard features a map where users can click on any property to instantly see its details and compare it with its nearest neighbors. 
#Side-by-side tables and visualizations highlight key differences, and summary statistics show what most commonly causes some homes to require immediate retreat while others do not. 
#The result is an intuitive tool that helps users quickly understand why certain coastal properties are more at risk than their neighbors, making the analysis actionable for planning and decision-making.

#Author: Will
rm(list=ls())
library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(sf)
library(raster)
library(plotly)
library(DT)
library(ggplot2)
library(scales)
library(viridis)


# Read vulnerability metrics script
source("code/processed/calc_vulnerability_metrics.R")


# Read sf and convert to df
redfin_df <- readr::read_csv("data/carpinteria/redfin_sf.csv")

# Convert back to sf (using original columns, not those extracted by st_coordinates)
redfin_sf <- st_as_sf(redfin_df, coords = c("longitude", "latitude"), crs = 4326)

# Calculate vulnerability metrics for all scenarios (long format)
redfin_metrics <- calc_vulnerability_metrics(redfin_sf)

st_geometry(redfin_metrics)

# Add scenario labels
redfin_metrics <- redfin_metrics %>%
  mutate(
    scenario_label = case_when(
      scenario == "Intermediate" ~ "Base Intermediate",
      scenario == "Intermediate_High" ~ "Base Intermediate High",
      scenario == "High" ~ "Base High",
      scenario == "p5_Intermediate" ~ "5th Percentile Intermediate",
      scenario == "p5_Intermediate_High" ~ "5th Percentile Intermediate High",
      scenario == "p5_High" ~ "5th Percentile High",
      scenario == "p95_Intermediate" ~ "95th Percentile Intermediate",
      scenario == "p95_Intermediate_High" ~ "95th Percentile Intermediate High",
      scenario == "p95_High" ~ "95th Percentile High",
      TRUE ~ scenario  # fallback for other scenarios
    )
  )

# Define dashboard function
retreat_dashboard <- function(enhanced_data) {
  
  ui <- dashboardPage(
    dashboardHeader(title = "Coastal Retreat Analysis: Why Neighbors Differ"),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Interactive Map", tabName = "map", icon = icon("map")),
        menuItem("Economic Risk Analysis", tabName = "risk", icon = icon("chart-line")),
        menuItem("Key Insights", tabName = "insights", icon = icon("lightbulb")),
        menuItem("About This Analysis", tabName = "info", icon = icon("info-circle"))
      ),
      selectInput(
        "scenario_choice", "Select SLR Scenario:",
        choices = c("Base Intermediate", "Base Intermediate High", "Base High"),
        selected = "Base Intermediate"
      ),
      selectInput(
        "year_choice", "Select Year:",
        choices = c(0,10,20,30,40,50,60,70,80), selected = 10)
    ),
    
    dashboardBody(
      withMathJax(),
      tabItems(
        # Map Tab
        tabItem(tabName = "map",
                fluidRow(
                  box(title = "Click any property to compare with neighbors",
                      status = "primary", solidHeader = TRUE, width = 8,
                      leafletOutput("interactive_map", height = "600px")),
                  box(title = "Selected Property Details",
                      status = "info", solidHeader = TRUE, width = 4,
                      verbatimTextOutput("selected_info"), hr(),
                      h4("Nearest Neighbors:"), DT::dataTableOutput("neighbor_table"))
                )
        ),
        
        # Economic Risk Tab
        tabItem(tabName = "risk",
                fluidRow(
                  box(
                    title = "Economic Risk Drivers",
                    status = "primary", solidHeader = TRUE, width = 12,
                    tabBox(
                      width = 12,
                      tabPanel(
                        "Rent-to-Structure Ratio",
                        plotlyOutput("rent_structure_plot"),
                        p("Properties with ratios < 0.02 (red zone) face immediate economic pressure to retreat")
                      ),
                      tabPanel(
                        "Elevation vs Retreat Pressure",
                        plotlyOutput("elevation_pressure_plot"),
                        p("Higher retreat pressure indicates properties where high structure value meets flood risk")
                      )
                    )
                  )
                ),
                fluidRow(
                  box(
                    title = "Risk Category Summary",
                    status = "success", solidHeader = TRUE, width = 12,
                    DT::dataTableOutput("risk_summary_table"),
                    p("*Note: Poor Economics = rent/structure ratio < 0.02")
                  )
                )
        ),
        
        # Key Insights Tab
        tabItem(tabName = "insights",
                fluidRow(
                  valueBoxOutput("immediate_count", width = 4),
                  valueBoxOutput("avg_elevation_diff", width = 4),
                  valueBoxOutput("main_driver", width = 4)
                ),
                fluidRow(
                  box(title = "What Makes Neighbors Different?",
                      status = "warning", solidHeader = TRUE, width = 12,
                      plotlyOutput("driver_analysis"))
                )
        ),
        
        # About Tab
        tabItem(tabName = "info",
                box(
                  title = "About This Coastal Retreat Analysis",
                  status = "info", solidHeader = TRUE, width = 12,
                  tabBox(
                    width = 12,
                    tabPanel(
                      "Analysis Overview",
                      h3("Coastal Retreat Decision Framework"),
                      p("This dashboard helps identify which coastal properties should retreat now versus later by balancing:"),
                      tags$ul(
                        tags$li(strong("Physical Risk:"), "Flood exposure based on elevation and sea-level rise"),
                        tags$li(strong("Economic Factors:"), "Property value, rental income, and replacement costs"),
                        tags$li(strong("Time Value:"), "Future costs/benefits discounted to present value")
                      ),
                      hr(),
                      h3("Key Questions Answered"),
                      tags$ul(
                        tags$li("Why do similar neighboring properties have different retreat timelines?"),
                        tags$li("What makes some properties require immediate retreat?"),
                        tags$li("How do economic and physical factors interact to determine optimal retreat timing?")
                      )
                    ),
                    tabPanel(
                      "Key Metrics",
                      h3("Retreat Pressure Formula"),
                      withMathJax(),
                      p("$$\\text{retreat\\_pressure} = \\frac{\\text{structure\\_value}}{\\text{annual\\_rent}} \\times \\frac{1}{\\text{elevation}}$$"),
                      p("Quantifies urgency to retreat by combining:"),
                      tags$ul(
                        tags$li(strong("Economic Vulnerability:"), "High structure value relative to rent"),
                        tags$li(strong("Physical Vulnerability:"), "Low elevation increases flood risk")
                      )
                    ),
                    tabPanel(
                      "Model Details",
                      h3("Optimal Stopping Framework"),
                      p("The model uses backward induction to determine the optimal retreat year by comparing two options each year:"),
                      tags$ol(
                        tags$li(strong("Continue:"), "Earn rent but risk flood damage"),
                        tags$li(strong("Retreat:"), "Sell land and avoid future risks")
                      )
                    )
                  )
                )
        )
      )
    )
  )
  
  server <- function(input, output, session) {
    sf::sf_use_s2(FALSE)
    
    # Filter data based on selected scenario
    scenario_data <- reactive({
      enhanced_data %>%
        filter(scenario_label == input$scenario_choice) %>%
        mutate(id = row_number())
    })
    
    # Reactive values to store selected property and neighbors
    selected_property <- reactiveVal(NULL)
    neighbors <- reactiveVal(NULL)
    
    #Inundation raster
    inundation_raster <- reactive({
      scen_map <- c(
        "Base Intermediate" = "Intermediate",
        "Base Intermediate High" = "Intermediate_High",
        "Base High" = "High"
      )
      scenario_name <- scen_map[input$scenario_choice]
      year <- input$year_choice
      tif_path <- paste0("data/carpinteria/inundation_", scenario_name, "_", year, ".tif")
      if(file.exists(tif_path)) {
        terra::rast(tif_path)
      } else {
        NULL
      }
    })
    
    # Interactive map
    output$interactive_map <- renderLeaflet({
      data <- scenario_data()
      pal <- colorFactor(
        palette = c("#E24A33", "#FBC15E", "#8EBA42", "#348ABD", "#B2B2B2"),
        domain = na.omit(data$retreat_category)
      )
      leaflet(data) %>%
        addTiles() %>%
        addCircleMarkers(
          radius = ~pmax(sqrt(retreat_pressure)/3, 2),
          color = ~pal(retreat_category),
          stroke = TRUE, fillOpacity = 0.7,
          label = ~ADDRESS, layerId = ~id,
          popup = ~paste0(
            "", ADDRESS, "",
            "Scenario: ", scenario_label, "",
            "Retreat Year: ", retreat_year, "",
            "Risk Level: ", risk_level, "",
            "Primary Driver: ", primary_driver, "",
            "Elevation: ", round(elevation, 1), "m"
          )
        ) %>%
        # Overlay inundation raster—Blues palette, 45% opacity
        {
          rast <- inundation_raster()
          if(!is.null(rast)) {
            addRasterImage(., rast,
                           colors = colorNumeric("Blues", domain = NULL, na.color = "transparent"),
                           opacity = 0.45, project = FALSE)
          } else {
            .
          }
        } %>%
        addLegend(pal = pal, values = ~retreat_category,
                  title = "Retreat Timeline", position = "bottomright")
    })
    
    # Property click
    observeEvent(input$interactive_map_marker_click, {
      click <- input$interactive_map_marker_click
      if (!is.null(click)) {
        prop <- scenario_data()[as.numeric(click$id), ]
        selected_property(prop)
        
        selected_point <- st_transform(prop, 3857)
        all_points <- st_transform(scenario_data(), 3857)
        
        suppressWarnings({
          neighbor_ids <- st_is_within_distance(selected_point, all_points, dist = 200)[[1]]
        })
        neighbor_ids <- setdiff(neighbor_ids, as.numeric(click$id))
        
        distances <- st_distance(selected_point, all_points[neighbor_ids, ])
        neighbors_df <- scenario_data()[neighbor_ids, ] %>%
          mutate(distance_m = as.numeric(distances)) %>%
          arrange(distance_m) %>%
          head(5)
        
        neighbors(neighbors_df)
      }
    })
    
    # Selected property info
    output$selected_info <- renderText({
      prop <- selected_property()
      if (!is.null(prop)) {
        # Map the chosen base scenario to its MC percentiles
        mc_scenarios <- switch(
          input$scenario_choice,
          "Base Intermediate" = c("p5_Intermediate", "p95_Intermediate"),
          "Base Intermediate High" = c("p5_Intermediate_High", "p95_Intermediate_High"),
          "Base High" = c("p5_High", "p95_High")
        )
        
        mc_data <- enhanced_data %>%
          filter(full_address == prop$full_address, scenario %in% mc_scenarios) %>%
          select(scenario_label, retreat_year)
        
        mc_text <- mc_data %>%
          mutate(label = case_when(
            grepl("^p5", scenario_label) ~ "5th Percentile",
            grepl("^p95", scenario_label) ~ "95th Percentile",
            TRUE ~ scenario_label
          )) %>%
          transmute(text = paste0(label, ": ", retreat_year)) %>%
          pull(text) %>%
          paste(collapse = "\n")
        
        paste0(
          "ADDRESS: ", prop$ADDRESS, "\n",
          "Scenario Retreat Year: ", prop$retreat_year, "\n",
          "Risk Level: ", prop$risk_level, "\n",
          "Primary Driver: ", prop$primary_driver, "\n",
          "Elevation: ", round(prop$elevation, 1), " m\n",
          "Structure Value: $", format(prop$strval, big.mark = ","), "\n",
          "Annual Rent: $", format(prop$rent, big.mark = ","), "\n",
          "Retreat Pressure: ", round(prop$retreat_pressure, 1), "\n\n",
          "Monte Carlo Results:\n", mc_text
        )
      } else {
        "Click a property on the map to see details"
      }
    })
    
    # Neighbor table
    output$neighbor_table <- DT::renderDataTable({
      req(neighbors())
      neighbors() %>%
        st_drop_geometry() %>%
        dplyr::select(
          Address = ADDRESS,
          `Retreat Year` = retreat_year,
          `Risk Level` = risk_level,
          `Elevation (m)` = elevation,
          `Distance (m)` = distance_m
        ) %>%
        mutate(across(where(is.numeric), round, 1))
    }, options = list(pageLength = 5, dom = 't'))
    
    output$rent_structure_plot <- renderPlotly({
      data <- scenario_data()
      validate(need(nrow(data) > 0, "No data for this scenario"))
      plot_ly(data, x = ~rent_to_structure_ratio, type = "histogram", nbinsx = 20) %>%
        layout(title = "Rent-to-Structure Ratio")
    })
    
    output$elevation_pressure_plot <- renderPlotly({
      data <- scenario_data()
      validate(need(nrow(data) > 0, "No data for this scenario"))
      plot_ly(data, x = ~elevation, y = ~retreat_pressure, type = "scatter", mode = "markers") %>%
        layout(title = "Elevation vs Retreat Pressure", xaxis = list(title = "Elevation (m)"), yaxis = list(title = "Retreat Pressure"))
    })
    
    output$risk_summary_table <- DT::renderDataTable({
      # Example: summarize properties by retreat category
      data <- scenario_data()
      req(nrow(data) > 0)
      data %>%
        group_by(retreat_category) %>%
        summarize(
          n = n(),
          avg_elevation = mean(elevation, na.rm = TRUE),
          avg_retreat_pressure = mean(retreat_pressure, na.rm = TRUE)
        )
    })
    
    # Key Insights (example driver plot)
    output$driver_analysis <- renderPlotly({
      data <- scenario_data()
      validate(need(nrow(data) > 0, "No data for this scenario"))
      plot_ly(data, x = ~primary_driver, type = "histogram") %>%
        layout(title = "Primary Risk Driver")
    })
    
    output$immediate_count <- renderValueBox({
      data <- scenario_data()
      n <- sum(data$retreat_category == "0-10 years", na.rm = TRUE)
      valueBox(n, "Immediate Retreat (0-10 yrs)", icon = icon("tree"), color = "red")
    })
    output$avg_elevation_diff <- renderValueBox({
      data <- scenario_data()
      avg <- round(mean(data$elevation, na.rm = TRUE), 2)
      valueBox(avg, "Avg Elevation (m)", icon = icon("globe"), color = "aqua")
    })
    output$main_driver <- renderValueBox({
      data <- scenario_data()
      pd <- data %>% count(primary_driver) %>% arrange(desc(n)) %>% slice(1) %>% pull(primary_driver)
      valueBox(pd, "Most Common Driver", icon = icon("exchange"), color = "purple")
    })
    
  }
  
  shinyApp(ui, server)
}

# Run dashboard
retreat_dashboard(redfin_metrics)

