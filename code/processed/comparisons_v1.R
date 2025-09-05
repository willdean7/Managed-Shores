#Author: Will
rm(list=ls())
library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)
library(plotly)
library(dplyr)
library(sf)
library(units)

# Read vulnerability metrics script
source("code/processed/calc_vulnerability_metrics.R")

redfin_sf <- readr::read_csv("data/carpinteria/redfin_sf.csv") %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  calc_vulnerability_metrics() %>%
  mutate(
    id = row_number(),
    scenario = case_when(
      scenario == "Intermediate" ~ "Base Intermediate",
      scenario == "Intermediate_High" ~ "Base Intermediate High",
      scenario == "High" ~ "Base High",
      TRUE ~ scenario
    ),
    retreat_category_label = dplyr::case_when(
      retreat_category == "0-10 years" ~ "Immediate Retreat (0-10 years)",
      retreat_category == "10-25 years" ~ "Early Retreat (10-25 years)",
      retreat_category == "25-50 years" ~ "Mid-term Retreat (25-50 years)",
      retreat_category == "50-100 years" ~ "Late Retreat (50-100 years)",
      retreat_category == ">100 years" ~ "Delayed Retreat"
    )
  )

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
      hr(),
      selectInput(
        "scenario_choice", "Select SLR Scenario:",
        choices = c("Base Intermediate", "Base Intermediate High", "Base High"),
        selected = "Base Intermediate"
      ),
      
      selectInput(
        "year_choice", "Select Year:",
        choices = c(0, 10, 20, 30, 40, 50, 60, 70, 80),
        selected = 10
      )
    ),
    dashboardBody(
      withMathJax(),
      tabItems(
        tabItem(tabName = "map",
                fluidRow(
                  box(title = "Click any property to compare with neighbors", status = "primary", solidHeader = TRUE, width = 8,
                      leafletOutput("interactive_map", height = "600px")),
                  box(title = "Selected Property Details", status = "info", solidHeader = TRUE, width = 4,
                      verbatimTextOutput("selected_info"), hr(), h4("Nearest Neighbors:"),
                      DT::dataTableOutput("neighbor_table"))
                )
        ),
        tabItem(tabName = "risk",
                fluidRow(
                  box(
                    title = "Economic Risk Drivers",
                    status = "primary", 
                    solidHeader = TRUE,
                    width = 12,
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
                    status = "success",
                    solidHeader = TRUE,
                    width = 12,
                    DTOutput("risk_summary_table")
                  )
                )
        ),
        tabItem(tabName = "insights",
                fluidRow(
                  valueBoxOutput("immediate_count"),
                  valueBoxOutput("avg_elevation_diff"),
                  valueBoxOutput("main_driver")
                ),
                fluidRow(
                  box(title = "What Makes Neighbors Different?", status = "warning", solidHeader = TRUE, width = 12,
                      plotlyOutput("driver_analysis"))
                )
        ),
        tabItem(
          tabName = "info",
          box(
            title = "About This Coastal Retreat Analysis",
            status = "info",
            solidHeader = TRUE,
            width = 12,
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
                ),
                hr(),
                h3("Other Important Metrics"),
                tags$ul(
                  tags$li(strong("Rent-to-Structure Ratio:"), "Annual rent ÷ structure value (higher = more sustainable)"),
                  tags$li(strong("Optimal Retreat Year (T*):"), "Year when retreat becomes economically preferable to staying"),
                  tags$li(strong("Risk Level:"), "Categorical risk based on T* (Critical to Minimal)")
                )
              ),
              tabPanel(
                "Model Details",
                h3("Optimal Stopping Framework"),
                p("The model uses backward induction to determine the optimal retreat year by comparing two options each year:"),
                tags$ol(
                  tags$li(strong("Continue:"), "Earn rent but risk flood damage"),
                  tags$li(strong("Retreat:"), "Sell land and avoid future risks")
                ),
                withMathJax(),
                p("Value function for property \\(i\\) in year \\(t\\):"),
                p("$$V_t(i) = \\max \\begin{cases} 
      \\text{Land Value} & \\text{(Retreat)} \\\\
      \\text{Rent}_t - \\text{Damage}_t + \\beta V_{t+1}(i) & \\text{(Continue)}
      \\end{cases}$$"),
                hr(),
                h3("Key Parameters"),
                tags$ul(
                  tags$li(strong("Time Horizon:"), "100 years (bigT=100)"),
                  tags$li(strong("Discount Factor:"), "β = 0.97 (values future benefits)"),
                  tags$li(strong("Flood Damage:"), "Function of sea-level rise, wave runup, and elevation")
                )
              ),
              tabPanel(
                "Interpretation Guide",
                h3("Understanding the Dashboard"),
                tags$ul(
                  tags$li(strong("Map Colors:"), "Show retreat timeline from Immediate (red) to Delayed (gray)"),
                  tags$li(strong("Point Size:"), "Larger points = higher retreat pressure"),
                  tags$li(strong("Primary Drivers:"), "Explain why a property has its specific retreat year")
                ),
                hr(),
                h3("How to Use Insights"),
                p("Prioritize properties with:"),
                tags$ul(
                  tags$li("T* = 1 (Critical Risk) for buyout programs"),
                  tags$li("High retreat pressure but T* > 1 for targeted resilience investments"),
                  tags$li("Clusters of early-retreat properties for neighborhood-scale solutions")
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
    
    # Reactive: filter data by scenario choice
    scenario_data <- reactive({
      req(input$scenario_choice)
      enhanced_data %>%
        filter(scenario == input$scenario_choice) %>%
        mutate(
          retreat_category_label = factor(
            retreat_category_label,
            levels = c(
              "Immediate Retreat (0-10 years)",
              "Early Retreat (10-25 years)",
              "Mid-term Retreat (25-50 years)",
              "Late Retreat (50-100 years)",
              "Delayed Retreat"
            ),
            ordered = TRUE
          ),
          id = row_number()
        )
    })
    
    
    selected_property <- reactiveVal(NULL)
    neighbors <- reactiveVal(NULL)
    
    output$interactive_map <- renderLeaflet({
      data <- scenario_data()
      
      pal <- colorFactor(
        palette = c("#E24A33", "#FBC15E", "#8EBA42", "#348ABD", "#B2B2B2"),
        domain = data$retreat_category
      )
      
      leaflet(data) %>%
        addTiles() %>%
        addCircleMarkers(
          radius = ~sqrt(retreat_pressure) / 3,
          color = ~pal(retreat_category),
          stroke = TRUE, fillOpacity = 0.7,
          label = ~ADDRESS, layerId = ~id,
          popup = ~paste0(
            "<b>", ADDRESS, "</b><br>",
            "Retreat Year: ", retreat_year, "<br>",
            "Risk Level: ", risk_level, "<br>",
            "Primary Driver: ", primary_driver, "<br>",
            "Elevation: ", round(elevation, 1), "m"
          )
        ) %>%
        addLegend(pal = pal, values = ~retreat_category, title = "Retreat Timeline", position = "bottomright")
    })
    
    observeEvent(input$interactive_map_marker_click, {
      click <- input$interactive_map_marker_click
      if (!is.null(click)) {
        data <- scenario_data()
        prop <- data[as.numeric(click$id), ]
        selected_property(prop)
        
        selected_point <- st_transform(prop, 3857)
        all_points <- st_transform(data, 3857)
        
        suppressWarnings({
          neighbor_ids <- st_is_within_distance(
            selected_point,
            all_points,
            dist = 200
          )[[1]]
        })
        neighbor_ids <- setdiff(neighbor_ids, as.numeric(click$id))
        
        dists <- st_distance(selected_point, all_points[neighbor_ids, ])
        neighbors_df <- data[neighbor_ids, ] %>%
          mutate(distance_m = as.numeric(dists)) %>%
          arrange(distance_m) %>%
          head(5)
        
        neighbors(neighbors_df)
      }
    })
    
    output$selected_info <- renderText({
      prop <- selected_property()
      if (!is.null(prop)) {
        paste0(
          "ADDRESS: ", prop$ADDRESS, "\n",
          "Retreat Year: ", prop$retreat_year, "\n",
          "Risk Level: ", prop$risk_level, "\n",
          "Primary Driver: ", prop$primary_driver, "\n",
          "Elevation: ", round(prop$elevation, 1), "m\n",
          "Structure Value: $", format(prop$strval, big.mark = ","), "\n",
          "Annual Rent: $", format(prop$rent, big.mark = ","), "\n",
          "Retreat Pressure: ", round(prop$retreat_pressure, 1)
        )
      } else {
        "Click a property on the map to see details"
      }
    })
    
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
      ratio_data <- scenario_data()$rent_to_structure_ratio
      ratio_data <- ratio_data[!is.na(ratio_data)]
      
      min_val <- min(ratio_data, na.rm = TRUE)
      max_val <- max(ratio_data, na.rm = TRUE)
      buffer <- 0.05
      min_val_adj <- floor((min_val - buffer) * 1000) / 1000
      max_val_adj <- ceiling((max_val + buffer) * 1000) / 1000
      
      breaks <- seq(min_val_adj, max_val_adj, by = 0.005)
      
      hist_data <- hist(ratio_data, breaks = breaks, plot = FALSE)
      
      p <- ggplot(scenario_data(), aes(x = rent_to_structure_ratio)) +
        geom_histogram(binwidth = 0.005, fill = "steelblue", alpha = 0.8) +
        geom_vline(xintercept = 0.02, color = "red", linetype = "dashed") +
        annotate("text",
                 x = 0.03,
                 y = max(hist_data$counts, na.rm = TRUE) / 2,
                 label = "Poor Economics Threshold", color = "red") +
        labs(title = "Distribution of Rent-to-Structure Ratios",
             x = "Annual Rent / Structure Value",
             y = "Number of Properties") +
        scale_x_continuous(labels = scales::percent, limits = c(0,0.1)) +
        theme_minimal()
      
      ggplotly(p)
    })
    
    output$elevation_pressure_plot <- renderPlotly({
      p <- ggplot(scenario_data(), aes(x = elevation, y = retreat_pressure,
                                     color = factor(retreat_year))) +
        geom_point(alpha = 0.7, size = 3) +
        scale_color_viridis_d(name = "Retreat Year",
                              breaks = c(1, 25, 50, 75, 100),
                              labels = c("Immediate", "25 yrs", "50 yrs", "75 yrs", "Delayed")) +
        labs(title = "Elevation vs Economic Retreat Pressure",
             x = "Elevation (m)",
             y = "Retreat Pressure (Structure Value / Rent / Elevation)") +
        scale_y_continuous(limits = c(0,100)) +
        theme_minimal()
      
      ggplotly(p)
    })
    
    output$risk_summary_table <- renderDT({
      # Summarize data
      summary_data <- scenario_data() %>%
        group_by(retreat_category_label) %>%
        summarise(
          Properties = n(),
          `Avg Elevation (m)` = mean(elevation, na.rm = TRUE),
          `Avg Structure Value` = mean(strval, na.rm = TRUE),
          `Avg Annual Rent` = mean(rent, na.rm = TRUE),
          `Avg Rent/Structure Ratio` = mean(rent_to_structure_ratio, na.rm = TRUE),
          `Poor Economics (%)` = mean(rent_to_structure_ratio < 0.02, na.rm = TRUE)
        ) %>%
        arrange(retreat_category_label) %>% 
        sf::st_drop_geometry()
      
      # Rename columns for display
      summary_data <- summary_data %>%
        rename(
          "Retreat Category" = retreat_category_label,
          "Properties" = Properties,
          "Avg Elevation (m)" = `Avg Elevation (m)`,
          "Avg Structure Value" = `Avg Structure Value`,
          "Avg Annual Rent" = `Avg Annual Rent`,
          "Avg Rent/Structure Ratio" = `Avg Rent/Structure Ratio`,
          "Poor Economics (%)" = `Poor Economics (%)`
        )
      
      # Add info tooltips to headers
      header_names <- names(summary_data)
      header_names[header_names == "Avg Rent/Structure Ratio"] <- paste0(
        "Avg Rent/Structure Ratio ",
        '<i class="fa fa-info-circle" title="This column shows the average ratio of annual rent to structure value for each retreat category (annual rent ÷ structure value). A higher value means more sustainable rental economics; properties yield a larger percentage of their value in rent."></i>'
      )
      header_names[header_names == "Poor Economics (%)"] <- paste0(
        "Poor Economics (%) ",
        '<i class="fa fa-info-circle" title="Percentage of properties in each retreat category where annual rent/structure value < 0.02 (low rental yield relative to property value). Higher values mean more properties in economic distress—these are candidates for retreat or buyout first."></i>'
      )
      
      # Render DT table
      datatable(
        summary_data,
        options = list(pageLength = 5, dom = 't'),
        rownames = FALSE,
        escape = FALSE,
        colnames = header_names
      ) %>%
        formatCurrency(c("Avg Structure Value", "Avg Annual Rent"), "$") %>%
        formatPercentage(c("Avg Rent/Structure Ratio", "Poor Economics (%)"), 1)
    })
    
    
    # Key Insights
    output$immediate_count <- renderValueBox({
      valueBox(sum(scenario_data()$retreat_year == 1), "Properties Need Immediate Retreat", icon = icon("exclamation-triangle"), color = "red")
    })
    
    output$avg_elevation_diff <- renderValueBox({
      immediate <- scenario_data() %>% filter(retreat_category_label == "Immediate Retreat")
      delayed  <- scenario_data() %>% filter(retreat_category_label == "Delayed Retreat")
      
      if (nrow(immediate) == 0 || nrow(delayed) == 0) {
        diff <- NA
      } else {
        diff <- round(mean(delayed$elevation, na.rm = TRUE) - mean(immediate$elevation, na.rm = TRUE), 1)
      }
      
      valueBox(
        ifelse(is.na(diff), "N/A", paste0(diff, "m")),
        "Avg Elevation Difference (Delayed vs Immediate)",
        icon = icon("mountain"),
        color = "blue"
      )
    })
    
    output$main_driver <- renderValueBox({
      top_driver <- scenario_data() %>%
        filter(retreat_year == 1) %>%
        count(primary_driver, sort = TRUE) %>%
        slice(1) %>%
        pull(primary_driver)
      valueBox(top_driver, "Most Common Driver for Immediate Retreat", icon = icon("search"), color = "yellow")
    })
    
    output$driver_analysis <- renderPlotly({
      p <- scenario_data() %>%
        count(primary_driver, retreat_category_label) %>%
        ggplot(aes(x = primary_driver, y = n, fill = retreat_category_label)) +
        geom_col(position = "stack") +
        coord_flip() +
        labs(title = "What Drives Different Retreat Timelines?",
             x = "Primary Driver", y = "Number of Properties", fill = "Retreat Timeline") +
        theme_minimal() +
        scale_fill_viridis_d()
      ggplotly(p)
    })
  }
  
  shinyApp(ui, server)
}

retreat_dashboard(redfin_sf)
