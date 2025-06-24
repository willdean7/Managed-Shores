###This script makes an interactive dashboard for neighborhood comparisons
#It starts by preparing your property data—calculating vulnerability metrics, categorizing properties based 
#on their optimal retreat year, and identifying the main factors driving those decisions (such as elevation, structure value, and rental income). 
#The dashboard features a map where users can click on any property to instantly see its details and compare it with its nearest neighbors. 
#Side-by-side tables and visualizations highlight key differences, and summary statistics show what most commonly causes some homes to require immediate retreat while others do not. 
#The result is an intuitive tool that helps users quickly understand why certain coastal properties are more at risk than their neighbors, making the analysis actionable for planning and decision-making.

#Author: Will
rm(list=ls()) # clear the environment
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

# Read and process data
redfin_sf <- readr::read_csv("data/carpinteria/redfin_sf.csv") %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  calc_vulnerability_metrics() %>%
  mutate(
    id = row_number(),
    retreat_category = case_when(
      retreat_year == 1 ~ "Immediate Retreat",
      retreat_year <= 25 ~ "Early Retreat (1-25 years)",
      retreat_year <= 50 ~ "Mid-term Retreat (26-50 years)",
      retreat_year <= 75 ~ "Late Retreat (51-75 years)",
      TRUE ~ "Never Retreat"
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
                    DTOutput("risk_summary_table"),
                    p("*Note: Poor Economics = rent/structure ratio < 0.02")
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
                )),
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
                )),
              tabPanel(
                "Interpretation Guide",
                h3("Understanding the Dashboard"),
                tags$ul(
                  tags$li(strong("Map Colors:"), "Show retreat timeline from Immediate (red) to Never (green)"),
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
                ))
            )
          )
        )
      )
    )
  )
  
  server <- function(input, output, session) {
    sf::sf_use_s2(FALSE)
    
    selected_property <- reactiveVal(NULL)
    neighbors <- reactiveVal(NULL)
    
    output$interactive_map <- renderLeaflet({
      pal <- colorFactor(
        palette = c("#8e0152", "#c51b7d", "#de77ae", "#7fbc41", "#276419"),
        domain = enhanced_data$retreat_category
      )
      
      leaflet(enhanced_data) %>%
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
        prop <- enhanced_data[as.numeric(click$id), ]
        selected_property(prop)
        
        selected_point <- st_transform(prop, 3857)
        all_points <- st_transform(enhanced_data, 3857)
        
        suppressWarnings({
          neighbor_ids <- st_is_within_distance(
            selected_point, 
            all_points, 
            dist = 200
          )[[1]]
        })
        neighbor_ids <- setdiff(neighbor_ids, as.numeric(click$id))
        
        dists <- st_distance(selected_point, all_points[neighbor_ids, ])
        neighbors_df <- enhanced_data[neighbor_ids, ] %>%
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
    #main page
    output$neighbor_table <- DT::renderDataTable({
      req(neighbors())
      neighbors() %>%
        st_drop_geometry() %>%
        select(ADDRESS, retreat_year, risk_level, elevation, distance_m) %>%
        mutate(across(where(is.numeric), round, 1)) %>%
        rename_with(~c("Address", "Retreat Year", "Risk Level", "Elevation (m)", "Distance (m)"))
    }, options = list(pageLength = 5, dom = 't'))
    
    # Economic risk analysis plots
    output$rent_structure_plot <- renderPlotly({
      ratio_data <- enhanced_data$rent_to_structure_ratio
      ratio_data <- ratio_data[!is.na(ratio_data)]
      
      min_val <- min(ratio_data, na.rm = TRUE)
      max_val <- max(ratio_data, na.rm = TRUE)
      buffer <- 0.05
      min_val_adj <- floor((min_val - buffer) * 1000) / 1000
      max_val_adj <- ceiling((max_val + buffer) * 1000) / 1000
      
      breaks <- seq(min_val_adj, max_val_adj, by = 0.005)
      
      hist_data <- hist(ratio_data, breaks = breaks, plot = FALSE)
      
      p <- ggplot(enhanced_data, aes(x = rent_to_structure_ratio)) +
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
        p <- ggplot(enhanced_data, aes(x = elevation, y = retreat_pressure, 
                                   color = factor(retreat_year))) +
          geom_point(alpha = 0.7, size = 3) +
          scale_color_viridis_d(name = "Retreat Year", 
                                breaks = c(1, 25, 50, 75, 100),
                                labels = c("Immediate", "25 yrs", "50 yrs", "75 yrs", "Never")) +
          labs(title = "Elevation vs Economic Retreat Pressure",
               x = "Elevation (m)", 
               y = "Retreat Pressure (Structure Value / Rent / Elevation)") +
          scale_y_continuous(limits = c(0,100)) +
          theme_minimal()
        
        ggplotly(p)
      })
      
      output$risk_summary_table <- renderDT({
        summary_data <- enhanced_data %>%
          group_by(retreat_category) %>%
          summarise(
            Properties = n(),
            `Avg Elevation (m)` = mean(elevation, na.rm = TRUE),
            `Avg Structure Value` = mean(strval, na.rm = TRUE),
            `Avg Annual Rent` = mean(rent, na.rm = TRUE),
            `Avg Rent/Structure Ratio` = mean(rent_to_structure_ratio, na.rm = TRUE),
            `Poor Economics (%)` = mean(rent_to_structure_ratio < 0.02, na.rm = TRUE)
          ) %>%
          arrange(factor(retreat_category, 
                         levels = c("Immediate Retreat", "Early Retreat (1-25 years)", 
                                    "Mid-term Retreat (26-50 years)", "Late Retreat (51-75 years)", 
                                    "Never Retreat")))
        
        datatable(summary_data,
                  options = list(pageLength = 5, dom = 't'), 
                  rownames = FALSE) %>%
          formatCurrency(c("Avg Structure Value", "Avg Annual Rent"), "$") %>%
          formatPercentage(c("Avg Rent/Structure Ratio", "Poor Economics (%)"), 1)
      })
    
    # Key Insights
    output$immediate_count <- renderValueBox({
      valueBox(sum(enhanced_data$retreat_year == 1), "Properties Need Immediate Retreat", icon = icon("exclamation-triangle"), color = "red")
    })
    
    output$avg_elevation_diff <- renderValueBox({
      immediate <- enhanced_data %>% filter(retreat_year == 1)
      never <- enhanced_data %>% filter(retreat_year == 100)
      diff <- round(mean(never$elevation) - mean(immediate$elevation), 1)
      valueBox(paste0(diff, "m"), "Avg Elevation Difference (Never vs Immediate)", icon = icon("mountain"), color = "blue")
    })
    
    output$main_driver <- renderValueBox({
      top_driver <- enhanced_data %>%
        filter(retreat_year == 1) %>%
        count(primary_driver, sort = TRUE) %>%
        slice(1) %>%
        pull(primary_driver)
      valueBox(top_driver, "Most Common Driver for Immediate Retreat", icon = icon("search"), color = "yellow")
    })
    
    output$driver_analysis <- renderPlotly({
      p <- enhanced_data %>%
        count(primary_driver, retreat_category) %>%
        ggplot(aes(x = primary_driver, y = n, fill = retreat_category)) +
        geom_col(position = "stack") +
        coord_flip() +
        labs(title = "What Drives Different Retreat Timelines?", x = "Primary Driver", y = "Number of Properties", fill = "Retreat Timeline") +
        theme_minimal() +
        scale_fill_viridis_d()
      ggplotly(p)
    })
  }
  
  shinyApp(ui, server)
}

retreat_dashboard(redfin_sf)

