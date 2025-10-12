#Author: Will
rm(list=ls())
library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)
library(plotly)
library(dplyr)
library(janitor)
library(ggplot2)
library(scales)
library(sf)
library(units)

# Read vulnerability metrics script
source("code/processed/calc_vulnerability_metrics.R")

redfin_sf <- readr::read_csv("data/carpinteria/redfin_sf.csv") |> ####make sure to change the folder to appropriate location
  dplyr::mutate(
    ADDRESS = trimws(tolower(ADDRESS))  # normalize for joining
  ) |>
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  calc_vulnerability_metrics() |>
  dplyr::mutate(
    id = dplyr::row_number(),
    # Harmonize scenario labels to match UI control
    scenario = dplyr::case_when(
      scenario == "Intermediate"        ~ "Intermediate",
      scenario == "Intermediate_High"   ~ "Intermediate High",
      scenario == "High"                ~ "High",
      TRUE ~ scenario
    ),
    # Friendly labels for facets/legends
    retreat_category_label = dplyr::case_when(
      retreat_category == "0-10 years"   ~ "Immediate Retreat (0-10 years)",
      retreat_category == "10-25 years"  ~ "Early Retreat (10-25 years)",
      retreat_category == "25-50 years"  ~ "Mid-term Retreat (25-50 years)",
      retreat_category == "50-100 years" ~ "Late Retreat (50-100 years)",
      retreat_category == ">100 years"   ~ "Delayed Retreat",
      TRUE ~ as.character(retreat_category)
    )
  )

## Parcel run-up history (annual maxima per parcel)
runup_parcel <- readr::read_csv("data/carpinteria/run_up_parcel.csv") |>
  janitor::clean_names() |>
  dplyr::rename(year = yyyy, run_up = max_run_up) |>
  dplyr::mutate(
    parcel_id = as.character(parcel_id),
    address   = trimws(tolower(address))
  )

# address -> parcel_id lookup (deduped)
runup_lookup <- runup_parcel |>
  dplyr::select(address, parcel_id) |>
  dplyr::distinct()

# attach parcel_id to the points
redfin_sf <- redfin_sf |>
  dplyr::left_join(runup_lookup, by = c("ADDRESS" = "address")) |>
  dplyr::mutate(parcel_id = as.character(parcel_id))

message("Missing parcel_id rows: ", sum(is.na(redfin_sf$parcel_id)))

# parcel_id -> vector of run-up values (keep finite only)
parcel_runup_clean <- runup_parcel |>
  dplyr::mutate(run_up = ifelse(is.finite(run_up), run_up, NA_real_)) |>
  tidyr::drop_na(run_up)

parcel_runup_list <- split(
  parcel_runup_clean$run_up,
  as.character(parcel_runup_clean$parcel_id)
)

## OPC 2024 SLR scenarios -> quadratic fits -> 100-year vectors
ft_to_m <- 0.3048
make_scenario_df <- function(feet_vec) {
  data.frame(
    time = c(0,10,20,30,40,50,60,70,80),
    sea_level_rise = feet_vec * ft_to_m
  )
}

sea_level_data <- list(
  "Intermediate"      = make_scenario_df(c(0, 0.4, 0.6, 0.8, 1.1, 1.4, 1.8, 2.4, 3.1)),
  "Intermediate High" = make_scenario_df(c(0, 0.4, 0.7, 1.0, 1.5, 2.2, 3.0, 3.9, 4.9)),
  "High"              = make_scenario_df(c(0, 0.4, 0.8, 1.2, 2.0, 3.0, 4.1, 5.4, 6.6))
)

fit_slr_model <- function(df, years = 1:100) {
  df$timesquared <- df$time^2
  m  <- stats::lm(sea_level_rise ~ time + timesquared, data = df)
  nd <- data.frame(time = years, timesquared = years^2)
  as.numeric(stats::predict(m, newdata = nd))
}

# named list: each element is a numeric vector (length 100, meters)
slr_scenarios <- lapply(sea_level_data, fit_slr_model)

## Depth-damage curve & helpers
depth_df <- data.frame(
  depth = c(-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9),
  damagepct = c(0,0,0,0,0,0,0,0.01,0.07,0.14,0.20,0.26,0.30,0.33,0.36,0.38,0.39,0.39)
)

logistic_nls <- nls(
  damagepct ~ a/(1 + exp(-b * (depth - d))),
  data = depth_df,
  start = list(a = 0.4, b = 1, d = 2)
)
depth_params <- setNames(coef(logistic_nls), c("a","b","d"))

npvrentfxn <- function(littleT, bigT, rentvalues, beta) {
  n      <- bigT - littleT + 1
  betas  <- beta^(1:n)
  rentsT <- outer(rentvalues, rep(1, n))
  as.vector(rentsT %*% betas)
}

# Expected damages from t..bigT for a single parcel (supports scalar or vector shocks)
expdamagefxn_parcel_once <- function(littleT, bigT, depthparams, slr, shocks_vec, elev, strval, beta) {
  yrs       <- littleT:bigT
  sea_part  <- slr[yrs]                              # length n
  shocks    <- as.numeric(shocks_vec)                # length k (k can be 1)
  depth_mat <- outer(sea_part, shocks, `+`) - elev   # n x k
  
  a <- depthparams[["a"]]; b <- depthparams[["b"]]; d <- depthparams[["d"]]
  dmg_frac   <- a / (1 + exp(-b * (depth_mat - d)))
  exp_dmg_t  <- strval * rowMeans(pmax(dmg_frac, 0), na.rm = TRUE)  # length n
  sum(exp_dmg_t * beta^(1:length(yrs)))
}

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
        choices = c("Intermediate", "Intermediate High", "High"),
        selected = "Intermediate"
      ) 
    ),
    dashboardBody(
      withMathJax(),
      tabItems(
        tabItem(tabName = "map",
                fluidRow(
                  box(title = "Click any property to compare with neighbors", status = "primary", solidHeader = TRUE, width = 8,
                      leafletOutput("interactive_map", height = "600px")),
                  # box(title = "Selected Property Details", status = "info", solidHeader = TRUE, width = 4,
                  #     verbatimTextOutput("selected_info"), hr(), h4("Nearest Neighbors:"),
                  #     DT::dataTableOutput("neighbor_table")),
                  box(
                    title = "Selected Property Details", status = "info", solidHeader = TRUE, width = 4,
                    verbatimTextOutput("selected_info"),
                    hr(),
                    h4("Why This Retreat Year?"),
                    plotOutput("npv_damage_plot", height = "250px"),
                    p(em("Green: NPV of rent from year t to 100;  Red: expected flood damages (discounted) from year t to 100.")),
                    p(em("Dashed line = optimal retreat year (T*)."))
                  )
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
    output$npv_damage_plot <- renderPlot({
      prop <- selected_property()
      req(prop)
      
      # Fixed params
      beta  <- 0.97
      bigT  <- 100L
      years <- 1:bigT
      
      # ---- Get run-up history for this parcel (empirical samples) ----
      pid <- as.character(prop$parcel_id[1])
      
      msg_plot <- function(txt) {
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = txt, size = 5) +
          theme_void()
      }
      
      if (is.na(pid) || !pid %in% names(parcel_runup_list)) {
        return(msg_plot("No run-up history matched to this parcel (missing parcel_id join)."))
      }
      
      shocks_vec <- parcel_runup_list[[pid]]
      if (is.null(shocks_vec) || length(shocks_vec) == 0) {
        return(msg_plot("No run-up history for this parcel."))
      }
      
      shocks_vec <- shocks_vec[is.finite(shocks_vec)]
      if (length(shocks_vec) == 0) {
        return(msg_plot("Run-up series is empty after removing NAs/Inf."))
      }
      
      # ---- SLR scenario (meters) ----
      scen    <- as.character(req(input$scenario_choice))
      slr_vec <- slr_scenarios[[scen]]
      if (is.null(slr_vec) || length(slr_vec) < bigT) {
        return(msg_plot("SLR vector shorter than horizon; check scenario fit."))
      }
      
      # ---- Parcel inputs ----
      elev <- as.numeric(prop$elevation)
      strv <- as.numeric(prop$strval)
      rent <- as.numeric(prop$rent)
      
      # ---- NPV of rents from t..100 ----
      npv_rent_t <- vapply(
        years,
        function(t) npvrentfxn(t, bigT, rentvalues = rent, beta = beta),
        numeric(1)
      )
      
      # ---- Expected damages from t..100 using empirical run-up ----
      npv_dmg_t <- vapply(
        years,
        function(t) expdamagefxn_parcel_once(
          littleT = t, bigT = bigT,
          depthparams = depth_params,
          slr = slr_vec,
          shocks_vec = shocks_vec,
          elev = elev,
          strval = strv,
          beta = beta
        ),
        numeric(1)
      )
      
      t_star <- suppressWarnings(as.numeric(prop$retreat_year[1]))
      draw_vline <- is.finite(t_star) && t_star >= 1 && t_star <= bigT
      
      df_plot <- data.frame(
        year        = years,
        npv_rent    = as.numeric(npv_rent_t),
        npv_damages = as.numeric(npv_dmg_t)
      )
      
      p <- ggplot(df_plot, aes(year)) +
        geom_line(aes(y = npv_rent),    size = 1.1, color = "#1b7837") +
        geom_line(aes(y = npv_damages), size = 1.1, color = "#a50026") +
        scale_y_continuous(labels = scales::label_dollar(
          scale_cut = scales::cut_short_scale()
        )) +
        labs(
          x = "Decision year t",
          y = "NPV from t..100",
          title = paste0("NPV of Staying vs. Expected Damages — ", scen)
        ) +
        theme_minimal(base_size = 12)
      
      if (draw_vline) p <- p + geom_vline(xintercept = t_star, linetype = "dashed")
      
      p
    })
    
    # output$neighbor_table <- DT::renderDataTable({
    #   req(neighbors())
    #   neighbors() %>%
    #     st_drop_geometry() %>%
    #     dplyr::select(
    #       Address = ADDRESS,
    #       `Retreat Year` = retreat_year,
    #       `Risk Level` = risk_level,
    #       `Elevation (m)` = elevation,
    #       `Distance (m)` = distance_m
    #     ) %>%
    #     mutate(across(where(is.numeric), round, 1))
    # }, options = list(pageLength = 5, dom = 't'))
    
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
      immediate <- scenario_data() %>%
        filter(retreat_category_label == "Immediate Retreat (0-10 years)")
      
      delayed <- scenario_data() %>%
        filter(retreat_category_label == "Delayed Retreat")
      
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
