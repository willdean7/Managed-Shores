# cosmos_spatial_comparisons.R
# Purpose: Create spatial maps comparing retreat timing across different parameters
# Author: Will Dean (Managed Shores)
#
# Input:
#   - data/{site}/derived/retreat_schedule_baseline.csv
#   - data/{site}/derived/retreat_schedule_sensitivity.csv
#
# Output:
#   - figures/spatial_parameter_comparison_grid.png
#   - figures/spatial_storm_comparison.png
#   - figures/spatial_rental_yield_comparison.png
#   - figures/spatial_owner_utility_comparison.png

rm(list = ls())
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(sf)
library(patchwork)
library(scales)


# CONFIGURATION

case_name <- "carpinteria"
data_dir <- file.path("data", case_name)
derived_dir <- file.path(data_dir, "derived")
fig_dir <- "figures"
dir.create(data_dir, fig_dir, recursive = TRUE, showWarnings = FALSE)

# Color scheme for retreat years (consistent across all maps)
retreat_colors <- c(
  "Immediate\n(0-10 yr)" = "#d73027",
  "Early\n(11-25 yr)" = "#fc8d59",
  "Mid-term\n(26-50 yr)" = "#fee08b",
  "Late\n(51-100 yr)" = "#91bfdb",
  "No Retreat\n(>100 yr)" = "#4575b4"
)


# LOAD DATA

baseline_results <- read_csv(
  file.path(derived_dir, "retreat_schedule_baseline.csv"),
  show_col_types = FALSE
)

sensitivity_results <- read_csv(
  file.path(derived_dir, "retreat_schedule_sensitivity.csv"),
  show_col_types = FALSE
)

# Combine all results
all_results <- bind_rows(baseline_results, sensitivity_results)

# Create spatial version
results_sf <- all_results %>%
  filter(!is.na(LATITUDE), !is.na(LONGITUDE)) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326, remove = FALSE) %>%
  mutate(
    retreat_category = case_when(
      retreat_year <= 10 ~ "Immediate\n(0-10 yr)",
      retreat_year <= 25 ~ "Early\n(11-25 yr)",
      retreat_year <= 50 ~ "Mid-term\n(26-50 yr)",
      retreat_year <= 100 ~ "Late\n(51-100 yr)",
      TRUE ~ "No Retreat\n(>100 yr)"
    ),
    retreat_category = factor(retreat_category, levels = names(retreat_colors))
  )


# HELPER FUNCTION: CREATE SPATIAL MAP

create_spatial_map <- function(data, title_text, show_legend = TRUE) {
  
  # Get bounding box for consistent map extent
  bbox <- st_bbox(data)
  
  # Create base map - clean and simple
  p <- ggplot(data) +
    geom_sf(aes(color = retreat_category, fill = retreat_category), 
            size = 2.8, alpha = 0.85, shape = 21, stroke = 1.2) +
    scale_color_manual(values = retreat_colors, name = "Retreat Timing", drop = FALSE) +
    scale_fill_manual(values = retreat_colors, name = "Retreat Timing", drop = FALSE) +
    coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), 
             ylim = c(bbox["ymin"], bbox["ymax"])) +
    labs(title = title_text) +
    theme_minimal(base_size = 10) +
    theme(
      plot.title = element_text(face = "bold", size = 11, hjust = 0.5, margin = margin(b = 8)),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "gray60", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "gray40", fill = NA, linewidth = 1),
      axis.text = element_text(size = 8, color = "gray30"),
      axis.title = element_blank(),
      plot.margin = margin(5, 5, 5, 5)
    )
  
  if (!show_legend) {
    p <- p + theme(legend.position = "none")
  } else {
    p <- p + theme(
      legend.position = "right",
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 9),
      legend.key.size = unit(0.6, "cm"),
      legend.background = element_rect(fill = "white", color = "gray60", linewidth = 0.5),
      legend.key = element_rect(fill = "white", color = NA),
      legend.margin = margin(5, 5, 5, 5)
    )
  }
  
  return(p)
}


# FIGURE 1: COMPREHENSIVE PARAMETER GRID (High SLR)

# Select key parameter combinations for the grid
param_scenarios <- c(
  "baseline",
  "rental_yield_4pct",
  "rental_yield_6pct",
  "storm_w100",
  "owner_utility_25k",
  "owner_utility_50k"
)

# Nice labels for each panel
scenario_labels <- c(
  "baseline" = "Baseline\n(5%, 2%, 15cm, $0, w000)",
  "rental_yield_4pct" = "Rental Yield: 4%\n(Poor Economics)",
  "rental_yield_6pct" = "Rental Yield: 6%\n(Strong Economics)",
  "storm_w100" = "Storm: w100\n(100-year Storm)",
  "owner_utility_25k" = "Owner Utility: $25K/yr\n(Moderate Attachment)",
  "owner_utility_50k" = "Owner Utility: $50K/yr\n(High Attachment)"
)

# Filter data for High SLR scenario and selected parameters
grid_data <- results_sf %>%
  filter(
    scenario == "High",
    parameter_set %in% param_scenarios
  ) %>%
  mutate(
    param_label = factor(parameter_set, levels = param_scenarios, 
                         labels = scenario_labels[param_scenarios])
  )

# Create individual maps for each parameter
maps_list <- list()
for (i in seq_along(param_scenarios)) {
  param <- param_scenarios[i]
  param_data <- grid_data %>% filter(parameter_set == param)
  
  show_leg <- (i == length(param_scenarios))  # Only show legend on last panel
  
  maps_list[[i]] <- create_spatial_map(
    param_data,
    scenario_labels[param],
    show_legend = show_leg
  )
}

# Combine into grid
fig1 <- wrap_plots(maps_list, ncol = 3) +
  plot_annotation(
    title = "Spatial Comparison: How Parameters Affect Retreat Timing",
    subtitle = "High SLR Scenario - Carpinteria Sand Point",
    caption = "Each dot represents a property colored by retreat timing category",
    theme = theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
      plot.caption = element_text(size = 9, color = "gray50", hjust = 0.5)
    )
  )

ggsave(file.path(fig_dir, "spatial_parameter_comparison_grid.png"), 
       fig1, width = 14, height = 10, dpi = 300)


# FIGURE 2: STORM SCENARIO COMPARISON (All 3 SLR Scenarios)

storm_data <- results_sf %>%
  filter(
    parameter_set %in% c("baseline", "storm_w100"),
    rental_yield == 0.05,
    discount_rate == 0.02,
    damage_threshold == 0.15,
    owner_utility == 0
  ) %>%
  mutate(
    panel_label = paste0(
      scenario, " - ",
      ifelse(storm_scenario == "w000", "Average (w000)", "100-yr Storm (w100)")
    )
  )

# Create maps for each combination
storm_maps <- list()
counter <- 1

for (slr in c("Intermediate", "Intermediate_High", "High")) {
  for (storm in c("w000", "w100")) {
    storm_subset <- storm_data %>%
      filter(scenario == slr, storm_scenario == storm)
    
    title <- paste0(slr, "\n", 
                    ifelse(storm == "w000", "Average", "100-yr Storm"))
    
    show_leg <- (counter == 6)  # Legend on last panel
    
    storm_maps[[counter]] <- create_spatial_map(
      storm_subset,
      title,
      show_legend = show_leg
    )
    
    counter <- counter + 1
  }
}

fig2 <- wrap_plots(storm_maps, ncol = 2) +
  plot_annotation(
    title = "Storm Scenario Impact Across SLR Projections",
    subtitle = "Comparing average conditions (w000) vs 100-year storm (w100)",
    caption = "Left column: Average conditions | Right column: 100-year storm",
    theme = theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
      plot.caption = element_text(size = 10, color = "gray50", hjust = 0.5)
    )
  )

ggsave(file.path(fig_dir, "spatial_storm_comparison.png"), 
       fig2, width = 12, height = 14, dpi = 300)


# FIGURE 3: RENTAL YIELD PROGRESSION (High SLR)

rental_data <- results_sf %>%
  filter(
    scenario == "High",
    parameter_set %in% c("rental_yield_4pct", "baseline", "rental_yield_6pct"),
    storm_scenario == "w000",
    damage_threshold == 0.15,
    owner_utility == 0
  ) %>%
  mutate(
    yield_label = case_when(
      rental_yield == 0.04 ~ "4% Rental Yield\n(Poor Economics)",
      rental_yield == 0.05 ~ "5% Rental Yield\n(Baseline)",
      rental_yield == 0.06 ~ "6% Rental Yield\n(Strong Economics)"
    ),
    yield_label = factor(yield_label, levels = c(
      "4% Rental Yield\n(Poor Economics)",
      "5% Rental Yield\n(Baseline)",
      "6% Rental Yield\n(Strong Economics)"
    ))
  )

rental_maps <- list()
for (i in 1:3) {
  rental_subset <- rental_data %>%
    filter(yield_label == levels(rental_data$yield_label)[i])
  
  show_leg <- (i == 3)
  
  rental_maps[[i]] <- create_spatial_map(
    rental_subset,
    levels(rental_data$yield_label)[i],
    show_legend = show_leg
  )
}

fig3 <- wrap_plots(rental_maps, ncol = 3) +
  plot_annotation(
    title = "Impact of Rental Yield on Retreat Timing",
    subtitle = "High SLR Scenario - Higher yields enable properties to stay longer",
    caption = "Lower rental income → Earlier retreat | Higher rental income → Later retreat",
    theme = theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
      plot.caption = element_text(size = 10, color = "gray50", hjust = 0.5)
    )
  )

ggsave(file.path(fig_dir, "spatial_rental_yield_comparison.png"), 
       fig3, width = 14, height = 5, dpi = 300)


# FIGURE 4: OWNER UTILITY PROGRESSION (High SLR)

utility_data <- results_sf %>%
  filter(
    scenario == "High",
    parameter_set %in% c("baseline", "owner_utility_25k", "owner_utility_50k"),
    storm_scenario == "w000",
    rental_yield == 0.05,
    damage_threshold == 0.15
  ) %>%
  mutate(
    utility_label = case_when(
      owner_utility == 0 ~ "No Owner Utility\n(Baseline - $0/yr)",
      owner_utility == 25000 ~ "Moderate Attachment\n($25K/yr)",
      owner_utility == 50000 ~ "High Attachment\n($50K/yr)"
    ),
    utility_label = factor(utility_label, levels = c(
      "No Owner Utility\n(Baseline - $0/yr)",
      "Moderate Attachment\n($25K/yr)",
      "High Attachment\n($50K/yr)"
    ))
  )

utility_maps <- list()
for (i in 1:3) {
  utility_subset <- utility_data %>%
    filter(utility_label == levels(utility_data$utility_label)[i])
  
  show_leg <- (i == 3)
  
  utility_maps[[i]] <- create_spatial_map(
    utility_subset,
    levels(utility_data$utility_label)[i],
    show_legend = show_leg
  )
}

fig4 <- wrap_plots(utility_maps, ncol = 3) +
  plot_annotation(
    title = "Impact of Owner Utility (Coastal Attachment) on Retreat Timing",
    subtitle = "High SLR Scenario - Non-financial value delays retreat",
    caption = "Owner utility represents the annual value homeowners place on coastal living beyond rental income",
    theme = theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
      plot.caption = element_text(size = 10, color = "gray50", hjust = 0.5)
    )
  )

ggsave(file.path(fig_dir, "spatial_owner_utility_comparison.png"), 
       fig4, width = 14, height = 5, dpi = 300)


# FIGURE 5: SIDE-BY-SIDE BASELINE VS MOST EXTREME SCENARIOS

extreme_data <- results_sf %>%
  filter(
    scenario == "High",
    parameter_set %in% c("baseline", "rental_yield_4pct", "storm_w100", "owner_utility_50k")
  ) %>%
  mutate(
    extreme_label = case_when(
      parameter_set == "baseline" ~ "Baseline\n(Median: 74 yrs)",
      parameter_set == "rental_yield_4pct" ~ "Worst Case\nRental Yield 4%\n(Median: 60 yrs)",
      parameter_set == "storm_w100" ~ "Worst Case\n100-yr Storm\n(Median: 59 yrs)",
      parameter_set == "owner_utility_50k" ~ "Best Case\nHigh Attachment\n(Median: 82 yrs)"
    ),
    extreme_label = factor(extreme_label, levels = c(
      "Worst Case\nRental Yield 4%\n(Median: 60 yrs)",
      "Worst Case\n100-yr Storm\n(Median: 59 yrs)",
      "Baseline\n(Median: 74 yrs)",
      "Best Case\nHigh Attachment\n(Median: 82 yrs)"
    ))
  )

extreme_maps <- list()
for (i in 1:4) {
  extreme_subset <- extreme_data %>%
    filter(extreme_label == levels(extreme_data$extreme_label)[i])
  
  show_leg <- (i == 4)
  
  extreme_maps[[i]] <- create_spatial_map(
    extreme_subset,
    levels(extreme_data$extreme_label)[i],
    show_legend = show_leg
  )
}

fig5 <- wrap_plots(extreme_maps, ncol = 2) +
  plot_annotation(
    title = "Range of Retreat Outcomes: Best Case to Worst Case",
    subtitle = "High SLR Scenario - Parameter uncertainty creates 23-year range in median retreat timing",
    caption = "Left: Early retreat scenarios | Right: Baseline and delayed retreat scenario",
    theme = theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
      plot.caption = element_text(size = 10, color = "gray50", hjust = 0.5)
    )
  )

ggsave(file.path(fig_dir, "spatial_extreme_scenarios.png"), 
       fig5, width = 12, height = 12, dpi = 300)


# SUMMARY TABLE: SPATIAL CLUSTERING ANALYSIS

# Calculate how many properties change retreat category across scenarios
category_changes <- results_sf %>%
  st_drop_geometry() %>%
  filter(scenario == "High") %>%
  mutate(retreat_category = as.character(retreat_category)) %>%  # Convert factor to character
  select(parcel_id, parameter_set, retreat_category) %>%
  pivot_wider(names_from = parameter_set, values_from = retreat_category) %>%
  mutate(
    changes_from_baseline = rowSums(across(-c(parcel_id, baseline)) != baseline, na.rm = TRUE),
    most_volatile = changes_from_baseline == max(changes_from_baseline, na.rm = TRUE)
  )

message("Properties with most sensitivity (change category in all scenarios): ",
        sum(category_changes$changes_from_baseline >= 8, na.rm = TRUE))

# Save clustering summary
clustering_summary <- category_changes %>%
  summarise(
    total_properties = n(),
    stable_properties = sum(changes_from_baseline == 0, na.rm = TRUE),
    moderate_sensitivity = sum(changes_from_baseline > 0 & changes_from_baseline < 4, na.rm = TRUE),
    high_sensitivity = sum(changes_from_baseline >= 4, na.rm = TRUE),
    max_changes = max(changes_from_baseline, na.rm = TRUE)
  )

write_csv(clustering_summary, file.path(fig_dir, "spatial_clustering_summary.csv"))

# FINAL SUMMARY
message("\nGenerated figures:")
message("  1. spatial_parameter_comparison_grid.png - 6-panel comparison of key parameters")
message("  2. spatial_storm_comparison.png - w000 vs w100 across all SLR scenarios")
message("  3. spatial_rental_yield_comparison.png - 4%, 5%, 6% yield comparison")
message("  4. spatial_owner_utility_comparison.png - $0, $25K, $50K/yr comparison")
message("  5. spatial_extreme_scenarios.png - Best case vs worst case scenarios")
message("\nAll figures saved to: ", normalizePath(fig_dir))
message("\nKey spatial insights:")
message("  - Parameter sensitivity varies by location")
message("  - Storm scenario shows strongest spatial impact")
message("  - Properties at similar elevations show similar sensitivity patterns")
message("\n========================================\n")

