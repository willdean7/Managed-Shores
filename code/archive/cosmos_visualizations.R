# sensitivity_visualizations.R
# Purpose: Create visualizations comparing sensitivity analysis results
# Author: Will Dean (Managed Shores)
#
# Input:
#   - data/{site}/derived/sensitivity_summary.csv
#   - data/{site}/derived/retreat_schedule_baseline.csv
#   - data/{site}/derived/retreat_schedule_sensitivity.csv
#
# Output:
#   - figures/sensitivity_retreat_timing_comparison.png
#   - figures/sensitivity_immediate_retreat_bars.png
#   - figures/sensitivity_properties_at_risk.png
#   - figures/parameter_tornado_plot.png

rm(list = ls())
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(forcats)
library(scales)
library(patchwork)  # For combining plots

# CONFIGURATION
case_name <- "carpinteria"
data_dir <- file.path("data", case_name)
derived_dir <- file.path(data_dir, "derived")
fig_dir <- "figures"
dir.create(data_dir, fig_dir, recursive = TRUE, showWarnings = FALSE)

# Theme settings for publication-quality plots
theme_set(theme_minimal(base_size = 12))

# Improved color palette - distinct, colorblind-friendly
custom_colors <- c(
  "baseline" = "#1f77b4",        # Strong blue
  "rental_yield" = "#ff7f0e",    # Orange
  "discount_rate" = "#2ca02c",   # Green
  "damage_threshold" = "#d62728", # Red
  "owner_utility" = "#9467bd",   # Purple
  "storm_scenario" = "#e377c2"   # Pink
)

# SLR scenario colors
slr_colors <- c(
  "Intermediate" = "#4575b4",      # Blue
  "Intermediate_High" = "#fdae61", # Orange
  "High" = "#d73027"               # Red
)

# Timing category colors (sequential from urgent to delayed)
timing_colors <- c(
  "Immediate\n(0-10 yr)" = "#d73027",   # Dark red (urgent)
  "Early\n(11-25 yr)" = "#fc8d59",      # Orange-red
  "Mid-term\n(26-50 yr)" = "#fee08b",   # Yellow
  "Late\n(51-100 yr)" = "#91bfdb",      # Light blue
  "No Retreat\n(>100 yr)" = "#4575b4"   # Dark blue (safe)
)


# LOAD DATA

sensitivity_summary <- read_csv(
  file.path(derived_dir, "sensitivity_summary.csv"),
  show_col_types = FALSE
)

baseline_results <- read_csv(
  file.path(derived_dir, "retreat_schedule_baseline.csv"),
  show_col_types = FALSE
)

sensitivity_results <- read_csv(
  file.path(derived_dir, "retreat_schedule_sensitivity.csv"),
  show_col_types = FALSE
)



# DATA PREPARATION
# Create parameter category labels
sensitivity_summary <- sensitivity_summary %>%
  mutate(
    param_category = case_when(
      grepl("rental_yield", parameter_set) ~ "rental_yield",
      grepl("discount_rate", parameter_set) ~ "discount_rate",
      grepl("threshold", parameter_set) ~ "damage_threshold",
      grepl("owner_utility", parameter_set) ~ "owner_utility",
      grepl("storm", parameter_set) ~ "storm_scenario",
      TRUE ~ "baseline"
    ),
    # Create clean labels for plotting
    param_label = case_when(
      parameter_set == "baseline" ~ "Baseline (5%, 2%, 15cm, $0, w000)",
      parameter_set == "rental_yield_4pct" ~ "Rental Yield: 4%",
      parameter_set == "rental_yield_6pct" ~ "Rental Yield: 6%",
      parameter_set == "discount_rate_1.5pct" ~ "Discount Rate: 1.5%",
      parameter_set == "discount_rate_3.0pct" ~ "Discount Rate: 3.0%",
      parameter_set == "threshold_0cm" ~ "Threshold: 0cm",
      parameter_set == "threshold_30cm" ~ "Threshold: 30cm",
      parameter_set == "owner_utility_25k" ~ "Owner Utility: $25K/yr",
      parameter_set == "owner_utility_50k" ~ "Owner Utility: $50K/yr",
      parameter_set == "storm_w100" ~ "Storm: w100 (100-yr)",
      TRUE ~ parameter_set
    )
  )


# FIGURE 1: MEDIAN RETREAT YEAR COMPARISON

# Focus on the most interesting scenario (High) for main figure
high_summary <- sensitivity_summary %>%
  filter(scenario == "High") %>%
  mutate(
    param_label = fct_reorder(param_label, median_retreat),
    baseline_flag = parameter_set == "baseline"
  )

fig1 <- ggplot(high_summary, aes(x = median_retreat, y = param_label, 
                                 fill = param_category)) +
  geom_col(aes(alpha = baseline_flag), width = 0.7) +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.75), guide = "none") +
  scale_fill_manual(values = custom_colors, guide = "none") +
  geom_vline(xintercept = high_summary$median_retreat[high_summary$baseline_flag], 
             linetype = "dashed", color = "gray20", linewidth = 1) +
  annotate("text", 
           x = high_summary$median_retreat[high_summary$baseline_flag] - 2, 
           y = nrow(high_summary) - 0.5,
           label = "Baseline", color = "gray20", size = 4, hjust = 1, fontface = "bold") +
  labs(
    title = "Sensitivity of Median Retreat Year to Model Parameters",
    subtitle = "High SLR Scenario - Carpinteria Sand Point",
    x = "Median Retreat Year",
    y = NULL,
    caption = "Baseline: 5% rental yield, 2% discount rate, 15cm threshold, $0 owner utility, w000 storm"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    plot.caption = element_text(size = 9, color = "gray50", hjust = 0),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(fig_dir, "sensitivity_median_retreat_year.png"), 
       fig1, width = 10, height = 6, dpi = 300)


# FIGURE 2: IMMEDIATE RETREAT COMPARISON (STACKED BARS)

# Show all three scenarios side by side
immediate_data <- sensitivity_summary %>%
  mutate(
    param_label = factor(param_label, levels = rev(levels(high_summary$param_label)))
  )

fig2 <- ggplot(immediate_data, aes(x = immediate_pct, y = param_label, fill = scenario)) +
  geom_col(position = "dodge", alpha = 0.85, width = 0.7) +
  scale_fill_manual(values = slr_colors, name = "SLR Scenario") +
  labs(
    title = "Immediate Retreat Sensitivity (≤10 Years)",
    subtitle = "Percentage of properties requiring immediate retreat under different parameters",
    x = "Properties Requiring Immediate Retreat (%)",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

ggsave(file.path(fig_dir, "sensitivity_immediate_retreat.png"), 
       fig2, width = 10, height = 6, dpi = 300)


# FIGURE 3: PROPERTIES AT RISK BY TIMING CATEGORY

# Calculate distribution across timing categories for each parameter set
timing_dist <- sensitivity_summary %>%
  filter(scenario == "High") %>%
  select(parameter_set, param_label, param_category,
         immediate_pct, early_pct, mid_pct, late_pct, no_retreat_pct) %>%
  pivot_longer(
    cols = ends_with("_pct"),
    names_to = "timing_category",
    values_to = "percentage"
  ) %>%
  mutate(
    timing_category = factor(
      timing_category,
      levels = c("immediate_pct", "early_pct", "mid_pct", "late_pct", "no_retreat_pct"),
      labels = c("Immediate\n(0-10 yr)", "Early\n(11-25 yr)", 
                 "Mid-term\n(26-50 yr)", "Late\n(51-100 yr)", "No Retreat\n(>100 yr)")
    ),
    param_label = fct_reorder(param_label, -percentage, .fun = function(x) x[1])
  )

fig3 <- ggplot(timing_dist, aes(x = param_label, y = percentage, fill = timing_category)) +
  geom_col(position = "stack", width = 0.8) +
  scale_fill_manual(values = timing_colors, name = "Retreat Timing") +
  coord_flip() +
  labs(
    title = "Distribution of Retreat Timing Across Parameters",
    subtitle = "High SLR Scenario - How parameter choices affect when properties retreat",
    x = NULL,
    y = "Percentage of Properties (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    panel.grid.major.y = element_blank(),
    legend.position = "bottom",
    legend.key.size = unit(0.8, "cm")
  )

ggsave(file.path(fig_dir, "sensitivity_timing_distribution.png"), 
       fig3, width = 10, height = 7, dpi = 300)

# FIGURE 4: TORNADO PLOT - PARAMETER IMPACT

# Calculate deviation from baseline for each parameter
baseline_median <- sensitivity_summary %>%
  filter(parameter_set == "baseline", scenario == "High") %>%
  pull(median_retreat)

tornado_data <- sensitivity_summary %>%
  filter(scenario == "High", parameter_set != "baseline") %>%
  mutate(
    deviation = median_retreat - baseline_median,
    abs_deviation = abs(deviation),
    direction = ifelse(deviation > 0, "Delays Retreat", "Accelerates Retreat")
  ) %>%
  arrange(desc(abs_deviation))

fig4 <- ggplot(tornado_data, aes(x = deviation, y = fct_reorder(param_label, abs_deviation),
                                 fill = direction)) +
  geom_col(alpha = 0.85, width = 0.7) +
  geom_vline(xintercept = 0, linetype = "solid", color = "black", linewidth = 1) +
  scale_fill_manual(
    values = c("Accelerates Retreat" = "#d73027", "Delays Retreat" = "#4575b4"),
    name = NULL
  ) +
  labs(
    title = "Parameter Sensitivity: Impact on Median Retreat Year",
    subtitle = paste0("High SLR Scenario - Deviation from baseline (", round(baseline_median, 1), " years)"),
    x = "Change in Median Retreat Year (years)",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    panel.grid.major.y = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 11)
  )

ggsave(file.path(fig_dir, "sensitivity_tornado_plot.png"), 
       fig4, width = 10, height = 6, dpi = 300)

message("  ✓ Saved: sensitivity_tornado_plot.png")


# FIGURE 5: PROPERTY VALUE AT RISK

value_data <- sensitivity_summary %>%
  filter(scenario == "High") %>%
  mutate(
    param_label = fct_reorder(param_label, total_value_at_risk_millions)
  )

fig5 <- ggplot(value_data, aes(x = total_value_at_risk_millions, y = param_label,
                               fill = param_category)) +
  geom_col(alpha = 0.8) +
  scale_fill_manual(values = custom_colors, guide = "none") +
  labs(
    title = "Total Property Value at Risk by Parameter Setting",
    subtitle = "High SLR Scenario - Properties requiring retreat within 100 years",
    x = "Total Property Value at Risk ($ Millions)",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    panel.grid.major.y = element_blank()
  )

ggsave(file.path(fig_dir, "sensitivity_value_at_risk.png"), 
       fig5, width = 10, height = 6, dpi = 300)


# FIGURE 6: MULTI-PANEL COMPARISON ACROSS SLR SCENARIOS

# Create individual plots for each SLR scenario
create_scenario_plot <- function(scen_name) {
  scen_data <- sensitivity_summary %>%
    filter(scenario == scen_name) %>%
    mutate(param_label = fct_reorder(param_label, median_retreat))
  
  baseline_val <- scen_data %>% 
    filter(parameter_set == "baseline") %>% 
    pull(median_retreat)
  
  ggplot(scen_data, aes(x = median_retreat, y = param_label, fill = param_category)) +
    geom_col(alpha = 0.8) +
    geom_vline(xintercept = baseline_val, linetype = "dashed", 
               color = "gray40", linewidth = 0.6) +
    scale_fill_manual(values = custom_colors, guide = "none") +
    labs(
      title = scen_name,
      x = if(scen_name == "High") "Median Retreat Year" else NULL,
      y = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
      panel.grid.major.y = element_blank(),
      axis.text.y = if(scen_name == "Intermediate") element_text() else element_blank()
    )
}

p_int <- create_scenario_plot("Intermediate")
p_int_high <- create_scenario_plot("Intermediate_High")
p_high <- create_scenario_plot("High")

fig6 <- p_int / p_int_high / p_high +
  plot_annotation(
    title = "Sensitivity Analysis Across All SLR Scenarios",
    subtitle = "Median retreat year for different parameter combinations",
    theme = theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11, color = "gray40")
    )
  )

ggsave(file.path(fig_dir, "sensitivity_all_scenarios_panel.png"), 
       fig6, width = 10, height = 12, dpi = 300)


# SUMMARY STATISTICS TABLE

# Key insights table
key_stats <- sensitivity_summary %>%
  filter(scenario == "High") %>%
  group_by(param_category) %>%
  summarise(
    parameters = n(),
    min_median_retreat = min(median_retreat, na.rm = TRUE),
    max_median_retreat = max(median_retreat, na.rm = TRUE),
    range = max_median_retreat - min_median_retreat,
    max_immediate_pct = max(immediate_pct, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(range))

print(key_stats)

# Save summary table
write_csv(key_stats, file.path(fig_dir, "sensitivity_parameter_ranges.csv"))

# FINAL SUMMARY
message("\nGenerated figures:")
message("  1. sensitivity_median_retreat_year.png - Main comparison of median retreat years")
message("  2. sensitivity_immediate_retreat.png - Immediate retreat percentages")
message("  3. sensitivity_timing_distribution.png - Distribution across timing categories")
message("  4. sensitivity_tornado_plot.png - Parameter impact (tornado diagram)")
message("  5. sensitivity_value_at_risk.png - Property value at risk")
message("  6. sensitivity_all_scenarios_panel.png - Multi-panel comparison")
message("\nAll figures saved to: ", normalizePath(fig_dir))
message("\nKey findings:")
message("  - Storm scenario (w100) has the largest impact on retreat timing")
message("  - Rental yield significantly affects economic viability of staying")
message("  - Owner utility can delay retreat substantially")
message("  - Discount rate and damage threshold have minimal impact")

