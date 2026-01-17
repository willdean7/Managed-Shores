# Code Folder README

This README provides documentation and guidance for the `code` folder of the Managed_Shores project. It outlines the folder structure, describes the main scripts, and provides instructions for usage and best practices.

---

## Folder Structure

- **processed/**: Contains current, active analysis scripts for the Managed Shores project
- **archive/**: Contains deprecated script versions and experimental code for reference
- **raw/**: Contains original, unmodified scripts and data files received from collaborators (Jonah Danzinger)
- **README.md**: This documentation file for the code folder

---

## Active Scripts (processed/)

### CoSMoS Data Processing

| Script Name                        | Description                                                                                              | Author          | Notes                                      |
|-----------------------------------|----------------------------------------------------------------------------------------------------------|-----------------|--------------------------------------------|
| `extract_cosmos_metrics_unified.R` | Extracts and processes CoSMoS sea level rise hazard metrics (inundation, erosion, wave impacts) for all case study sites | William Dean | Unified extraction pipeline for all locations |
| `cosmos_process.R`                | Processes raw CoSMoS data for individual case study locations | William Dean | Site-specific processing |
| `cosmos_process_cliff.R`          | Specialized processing for cliff erosion hazard data | William Dean | Used for bluff/cliff properties |
| `interpolate_cosmos.R`            | Interpolates CoSMoS hazard data to property parcel locations | William Dean | Spatial interpolation to match property data |

### Property & Hazard Data

| Script Name                | Description                                                                                              | Author          | Notes                                      |
|---------------------------|----------------------------------------------------------------------------------------------------------|-----------------|--------------------------------------------|
| `redfin_data_code_ms.R`   | Aggregates and cleans Redfin property sales data, scrapes rental value estimates, performs regression to estimate missing rental values | Jonah Danzinger (adapted by WD) | Integrates US Census tract-level variables |

### Economic Valuation & Analysis

| Script Name                     | Description                                                                                              | Author          | Notes                                      |
|--------------------------------|----------------------------------------------------------------------------------------------------------|-----------------|--------------------------------------------|
| `cosmos_valuation_v3.R`        | **CURRENT VERSION** - Main economic valuation model integrating property, hazard, and land value data; calculates NPV of buyout-leaseback scenarios and optimal retreat timing | William Dean | Primary analysis script for thesis |
| `monte_carlo_storms.R`         | Stochastic storm modeling for uncertainty analysis in damage estimates | William Dean | Monte Carlo simulations for storm damages |

### Visualization & Interactive Tools

| Script Name              | Description                                                                                              | Author          | Notes                                      |
|-------------------------|----------------------------------------------------------------------------------------------------------|-----------------|--------------------------------------------|
| `cosmos_shiny_v2.R`     | **CURRENT VERSION** - Interactive Shiny dashboard for exploring CoSMoS hazard data, property vulnerability, and buyout-leaseback scenarios | William Dean | Main visualization tool for thesis |

---

## Archived Scripts (archive/)

Scripts in this folder represent earlier versions or experimental approaches that have been superseded by current scripts in `processed/`.

### Earlier Valuation Models
- `cosmos_valuation_v2.R` - Previous version of economic model
- `cosmos_valuation_v1.R` - Initial version of economic model  
- `cosmos_valuation.R` - Original prototype
- `MSGRP_valuationcode_ms.R` - Reference implementation from Jonah Danzinger

### Earlier Visualization Scripts
- `cosmos_shiny_v1.R` - Previous Shiny dashboard version
- `cosmos_shiny.R` - Original Shiny prototype
- `cosmos_visualizations.R` - Static visualization scripts
- `cosmos_spatial_comparisons.R` - Spatial comparison tools
- `comparisons_v1.R` - Earlier comparison dashboard version
- `comparisons.R` - Comparison tool prototype

### Supporting/Utility Scripts
- `calc_vulnerability_metrics.R` - Vulnerability metric calculations (functionality now integrated into v3)
- `cosmos_parcelprocess.R` - Parcel processing utilities
- `waves_ms_parcel.R` - Wave run-up to parcel assignment
- `waves_ms_parcel_fast.R` - Optimized version of wave processing
- `waves_ms.R` - Original wave processing script
- `shoreline.R` - Shoreline position processing
- `slr_inundation.R` - Sea level rise inundation calculations
- `cliffs.R` - Cliff erosion processing
- `background.R` - Background data processing utilities

---

## Original Scripts (raw/)

Contains original scripts received from collaborator Jonah Danzinger:

| Script Name                  | Description                                                                                              | Author          | Notes                                      |
|-----------------------------|----------------------------------------------------------------------------------------------------------|-----------------|--------------------------------------------|
| `redfin_data_code.R`        | Original Redfin data processing script | Jonah Danzinger | Adapted version in processed/ as `redfin_data_code_ms.R` |
| `MSGRP_valuationcode.R`     | Original economic valuation model | Jonah Danzinger | Reference implementation - adapted in v3 |
| `waves.R`                   | Original wave processing script | Jonah Danzinger | Adapted and expanded in processed/ |
| `landprices_CA.csv`         | California home value index data | FHFA | Moved to data/ folder |

---

## Data Processing Workflow

1. **CoSMoS Extraction**: `extract_cosmos_metrics_unified.R` → extracts hazard data for all case study sites
2. **Property Data**: `redfin_data_code_ms.R` → cleans Redfin data and estimates rental values
3. **Spatial Integration**: `interpolate_cosmos.R` + `cosmos_process.R` → match hazards to properties
4. **Economic Modeling**: `cosmos_valuation_v3.R` → run buyout-leaseback economic model with optimal retreat timing
5. **Visualization**: `cosmos_shiny_v2.R` → explore results interactively with dashboard

---

## Usage Instructions

1. **Running Analysis**: Execute scripts in the order specified in the workflow above
2. **Case Study Selection**: Most scripts accept a case study location parameter: "carpinteria", "king_salmon", "isla_vista", "pacifica", or "silver_strand"
3. **Output Location**: Processed data outputs are saved to `data/[case_study]/derived/`
4. **Working with Archive**: Reference archived scripts for methodology evolution, but use current versions in `processed/` for analysis

---

## Best Practices

- **Use Current Versions**: Always use scripts from `processed/` folder for active analysis
- **Version Control**: When significantly updating a script, increment version number (v3 → v4) and move old version to `archive/`
- **Archive Don't Delete**: Move superseded scripts to `archive/` rather than deleting for methodology transparency
- **Document Changes**: Note major changes in script headers or commit messages
- **Preserve Originals**: Never modify scripts in `raw/` - create adapted versions in `processed/`

---

## Script Dependencies

**Core R Packages:**
- `tidyverse` - data manipulation and visualization
- `sf` - spatial data handling  
- `terra` / `raster` - raster data processing for CoSMoS
- `shiny` - interactive dashboards
- `leaflet` - interactive mapping
- `DT` - interactive data tables in Shiny

**Analysis-Specific:**
- `rvest` - web scraping for Redfin rental estimates
- `tidycensus` - Census demographic data
- Various spatial packages (`lwgeom`, `stars`, etc.)

---

## Contact

For questions about specific scripts or modeling approaches, contact the repository owner (willdean7).

---