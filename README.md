# Managed-Shores

## Project Description
A framework and financial modeling toolkit for managed shoreline retreat in California.

## Motivation
Rising sea levels threaten California’s coastlines. This project provides tools and analysis to support local adaptation and managed retreat strategies.

## Methods and Approach
-Wave Runup and Flood Risk Modeling:
High-resolution modeling of coastal wave run-up, flood exposure, and parcel-level flood risk using open geospatial and oceanographic data.

-Financial Evaluation:
Buyout-leaseback and managed retreat financial modeling, including net present value (NPV) of rental income, structure/land value, and flood damages.

-Case Studies and Qualitative Analysis:
Application to real-world California communities (e.g., Carpinteria, Silver Strand) with integration of property, land, and demographic data.

-Interactive Dashboards:
Shiny dashboards for exploring retreat timing, vulnerability, and economic/physical risk drivers.


## Repository Structure
Managed-Shores/
├── code/
│   ├── raw/                  # Original, unmodified data and scripts
│   ├── processed/            # Cleaned and derived datasets/scripts
│   ├── redfin_data_code_ms.R # Property data cleaning, price/rent scraping, geocoding
│   ├── waves_ms.R            # Regional wave run-up modeling
│   ├── waves_ms_parcel.R     # Parcel-level wave run-up assignment
│   ├── MSGRP_ms.R            # Economic modeling, NPV, and retreat timing
│   ├── calc_vulnerability_metrics.R # Vulnerability metric calculation
│   ├── comparisons.R         # Shiny dashboard for retreat/vulnerability comparison
│   └── README.md             # Code folder documentation
├── data/
│   ├── raw/                  # Raw data files (Redfin, land values, wave data, etc.)
│   ├── processed/            # Processed/cleaned datasets for analysis
│   └── README.md             # Data folder documentation
├── doc/
│   ├── LICENSE-data.txt      # Data/documentation license (CC BY 4.0)
│   └── ...                   # Project reports, figures, supplementary docs
├── LICENSE                   # Code license (MIT)
├── README.md                 # This general project README
└── .gitignore                # Files to be excluded from version control


## Installation
Prerequisites:
R (>= 4.0) and RStudio recommended
R packages: tidyverse, sf, rnaturalearth, elevatr, tidygeocoder, mapview, shiny, shinydashboard, plotly, DT, and others as called in scripts

Clone the repository:
https://github.com/willdean7/Managed-Shores.git

Download or place raw data files into data/raw/ as needed.

## Usage
Data Preparation:
Run code/redfin_data_code_ms.R to clean and enrich property data.

Wave Runup Modeling:
Use code/waves_ms.R or code/waves_ms_parcel.R for regional and parcel-level wave run-up estimates.

Economic and Retreat Modeling:
Run code/MSGRP_ms.R to integrate property, hazard, and economic data and compute optimal retreat timing.

Vulnerability Metrics:
Use code/calc_vulnerability_metrics.R to generate vulnerability and risk metrics for each parcel.

Visualization and Exploration:
Launch the interactive dashboard with code/comparisons.R to explore results and compare properties.

Reproducibility:
All major outputs are saved in data/processed/ for traceability and further analysis.

## Data and Documentation
Data Sources:
Redfin property data, NOAA/NDBC wave data, US Census/ACS, Natural Earth, AWS elevation, and California land value datasets.

Documentation:
See code/README.md and data/README.md for detailed documentation of scripts and datasets.

## Contributing
Contributions are welcome!

Please open issues or pull requests for bug reports, feature requests, or improvements.

Follow best practices for code style, documentation, and version control.

See code/README.md for more on project organization and script structure.

## Credits
Data Managers: William Dean and Wesley Noble
Base scripts and framework: Jonah Danzinger
Contributors: Lilia Mourier, Daniel O'Shea, Ada Ekpezu Olumba
Faculty Advisor: Andrew Plantinga
External Advisors: Summer Gray, Kim Kimbell
Sponsors/Clients: California Coastal Commission, Rincon Consultants, Charles Lester (UCSB Ocean and Coastal Policy Center)
## License
- Code: MIT License (see LICENSE)
- Data/Docs: CC BY 4.0 (see doc/LICENSE-data.txt)
