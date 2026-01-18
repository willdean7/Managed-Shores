# Managed Shores

## Project Description
A buyout-leaseback economic modeling framework for managed coastal retreat in California, integrating high-resolution sea level rise hazard data with property-scale financial analysis to determine optimal retreat timing under climate change scenarios.

## Motivation
Rising sea levels and coastal hazards threaten California's coastal communities and property values. Traditional adaptation approaches often focus on hard armoring or abrupt relocation. This project develops and applies a novel "buyout-leaseback" model where governments purchase at-risk properties before catastrophic damage but lease them back to current owners, allowing continued use while managing long-term risk. The framework provides tools to evaluate when retreat becomes economically optimal compared to continued occupancy.

## Methods and Approach

### CoSMoS Hazard Data Integration
High-resolution coastal hazard modeling using USGS CoSMoS (Coastal Storm Modeling System) data including:
- Sea level rise inundation scenarios (0-10 feet SLR)
- Episodic coastal flooding from storm events
- Chronic beach and cliff erosion projections
- Wave impacts and coastal change envelopes

### Buyout-Leaseback Economic Model
Novel financial framework evaluating:
- Net present value (NPV) of government buyout-leaseback programs
- Optimal retreat timing balancing rental income against flood damages
- Structure depreciation and land value appreciation over time
- Government vs. private property owner economic perspectives
- Sensitivity analysis across discount rates, damage scenarios, and SLR projections

### Case Studies
Application to five California coastal communities:
- **Carpinteria** - Mix of residential and commercial beachfront
- **Isla Vista** - High-density student housing on coastal bluffs
- **King Salmon** - Low-income community on Humboldt Bay
- **Pacifica** - Eroding bluff properties south of San Francisco
- **Silver Strand** - Narrow barrier island in Oxnard

### Interactive Visualization
Shiny dashboard (`cosmos_shiny_v2.R`) for exploring:
- Property-level retreat timing and economic outcomes
- Spatial patterns of vulnerability and optimal retreat years
- Comparison across case studies and scenarios
- Real-time parameter sensitivity testing

## Repository Structure
```
Managed_Shores/
├── code/
│   ├── processed/                          # Current analysis scripts
│   │   ├── extract_cosmos_metrics_unified.R   # CoSMoS data extraction
│   │   ├── cosmos_process.R                   # Hazard data processing
│   │   ├── cosmos_process_cliff.R             # Cliff erosion processing
│   │   ├── interpolate_cosmos.R               # Spatial interpolation
│   │   ├── redfin_data_code_ms.R             # Property data processing
│   │   ├── cosmos_valuation_v3.R             # Main economic model (CURRENT)
│   │   ├── monte_carlo_storms.R              # Storm damage uncertainty
│   │   └── cosmos_shiny_v2.R                 # Interactive dashboard (CURRENT)
│   ├── archive/                            # Deprecated script versions
│   ├── raw/                                # Original collaborator scripts
│   └── README.md                           # Code documentation
├── data/
│   ├── carpinteria/                        # Case study: Carpinteria
│   │   ├── cosmos/                         # CoSMoS hazard data
│   │   ├── derived/                        # Processed datasets
│   │   └── redfin_2025-01.csv             # Property data
│   ├── isla_vista/                         # Case study: Isla Vista
│   ├── king_salmon/                        # Case study: King Salmon
│   ├── pacifica/                           # Case study: Pacifica
│   ├── silver_strand/                      # Case study: Silver Strand
│   ├── landprices_CA_ms.csv               # State land value index
│   └── README.md                           # Data documentation
├── doc/                                    # Project documentation
│   └── LICENSE-data.txt                    # Data license (CC BY 4.0)
├── LICENSE                                 # Code license (MIT)
├── README.md                               # This file
├── .gitignore                              # Version control exclusions
└── Managed_Shores.Rproj                   # RStudio project file
```

## Installation

### Prerequisites
- **R** (>= 4.0) and RStudio recommended
- **R packages**: 
  ```r
  # Core packages
  install.packages(c("tidyverse", "sf", "terra", "raster"))
  
  # Spatial analysis
  install.packages(c("lwgeom", "stars", "units"))
  
  # Data acquisition
  install.packages(c("tidycensus", "rvest", "httr"))
  
  # Visualization
  install.packages(c("shiny", "leaflet", "plotly", "DT"))
  ```

### Setup
1. Clone the repository:
   ```bash
   git clone https://github.com/willdean7/Managed-Shores.git
   cd Managed_Shores
   ```

2. Open `Managed_Shores.Rproj` in RStudio

3. CoSMoS data can be downloaded from [USGS CoSMoS](https://www.usgs.gov/apps/coastal-storm-modeling-system-cosmos) or contact the project authors for preprocessed datasets

## Usage

### Complete Analysis Workflow

1. **Extract CoSMoS Hazard Data**
   ```r
   source("code/processed/extract_cosmos_metrics_unified.R")
   # Extracts inundation, erosion, and wave data for all case studies
   ```

2. **Process Property Data**
   ```r
   source("code/processed/redfin_data_code_ms.R")
   # Cleans Redfin sales data, estimates rental values, geocodes properties
   ```

3. **Spatial Integration**
   ```r
   source("code/processed/interpolate_cosmos.R")
   # Matches CoSMoS hazards to property parcel locations
   ```

4. **Run Economic Model**
   ```r
   source("code/processed/cosmos_valuation_v3.R")
   # Calculates NPV of buyout-leaseback scenarios
   # Determines optimal retreat timing for each property
   # Outputs: data/[case_study]/derived/valuation_results.csv
   ```

5. **Launch Interactive Dashboard**
   ```r
   source("code/processed/cosmos_shiny_v2.R")
   # Explore results, compare properties, test sensitivity
   ```

### Key Outputs
All processed datasets saved to `data/[case_study]/derived/`:
- Property-level hazard exposure timeseries
- Economic valuation results with optimal retreat years
- Vulnerability metrics and risk categorizations
- NPV calculations for government and property owner perspectives

## Data Sources

- **Property Data**: Redfin residential sales and tax assessor records
- **Hazard Data**: USGS Coastal Storm Modeling System (CoSMoS) v3.0
- **Land Values**: Federal Housing Finance Agency (FHFA) House Price Index
- **Demographics**: US Census Bureau American Community Survey (ACS)
- **Coastline**: Natural Earth and NOAA coastal datasets

## Key Findings (Preliminary)

Analysis across case studies reveals:
- Optimal retreat timing varies from 2030s-2080s depending on property characteristics and hazard exposure
- Buyout-leaseback programs can be economically advantageous for governments when retreat occurs before catastrophic damage
- High-value properties with low flood risk show later optimal retreat years
- Cliff erosion drives earlier retreat in communities like Pacifica and Isla Vista
- Economic outcomes highly sensitive to discount rates and damage assumptions

*Full results pending thesis completion (Spring 2026)*

## Contributing

Contributions welcome! This is an active research project.

- **Bug reports**: Open an issue describing the problem and steps to reproduce
- **Feature requests**: Describe the proposed enhancement and use case
- **Code contributions**: Fork the repo, create a feature branch, and submit a pull request

Please maintain:
- Clear code comments and documentation
- Descriptive commit messages
- Consistency with existing code style
- Updates to relevant README files

See `code/README.md` for detailed script documentation and best practices.

## Credits

**Lead Researcher & Developer**: William Dean (MESM '26, UC Santa Barbara Bren School)

**Project Team**:
- Wesley Noble 
- Lilia Mourier 
- Daniel O'Shea 
- Ada Ekpezu Olumba 

**Faculty Advisor**: Dr. Andrew Plantinga (Bren School & Economics Dept.)

**External Advisors**: 
- Summer Gray (California Coastal Commission)
- Kim Kimbell
- Charles Lester (UCSB Ocean and Coastal Policy Center)

**Technical Foundations**: 
- Base scripts and framework: Jonah Danzinger

**Sponsors/Clients**: 
- California Coastal Commission
- Rincon Consultants
- UCSB Ocean and Coastal Policy Center

## Academic Context

This project serves as the Group Project thesis for the Master of Environmental Science and Management (MESM) program at UC Santa Barbara's Bren School of Environmental Science & Management. The work integrates environmental economics, coastal engineering, spatial analysis, and climate adaptation policy.

**Thesis Defense**: Spring 2026

## License

- **Code**: MIT License (see `LICENSE`)
- **Data & Documentation**: Creative Commons Attribution 4.0 International (CC BY 4.0) (see `doc/LICENSE-data.txt`)

## Citation

If you use this work, please cite:

```
Dean, W., Noble, W., Mourier, L., O'Shea, D., & Olumba, A.E. (2026). 
Managed Shores: A Buyout-Leaseback Framework for Coastal Retreat in California. 
Master's Group Project, Bren School of Environmental Science & Management, 
UC Santa Barbara.
```

## Contact

**William Dean**  
willdean7@bren.ucsb.edu  
[GitHub](https://github.com/willdean7)

For questions about the project, data access, or collaboration opportunities, please open an issue or contact directly.

---

*Last updated: January 2026*
