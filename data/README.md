# Data Folder README

This folder contains all datasets used in the Managed_Shores project.

## Structure/Case Study Folders

- **carpinteria/**: Contains data and metadata specific to the Carpinteria case study (e.g., CoSMoS hazard data, Redfin property data)
- **silver_strand/**: Contains data and metadata specific to the Silver Strand case study (e.g., CoSMoS hazard data, Redfin property data)
- **king_salmon/**: Contains data and metadata specific to the King Salmon case study (e.g., CoSMoS hazard data, Redfin property data)
- **isla_vista/**: Contains data and metadata specific to the Isla Vista case study (e.g., CoSMoS hazard data, Redfin property data)
- **pacifica/**: Contains data and metadata specific to the Pacifica case study (e.g., CoSMoS hazard data, Redfin property data)

## File Descriptions

| File Name              | Description                                    | Source                                      | Notes                |
|------------------------|------------------------------------------------|---------------------------------------------|----------------------|
| landprices_CA_ms.csv   | Raw California home value index data           | https://www.fhfa.gov/research/papers/wp1901 | Do not edit directly |

### Case Study Folder Contents

Each case study folder contains:
- **cosmos/**: CoSMoS sea level rise hazard data (inundation, erosion, wave impacts)
- **derived/**: Processed datasets derived from raw inputs
- **redfin_2025-XX.csv**: Property sales and valuation data from Redfin
- **redfin_df.csv**: Cleaned and processed Redfin data for analysis

## Usage

- Update this README as new data files are added or changed.
- Do not edit raw data files directly - create derived versions in the `derived/` subfolder

## Contact

For questions about the data, contact the repository owner (willdean7).