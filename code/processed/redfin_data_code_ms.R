#Author: Jonah Danziger
#Edits: William Dean
# Purpose: Prepare Redfin property data for coastal retreat economic analysis
# 
# Inputs:
#   - data/{site}/redfin_2025-##.csv (Redfin export files, max 350 properties each)
# 
# Outputs:
#   - data/{site}/redfin_df.csv (cleaned property dataset with rental values)
#   - data/{site}/redfin_df_temp.csv (checkpoint before final processing)
# 
# Workflow:
#   1. Concatenate multiple Redfin CSV files (Redfin limits exports to 350 rows)
#   2. Clean and filter to relevant property types
#   3. Flag outdated prices (< threshold) and scrape current Redfin Estimates
#   4. Geocode addresses to create spatial dataset
#   5. Apply 5% baseline rental yield (literature-based, not scraped)
#   6. Add spatial covariates (elevation, distance to coast)
#   7. Add demographic covariates (ACS tract-level data)
#   8. Assign primary hazard type based on site geomorphology
#   9. Create stable property IDs and save final dataset

rm(list=ls()) # clear the environment

library(tidyverse)   
library(here)
library(rvest)       
library(httr)        
library(jsonlite)    
library(sf)          
library(elevatr)     
library(rnaturalearth)  
library(tidycensus)
library(tidygeocoder)
library(mapview)     
library(digest)      
library(mgcv)        


# CONFIGURATION - EDIT THIS SECTION FOR EACH CASE STUDY
# Study area configuration
CASE_NAME <- "king_salmon"  # Change to study area: carpinteria, pacifica, stinson, isla_vista, etc.
DATA_DIR <- here("data", CASE_NAME)
dir.create(DATA_DIR, recursive = TRUE, showWarnings = FALSE)

# Price threshold: flag properties below this value as potentially outdated
# Recommendation: Set to ~50-70% of local median home price
# 
# How to set this:
#   1. Look up median home price for your area (Redfin, Zillow)
#   2. Set threshold to 50-70% of that median
#   3. Higher % = fewer flags (only catch obvious outliers)
#   4. Lower % = more flags (catch more potentially old sale data)
#
# Example thresholds for California coastal towns (2024-2025 data):
#   Carpinteria:   $950,000    (median ~$1.35-1.40M, Redfin/Rocket Homes 2025)
#   Isla Vista:    $850,000    (median ~$1.35-1.40M single-family, Zillow 2025)*
#   King Salmon:   $270,000    (median ~$400-450K est., Humboldt County MLS 2025)**
#   Malibu:        $1,900,000  (median ~$2.9-3.3M, Redfin/Zillow Nov 2025)
#   Pacifica:      $750,000    (median ~$1.15-1.30M, Redfin/ATTOM 2025)
#   Stinson Beach: $3,300,000  (median ~$5.0-5.2M, Redfin/PropertyShark 2025)
#
# Sources: 
#   - Redfin Housing Market Reports (accessed Jan 2026)
#   - Zillow Home Value Index (accessed Jan 2026)
#   - PropertyShark Market Analysis (accessed Jan 2026)
#   - Rocket Homes/ATTOM Market Reports (2025)
#   - Humboldt Association of REALTORS® (2025)
#
# Market-specific notes:
#   * Isla Vista: Market dominated by UCSB student housing; median varies 
#     significantly ($625K-$3M) depending on whether multi-family rental 
#     properties vs. single-family homes are included. Use $850K threshold 
#     to capture typical coastal residential properties.
#
#   ** King Salmon: Very small community (~200-300 people) with limited sales 
#      data. Estimated median based on Humboldt County median ($425K), Eureka 
#      median ($380K), and observed listing range ($275K-$540K). Community 
#      characterized as "affordable waterfront" and is identified as highest 
#      sea level rise vulnerability on U.S. West Coast—critical for managed 
#      retreat analysis.

PRICE_THRESHOLD <- 270000  # ← UPDATE THIS for each case study location

# Validation flags
RUN_CROSS_VALIDATION <- TRUE  # Set FALSE for faster runs (but less validation)
BOOTSTRAP_ITERATIONS <- 300   # For rental model uncertainty

message("Property Data Processing: ", toupper(CASE_NAME))
message("Price threshold: $", format(PRICE_THRESHOLD, big.mark = ","))
message("  (Properties below this flagged as potentially outdated)")


# PART 1: Load and Combine Redfin Data
message("→ Loading Redfin export files...")

redfin_df <- read_csv(here(DATA_DIR, "redfin_2025-01.csv"))

# Combine multiple CSV files (Redfin limits exports to 350 properties)
for (i in 2:70){
  if (i < 10){
    path <- file.path(DATA_DIR, paste0("redfin_2025-0", i, ".csv"))
  } else {
    path <- file.path(DATA_DIR, paste0("redfin_2025-", i, ".csv"))
  }
  
  if (file.exists(path)) {
    red_temp <- read_csv(path, show_col_types = FALSE)
    redfin_df <- rbind(redfin_df, red_temp)
  }
}

message("✓ Loaded ", nrow(redfin_df), " properties from Redfin exports\n")

# Clean column names
names(redfin_df) <- str_trim(names(redfin_df))

# Rename long URL column
redfin_df <- redfin_df |> 
  rename(
    URL = `URL (SEE https://www.redfin.com/buy-a-home/comparative-market-analysis FOR INFO ON PRICING)`
  )

# Filter to relevant sale and property types
redfin_df <- redfin_df |> 
  filter(`SALE TYPE` %in% c("PAST SALE", "RENTAL", "MLS Listing")) %>% 
  filter(`PROPERTY TYPE` %in% c("Single Family Residential", "Condo/Co-op", 
                                "Townhouse", "Multi-Family (2-4 Unit)"))

# Deduplicate by address (best proxy for same property)
n_before_dedup <- nrow(redfin_df)
redfin_df <- redfin_df %>% 
  distinct(ADDRESS, `PROPERTY TYPE`, .keep_all = TRUE)
n_after_dedup <- nrow(redfin_df)

message("→ Cleaned and filtered data:")
message("  Removed ", n_before_dedup - n_after_dedup, " duplicates")
message("  Final count: ", n_after_dedup, " unique properties\n")


# PART 2: Replace Old or Missing Prices via Redfin Scraper


message("→ Identifying outdated property prices...")
message("  Using threshold: $", format(PRICE_THRESHOLD, big.mark = ","))
message("  (Recommendation: ~50-70% of local median home price)\n")

# Flag outdated or missing prices
redfin_df <- redfin_df %>%
  mutate(flag_low_price = PRICE < PRICE_THRESHOLD | is.na(PRICE))

n_flagged <- sum(redfin_df$flag_low_price, na.rm = TRUE)
pct_flagged <- round(100 * n_flagged / nrow(redfin_df), 1)

message("  Flagged ", n_flagged, " properties (", pct_flagged, "% of total)")
message("  These will be updated with current Redfin estimates\n")

# Quality check
if (pct_flagged > 50) {
  warning("     WARNING: ", pct_flagged, "% of properties flagged!")
  warning("     This is unusually high - consider raising PRICE_THRESHOLD")
  warning("     Current threshold: $", format(PRICE_THRESHOLD, big.mark = ","))
  warning("     Suggested: Check median price for ", CASE_NAME, " and set threshold to ~60% of median\n")
} else if (pct_flagged < 5) {
  message("     NOTE: Only ", pct_flagged, "% flagged (threshold may be conservative)")
  message("     This is fine - we'll only update obvious outliers\n")
}

# Initialize column to store scraped estimates
redfin_df$assessed_or_estimate <- NA_real_

# Scraper function (unchanged - works well)
get_redfin_estimate <- function(url) {
  page <- tryCatch({
    GET(url, add_headers(
      `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36"
    ))
  }, error = function(e) {
    message("GET failed: ", url)
    return(NA_real_)
  })
  
  if (!inherits(page, "response") || status_code(page) != 200) {
    message("Invalid response: ", url)
    return(NA_real_)
  }
  
  html <- tryCatch(read_html(page), error = function(e) {
    message("HTML parsing failed: ", url)
    return(NA_real_)
  })
  if (is.null(html)) return(NA_real_)
  
  # Extract full text of the page
  full_text <- html_text2(html)
  
  # Find a pattern where a dollar value is followed closely by "Redfin Estimate"
  pattern <- "\\$[\\d,]+(?=\\s*Redfin Estimate)"
  match <- str_extract(full_text, pattern)
  
  if (!is.na(match)) {
    return(as.numeric(gsub("[\\$,]", "", match)))
  }
  
  return(NA_real_)
}

# Run the function on flagged properties
message("→ Scraping current Redfin estimates (this may take a while)...")
low_price_indices <- which(redfin_df$flag_low_price & !is.na(redfin_df$URL) & is.na(redfin_df$assessed_or_estimate))

if (length(low_price_indices) > 0) {
  for (i in low_price_indices) {
    url <- redfin_df$URL[i]
    if (i %% 10 == 0) {  # Progress every 10 properties
      message("  Scraped ", i, " of ", length(low_price_indices), " properties...")
    }
    redfin_df$assessed_or_estimate[i] <- get_redfin_estimate(url)
    Sys.sleep(2.5)  # Rate limiting
  }
  message("✓ Scraping complete\n")
}

# Replace low prices with scraped Redfin Estimate values when available
n_updated <- sum(!is.na(redfin_df$assessed_or_estimate))
redfin_df <- redfin_df %>%
  mutate(PRICE = ifelse(flag_low_price & !is.na(assessed_or_estimate), assessed_or_estimate, PRICE))

message("  Updated ", n_updated, " property prices with current estimates")

# Drop flagged properties where no Redfin Estimate was found 
n_before_filter <- nrow(redfin_df)
redfin_df <- redfin_df %>%
  filter(!(flag_low_price & is.na(assessed_or_estimate)))
n_dropped <- n_before_filter - nrow(redfin_df)

message("  Dropped ", n_dropped, " properties without valid prices")
message("  Remaining: ", nrow(redfin_df), " properties\n")

# Remove temporary columns
redfin_df <- redfin_df %>% dplyr::select(-flag_low_price, -assessed_or_estimate)


# PART 3: Geocoding

message("→ Geocoding property addresses...")

library(tidygeocoder)
library(mapview)

# Combine address fields
redfin_df <- redfin_df |> 
  mutate(full_address = paste(ADDRESS, CITY, `STATE OR PROVINCE`, sep = ", "))

# Geocode in batches to avoid overwhelming the service
batch_size <- 50
n <- nrow(redfin_df)
batches <- split(redfin_df, ceiling(seq_len(n) / batch_size))

message("  Processing ", length(batches), " batches of ", batch_size, " addresses each...")

# Geocode batches with a pause between them
results <- map_dfr(batches, function(df_batch) {
  Sys.sleep(5)  # Pause between batches
  tryCatch(
    geocode(df_batch, address = full_address, method = "osm", lat = LATITUDE, long = LONGITUDE),
    error = function(e) {
      warning("Skipping batch due to error: ", conditionMessage(e))
      df_batch %>% mutate(LATITUDE = NA, LONGITUDE = NA)
    }
  )
})

# Convert to spatial object
redfin_geocoded <- results %>%
  rename(
    LATITUDE = LATITUDE...26,
    LONGITUDE = LONGITUDE...27
  )

# Count successful geocoding
n_geocoded <- sum(!is.na(redfin_geocoded$LATITUDE) & !is.na(redfin_geocoded$LONGITUDE))
message("✓ Successfully geocoded ", n_geocoded, " / ", nrow(redfin_geocoded), " properties\n")

redfin_sf <- st_as_sf(redfin_geocoded, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

# Optional: Visualize to check geocoding quality
mapview(redfin_sf, zcol = "SALE TYPE", col.regions = c("blue", "green"))


# PART 4: Apply Baseline Rental Yield

message("→ Applying baseline rental yield from literature...\n")

# Based on literature review and preliminary scraping attempts that yielded
# unreliable results (1-3% annual yields suggesting data quality issues),
# we apply a conservative 5% annual rental yield baseline.
# 
# Literature support:
#   - Typical residential rental yields: 3-8% annually
#   - Coastal California markets: 4-6% (accounting for higher property values)
#   - Conservative estimate: 5% (will test 4% and 6% in sensitivity analysis)

BASELINE_YIELD_PCT <- 5.0

message("  RENTAL YIELD ASSUMPTION:")
message("  → Baseline annual yield: ", BASELINE_YIELD_PCT, "%")
message("  → Justification: Literature-based estimate")
message("  → Note: Scraped Redfin rental estimates were unreliable (1-3% yields)")
message("          suggesting data quality issues with online rental listings")
message("  Sensitivity analysis will test:")
message("    - Conservative: ", BASELINE_YIELD_PCT - 1, "%")
message("    - Baseline: ", BASELINE_YIELD_PCT, "%")
message("    - Optimistic: ", BASELINE_YIELD_PCT + 1, "%")

# Calculate monthly rental value for each property
redfin_df <- redfin_df %>%
  mutate(
    annual_rental_income = PRICE * (BASELINE_YIELD_PCT / 100),
    monthly_rental_income = annual_rental_income / 12,
    rentalval = monthly_rental_income,
    rentalval_note = "baseline_5pct"
  )

message("✓ Applied ", BASELINE_YIELD_PCT, "% rental yield to all ", nrow(redfin_df), " properties\n")


# PART 5: Add Spatial Covariates

message("→ Adding spatial covariates...")

# Convert to sf for spatial operations
redfin_sf <- st_as_sf(redfin_df, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

# Get elevation data
message("  Fetching elevation data...")
elevations <- get_elev_point(redfin_sf, src = "aws", z = 14)  # z=16 for 3m resolution
redfin_df$elevation <- elevations$elevation

message("  Calculating distance to coast...")
# Distance to coast for ALL properties
coastline <- ne_download(scale = 10, type = "coastline", category = "physical", returnclass = "sf")
coastline_proj <- st_transform(coastline, 3857)
redfin_proj_all <- st_transform(redfin_sf, 3857)

coastline_u <- st_union(coastline_proj)
redfin_df$distance_to_coast_meters <- as.numeric(st_distance(redfin_proj_all, coastline_u))
redfin_df$distance_to_coast_km <- redfin_df$distance_to_coast_meters / 1000

message("✓ Spatial covariates added\n")


# PART 6: Add Demographic Covariates

message("→ Adding demographic covariates from ACS...")

# Get ACS tract-level demographic data for California
variables <- c(
  median_age = "B01002_001",
  white = "B02001_002",
  black = "B02001_003",
  asian = "B02001_005",
  hispanic = "B03003_003",
  median_income = "B19013_001",
  education = "B15003_022",  # Bachelor's degree
  unemployment = "B23025_005"
)

acs_data <- get_acs(
  geography = "tract",
  variables = variables,
  state = "CA",
  year = 2022,
  geometry = TRUE,
  output = "wide"
)

# Transform ACS data to match redfin CRS
acs_data <- st_transform(acs_data, crs = st_crs(redfin_sf))
acs_joined_all <- st_join(redfin_sf, acs_data, join = st_within, left = TRUE)
acs_cols <- c("median_ageE", "whiteE", "blackE", "asianE", "hispanicE",
              "median_incomeE", "educationE", "unemploymentE")
redfin_df[, acs_cols] <- st_drop_geometry(acs_joined_all)[, acs_cols]

message("✓ Demographic covariates added\n")

# Save temporary checkpoint
temp_path <- file.path(DATA_DIR, "redfin_df_temp.csv")
write.csv(redfin_df, file = temp_path, row.names = FALSE)
message("→ Saved temporary checkpoint to ", basename(temp_path), "\n")


# HAZARD TYPE ASSIGNMENT
### This will need to be adjusted ###
message("→ Assigning hazard type based on case study site...\n")

# Site-level hazard classification based on geomorphology
# Each case study represents a distinct coastal hazard context:
#   - Beachfront communities: Inundation from SLR + storm surge
#   - Blufftop communities: Cliff erosion + coastal squeeze
#
# This approach is more defensible than elevation/distance proxies
# because hazard type is determined by site selection, not arbitrary thresholds.

SITE_HAZARD_TYPES <- list(
  carpinteria   = "flood",  # Low-lying beachfront community
  stinson_beach = "flood",  # Sandy beach, lagoon setting
  king_salmon   = "flood",  # Low coastal plain
  isla_vista    = "bluff",  # Mesa blufftop setting
  pacifica      = "bluff"   # Coastal bluffs and terraces
)

# Assign hazard type
if (CASE_NAME %in% names(SITE_HAZARD_TYPES)) {
  redfin_df$hazard_type <- SITE_HAZARD_TYPES[[CASE_NAME]]
  message("  Site classification: ", toupper(CASE_NAME))
  message("  Primary hazard type: ", SITE_HAZARD_TYPES[[CASE_NAME]])
  message("")
  message("  Rationale:")
  if (SITE_HAZARD_TYPES[[CASE_NAME]] == "flood") {
    message("    - Beachfront/low-lying coastal setting")
    message("    - Primary risk: Inundation from SLR + storm surge")
    message("    - CoSMoS flood depth/duration data will be used in economic model")
  } else {
    message("    - Elevated blufftop/terrace setting")
    message("    - Primary risk: Cliff erosion + coastal squeeze")
    message("    - CoSMoS cliff retreat data will be used in economic model")
  }
  message("")
} else {
  warning("  ️  Unknown case study: ", CASE_NAME)
  warning("     Add to SITE_HAZARD_TYPES configuration")
  warning("     Defaulting to 'flood' hazard type")
  redfin_df$hazard_type <- "flood"
  message("")
}

# Summary
n_properties <- nrow(redfin_df)
message("  All ", n_properties, " properties assigned '", 
        unique(redfin_df$hazard_type), "' hazard type\n")


# Create Stable Property IDs

redfin_df <- redfin_df %>%
  mutate(property_id = digest(paste(ADDRESS,
                                    `ZIP OR POSTAL CODE`,
                                    round(LATITUDE, 6),
                                    round(LONGITUDE, 6))))

# Final Filtering and Output

message("→ Final data cleaning...\n")

# All properties now have rental values from 5% baseline - no filtering needed
# Just verify data integrity

n_properties_final <- nrow(redfin_df)
n_with_rental <- sum(!is.na(redfin_df$rentalval) & redfin_df$rentalval > 0)

message("  Final dataset: ", n_properties_final, " properties")
message("  All properties have rental values from ", BASELINE_YIELD_PCT, "% baseline\n")

# Save the final dataframe
final_path <- file.path(DATA_DIR, "redfin_df.csv")
write_csv(redfin_df, final_path)
