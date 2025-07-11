#Author: Jonah Danziger
#Edits: William Dean
#This code does 3 things
#Concatenates the data into one dataset
#Scrapes the data for any available rental data
#Runs a simple regression to estimate the rental values


rm(list=ls()) # clear the environment

library(tidyverse)
library(lubridate)
library(janitor)
library(rvest)
library(dplyr)
library(stringr)
library(parsedate)
library(pracma)
library(readxl)
library(elevatr)
library(jsonlite)
library(sf)
library(ggplot2)
library(tidycensus)
library(tigris)
library(rnaturalearth)
library(rnaturalearthdata)
library(raster)
library(here)
library(lwgeom)
library(nngeo)
library(readr)
library(httr)

#Part 1 Concatenate into a unified dataset
#Choose your working directory/location of interest
redfin_df<-read_csv(here("data/carpinteria/redfin_2025-01.csv"))

##This is for combining multiple redfin .csv (Redfin imposes 350 home limit on downloads)
for (i in 2:70){
  if (i<10){
    path<-paste0("data/carpinteria/redfin_2025-0",i,".csv")
  }else{
    path<-paste0("data/carpinteria/redfin_2025-",i,".csv")
  }
  red_temp<-read_csv(path)
  redfin_df<-rbind(redfin_df,red_temp)
}
# Clean column names if needed
names(redfin_df) <- str_trim(names(redfin_df))

# Rename long URL column
redfin_df <- redfin_df |> 
  rename(
    URL = `URL (SEE https://www.redfin.com/buy-a-home/comparative-market-analysis FOR INFO ON PRICING)`
  )

# Filter sale types you're interested in
redfin_df <- redfin_df |> 
  filter(`SALE TYPE` %in% c("PAST SALE", "RENTAL", "MLS Listing")) %>% 
  filter (`PROPERTY TYPE` %in% c("Single Family Residential", "Condo/Co-op", 
                                 "Townhouse", "Multi-Family (2-4 Unit)"))

# Deduplicate by address (best proxy for same property)
redfin_df <- redfin_df |> 
  distinct(ADDRESS, .keep_all = TRUE)

# # Filter out properties missing price
# redfin_df <- redfin_df |> 
#   filter(!is.na(PRICE))

# # Parse the sold date and filter out sale before 2018
# redfin_df <- redfin_df %>%
#   mutate(sold_date_parsed = parse_date_time(`SOLD DATE`, orders = "B-d-Y")) %>% 
#   filter(is.na(sold_date_parsed) | year(sold_date_parsed) >= 2018)
  
### Code to scrape assessed values for incorrectly priced homes(During download Redfin uses the last sale price which can be very old)

# Flag homes with suspiciously low price or NA
redfin_df <- redfin_df %>%
  mutate(flag_low_price = PRICE < 650000 | is.na(PRICE))

# Initialize column to store scraped estimates
redfin_df$assessed_or_estimate <- NA_real_

# Scraper function
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
low_price_indices <- which(redfin_df$flag_low_price & !is.na(redfin_df$URL) & is.na(redfin_df$assessed_or_estimate))
for (i in low_price_indices) {
  url <- redfin_df$URL[i]
  message("Scraping ", i, " of ", length(low_price_indices), ": ", url)
  redfin_df$assessed_or_estimate[i] <- get_redfin_estimate(url)
  Sys.sleep(2.5)
}

# Replace low prices with scraped Redfin Estimate values when available
redfin_df <- redfin_df %>%
  mutate(PRICE = ifelse(flag_low_price & !is.na(assessed_or_estimate), assessed_or_estimate, PRICE))
# Drop flagged properties where no Redfin Estimate was found 
redfin_df <- redfin_df %>%
  filter(!(flag_low_price & is.na(assessed_or_estimate)))
#remove unneeded columns
redfin_df <- redfin_df %>% dplyr::select(-flag_low_price, -assessed_or_estimate)
#####

#plot the df to see where the addresses lie
library(tidygeocoder)
library(mapview)
# Combine address fields (adjust if needed)
redfin_df <- redfin_df |> 
  mutate(full_address = paste(ADDRESS, CITY, `STATE OR PROVINCE`, sep = ", "))

# Geocode to get latitude and longitude (uses US Census by default, or switch to "osm")
# Create batches to prevent overwhelming the geocoding service
batch_size <- 50
n <- nrow(redfin_df)
batches <- split(redfin_df, ceiling(seq_len(n) / batch_size))

# Geocode batches with a pause between them (this will take a while, don't let screen turn off)
results <- map_dfr(batches, function(df_batch) {
  Sys.sleep(5)  # Pause between batches to avoid server overload
  tryCatch(
    geocode(df_batch, address = full_address, method = "osm", lat = LATITUDE, long = LONGITUDE),
    error = function(e) {
      warning("Skipping batch due to error: ", conditionMessage(e))
      df_batch %>% mutate(LATITUDE = NA, LONGITUDE = NA)
    }
  )
})

# Convert to spatial object
redfin_geocoded <- results
redfin_sf <- st_as_sf(redfin_geocoded, coords = c("LONGITUDE...27", "LATITUDE...26"), crs = 4326)

# Check if geocoding was successful
mapview(redfin_sf, zcol = "SALE TYPE", col.regions = c("blue", "green"))

### Code in progress to scrape all off-market homes within a custom polygon ###

# ### This function fetches Redfin data using a custom-drawn polygon and returns a formatted data frame
# 
# `%||%` <- function(x, y) if (!is.null(x)) x else y
# 
# # ===== Helper: Parse user_poly into matrix =====
# parse_user_poly <- function(user_poly_string) {
#   coords_pairs <- strsplit(user_poly_string, ",")[[1]]
#   coords_matrix <- do.call(rbind, lapply(coords_pairs, function(x) {
#     as.numeric(strsplit(trimws(x), " ")[[1]])
#   }))
#   # Ensure polygon is closed
#   if (!identical(coords_matrix[1, ], coords_matrix[nrow(coords_matrix), ])) {
#     coords_matrix <- rbind(coords_matrix, coords_matrix[1, ])
#   }
#   return(coords_matrix)
# }
# 
# # ===== Main Function to Fetch Redfin Data =====
# fetch_redfin_data <- function(user_poly) {
#   api_url <- paste0(
#     "https://www.redfin.com/stingray/api/gis?",
#     "al=1&include_nearby_homes=false&market=socal&",
#     "num_homes=500&ord=redfin-recommended-asc&",
#     "page_number=1&sold_within_days=3650&status=9&",
#     "user_poly=", URLencode(user_poly, reserved = TRUE), "&v=8"
#   )
#   
#   response <- GET(api_url, add_headers(`User-Agent` = "Mozilla/5.0"))
#   if (status_code(response) != 200) stop("API request failed: ", status_code(response))
#   
#   raw_data <- content(response, as = "text")
#   clean_data <- gsub("^\\{\\}&&", "", raw_data)
#   json_data <- fromJSON(clean_data)
#   
#   if (!is.null(json_data$errorMessage) && json_data$errorMessage != "Success") {
#     stop("Redfin API error: ", json_data$errorMessage)
#   }
#   
#   homes <- json_data$payload$homes
#   if (length(homes) == 0 || nrow(homes) == 0) {
#     warning("No properties found in the specified area")
#     return(data.frame())
#   }
#   
#   extract_coord <- function(type) {
#     sapply(homes$latLong, function(x) {
#       if (is.null(x) || !is.list(x) || is.null(x[[type]])) return(NA_real_)
#       x[[type]]
#     })
#   }
#   
#   rental_estimate <- sapply(homes$sashes, function(sash) {
#     if (!is.null(sash) && any(grepl("Rent Estimate: \\$[0-9,]+", sash))) {
#       as.numeric(gsub("[^0-9]", "", regmatches(sash, regexpr("Rent Estimate: \\$[0-9,]+", sash))))
#     } else {
#       NA_real_
#     }
#   })
#   
#   redfin_df <- data.frame(
#     URL = paste0("https://www.redfin.com", homes$url),
#     STATUS = homes$mlsStatus %||% NA_character_,
#     SALE.TYPE = case_when(
#       homes$mlsStatus == "Closed" ~ "PAST SALE",
#       homes$mlsStatus == "Off Market" ~ "OFF MARKET",
#       homes$mlsStatus == "Active" ~ "ACTIVE",
#       homes$mlsStatus == "Pending" ~ "PENDING",
#       TRUE ~ "OTHER"
#     ),
#     ADDRESS = homes$streetLine %||% NA_character_,
#     SOLD.DATE = as.Date(as.POSIXct(ifelse(is.null(homes$soldDate), NA, homes$soldDate/1000), origin = "1970-01-01")),
#     PRICE = homes$price %||% NA_real_,
#     LATITUDE = extract_coord("latitude"),
#     LONGITUDE = extract_coord("longitude"),
#     SQUARE.FEET = homes$sqFt %||% NA_real_,
#     BEDS = homes$beds %||% NA_real_,
#     BATHS = homes$baths %||% NA_real_,
#     YEAR.BUILT = homes$yearBuilt %||% NA_real_,
#     LOT.SIZE = homes$lotSize %||% NA_real_,
#     PROPERTY.TYPE = homes$propertyType %||% NA_real_,
#     RENTAL_ESTIMATE = rental_estimate,
#     stringsAsFactors = FALSE
#   )
#   
#   return(redfin_df)
# }
# 
# # ===== Main Execution =====
# 
# user_poly <- "-119.548079 34.405097,-119.542671 34.404211,-119.524733 34.394969,-119.524089 34.393411,-119.527908 34.393163,-119.541126 34.396704,-119.545289 34.400033,-119.548079 34.405097"
# 
# # Fetch data
# redfin_df <- fetch_redfin_data(user_poly) |> distinct(URL, .keep_all = TRUE)
# 
# if (!"LATITUDE" %in% names(redfin_df)) {
#   redfin_df <- redfin_df |>
#     rename(
#       LATITUDE = LATITUDE.value,
#       LONGITUDE = LONGITUDE.value
#     )
# }
# 
# 
# if (nrow(redfin_df) > 0) {
#   # Create polygon
#   coords <- parse_user_poly(user_poly)
#   polygon <- st_polygon(list(coords)) |> st_sfc(crs = 4326)
#   
#   # Bounding box filter
#   lon_bounds <- range(coords[, 1])
#   lat_bounds <- range(coords[, 2])
#   redfin_df <- redfin_df |> filter(
#     LATITUDE >= lat_bounds[1], LATITUDE <= lat_bounds[2],
#     LONGITUDE >= lon_bounds[1], LONGITUDE <= lon_bounds[2]
#   )
#   
#   # Spatial filtering
#   sf::sf_use_s2(FALSE)
#   redfin_sf <- st_as_sf(redfin_df, coords = c("LONGITUDE", "LATITUDE"), crs = 4326, remove = FALSE)
#   within_poly <- st_within(redfin_sf, polygon, sparse = FALSE)[, 1]
#   redfin_final <- redfin_sf[within_poly, ] |> st_drop_geometry()
#   
#   # Optional: Coastal distance (update path as needed)
#   # coastline <- st_read("path/to/coastline.shp")
#   # redfin_final_sf <- st_as_sf(redfin_final, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
#   # redfin_final$COAST_DISTANCE <- st_distance(redfin_final_sf, coastline)
#   
# } else {
#   redfin_final <- redfin_df
# }
# 
# # View final table
# View(redfin_final)
####


#Part 2 Scrapes the rental data
#Note: you might have to change some of this because I have a PC and not MAC if you are a MAC user
#See if we can get rental values
# Pick your URL
url <- redfin_df$URL[1]

# Use httr::GET to make a browser-like request
page <- GET(
  url,
  add_headers(`User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36")
)

# Then parse it with rvest
html <- read_html(page)
# Example: grab rental estimate
rental <- html %>%
  html_nodes(".estimate") %>%
  html_text() %>%
  stringr::str_extract("\\$[\\d,]+") %>%
  stringr::str_remove_all("[$,]") %>%
  as.numeric()


# Initialize empty result list
rental_estimates <- vector("numeric", length = nrow(redfin_df))

# Loop through each URL (will take a while)
for (i in 1:length(redfin_df$URL)) {
  url <- redfin_df$URL[i]
  success <- FALSE
  attempt <- 1
  
  while (!success && attempt <= 2) {
    try({
      # Make GET request with browser user-agent
      page <- GET(
        url,
        add_headers(`User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36")
      )
      
      # Check if the request succeeded
      if (status_code(page) == 200) {
        html <- read_html(page)
        rental <- html %>%
          html_nodes(".estimate") %>%
          html_text() %>%
          str_extract("\\$[\\d,]+") %>%
          str_remove_all("[$,]") %>%
          as.numeric()
        
        rental_estimates[i] <- rental
        success <- TRUE
      } else {
        warning(paste("Failed at index", i, "- status:", status_code(page)))
      }
    }, silent = TRUE)
    
    if (!success && attempt == 1) {
      Sys.sleep(5)  # wait before retry
    }
    
    attempt <- attempt + 1
  }
  
  # Optional: print progress
  if (i %% 100 == 0) {
    cat("Processed", i, "of", length(redfin_df$URL), "\n")
  }
}
redfin_df$rentalval = rental_estimates
# Convert data frame to an sf object
redfin_sf <- st_as_sf(redfin_df, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

# Get elevation data
elevations <- get_elev_point(redfin_sf, src = "aws", z=14) # Note z=16 is 3m data 

# Add elevation back to the original dataframe
redfin_df$elevation <- elevations$elevation




#save temporary csv to appropriate folder
write.csv(redfin_df, file = "data/silver_strand/redfin_df.csv", row.names = FALSE)




#Part 3 Regression to fill in missing data
redfin_df_rent <- redfin_df |> filter(rentalval>0) |> mutate(asset_ratio = rentalval/PRICE)
redfin_df_no_rent<- redfin_df |> filter(rentalval==0) |> mutate(asset_ratio = NA_real_)


###First handle redfin_df_rent

# 1. Download coastline shapefile as sf object
coastline <- ne_download(scale = 10, type = "coastline", category = "physical", returnclass = "sf")

# 2. Convert redfin_df_rent to an sf object (assuming lon/lat columns are named 'longitude' and 'latitude')
redfin_sf <- st_as_sf(redfin_df_rent, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

# 3. Project both to a planar CRS (Web Mercator for global coverage, EPSG:3857)
redfin_proj <- st_transform(redfin_sf, crs = 3857)
coastline_proj <- st_transform(coastline, crs = 3857)

# 4. Calculate distances from each point to the nearest point on the coastline
distances <- st_distance(redfin_proj, coastline_proj)
min_distances <- apply(distances, 1, min)  # Minimum distance to coast for each observation

# 5. Add result back to the original dataframe (as meters and kilometers)
redfin_df_rent$distance_to_coast_meters <- as.numeric(min_distances)
redfin_df_rent$distance_to_coast_km <- redfin_df_rent$distance_to_coast_meters / 1000
# 5. Get ACS tract-level demographic data for California
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

# 6. Transform ACS data to match redfin CRS
acs_data <- st_transform(acs_data, crs = st_crs(redfin_sf))

# 7. Spatial join: assign tract demographics to Redfin points
redfin_joined <- st_join(redfin_sf, acs_data, join = st_within, left = TRUE)

# First, drop the geometry to make `redfin_joined` a regular data frame
redfin_with_acs <- st_drop_geometry(redfin_joined)

# Identify the columns that came from ACS
acs_columns <- c(
  "median_ageE", "whiteE", "blackE", "asianE", "hispanicE", 
  "median_incomeE", "educationE", "unemploymentE"
)

# Select only those columns plus the row identifier (assuming redfin_df_rent rows match 1:1)
acs_data_to_add <- redfin_with_acs[, acs_columns]

# Bind ACS data columns to the original redfin_df_rent
redfin_df_rent <- bind_cols(redfin_df_rent, acs_data_to_add)


### Now, repeat the process for redfin_df_no_rent ####

# 1. Convert redfin_df_no_rent to an sf object (assuming lon/lat columns are named 'longitude' and 'latitude')
redfin_sf <- st_as_sf(redfin_df_no_rent, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

# 2. Project both to a planar CRS (Web Mercator for global coverage, EPSG:3857)
redfin_proj <- st_transform(redfin_sf, crs = 3857)

# 3. Calculate distances from each point to the nearest point on the coastline
distances <- st_distance(redfin_proj, coastline_proj)
min_distances <- apply(distances, 1, min)  # Minimum distance to coast for each observation

# 4. Add result back to the original dataframe (as meters and kilometers)
redfin_df_no_rent$distance_to_coast_meters <- as.numeric(min_distances)
redfin_df_no_rent$distance_to_coast_km <- redfin_df_no_rent$distance_to_coast_meters / 1000


# 5. Spatial join: assign tract demographics to Redfin points
redfin_joined <- st_join(redfin_sf, acs_data, join = st_within, left = TRUE)

# First, drop the geometry to make `redfin_joined` a regular data frame
redfin_with_acs <- st_drop_geometry(redfin_joined)

# Identify the columns that came from ACS
acs_columns <- c(
  "median_ageE", "whiteE", "blackE", "asianE", "hispanicE", 
  "median_incomeE", "educationE", "unemploymentE"
)

# Select only those columns plus the row identifier (assuming redfin_df_rent rows match 1:1)
acs_data_to_add <- redfin_with_acs[, acs_columns]

# Bind ACS data columns to the original redfin_df_rent
redfin_df_no_rent <- bind_cols(redfin_df_no_rent, acs_data_to_add)


## Changing asset_ratio to log(rentval) for regression
redfin_df_rent <- redfin_df_rent %>%
  mutate(log_rent = log(rentalval))


# Run the linear regression
reg_model <- lm(log_rent ~ distance_to_coast_meters +  `SQUARE FEET` + BEDS + BATHS + `YEAR BUILT`,
                data = redfin_df_rent)

summary(reg_model)

# With Demographics (since its such a small area all the demographics are identical...so this model doesn't work)
#reg_model2 <- lm(asset_ratio ~ distance_to_coast_meters +  `SQUARE FEET` + BEDS + BATHS + `YEAR BUILT` + median_ageE + blackE + asianE + hispanicE+median_incomeE+ educationE+unemploymentE,
               # data = redfin_df_rent)

#summary(reg_model2)

# Ensure the required predictor variables are present
required_vars <- c("distance_to_coast_meters",  "SQUARE FEET", "BEDS", "BATHS", "YEAR BUILT", "median_ageE", "whiteE", "blackE", "asianE", "hispanicE", "median_incomeE", "educationE", "unemploymentE")

# Create an NA column to hold predictions
redfin_df_no_rent$log_rent <- NA

# Find rows that have no missing values for predictors
complete_rows <- complete.cases(redfin_df_no_rent[, required_vars])

# Run prediction only on complete rows (using reg_model)
redfin_df_no_rent$log_rent[complete_rows] <- predict(
  reg_model,
  newdata = redfin_df_no_rent[complete_rows, ]
)
redfin_check <- redfin_df_no_rent |> #dplyr::select(-c(asset_ratio))|>
  filter(log_rent>0)

redfin_df_no_rent <- redfin_df_no_rent |>
  mutate(rentalval = ifelse(!is.na(log_rent), exp(log_rent), NA_real_))

#get rid of unneeded columns
redfin_df_rent <- redfin_df_rent |> dplyr::select(-c(log_rent, "median_ageE", "whiteE", "blackE", "asianE", "hispanicE", "median_incomeE", "educationE", "unemploymentE"))
redfin_df_no_rent <- redfin_df_no_rent |> dplyr::select(-c(log_rent, "median_ageE", "whiteE", "blackE", "asianE", "hispanicE", "median_incomeE", "educationE", "unemploymentE"))

#recombine
redfin_df_combine <- rbind(redfin_df_rent, redfin_df_no_rent) %>% filter(!is.na(rentalval)) %>% 
  dplyr::select(URL, rentalval) 

# Merge back into original redfin_df
redfin_df <- redfin_df %>%
  left_join(redfin_df_combine, by = "URL", suffix = c("", "_new")) %>%
  mutate(rentalval = coalesce(rentalval_new, rentalval)) %>%
  dplyr::select(-rentalval_new)

#Filter out remaining properties that are still missing rentalvals
redfin_df <- redfin_df |> 
  filter(!is.na(rentalval) & rentalval != 0)

#redfin_df <- redfin_df |> rename(`ZIP OR POSTAL CODE`=ZIP.OR.POSTAL.CODE)

####Save the final dataframe to a CSV file in appropriate folder!!!!!


write.csv(redfin_df, file = "data/carpinteria/redfin_df.csv", row.names = FALSE)


# 
# # Loop through rows where rentalval == 0
# for (i in which(redfin_sf$rentalval == 0)) {
#   url <- redfin_sf$URL[i]
#   success <- FALSE
#   attempt <- 1
#   rental <- 0
#   
#   while (!success && attempt <= 2) {
#     try({
#       # Make request with browser-like user agent
#       page <- GET(
#         url,
#         add_headers(`User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36")
#       )
#       
#       if (status_code(page) == 200) {
#         html <- read_html(page)
#         rental <- html %>%
#           html_nodes(".estimate") %>%
#           html_text() %>%
#           str_extract("\\$[\\d,]+") %>%
#           str_remove_all("[$,]") %>%
#           as.numeric()
#         print(rental)
#         # Only update if we get a valid (non-zero, non-NA) rental
#         if (!is.na(rental) && rental > 0) {
#           redfin_sf$rentalval[i] <- rental
#           success <- TRUE
#         }
#       }
#     }, silent = TRUE)
#     
#     if (!success && attempt == 1) {
#       Sys.sleep(3)  # short pause before retry
# 
#       
#       
#           }
#     
#     attempt <- attempt + 1
#   }
#   
#   # Track progress
#   
#   cat("Checked", i, "rows\n")
#   
# }
# 
# 
