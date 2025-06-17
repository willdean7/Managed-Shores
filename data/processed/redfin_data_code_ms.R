#Author: Jonah Danziger
#This code does 3 things
#Concatenates the data into one dataset
#Scrapes the data for any available rental data
#Runs a simple regression to estimate the rental values


rm(list=ls()) # clear the environment
#-------Import necessary packages here-------------------#
library(tidyverse) # importing a package
library(lubridate)
library(janitor)
library(rvest)
library(dplyr)
library(stringr)
library(parsedate)
library(pracma)
library(readxl)
library(elevatr)
library(sf)
library(ggplot2)
library(tidycensus)
library(tigris)
library(rnaturalearth)
library(rnaturalearthdata)
library(raster)
library(lwgeom)
library(nngeo)
library(readr)
#Part 1 Concatenate into a unified dataset.
redfin_df<-read_csv("C:/Users/jonah/Documents/Research/Wetland Restoration/Redfin/Huntington Beach/redfin_2024-01.csv")

for (i in 2:70){
  if (i<10){
    path<-paste0("C:/Users/jonah/Documents/Research/Wetland Restoration/Redfin/Huntington Beach/redfin_2024-0",i,".csv")
  }else{
    path<-paste0("C:/Users/jonah/Documents/Research/Wetland Restoration/Redfin/Huntington Beach/redfin_2024-",i,".csv")
  }
  red_temp<-read_csv(path)
  redfin_df<-rbind(redfin_df,red_temp)
}
redfin_df<-unique(redfin_df) |> filter(`SALE TYPE`=='PAST SALE') |> distinct() |>
  rename("URL"="URL (SEE https://www.redfin.com/buy-a-home/comparative-market-analysis FOR INFO ON PRICING)")
#redfin_df<-redfin_df |> dplyr::select(-c("SOLD DATE", PRICE, LATITUDE, "LONGITUDE", "ZIP OR POSTAL CODE","LOT SIZE", URL)) #|> rename(longitude=LONGITUDE, latitude=LATITUDE) 
redfin_df<-redfin_df |> rename(longitude=LONGITUDE, latitude=LATITUDE, SOLD.DATE=`SOLD DATE`) 

#Part 2 Scrapes the rental data
#Note: you might have to change some of this because I have a PC and not MAC if you are a MAC user
#See if we can get rental values
library(httr)

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

# Loop through each URL
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
redfin_sf <- st_as_sf(redfin_df, coords = c("longitude", "latitude"), crs = 4326)

# Get elevation data
elevations <- get_elev_point(redfin_sf, src = "aws", z=14) # Note z=16 is 3m data 

# Add elevation back to the original dataframe
redfin_df$elevation <- elevations$elevation





write.csv(redfin_df, file = "C:/Users/jonah/Documents/Research/Wetland Restoration/Redfin/redfin_df.csv", row.names = FALSE)




#Part 3 Regression to fill in missing data
redfin_df_rent <- redfin_df |> filter(rentalval>0) |> mutate(asset_ratio = rentalval/PRICE)
redfin_df_no_rent<- redfin_df |> filter(rentalval==0)


# 1. Download coastline shapefile as sf object
coastline <- ne_download(scale = 10, type = "coastline", category = "physical", returnclass = "sf")

# 2. Convert redfin_df_rent to an sf object (assuming lon/lat columns are named 'longitude' and 'latitude')
redfin_sf <- st_as_sf(redfin_df_rent, coords = c("longitude", "latitude"), crs = 4326)

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

# 1. Download coastline shapefile as sf object
#coastline <- ne_download(scale = 10, type = "coastline", category = "physical", returnclass = "sf")

# 2. Convert redfin_df_no_rent to an sf object (assuming lon/lat columns are named 'longitude' and 'latitude')
redfin_sf <- st_as_sf(redfin_df_no_rent, coords = c("longitude", "latitude"), crs = 4326)

# 3. Project both to a planar CRS (Web Mercator for global coverage, EPSG:3857)
redfin_proj <- st_transform(redfin_sf, crs = 3857)
coastline_proj <- st_transform(coastline, crs = 3857)

# 4. Calculate distances from each point to the nearest point on the coastline
distances <- st_distance(redfin_proj, coastline_proj)
min_distances <- apply(distances, 1, min)  # Minimum distance to coast for each observation

# 5. Add result back to the original dataframe (as meters and kilometers)
redfin_df_no_rent$distance_to_coast_meters <- as.numeric(min_distances)
redfin_df_no_rent$distance_to_coast_km <- redfin_df_no_rent$distance_to_coast_meters / 1000


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
redfin_df_no_rent <- bind_cols(redfin_df_no_rent, acs_data_to_add)




# Run the linear regression
reg_model <- lm(asset_ratio ~ distance_to_coast_meters +  `SQUARE FEET` + BEDS + BATHS + `YEAR BUILT`,
                data = redfin_df_rent)

summary(reg_model)

# With Demographics
reg_model2 <- lm(asset_ratio ~ distance_to_coast_meters +  `SQUARE FEET` + BEDS + BATHS + `YEAR BUILT` + median_ageE + whiteE + blackE + asianE + hispanicE+median_incomeE+ educationE+unemploymentE,
                data = redfin_df_rent)

summary(reg_model2)
# Ensure the required predictor variables are present
required_vars <- c("distance_to_coast_meters",  "SQUARE FEET", "BEDS", "BATHS", "YEAR BUILT", "median_ageE", "whiteE", "blackE", "asianE", "hispanicE", "median_incomeE", "educationE", "unemploymentE")

# Create an NA column to hold predictions
redfin_df_no_rent$asset_ratio <- NA

# Find rows that have no missing values for predictors
complete_rows <- complete.cases(redfin_df_no_rent[, required_vars])

# Run prediction only on complete rows
redfin_df_no_rent$asset_ratio[complete_rows] <- predict(
  reg_model2,
  newdata = redfin_df_no_rent[complete_rows, ]
)
redfin_check <- redfin_df_no_rent |> #dplyr::select(-c(asset_ratio))|>
  filter(asset_ratio>0)
redfin_df_no_rent <- redfin_df_no_rent |>
  mutate(rentalval = ifelse(asset_ratio>0, asset_ratio*PRICE,0))

redfin_df_combine <- rbind(redfin_df_rent, redfin_df_no_rent)
filter(redfin_df_combine)

redfin_df$rentalval <- coalesce(redfin_df_combine$rentalval, 0)
redfin_df$rentalval = redfin_df_combine$rentalval

#redfin_df <- redfin_df |> rename(`ZIP OR POSTAL CODE`=ZIP.OR.POSTAL.CODE)
write.csv(redfin_df, file = "C:/Users/jonah/Documents/Research/Wetland Restoration/Redfin/redfin_df.csv", row.names = FALSE)


# 
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
