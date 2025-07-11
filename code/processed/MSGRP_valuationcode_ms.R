# Jonah Danziger
# Purpose: Integrates property, land value, sea level, and wave data to assess coastal vulnerability and optimal retreat timing for coastal real estate.


rm(list=ls()) # clear the environment

library(tidyverse) # importing a package
library(lubridate)
library(janitor)
library(rvest)
library(parsedate)
library(pracma)
library(readxl)
library(elevatr)
library(sf)
library(ggplot2)
library(rnaturalearthdata)
library(rnaturalearth)
library(lwgeom)
library(nngeo)
library(foreach)
library(doParallel)
library(dplyr)
library(terra)
library(torch)
library(mapview)
library(viridis)

#### Load Redfin Data from appropriate folder and Prep it
redfin_df<-read_csv("data/carpinteria/redfin_df.csv")

# Define the bounding box (xmin, ymin, xmax, ymax) 
####Change this for the area of your interest####
#can copy and paste from "waves_ms.R"
bbox <- st_bbox(c(xmin = -119.2241, ymin = 34.1478, xmax = -119.2138, ymax = 34.1583), crs = st_crs(4326))

# Convert the dataframe into an sf object
redfin_sf<- redfin_df|>
  filter(!is.na(LONGITUDE) & !is.na(LATITUDE))|>
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)|>
  st_crop(bbox) |>
  mutate(rent=12*rentalval)

# Making separate columns for longitude and latitude
coordinates <- st_coordinates(redfin_sf)
redfin_sf$longitude <- coordinates[,1]
redfin_sf$latitude <- coordinates[,2]

### Integrating the Land Value Data
#The dataset landprices_CA_ms.csv provides land values per acre and the land share of property value for California ZIP codes in 2022
#Each property in the Redfin dataset (redfin_sf) has a ZIP code and a total price (PRICE).
#The land value data is joined to the property data by matching ZIP codes.
#The land share (landshare) represents the proportion of the property price attributable to land
#For each property: landval = PRICE x landshare 
#If the ZIP code is missing or unmatched, the average land share across all ZIP codes is used as a fallback: landval = PRICE x avg_share
#The structural value is the residual value of the property after subtracting land value from total price: strval = PRICE - landval
#load data (per acre and land share of property value)
landvals <- read_csv("data/landprices_CA_ms.csv", skip = 1)
landvals<-landvals |> filter (Year==2022) |> dplyr::select("Land Value\n(Per Acre, As-Is)", "ZIP Code","Land Share of Property Value")|>
  rename(landval_acre="Land Value\n(Per Acre, As-Is)",landshare="Land Share of Property Value", "ZIP OR POSTAL CODE"="ZIP Code") |>
  mutate(`ZIP OR POSTAL CODE` = as.character(`ZIP OR POSTAL CODE`))
# calculate the average land share for properties without zip match
avg_share<- mean(landvals$landshare)
#join the land value data to the redfin data by ZIP Code
redfin_sf<-redfin_sf |> 
  mutate(`ZIP OR POSTAL CODE` = as.character(`ZIP OR POSTAL CODE`)) |>
  left_join(landvals,by=c("ZIP OR POSTAL CODE")) |>
  mutate(landval=PRICE*landshare)|>
  mutate(landval=ifelse(is.na(landval),PRICE*avg_share, landval))|>
  mutate(strval=PRICE-landval)


### Integrating Wave Run-up Data

#load data from appropriate folder
wave_data<-read_csv("data/silver_strand/run_up_data.csv")
##Establish the baseline sea level 
#These values of sealevel were taken from the NOAA 2022 Report Intermediate Scenario
sea_level_data_sim <- data.frame(
  time = c(0, 10, 20, 30, 40, 50, 60, 70, 80),
  sea_level_rise = c(0, 0.1158, 0.2012, 0.2987,  0.4694, 0.6797, 0.9114, 1.1613, 1.4508))
sea_level_data_sim$timesquared<-sea_level_data_sim$time**2
#Fit a quadratic model to project slr over time
sea_model <- lm(sea_level_rise ~ time +timesquared, data=sea_level_data_sim)
# Create prediction time range (1 to 100 yrs)
sea_level_base <- data.frame(
  time = 1:100,
  timesquared = (1:100)^2
)

# Predict slr for each year
sea_level_predictions <- predict(sea_model, newdata = sea_level_base)

# Combine time and prediction into a data frame
predicted_df <- cbind(sea_level_base, sea_level_base = sea_level_predictions)

#Estimate depth parameters
# Data
#The damages for depths are based on the single story house no basement in San Fran
#These values have to be inputed manually because the pdf is too much effort to digitize.
depth_df <- data.frame(
  depth = c(-8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9),
  damagepct = c(0, 0, 0, 0, 0, 0, 0, 0.01, 0.07, 0.14, 0.20, 0.26,
                0.30, 0.33, 0.36, 0.38, 0.39, 0.39)
)

# Fit logistic function to model damage as a function of flood depth
logistic_nls <- nls(
  damagepct ~ a / (1 + exp(-b * (depth - d))),
  data = depth_df,
  start = list(a = 0.4, b = 1, d = 2)  # Reasonable starting values
)

summary(logistic_nls)
depth_params <- coef(logistic_nls) #This will flex with whatever depths you end up using

#Structure damage in this period 
strdamagefxn <- function(depthparams, sealevel, elevation, strval){
  num_rows=len(elevation)
  # Calculate the depth (sealevel - elevation)
  depth = (sealevel - elevation)*3 # This accounts for meters to feet
  # Predict values using the logistic_nls
  a = depthparams[1]
  b = depthparams[2]
  d = depthparams[3]
  #applies damage parameters to real structure values 
  damage_percent = a / (1 + exp(-b * (depth - d)))
  strdamage <- damage_percent * strval ###Changed from str_view_all to strval
  return(strdamage)
}

# Function to calculate NPV of future rental income
npvrentfxn <- function(littleT, bigT, rentvalues, beta) {
  # Number of periods
  n_periods <- bigT - littleT + 1
  
  # Discount vector (row vector): beta^1 to beta^n
  betas <- beta ^ (1:n_periods)
  
  # Replicate rentvalues across time, assuming same rent each period
  rent_matrix <- outer(rentvalues, rep(1, n_periods))
  
  # Matrix multiply: row-wise rent * discount factors
  npv <- rent_matrix %*% betas
  
  return(as.vector(npv))  # Return vector of NPV per property
}
# test the NPV for properties from year 1 to 25 with discount rate of 3%/yr (0.97)
rent_test <- npvrentfxn(littleT=1, bigT=25, rentvalues=redfin_sf$rent, beta=0.97)

#function to calculate expected damages
expdamagefxnvect <- function(littleT, bigT, depthparams, sealevelbase, shocks, realestate, beta) {
  num_periods <- bigT - littleT + 1
  num_shocks <- length(shocks)
  elevation <- realestate$elevation
  rent <- realestate$rent
  prices <- realestate$PRICE
  strval <- realestate$strval
  num_houses <- length(elevation)
  betas <- beta ^ (1:num_periods)
  
  # Step 1: Sea level + shocks → [periods x scenarios]
  sea_shock_matrix <- outer(sealevelbase[littleT:bigT], shocks, `+`)
  
  # Step 2: Expand sea_shock_matrix to [houses x periods x scenarios]
  sea_array <- array(sea_shock_matrix, dim = c(1, num_periods, num_shocks))
  sea_array <- sea_array[rep(1, num_houses), , ]  # [houses x periods x scenarios]
  
  # Step 3: Expand elevation to [houses x periods x scenarios]
  elev_array <- array(elevation, dim = c(num_houses, 1, 1))
  elev_array <- array(elev_array, dim = c(num_houses, num_periods, num_shocks))
  
  # Step 4: Subtract to get depth
  depth <- sea_array - elev_array  # [houses x periods x scenarios]
  
  # Step 5: Damage function
  damage_percent <- depthparams[1] / (1 + exp(-depthparams[2] * (depth - depthparams[3])))
  
  # Step 6: Multiply by structure value (expand to [houses x 1 x 1] → [houses x periods x scenarios])
  strval_array <- array(strval, dim = c(num_houses, 1, 1))
  strval_array <- array(strval_array, dim = c(num_houses, num_periods, num_shocks))
  
  strval_damages <- damage_percent * strval_array  # [houses x periods x scenarios]
  
  # Step 7: Average across scenarios (dimension 3)
  mean_damages <- apply(strval_damages, c(1, 2), mean)  # [houses x periods]
  
  # Step 8: Multiply by discount factor
  beta_matrix <- matrix(rep(betas, each = num_houses), nrow = num_houses)
  
  # Step 9: Sum across time
  present_value_damages <- rowSums(mean_damages * beta_matrix)  # [houses]
  
  
  return(present_value_damages)
}
# Test the function with example parameters
expdamage_test<-expdamagefxnvect(little=5, bigT=100, depthparams=depth_params, sealevelbase=sea_level_predictions, shocks=wave_data$run_up, realestate=redfin_sf, beta=0.97)

# Alternative function to calculate expected damages (simpler, less vectorized)
expdamagefxn <- function(littleT, bigT, depthparams, sealevelbase, shocks, realestate, beta) {
  num_periods <- bigT - littleT + 1
  num_shocks <- length(shocks)
  num_houses <- nrow(realestate)
  
  elevation <- realestate$elevation
  strval <- realestate$strval
  betas <- beta ^ (1:num_periods)
  
  # ✅ Subset sea level to the relevant time window
  sl_sub <- sealevelbase[littleT:bigT]
  
  # Step 1: Sea level + shocks
  sea_shock_matrix <- outer(sl_sub, shocks, `+`)  # [periods x scenarios]
  
  present_value_damages <- numeric(num_houses)
  
  for (i in 1:num_houses) {
    depth <- sea_shock_matrix - elevation[i]  # [periods x scenarios]
    
    damage_percent <- depthparams[1] / (1 + exp(-depthparams[2] * (depth - depthparams[3])))
    
    dollar_damages <- damage_percent * strval[i]
    
    mean_damages <- rowMeans(dollar_damages)
    
    present_value_damages[i] <- sum(mean_damages * betas)
  }
  
  return(present_value_damages)
}


expdamage_test2<-expdamagefxn(little=5, bigT=100, depthparams=depth_params, sealevelbase=sea_level_predictions, shocks=wave_data$run_up, realestate=redfin_sf, beta=0.97)

#Final Piece
# Function to find the optimal year to retreat for each property
optimalStoppingDate <- function(bigT, depthparams, sealevelbase, shocks, realestate, beta){
  num_rows <- nrow(realestate)
  T_star<- rep(100, num_rows)
  for (t in 1:bigT){
    print(t)
    # Calculate NPV of future rents from year t to bigT
    future_rent <- npvrentfxn(littleT=t, bigT=bigT, rentvalues=realestate$rent, beta=beta)
    # Calculate expected damages from year t to bigT
    exp_damages_t<- expdamagefxn(little=t, bigT=bigT, depthparams=depthparams, sealevelbase=sealevelbase, shocks=shocks, realestate=realestate , beta=beta)
    # Find where T_star hasn't been set (== 100) and condition is met
    to_stop <- (T_star == 100) & ((future_rent - exp_damages_t) <= 0)
    
    # Replace those values with current t
    T_star[to_stop] <- t
    
  }
  return(T_star)
  }

# Calculate optimal retreat year for each property
retreat_years <- optimalStoppingDate(bigT=100, depthparams= depth_params, 
                                     sealevelbase=sea_level_predictions, shocks=wave_data$run_up, 
                                     realestate=redfin_sf, beta=0.97)

# Assign retreat years to the real estate data
redfin_sf$retreat_year <- retreat_years

#map
mapview(
  redfin_sf,
  zcol = "retreat_year",           
  legend = TRUE,                   
  col.regions = rev(inferno(100)), 
  cex = 5,                         
  alpha = 0.7,                     
  layer.name = "Optimal Retreat Year"
)

#Save
# First we need to extract coordinates before saving 
coords <- st_coordinates(redfin_sf)
redfin_no_geom <- st_set_geometry(redfin_sf, NULL)
redfin_sf_clean <- cbind(redfin_no_geom, coords)

# Save with proper column names
write.csv(redfin_sf_clean, "data/silver_strand/redfin_sf.csv", row.names = FALSE)

