# Jonah Danziger
# Edits: William Dean
# Purpose: Integrates property, land value, sea level, and wave data to assess coastal vulnerability and optimal retreat timing for coastal real estate.


rm(list=ls())
library(tidyverse)
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
bbox <- st_bbox(c(xmin = -119.55, ymin = 34.38, xmax = -119.52, ymax = 34.41), crs = st_crs(4326))

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

#load data from appropriate folder!!!!!
wave_data<-read_csv("data/carpinteria/run_up_data.csv")

#for parcel data
wave_parcel <- read_csv("data/carpinteria/run_up_parcel.csv") |>
  janitor::clean_names() |>
  rename(year = yyyy, run_up = max_run_up)

##Establish the baseline sea level 
#These values of sealevel were taken from the OPC 2024 Intermediate Scenario (feet)

# Clean shocks
wave_shocks <- wave_data$run_up
shocks <- wave_shocks[is.finite(wave_shocks)]

#vectorize the parcel runup data
parcel_shocks <- wave_parcel |>
  group_by(parcel_id) |>
  summarise(run_up = list(run_up), .groups = "drop") |>
  tibble::deframe()

# Add parcel_id into redfin_sf using address
redfin_sf <- redfin_sf %>%
  left_join(
    wave_parcel %>% dplyr::select(parcel_id, address) %>% distinct(),
    by = c("ADDRESS" = "address")
  )

# Expect TRUE and few NA
table(is.na(redfin_sf$parcel_id))

# Make sure every parcel_id in redfin_sf exists in the shocks list
missing_keys <- setdiff(as.character(redfin_sf$parcel_id), names(parcel_shocks))
length(missing_keys)  # should be 0


# Define OPC 2024 scenarios
ft_to_m <- 0.3048
sea_level_data <- list(
  "Intermediate" = data.frame(
    time = c(0,10,20,30,40,50,60,70,80),
    sea_level_rise = c(0, 0.4, 0.6, 0.8, 1.1, 1.4, 1.8, 2.4, 3.1) * ft_to_m
  ),
  "Intermediate_High" = data.frame(
    time = c(0,10,20,30,40,50,60,70,80),
    sea_level_rise = c(0, 0.4, 0.7, 1.0, 1.5, 2.2, 3.0, 3.9, 4.9) * ft_to_m
  ),
  "High" = data.frame(
    time = c(0,10,20,30,40,50,60,70,80),
    sea_level_rise = c(0, 0.4, 0.8, 1.2, 2.0, 3.0, 4.1, 5.4, 6.6) * ft_to_m
  )
)

# Function to fit quadratic & predict
fit_slr_model <- function(df, years = 1:100) {
  df$timesquared <- df$time^2
  model <- lm(sea_level_rise ~ time + timesquared, data = df)
  newdat <- data.frame(time = years, timesquared = years^2)
  preds <- predict(model, newdata = newdat)
  return(data.frame(time = years, slr = preds))
}

# Run predictions for all scenarios
slr_preds <- lapply(sea_level_data, fit_slr_model)

# Combine into a long df for plotting/comparisons
slr_all <- bind_rows(
  lapply(names(slr_preds), function(nm) {
    slr_preds[[nm]] %>% mutate(scenario = nm)
  })
)

# Named list of just the SLR vectors
slr_scenarios <- lapply(slr_preds, function(df) df$slr)

## For the inundation map feature
#first load dem from waves_ms.R
dem <- rast("data/carpinteria/dem.tif") 

# Pick median or max run-up (meters)
runup_median <- median(shocks, na.rm=TRUE)
runup_max <- max(shocks, na.rm=TRUE)

# Years to plot
years <- c(0,10,20,30,40,50,60,70,80)

# Function to make inundation raster for a given scenario and year index
make_inundation_raster <- function(scenario_name, year_idx, runup="median") {
  slr <- slr_scenarios[[scenario_name]][year_idx]
  runup_val <- ifelse(runup == "max", runup_max, runup_median)
  t_wl <- slr + runup_val # total water level in meters
  inundation <- dem < t_wl
  inundation[!inundation] <- NA
  return(inundation)
}

# Ensure output directory exists
if(!dir.exists("data/carpinteria")) dir.create("data/carpinteria")
if(!dir.exists("data/carpinteria/inundation")) dir.create("data/carpinteria/inundation")

#loop to create inundation maps for each scenario and year
scenarios <- c("Intermediate","Intermediate_High","High")
years <- c(0,10,20,30,40,50,60,70,80)
for (scen in scenarios) {
  for (i in seq_along(years)) {
    rast <- make_inundation_raster(scen, i, runup="median")
    outname <- paste0("data/carpinteria/inundation_", scen, "_", years[i], ".tif")
    terra::writeRaster(rast, outname, overwrite=TRUE)
  }
}

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
  num_rows=length(elevation)
  # Calculate the depth (sealevel - elevation)
  depth = (sealevel - elevation) #*3 # This accounts for meters to feet
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
  
  # Ensure sealevelbase is long enough
  if (length(sealevelbase) < bigT) {
    stop("sealevelbase shorter than bigT")
  }
  
  # Sea level + shocks â†’ [periods x scenarios]
  sea_shock_matrix <- outer(sealevelbase[littleT:bigT], shocks, `+`)  
  stopifnot(all(dim(sea_shock_matrix) == c(num_periods, num_shocks)))
  
  # Expand to [houses x periods x shocks]
  sea_array <- array(rep(sea_shock_matrix, each = num_houses),
                     dim = c(num_houses, num_periods, num_shocks))
  
  # Elevation â†’ [houses x periods x shocks]
  elev_array <- array(rep(elevation, times = num_periods * num_shocks),
                      dim = c(num_houses, num_periods, num_shocks))
  
  # Subtract to get depth
  depth <- sea_array - elev_array 
  
  # Damage function
  damage_percent <- depthparams[1] / (1 + exp(-depthparams[2] * (depth - depthparams[3])))
  
  # Multiply by structure value
  strval_array <- array(strval, dim = c(num_houses, 1, 1))
  strval_array <- array(strval_array, dim = c(num_houses, num_periods, num_shocks))
  
  strval_damages <- damage_percent * strval_array 
  
  # Average across scenarios
  mean_damages <- apply(strval_damages, c(1, 2), mean)
  
  # Multiply by discount factor
  beta_matrix <- matrix(rep(betas, times = num_houses), 
                        nrow = num_houses, 
                        byrow = TRUE)
  
  # Sum across time
  present_value_damages <- rowSums(mean_damages * beta_matrix)
  
  
  return(present_value_damages)
}

# simplified function using parcel runup data
expdamagefxn_parcel <- function(littleT, bigT, depthparams, sealevelbase, parcel_shocks, realestate, beta) {
  num_periods <- bigT - littleT + 1
  sl_sub <- sealevelbase[littleT:bigT]
  betas <- beta ^ (1:num_periods)
  
  present_value_damages <- numeric(nrow(realestate))
  
  for (i in 1:nrow(realestate)) {
    shocks_i <- parcel_shocks[[ as.character(realestate$parcel_id[i]) ]]
    
    sea_shock_matrix <- outer(sl_sub, shocks_i, `+`)  # [periods x scenarios]
    depth <- sea_shock_matrix - realestate$elevation[i]
    
    damage_percent <- depthparams[1] / (1 + exp(-depthparams[2] * (depth - depthparams[3])))
    dollar_damages <- damage_percent * realestate$strval[i]
    mean_damages <- rowMeans(dollar_damages)
    
    present_value_damages[i] <- sum(mean_damages * betas)
  }
  
  return(present_value_damages)
}

#vectorized version of the parcel expdamagefunction if needed for speed 
# expdamagefxn_parcel_vect <- function(littleT, bigT, depthparams, sealevelbase, parcel_shocks, realestate, beta) {
#   num_periods <- bigT - littleT + 1
#   sl_sub <- sealevelbase[littleT:bigT]
#   betas <- beta ^ (1:num_periods)
#   
#   num_houses <- nrow(realestate)
#   
#   # Pad shocks so all parcels have same length
#   max_shocks <- max(lengths(parcel_shocks))
#   shocks_mat <- matrix(NA_real_, nrow = num_houses, ncol = max_shocks)
#   
#   for (i in seq_len(num_houses)) {
#     pid <- as.character(realestate$parcel_id[i])
#     shocks_i <- parcel_shocks[[pid]]
#     shocks_mat[i, seq_along(shocks_i)] <- shocks_i
#   }
#   
#   # Expand sealevel + shocks into [houses x periods x shocks] ---
#   # [periods x shocks] baseline
#   sea_shock_base <- outer(sl_sub, 1:max_shocks, function(t, j) sl_sub[t])
#   
#   # Add shocks per parcel: [houses x periods x shocks]
#   sea_array <- array(NA_real_, dim = c(num_houses, num_periods, max_shocks))
#   for (i in seq_len(num_houses)) {
#     sea_array[i,,] <- outer(sl_sub, shocks_mat[i,], `+`)
#   }
#   
#   elev_array <- array(realestate$elevation, dim = c(num_houses, num_periods, max_shocks))
#   
#   depth <- sea_array - elev_array
#   damage_percent <- depthparams[1] / (1 + exp(-depthparams[2] * (depth - depthparams[3])))
#   
#   # Multiply by structure values
#   strval_array <- array(realestate$strval, dim = c(num_houses, num_periods, max_shocks))
#   strval_damages <- damage_percent * strval_array
#   
#   # Mean across shocks, handling NA padding
#   mean_damages <- apply(strval_damages, c(1,2), function(x) mean(x, na.rm=TRUE))
#   
#   # Discount factor across time
#   beta_matrix <- matrix(rep(betas, times = num_houses), nrow = num_houses, byrow = TRUE)
#   
#   # Final present value damages 
#   present_value_damages <- rowSums(mean_damages * beta_matrix)
#   
#   return(present_value_damages)
# }

# # Test the function with example parameters
# # expdamage_test<-expdamagefxnvect(little=5, bigT=100, depthparams=depth_params, sealevelbase=sea_level_predictions, 
# #                                 shocks=wave_data$run_up, realestate=redfin_sf, beta=0.97)
# 
# # Alternative function to calculate expected damages (simpler, less vectorized)
# expdamagefxn <- function(littleT, bigT, depthparams, sealevelbase, shocks, realestate, beta) {
#   num_periods <- bigT - littleT + 1
#   num_shocks <- length(shocks)
#   num_houses <- nrow(realestate)
#   
#   elevation <- realestate$elevation
#   strval <- realestate$strval
#   betas <- beta ^ (1:num_periods)
#   
#   # âœ… Subset sea level to the relevant time window
#   sl_sub <- sealevelbase[littleT:bigT]
#   
#   # Step 1: Sea level + shocks
#   sea_shock_matrix <- outer(sl_sub, shocks, `+`)  # [periods x scenarios]
#   
#   present_value_damages <- numeric(num_houses)
#   
#   for (i in 1:num_houses) {
#     depth <- sea_shock_matrix - elevation[i]  # [periods x scenarios]
#     
#     damage_percent <- depthparams[1] / (1 + exp(-depthparams[2] * (depth - depthparams[3])))
#     
#     dollar_damages <- damage_percent * strval[i]
#     
#     mean_damages <- rowMeans(dollar_damages)
#     
#     present_value_damages[i] <- sum(mean_damages * betas)
#   }
#   
#   return(present_value_damages)
# }


# expdamage_test2<-expdamagefxn(little=5, bigT=100, depthparams=depth_params, sealevelbase=sea_level_predictions, shocks=wave_data$run_up, realestate=redfin_sf, beta=0.97)

#Final Piece
# Function to find the optimal year to retreat for each property
optimalStoppingDate <- function(bigT, depthparams, sealevelbase, shocks, realestate, beta, scenario_name){
  n <- nrow(realestate)
  T_star <- rep(bigT + 1, n)  # "no stop" default
  
  for (t in 1:bigT){
    # NPV of future rents from t..bigT  (vector length n)
    future_rent <- npvrentfxn(littleT = t, bigT = bigT, rentvalues = realestate$rent, beta = beta)
    
    # Expected damages from t..bigT  (vector length n)
    exp_damages_t <- expdamagefxnvect(
      littleT      = t,
      bigT         = bigT,
      depthparams  = depthparams,
      sealevelbase = sealevelbase,  # full vector; the fn slices [t:bigT]
      shocks       = shocks,
      realestate   = realestate,
      beta         = beta
    )
    
    # Robust stopping test (treat non-finite as "donâ€™t stop")
    net <- future_rent - exp_damages_t
    to_stop <- (T_star == (bigT + 1)) & is.finite(net) & (net <= 0)
    
    T_star[to_stop] <- t
  }
  
  # Return ADDRESS, scenario, and T_star so itâ€™s easy to bind later
  tibble::tibble(
    ADDRESS  = realestate$ADDRESS,
    scenario = scenario_name,
    T_star   = T_star
  )
}

#for parcel level
optimalStoppingDate_parcel <- function(bigT, depthparams, sealevelbase, parcel_shocks, realestate, beta, scenario_name){
  n <- nrow(realestate)
  T_star <- rep(bigT + 1, n)
  
  for (t in 1:bigT){
    future_rent <- npvrentfxn(t, bigT, rentvalues = realestate$rent, beta = beta)
    
    exp_damages_t <- expdamagefxn_parcel(
      littleT      = t,
      bigT         = bigT,
      depthparams  = depthparams,
      sealevelbase = sealevelbase,
      parcel_shocks = parcel_shocks,
      realestate   = realestate,
      beta         = beta
    )
    
    net <- future_rent - exp_damages_t
    to_stop <- (T_star == (bigT + 1)) & is.finite(net) & (net <= 0)
    T_star[to_stop] <- t
  }
  
  tibble::tibble(
    parcel_id = realestate$parcel_id,
    ADDRESS   = realestate$ADDRESS,
    scenario  = scenario_name,
    T_star    = T_star
  )
}

# Calculate optimal retreat year for each property
retreat_years_df <- purrr::map_dfr(names(slr_scenarios), function(scen) {
  optimalStoppingDate(
    bigT         = 100,
    depthparams  = depth_params,
    sealevelbase = slr_scenarios[[scen]],  
    parcel_shocks       = parcel_shocks,                  
    realestate   = redfin_sf,
    beta         = 0.97,
    scenario_name = scen
  )
})

#using parcel runup data
retreat_years_df <- purrr::map_dfr(names(slr_scenarios), function(scen) {
  optimalStoppingDate_parcel(
    bigT         = 100,
    depthparams  = depth_params,
    sealevelbase = slr_scenarios[[scen]],  
    parcel_shocks       = parcel_shocks,                  
    realestate   = redfin_sf,
    beta         = 0.97,
    scenario_name = scen
  )
})
# Make a column per scenario to join back to redfin_sf
retreat_years_wide <- tidyr::pivot_wider(
  retreat_years_df,
  names_from  = scenario,
  values_from = T_star,
  names_prefix = "retreat_year_"
)

# Join back to redfin_sf by ADDRESS
redfin_sf <- dplyr::left_join(redfin_sf, retreat_years_wide, by = "ADDRESS")

#map
mapview(
  redfin_sf,
  zcol = "retreat_year_High",           
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
write.csv(redfin_sf_clean, "data/carpinteria/redfin_sf.csv", row.names = FALSE)
