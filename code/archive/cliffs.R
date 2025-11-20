# Will Dean
# Trying to integrate CoSMoS coastal squeeze shapefiles so that blufftop communities can be included in the analysis

rm(list=ls())
library(sf)
library(dplyr)
library(stringr)
library(purrr)


squeeze_dir <- "data/CoSMoS_coastal_squeeze_socal/"

#load files
squeeze_files <- list.files(squeeze_dir, pattern = "\\.shp$", full.names = TRUE)

# Function to load one shapefile and tag it with the SLR increment
read_squeeze <- function(f) {
  slr_m <- str_match(basename(f), "SLR(\\d+)")[,2] %>% as.numeric() / 100  # e.g., "025" -> 0.25 m
  st_read(f, quiet = TRUE) %>%
    mutate(SLR_m = slr_m,
           source_file = basename(f))
}

# Load everything into one big sf object
squeeze_all <- map_dfr(squeeze_files, read_squeeze)


