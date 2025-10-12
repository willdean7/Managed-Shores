#Will
# Trying to integrate CoSMoS shoreline change projection shapefiles


rm(list=ls())
library(sf)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthhires)
library(raster)
library(elevatr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)


#load CoSMoS shoreline change projection shapefiles
#(nH_nN = No Hold the Line, No Nourishment ; nH_cN = No Hold the Line, Continuous Nourishment ; H_nN = Hold the Line, No Nourishment ; H_cN = Hold the Line, Continuous Nourishment)
nH_nN_raw <- "data/CoSMos_shoreline_change_projections/No_HoldtheLine_NoNourishment_shoreline.shp"
nH_nN_uncert_raw <- "data/CoSMos_shoreline_change_projections/No_HoldtheLine_NoNourishment_shoreline_uncertainty.shp"
nH_cN_raw <- "data/CoSMos_shoreline_change_projections/No_HoldtheLine_ContNourishment_shoreline.shp"
nH_cN_uncert_raw <- "data/CoSMos_shoreline_change_projections/No_HoldtheLine_ContNourishment_shoreline_uncertainty.shp"
H_nN_raw <- "data/CoSMos_shoreline_change_projections/HoldtheLine_NoNourishment_shoreline.shp"
H_nN_uncert_raw <- "data/CoSMos_shoreline_change_projections/HoldtheLine_NoNourishment_shoreline_uncertainty.shp"
H_cN_raw <- "data/CoSMos_shoreline_change_projections/HoldtheLine_ContNourishment_shoreline.shp"
H_cN_uncert_raw <- "data/CoSMos_shoreline_change_projections/HoldtheLine_ContNourishment_shoreline_uncertainty.shp"

#coastline boundary
coast_boundary_raw <- "data/CoSMos_shoreline_change_projections/COAST_ErosionBoundary.shp"

# Read scenarios and uncertainty bands
nH_nN <- st_read(nH_nN_raw)
nH_nN_uncert <- st_read(nH_nN_uncert_raw)
nH_cN <- st_read(nH_cN_raw)
nH_cN_uncert <- st_read(nH_cN_uncert_raw)
H_nN <- st_read(H_nN_raw)
H_nN_uncert <- st_read(H_nN_uncert_raw)
H_cN <- st_read(H_cN_raw)
H_cN_uncert <- st_read(H_cN_uncert_raw)
H_cN_uncert <- st_read(H_cN_uncert_raw)

# Store scenarios in a list for easy access
scenarios <- list(
  nH_nN = nH_nN,
  nH_nN_uncert = nH_nN_uncert,
  nH_cN = nH_cN,
  nH_cN_uncert = nH_cN_uncert,
  H_nN = H_nN,
  H_nN_uncert = H_nN_uncert,
  H_cN = H_cN,
  H_cN_uncert = H_cN_uncert
)

coast_boundary <- st_read(coast_boundary_raw)

# Preview structure and attribute fields
print(nH_nN)
glimpse(nH_nN)
print(nH_nN_uncert)
glimpse(nH_nN_uncert)