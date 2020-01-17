## Code for calculating WUA and HHS for Depth and Velocity rasters ##
#
# Created by: Richie Carmichael
# Modified by: Mike Ackerman
#
#####################################################################

## load necessary libraries
library(raster)
library(rgdal)
library(sf)
library(tidyverse)

## read in depth and velocity rasters
d_rast <- raster("data/D_Aug_All.tif")
v_rast <- raster("data/V_Aug_All.tif")
# summer_d <- raster("data/D_Aug_All.tif")
# summer_v <- raster("data/V_Aug_All.tif")
# winter_d <- raster("data/d_jan_v2")
# winter_v <- raster("data/v_jan_v2")

# Read in reach polygons
reaches <- st_read("data/geomorph/Lem_Poly_Label.shp")
# plot(reaches)

# Create one object for each reach
rch_names = unique(reaches$Name)
for(rch in rch_names) {
  assign(as.character(rch), filter(reaches, Name == rch))
}

# read in functions to calculate HSI
source("R/hsi_curves.R")

# set some arguments, for file naming, etc.
spc = "chnk" # species: either "chnk" or "sthd"
ls  = "juv"  # life stage: either "juv" or "spw"

# Calculate suitability insert appropriate function
d_suit <- calc(d_rast, chnk_juv_d)
v_suit <- calc(v_rast, chnk_juv_v) 

# Write rasters if desired
writeRaster(d_suit, paste0("output/hsi/", spc, "_", ls, "_", "d_suit.tif"))
writeRaster(v_suit, paste0("output/hsi/", spc, "_", ls, "_", "v_suit.tif"))

# Function to calculate composite suitability using geometric mean of depth and velocity
comp_hsi <- function(r1, r2) {
  y = (sqrt(r1*r2))
  return(y)
}

# Calculate geometric mean and create a new raster
comp_suit <- overlay(d_suit, v_suit, fun = comp_hsi)
writeRaster(comp_suit, paste0("output/hsi/", spc, "_", ls, "_", "comp_suit.tif"))

# Calculate metrics at reach scale wetted area, WUA, HHS
# Extract raster values at polygon 'reaches' from each object created using the reaches polygon
for(rch in rch_names) {
  sf_tmp = raster::extract(comp_suit,
                           get(as.character(rch)),
                           fun = NULL,
                           df = TRUE,
                           na.rm = TRUE) %>%
    drop_na() %>%
    mutate(ID = replace(ID, ID == 1, as.character(rch)))
  assign(paste0("hsi_extract_", rch), sf_tmp)
  rm(sf_tmp)
}

# Merge reach HSI's back together
hsi_merge = bind_rows(lapply(ls(pattern = "^hsi_extract"), function(x) get(x)))
names(hsi_merge) = c("ID", "value")

# Calculating the HSI metrics
pix_area = prod(res(comp_suit)) # first, set the pixel area
# for lem the pixels are 1m x 1m = 1sq meter
# for pahs, us the pixels are 3' x 3' = 1sq yard

hsi_mets = hsi_merge %>%
  mutate(pix_area = pix_area) %>%
  group_by(ID) %>%
  summarise(area_m2 = sum(pix_area),
            WUA = sum(value),
            HHS = WUA/area_m2) %>%
  mutate(species = spc,
         life_stage = ls)

# Write out HSI metrics to .csv
write.csv(hsi_mets, paste0("output/hsi/", spc, "_", ls, "_hsi_mets.csv"))
