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

## Set arguments, for file naming, etc.
# a single run
wtsd = "lemh" # watershed: either "lemh", "pahs", or "upsa"
spc  = "chnk" # species: either "chnk" or "sthd"
ls   = "juv"  # life stage: either "juv" or "spw"
ssn  = "sum"  # season: either "spr, "sum", or "win"

# to loop over scenarios (e.g.)
# watersheds = c("lemh", "pahs", "upsa") 
# species  = c("chnk", "sthd")        
# life_stages   = c("juv", "spw")
# seasons  = c("sum", "win")

for(wtsd in watersheds) {
  for(spc in species) {
    for(ls in life_stages) {
      for(ssn in seasons) {
  
        ## read in depth and velocity rasters
        if(wtsd == "lemh") {
          if(ssn == "spr") {
            d_rast <- raster("data/d_v_tifs/lemhi/d_jan_v2.tif")
            v_rast <- raster("data/d_v_tifs/lemhi/v_jan_v2.tif")  
          }
          if(ssn == "sum") {
            d_rast <- raster("data/d_v_tifs/lemhi/D_Aug_All.tif")
            v_rast <- raster("data/d_v_tifs/lemhi/V_Aug_All.tif")
          }
          if(ssn == "win") {
            d_rast <- raster("data/d_v_tifs/lemhi/d_jan_v2.tif")
            v_rast <- raster("data/d_v_tifs/lemhi/v_jan_v2.tif")  
          }
        } # end upper salmon
        
        if(wtsd == "pahs") {
          if(ssn == "spr") {
            d_rast <- raster("data/d_v_tifs/pahsimeroi/Pah_1pt5year_depth.tif")
            v_rast <- raster("data/d_v_tifs/pahsimeroi/Pah_1pt5year_velocity.tif")
          }
          if(ssn == "sum") { # still waiting on these .tifs from Sam
            #d_rast <- raster("data/d_v_tifs/pahsimeroi/Pah_WLow_depth.tif")
            #v_rast <- raster("data/d_v_tifs/pahsimeroi/Pah_WLow_velocity.tif")
          }
          if(ssn == "win") {
            d_rast <- raster("data/d_v_tifs/pahsimeroi/Pah_WLow_depth.tif")
            v_rast <- raster("data/d_v_tifs/pahsimeroi/Pah_WLow_velocity.tif")  
          }
        } # end pahsimeroi
        
        if(wtsd == "upsa") {
          if(ssn == "spr") {
            d_rast <- raster("data/d_v_tifs/upper_salmon/US_1pt5year_depth.tif")
            v_rast <- raster("data/d_v_tifs/upper_salmon/US_1pt5year_velocity.tif")
          }
          if(ssn == "sum") {
            d_rast <- raster("data/d_v_tifs/upper_salmon/US_Summer75_depth.tif")
            v_rast <- raster("data/d_v_tifs/upper_salmon/US_Summer75_velocity.tif")
          }
          if(ssn == "win") {
            d_rast <- raster("data/d_v_tifs/upper_salmon/US_Winter75_depth.tif")
            v_rast <- raster("data/d_v_tifs/upper_salmon/US_Winter75_velocity.tif")  
          }
        } # end upper salmon
        
        # Read in reach polygons
        if(wtsd == "lemh") { 
          reaches <- st_read("data/geomorph/geo_reaches/Lem_Poly_Label.shp") 
        } # end lemhi
        if(wtsd == "pahs") { 
          reaches <- st_read("data/geomorph/geo_reaches/Pah_Poly_Label.shp") %>%
            rename(Name = GeoReach) %>%
            mutate(Name = str_replace(Name, "-", "_"))
        } # end pahsimeroi
        if(wtsd == "upsa") { 
          reaches <- st_read("data/geomorph/geo_reaches/US_Poly_Label.shp") %>%
            select(GeoReach, geometry) %>%
            rename(Name = GeoReach) %>%
            mutate(Name = str_replace(Name, "-", "_"))
        } # end upper salmon
        # plot(reaches)
        
        # Create one object for each reach
        rch_names = unique(reaches$Name)
        for(rch in rch_names) {
          assign(as.character(rch), filter(reaches, Name == rch))
        }
        
        # read in functions to calculate HSI
        source("R/hsi_curves.R")
        
        # Calculate suitability insert appropriate function
        d_curve = paste0(spc, "_", ls, "_d")
        v_curve = paste0(spc, "_", ls, "_v")
        d_suit <- calc(d_rast, get(d_curve))
        v_suit <- calc(v_rast, get(v_curve)) 
        
        # Write rasters if desired
        writeRaster(d_suit, paste0("output/hsi_tifs/", wtsd, "_", spc, "_", ls, "_", ssn, "_d_suit.tif"), overwrite = T)
        writeRaster(v_suit, paste0("output/hsi_tifs/", wtsd, "_", spc, "_", ls, "_", ssn, "_v_suit.tif"), overwrite = T)
        
        # Function to calculate composite suitability using geometric mean of depth and velocity
        comp_hsi <- function(r1, r2) {
          y = (sqrt(r1*r2))
          return(y)
        }
        
        # Calculate geometric mean and create a new raster
        comp_suit <- overlay(d_suit, v_suit, fun = comp_hsi)
        writeRaster(comp_suit, paste0("output/hsi_tifs/", wtsd, "_", spc, "_", ls, "_", ssn, "_comp_suit.tif"), overwrite = T)
        
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
        if(wtsd == "lemh") { pix_area = prod(res(comp_suit)) } # for lem the pixels are 1m x 1m = 1sq meter
        if(wtsd == "pahs" | wtsd == "upsa") { pix_area = 0.836127 } # for pahs, us the pixels are 3' x 3' = 1sq yard
  
        hsi_mets = hsi_merge %>%
          mutate(pix_area = pix_area) %>%
          group_by(ID) %>%
          summarise(area_m2 = sum(pix_area),
                    WUA = sum(value),
                    HHS = WUA/area_m2) %>%
          mutate(watershed = wtsd,
                 species = spc,
                 life_stage = ls,
                 season = ssn)
        
        # Write out composite suitability values to a .RData
        hsi_values = hsi_merge %>%
          mutate(watershed = wtsd,
                 species = spc,
                 life_stage = ls,
                 season = ssn) %>%
          save(file = paste0("output/hsi_raw/", wtsd, "_", spc, "_", ls, "_", ssn, "_hsi_values.RData"))
        
        # Write out HSI metrics to .csv
        write.csv(hsi_mets, paste0("output/hsi_csvs/", wtsd, "_", spc, "_", ls, "_", ssn, "_hsi_mets.csv"))
  
      } # end season loop
    } # end life stage loop
  } # end species loop
} # end watershed loop
