## Code for merging HSI results with geomorph data/results ##
#
# Created by: Richie Carmichael
# Modified by: Mike Ackerman
#
##############################################################

## load necessary libraries
library(raster)
library(rgdal)
library(sf)
library(tidyverse)

## read in geomorph data with simple, complex, mixed designations
geo_r_base <- st_read("data/geomorph/LEM_Base_ComplexSimple.shp") 
geo_r_peak <- st_read("data/geomorph/LEM_Peak_ComplexSimple.shp") 

## merge the above together
geo_r = rbind(geo_r_base, geo_r_peak)

## read in the reach polygons
reach_poly <- st_read("data/geomorph/Lem_Poly_Label.shp") %>%
  st_transform(crs = crs(geo_r))
plot(reach_poly)

# merge geo data and geomorphic reach polygons
geo_merge <- st_join(geo_r,
                     reach_poly,
                     join = st_nearest_feature,
                     left = TRUE) %>%
  mutate(Length_m = Length/3.28083) %>%
  st_drop_geometry() %>%
  dplyr::select(-Id) %>%
  mutate(Name = paste0("GR_", str_pad(sub(".*_", "", Name), 2, pad = "0")))

# summary of geo_tiers: calculate the length of simple, mixed, and complexed by geomorphic reach 
# and then convert to proportions
tier_summary = geo_merge %>%
  dplyr::select(Name, Geo_Tier, Length_m) %>%
  group_by(Name, Geo_Tier) %>%
  summarise(Length_m = sum(Length_m)) %>%
  mutate(p_Geo_Tier = Length_m / sum(Length_m))

# summary of channel units: caluclate the length by channel unit within each geomorphic reach and
# then convert to proportions
cu_summary = geo_merge %>%
  dplyr::select(Name, ChanUnits, Length_m) %>%
  group_by(Name, ChanUnits) %>%
  summarise(Length_m = sum(Length_m)) %>%
  mutate(p_ChanUnit = Length_m / sum(Length_m))

# save geomorph summaries
save(tier_summary, cu_summary, file = "output/geomorph/geomorph_summary.RData")
