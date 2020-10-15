# Author: Kevin See
# Purpose: Calculate WUA and HHS for each river kilometer in the MRA watersheds
# Created: 10/13/20
# Last Modified: 10/13/20
# Notes: Based on HSI rasters produced by calculate_hsi_mets.R script

#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
library(raster)
library(sf)


#-------------------------
# set NAS prefix, depending on operating system
#-------------------------
if(.Platform$OS.type != 'unix') {
  nas_prefix = "S:"
} else if(.Platform$OS.type == 'unix') {
  nas_prefix = "~/../../Volumes/ABS/"
}

#-----------------------------------------------------------------
## Set arguments, for file naming, etc.
# a single run
wtsd = "lemh" # watershed: either "lemh", "pahs", or "upsa"
spc  = "chnk" # species: either "chnk" or "sthd"
ls   = "juv"  # life stage: either "juv" or "spw"
ssn  = "sum"  # season: either "spr, "sum", or "win"

# to loop over scenarios (e.g.)
watersheds = c("lemh", "pahs", "upsa")
species = c("chnk", "sthd")
life_stages = c("juv", "spw")
seasons = c("spr", "sum", "win")

for(wtsd in watersheds[-1]) {
  
  # Read in reach polygons (1 polygon for each river kilometer)
  rch_file = if_else(wtsd == "lemh",
                     paste0(nas_prefix, "data/habitat/HSI/MRA_HSI_reaches/Lemhi/Lemhi_Reaches.shp"),
                     if_else(wtsd == "pahs",
                             paste0(nas_prefix, "data/habitat/HSI/MRA_HSI_reaches/Pahsimeroi/Pah_Reaches.shp"),
                             if_else(wtsd == "upsa",
                                     paste0(nas_prefix, "data/habitat/HSI/MRA_HSI_reaches/Upper_Salmon/UpperSalmon_Reaches.shp"),
                                     NA_character_)))
  
  reaches = st_read(rch_file) %>%
    dplyr::select(Reach) %>%
    arrange(Reach)
  # plot(reaches)
  

  for(spc in species) {
    for(ls in life_stages) {
      for(ssn in seasons) {
        
        # check if the combined raster exisits
        if(!paste0(wtsd, "_", spc, "_", ls, "_", ssn, "_comp_suit.tif") %in% list.files(paste0(nas_prefix, "data/habitat/HSI/MRA_outputs"))) {
          next()
        }
        
        # read in combined depth and velocity raster
        comp_suit = raster(paste0(nas_prefix, "data/habitat/HSI/MRA_outputs/", wtsd, "_", spc, "_", ls, "_", ssn, "_comp_suit.tif"))
        
        # Extract raster values at polygon 'reaches' from each object created using the reaches polygon
        hsi_merge = NULL
        for(rch in unique(reaches$Reach)) {
          sf_tmp = raster::extract(comp_suit,
                                   reaches %>%
                                     filter(Reach == rch),
                                   fun = NULL,
                                   df = TRUE,
                                   na.rm = TRUE) %>%
            as_tibble() %>%
            drop_na() %>%
            mutate(ID = replace(ID, ID == 1, as.character(rch)))
          names(sf_tmp) = c("Reach", "value")
          hsi_merge = hsi_merge %>%
            bind_rows(sf_tmp)
          rm(sf_tmp)
        }
        
        # Calculate metrics at reach scale wetted area, WUA, HHS
        # For calculating the HSI metrics
        if(wtsd == "lemh") { pix_area = prod(res(comp_suit)) } # for lem the pixels are 1m x 1m = 1sq meter
        if(wtsd == "pahs" | wtsd == "upsa") { pix_area = 0.836127 } # for pahs, us the pixels are 3' x 3' = 1sq yard
        
        hsi_mets = hsi_merge %>%
          mutate_at(vars(Reach),
                    list(as.numeric)) %>%
          arrange(Reach) %>%
          mutate(pix_area = pix_area) %>%
          group_by(Reach) %>%
          summarise(area_m2 = sum(pix_area),
                    WUA = sum(value),
                    HHS = WUA/area_m2) %>%
          mutate(watershed = wtsd,
                 species = spc,
                 life_stage = ls,
                 season = ssn)
        
        # Write out composite suitability values to a .RData
        save(hsi_mets,
             file = paste0("output/hsi_rkm/", wtsd, "_", spc, "_", ls, "_", ssn, "_hsi_values.RData"))
        
        # Write out HSI metrics to .csv
        write.csv(hsi_mets, paste0("output/hsi_rkm/", wtsd, "_", spc, "_", ls, "_", ssn, "_hsi_mets.csv"))
        
      }
    }
  }
}

# make all the HSI estimates spatial
# get points for each rkm reach
rch_pts = tibble(watershed = c("lemh",
                               "pahs",
                               "upsa"),
                 nm = c('Lemhi',
                        'Pahsimeroi',
                        'Upper_Salmon')) %>%
  mutate(rch_pts = map(nm,
                       .f = function(x) {
                         all_files = list.files(paste0(nas_prefix, "data/habitat/HSI/MRA_HSI_reaches/", x))
                         pt_file = all_files[grepl("^Points", all_files) & grepl('.shp$', all_files)]
                         st_read(paste0(nas_prefix, "data/habitat/HSI/MRA_HSI_reaches/", x, "/", pt_file)) %>%
                           mutate(Reach = 1:n(),
                                  Reach = Reach - 1) %>%
                           dplyr::select(Reach)
                       })) %>%
  mutate(rch_pts = map(rch_pts,
                       .f = st_transform,
                       crs = st_crs(rch_pts[[1]]))) %>%
  unnest(cols = rch_pts) %>%
  st_as_sf()

## read in all of the HSI results
hsi_outputs = list.files(path = "output/hsi_rkm/", pattern = "*.csv", full.names = T)
hsi_rkm = sapply(hsi_outputs, read_csv, simplify = F) %>%
  bind_rows(.id = "id") %>%
  dplyr::select(-c(id, X1)) %>%
  dplyr::select(species, life_stage, season, rkm = Reach, everything()) %>%
  mutate(scenario = paste(life_stage, season, sep = "_")) %>%
  mutate(species = recode(species,
                          `chnk` = "Chinook",
                          `sthd` = "Steelhead")) %>%
  mutate(scenario = recode(scenario,
                           `juv_spr` = "Juvenile Spring Rearing",
                           `juv_sum` = "Juvenile Summer Rearing",
                           `juv_win` = "Juvenile Winter Rearing",
                           `spw_spr` = "Adult Spawning",
                           `spw_sum` = "Adult Spawning"))

# add HSI info to each point
rch_hsi = rch_pts %>%
  inner_join(hsi_rkm %>%
               rename(Reach = rkm))

# save as GPKG
st_write(rch_hsi,
         dsn = "output/hsi_rkm/hsi_rch_pts.gpkg")
