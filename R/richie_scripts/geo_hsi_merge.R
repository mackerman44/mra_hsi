library(tidyverse)
library(sf)

setwd("F:/LemhiHydraulicScenarios/Geomorph")

geo_r_1 <- st_read("LEM_Base_ComplexSimple.shp")
geo_r_2 <- st_read("LEM_Peak_ComplexSimple.shp")
geo_r_merge <- rbind(geo_r_1, geo_r_2)

reach_poly <- st_read("Lem_Poly_Label.shp") %>%
  st_transform(crs = crs(geo_r_1))

geo_merge <- st_join(geo_r_merge,
                     reach_poly,
                     join = st_nearest_feature,
                     left = TRUE) %>%
  mutate(Length_m = Length/3.28083)

mix_length <- geo_merge %>%
  filter(Geo_Tier == "Mixed") %>%
  group_by(Name) %>%
  summarise(mixed_length = sum(Length_m)) %>%
  st_drop_geometry()
  
comp_length <- geo_merge %>%
  filter(Geo_Tier == "Complex") %>%
  group_by(Name) %>%
  summarise(complex_length = sum(Length_m)) %>%
  st_drop_geometry()
  
simp_length <- geo_merge %>%
  filter(Geo_Tier == "Simplified") %>%
  group_by(Name) %>%
  summarise(simplified_length = sum(Length_m)) %>%
  st_drop_geometry()


sc_length <- geo_merge %>%
  filter(ChanUnits == "Side Channel") %>%
  group_by(Name) %>%
  summarise(sc_length = sum(Length_m)) %>%
  st_drop_geometry()

chann_length <- geo_merge %>%
  filter(ChanUnits == "Channelized") %>%
  group_by(Name) %>%
  summarise(channelized_length = sum(Length_m)) %>%
  st_drop_geometry()

sin_length <- geo_merge %>%
  filter(ChanUnits == "Sinuous") %>%
  group_by(Name) %>%
  summarise(sinuous_length = sum(Length_m)) %>%
  st_drop_geometry()

agg_wid_length <- geo_merge %>%
  filter(ChanUnits == "Aggradation and Widening") %>%
  group_by(Name) %>%
  summarise(agg_wide_length = sum(Length_m)) %>%
  st_drop_geometry()


arr_deg_length  <- geo_merge %>%
  filter(ChanUnits == "Arrested Degradation") %>%
  group_by(Name) %>%
  summarise(arrest_deg_length = sum(Length_m)) %>%
  st_drop_geometry()

lat_length  <- geo_merge %>%
  filter(ChanUnits == "Laterally Active") %>%
  group_by(Name) %>%
  summarise(later_length = sum(Length_m)) %>%
  st_drop_geometry()

deg_length  <- geo_merge %>%
  filter(ChanUnits == "Degradation") %>%
  group_by(Name) %>%
  summarise(deg_length = sum(Length_m)) %>%
  st_drop_geometry()

geo_length <- geo_merge %>%
  group_by(Name) %>%
  summarise(reach_length = sum(Length_m)) %>%
  st_drop_geometry() %>%
  left_join(mix_length, by = "Name") %>%
  left_join(comp_length, by = "Name") %>%
  left_join(simp_length, by = "Name") %>%
  left_join(sc_length, by = "Name") %>%
  left_join(chann_length, by = "Name") %>%
  left_join(agg_wid_length, by = "Name") %>%
  left_join(arr_deg_length, by = "Name") %>%
  left_join(lat_length, by = "Name") %>%
  left_join(deg_length, by = "Name")

geo_length[is.na(geo_length)] <- 0

geo_percent <- geo_length %>%
  mutate(perc_mixed = mixed_length/reach_length,
            perc_complex = complex_length/reach_length,
            perc_simple = simplified_length/reach_length,
            perc_side_chnl = sc_length/reach_length) %>%
  dplyr::select(1,2, 12:15) %>%
  rename("Reach" = "Name") 


##Readin HSI results##

chnk_spwn <- read.csv("F:/LemhiHydraulicScenarios/HSI/Chinook/Adult/Lem_Chnk_Spwn.csv") %>%
  dplyr::select(2:5) 
names(chnk_spwn) <- c("Reach", "chn_spwn_area", "chn_spwn_WUA", "chn_spwn_HHS")

chnk_juv <- read.csv("F:/LemhiHydraulicScenarios/HSI/Chinook/Juvenile/Lem_Juv_Chnk.csv") %>%
  dplyr::select(2:5) 
names(chnk_juv) <- c("Reach", "chn_juv_area", "chn_juv_WUA", "chn_juv_HHS")


chnk_juv_wint <- read.csv("F:/LemhiHydraulicScenarios/HSI/Chinook/Juvenile/Lem_Juv_Chnk_wint.csv") %>%
  dplyr::select(2:5) 
names(chnk_juv_wint) <- c("Reach", "chn_juvWint_area", "chn_juvWint_WUA", "chn_juvWint_HHS")

chnk_hsi <- chnk_spwn %>%
  left_join(chnk_juv, by = "Reach") %>%
  left_join(chnk_juv_wint, by = "Reach")


stlh_spwn <- read.csv("F:/LemhiHydraulicScenarios/HSI/Steelhead/Adult/HSI_mets.csv") %>%
  dplyr::select(2:5) 
names(stlh_spwn) <- c("Reach", "stlh_spwn_area", "stlh_spwn_WUA", "stlh_spwn_HHS")


stlh_juv <- read.csv("F:/LemhiHydraulicScenarios/HSI/Steelhead/Juvenile/Lem_Juv_Stlh.csv") %>%
  dplyr::select(2:5) 
names(stlh_juv) <- c("Reach", "stlh_juv_area", "stlh_juv_WUA", "stlh_juv_HHS")


stlh_juv_wint <- read.csv("F:/LemhiHydraulicScenarios/HSI/Steelhead/Juvenile/Lem_Juv_Stlh_wint.csv") %>%
  dplyr::select(2:5) 
names(stlh_juv_wint) <- c("Reach", "stlh_juv_wint_area", "stlh_juv_wint_WUA", "stlh_juv_wint_HHS")

stlh_hsi <- stlh_spwn %>%
  left_join(stlh_juv, by = "Reach") %>%
  left_join(stlh_juv_wint, by = "Reach")

geo_hsi <- geo_percent %>%
  left_join(chnk_hsi, by = "Reach") %>%
  left_join(stlh_hsi, by = "Reach")

write.csv(geo_hsi, "F:/LemhiHydraulicScenarios/HSI/Summary/Lem_geo_HSI.csv")





