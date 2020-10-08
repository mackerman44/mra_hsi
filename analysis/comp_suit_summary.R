## Summarizing raw composite suitability (depth and velocity) results ##
#
# Created by: Mike Ackerman
#
# This script reads in results from calculate_hsi_mets.R and geo_summary.R, 
# summarizes and plots them
#
##################################################

## load necessary libraries
library(tidyverse)
library(ggpubr)
library(sf)

# plot HSI curves for each species
hsi_df = read_csv("data/hsi_curves/maret_hsi_curves_for_r.csv")

# chinook hsi curves
chnk_hsi = hsi_df %>%
  filter(species == "Chinook") %>%
  ggplot(aes(x = value_m, y = si)) +
  geom_line(color = "royalblue3", size = 1.5) +
  coord_cartesian(xlim = c(0,2)) +
  theme_bw() +
  facet_grid(variable ~ life_stage) +
  labs(x = "Value (m | m/s)",
       y = "Suitability Index",
       title = "Chinook salmon")
chnk_hsi

# steelhead hsi curves
sthd_hsi = hsi_df %>%
  filter(species == "Steelhead") %>%
  ggplot(aes(x = value_m, y = si)) +
  geom_line(color = "royalblue3", size = 1.5) +
  coord_cartesian(xlim = c(0,2)) +
  theme_bw() +
  facet_grid(variable ~ life_stage) +
  labs(x = "Value (m | m/s)",
       y = "Suitability Index",
       title = "Steelhead")
sthd_hsi

## read in all raw composite suitability results and append into one object
raw_outputs = list.files(path = "output/hsi_raw/", pattern = "*.RData", full.names = T)
all_comp_suits = sapply(raw_outputs, function(x) mget(load(x)), simplify = T) %>%
  bind_rows() %>%
  mutate(geo_reach = paste0("GR_", str_pad(sub(".*_", "", ID), 2, pad = "0"))) %>%
  dplyr::select(-ID) %>%
  mutate(species = recode(species,
                          `chnk` = "Chinook",
                          `sthd` = "Steelhead"))

##################################
# UPPER LEMHI ABOVE HAYDEN CREEK #
##################################
# the geo reaches within the upper lemhi
ul_geos = c("GR_01", "GR_02", "GR_03", "GR_04",
            "GR_05", "GR_06", "GR_07", "GR_08")

# get upper lemhi data
ul_cs = all_comp_suits %>%
  filter(watershed == "lemh") %>%
  filter(geo_reach %in% ul_geos) %>%
  unite(scenario, life_stage, season, sep = "_") %>%
  mutate(scenario = recode(scenario,
                           `juv_sum` = "Juvenile Summer Rearing",
                           `juv_win` = "Juvenile Winter Rearing",
                           `spw_sum` = "Adult Spawning",
                           `spw_spr` = "Adult Spawning"))

# the violin plot
ul_vplot = ul_cs %>%
  ggplot(aes(x = geo_reach,
             y = value,
             fill = scenario)) +
  geom_violin(draw_quantiles = c(0.5),
              scale = "width") +
  scale_fill_brewer(palette = "Set3") +
  theme_bw() +
  facet_wrap(~ species, nrow = 2) +
  labs(x = "Geomorphic Reach",
       y = "Composite Suitability (Depth & Velocity)",
       fill = "Scenario",
       title = "Upper Lemhi") +
  theme(axis.text.x = element_text(angle = -45, vjust = 0))
ul_vplot

# plot of geomorphic tier data
load("output/geomorph/lemh_geomorph_summary.RData")
ul_tier_p = tier_summary %>%
  filter(Name %in% ul_geos) %>%
  ggplot(aes(x = Name, y = p_Geo_Tier, fill = Geo_Tier)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(x = "Geomorphic Reach",
       y = "p(Geomorphic Tier",
       fill = "Geomorphic Tier") +
  scale_fill_brewer(palette = "Set2") +
  theme(axis.text.x = element_text(angle = -45, vjust = 0))
ul_tier_p

# merge the violin and geomorphic tier plot
ul_p = ggarrange(plotlist = list(ul_vplot +
                                   theme(axis.text.x = element_blank(),
                                         legend.position = "top") +
                                   labs(x = NULL),
                                 ul_tier_p +
                                   theme(legend.position = "bottom")),
          nrow = 2,
          ncol = 1,
          heights = c(2, 1.25))
ul_p
ggsave("output/figures/ul_cs_plot.pdf", ul_p)

# map by scenario
ul_cs_map = ul_cs %>%
  group_by(species, scenario, geo_reach) %>%
  summarise(mean = mean(value),
            median = median(value),
            n = n()) %>%
  left_join(st_read("data/geomorph/geo_reaches/Lem_Poly_Label.shp") %>%
              mutate(Name = paste0("GR_", str_pad(sub(".*_", "", Name), 2, pad = "0"))),
            by = c("geo_reach" = "Name"))  %>%
  st_as_sf() %>%
  ggplot(aes(fill = mean)) +
  geom_sf() +
  #scale_fill_viridis_c(option = "magma") +
  #scale_fill_continuous(low = "blue", high = "red") +
  scale_fill_distiller(palette = "Spectral") +
  theme_bw() +
  labs(fill = "Mean Composite Suitability") +
  facet_grid(species ~ scenario) +
  theme(axis.text.x = element_text(angle = -45, vjust = 0),
        legend.position = "top")
ul_cs_map
ggsave("output/figures/ul_cs_map.pdf", ul_cs_map)  

#####################################
# LOWER LEMHI: HAYDEN TO CONFLUENCE #
#####################################
# geo reaches in the lower lemhi
ll_geos = c("GR_09", "GR_10", "GR_11", "GR_12",
            "GR_13", "GR_14", "GR_15", "GR_16")

# get lower lemhi data
ll_cs = all_comp_suits %>%
  filter(watershed == "lemh") %>%
  filter(geo_reach %in% ll_geos) %>%
  unite(scenario, life_stage, season, sep = "_") %>%
  mutate(scenario = recode(scenario,
                           `juv_sum` = "Juvenile Summer Rearing",
                           `juv_win` = "Juvenile Winter Rearing",
                           `spw_sum` = "Adult Spawning",
                           `spw_spr` = "Adult Spawning"))

# the violin plot
ll_vplot = ll_cs %>%
  ggplot(aes(x = geo_reach,
             y = value,
             fill = scenario)) +
  geom_violin(draw_quantiles = c(0.5),
              scale = "width") +
  scale_fill_brewer(palette = "Set3") +
  theme_bw() +
  facet_wrap(~ species, nrow = 2) +
  labs(x = "Geomorphic Reach",
       y = "Composite Suitability (Depth & Velocity)",
       fill = "Scenario",
       title = "Lower Lemhi") +
  theme(axis.text.x = element_text(angle = -45, vjust = 0))
ll_vplot

# plot of geomorphic tier data
load("output/geomorph/lemh_geomorph_summary.RData")
ll_tier_p = tier_summary %>%
  filter(Name %in% ll_geos) %>%
  ggplot(aes(x = Name, y = p_Geo_Tier, fill = Geo_Tier)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(x = "Geomorphic Reach",
       y = "p(Geomorphic Tier",
       fill = "Geomorphic Tier") +
  scale_fill_brewer(palette = "Set2") +
  theme(axis.text.x = element_text(angle = -45, vjust = 0))
ll_tier_p

# merge the violin and geomorphic tier plot
ll_p = ggarrange(plotlist = list(ll_vplot +
                                   theme(axis.text.x = element_blank(),
                                         legend.position = "top") +
                                   labs(x = NULL),
                                 ll_tier_p +
                                   theme(legend.position = "bottom")),
                 nrow = 2,
                 ncol = 1,
                 heights = c(2, 1.25))
ll_p
ggsave("output/figures/ll_cs_plot.pdf", ll_p)

# map by scenario
ll_cs_map = ll_cs %>%
  group_by(species, scenario, geo_reach) %>%
  summarise(mean = mean(value),
            median = median(value),
            n = n()) %>%
  left_join(st_read("data/geomorph/geo_reaches/Lem_Poly_Label.shp") %>%
              mutate(Name = paste0("GR_", str_pad(sub(".*_", "", Name), 2, pad = "0"))),
            by = c("geo_reach" = "Name"))  %>%
  st_as_sf() %>%
  ggplot(aes(fill = mean)) +
  geom_sf() +
  #scale_fill_viridis_c(option = "magma") +
  #scale_fill_continuous(low = "blue", high = "red") +
  scale_fill_distiller(palette = "Spectral") +
  theme_bw() +
  labs(fill = "Mean Composite Suitability") +
  facet_grid(species ~ scenario) +
  theme(axis.text.x = element_text(angle = -45, vjust = 0),
        legend.position = "top")
ll_cs_map
ggsave("output/figures/ll_cs_map.pdf", ll_cs_map)  

##############
# PAHSIMEROI #
##############
# get pahsimeroi data
ph_cs = all_comp_suits %>%
  filter(watershed == "pahs") %>%
  unite(scenario, life_stage, season, sep = "_") %>%
  mutate(scenario = recode(scenario,
                           `juv_sum` = "Juvenile Summer Rearing",
                           `juv_win` = "Juvenile Winter Rearing",
                           `spw_sum` = "Adult Spawning",
                           `spw_spr` = "Adult Spawning"))

# the violin plot
ph_vplot = ph_cs %>%
  ggplot(aes(x = geo_reach,
             y = value,
             fill = scenario)) +
  geom_violin(draw_quantiles = c(0.5),
              scale = "width") +
  scale_fill_brewer(palette = "Set3") +
  theme_bw() +
  facet_wrap(~ species, nrow = 2) +
  labs(x = "Geomorphic Reach",
       y = "Composite Suitability (Depth & Velocity)",
       fill = "Scenario",
       title = "Pahsimeroi") +
  theme(axis.text.x = element_text(angle = -45, vjust = 0))
ph_vplot

# plot of geomorphic tier data
load("output/geomorph/pahs_geomorph_summary.RData")
ph_tier_p = tier_summary %>%
  ggplot(aes(x = Name, y = p_Geo_Tier, fill = Geo_Tier)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(x = "Geomorphic Reach",
       y = "p(Geomorphic Tier",
       fill = "Geomorphic Tier") +
  scale_fill_brewer(palette = "Set2") +
  theme(axis.text.x = element_text(angle = -45, vjust = 0))
ph_tier_p

# merge the violin and geomorphic tier plot
ph_p = ggarrange(plotlist = list(ph_vplot +
                                   theme(axis.text.x = element_blank(),
                                         legend.position = "top") +
                                   labs(x = NULL),
                                 ph_tier_p +
                                   theme(legend.position = "bottom")),
                 nrow = 2,
                 ncol = 1,
                 heights = c(2, 1.25))
ph_p
ggsave("output/figures/ph_cs_plot.pdf", ph_p)

# map by scenario
ph_cs_map = ph_cs %>%
  group_by(species, scenario, geo_reach) %>%
  summarise(mean = mean(value),
            median = median(value),
            n = n()) %>%
  left_join(st_read("data/geomorph/geo_reaches/Pah_Poly_Label.shp") %>%
              rename(Name = GeoReach) %>%
              mutate(Name = str_replace(Name, "-", "_")) %>%
              mutate(Name = paste0("GR_", str_pad(sub(".*_", "", Name), 2, pad = "0"))),
            by = c("geo_reach" = "Name"))  %>%
  st_as_sf() %>%
  ggplot(aes(fill = mean)) +
  geom_sf() +
  #scale_fill_viridis_c(option = "magma") +
  #scale_fill_continuous(low = "blue", high = "red") +
  scale_fill_distiller(palette = "Spectral") +
  theme_bw() +
  labs(fill = "Mean Composite Suitability") +
  facet_grid(species ~ scenario) +
  theme(axis.text.x = element_text(angle = -45, vjust = 0),
        legend.position = "top")
ph_cs_map
ggsave("output/figures/ph_cs_map.pdf", ph_cs_map)  

################
# UPPER SALMON #
################
# get upper salmon data
us_cs = all_comp_suits %>%
  filter(watershed == "upsa") %>%
  unite(scenario, life_stage, season, sep = "_") %>%
  mutate(scenario = recode(scenario,
                           `juv_spr` = "Juvenile Spring Rearing",
                           `juv_sum` = "Juvenile Summer Rearing",
                           `juv_win` = "Juvenile Winter Rearing",
                           `spw_sum` = "Adult Spawning",
                           `spw_spr` = "Adult Spawning"))

# the violin plot
us_vplot = us_cs %>%
  ggplot(aes(x = geo_reach,
             y = value,
             fill = scenario)) +
  geom_violin(draw_quantiles = c(0.5),
              scale = "width") +
  scale_fill_brewer(palette = "Set3") +
  theme_bw() +
  facet_wrap(~ species, nrow = 2) +
  labs(x = "Geomorphic Reach",
       y = "Composite Suitability (Depth & Velocity)",
       fill = "Scenario",
       title = "Upper Salmon") +
  theme(axis.text.x = element_text(angle = -45, vjust = 0))
us_vplot

# plot of geomorphic tier data
load("output/geomorph/upsa_geomorph_summary.RData")
us_tier_p = tier_summary %>%
  filter(!Name == "GR_NA") %>%
  ggplot(aes(x = Name, y = p_Geo_Tier, fill = Geo_Tier)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(x = "Geomorphic Reach",
       y = "p(Geomorphic Tier",
       fill = "Geomorphic Tier") +
  scale_fill_brewer(palette = "Set2") +
  theme(axis.text.x = element_text(angle = -45, vjust = 0))
us_tier_p

# merge the violin and geomorphic tier plot
us_p = ggarrange(plotlist = list(us_vplot +
                                   theme(axis.text.x = element_blank(),
                                         legend.position = "top") +
                                   labs(x = NULL),
                                 us_tier_p +
                                   theme(legend.position = "bottom")),
                 nrow = 2,
                 ncol = 1,
                 heights = c(2, 1.25))
us_p
ggsave("output/figures/us_cs_plot.pdf", us_p)

# map by scenario
us_cs_map = us_cs %>%
  group_by(species, scenario, geo_reach) %>%
  summarise(mean = mean(value),
            median = median(value),
            n = n()) %>%
  left_join(st_read("data/geomorph/geo_reaches/US_Poly_Label.shp") %>%
              select(GeoReach, geometry) %>%
              rename(Name = GeoReach) %>%
              mutate(Name = str_replace(Name, "-", "_")) %>%
              mutate(Name = paste0("GR_", str_pad(sub(".*_", "", Name), 2, pad = "0"))),
            by = c("geo_reach" = "Name"))  %>%
  st_as_sf() %>%
  ggplot(aes(fill = mean)) +
  geom_sf() +
  #scale_fill_viridis_c(option = "magma") +
  #scale_fill_continuous(low = "blue", high = "red") +
  scale_fill_distiller(palette = "Spectral") +
  theme_bw() +
  labs(fill = "Mean Composite Suitability") +
  facet_grid(species ~ scenario) +
  theme(axis.text.x = element_text(angle = -45, vjust = 0),
        legend.position = "top")
us_cs_map
ggsave("output/figures/us_cs_map.pdf", us_cs_map)  
