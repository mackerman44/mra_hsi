## Summarizing raw composite suitability (depth and velocity) results ##
#
# Created by: Mike Ackerman
#
# This script reads in cresults from calculate_hsi_mets.R and geo_summary.R, 
# summarizes and plots them
#
##################################################

## load necessary libraries
library(tidyverse)

## read in all raw composite suitability results and append into one object
raw_outputs = list.files(path = "output/hsi_raw/", pattern = "*.RData", full.names = T)
all_comp_suits = sapply(raw_outputs, function(x) mget(load(x)), simplify = T) %>%
  bind_rows() %>%
  mutate(geo_reach = paste0("GR_", str_pad(sub(".*_", "", ID), 2, pad = "0"))) %>%
  dplyr::select(-ID) %>%
  mutate(species = recode(species,
                          `chnk` = "Chinook",
                          `sthd` = "Steelhead"))

######################
# SOME LEMHI TESTING #
######################
# Upper Lemhi = GR_01:GR08; Lower Lemhi = GR_09 = GR_16
ul_geos = c("GR_01", "GR_02", "GR_03", "GR_04",
            "GR_05", "GR_06", "GR_07", "GR_08")
ll_geos = c("GR_09", "GR_10", "GR_11", "GR_12",
            "GR_13", "GR_14", "GR_15", "GR_16")

ul_cs = all_comp_suits %>%
  filter(watershed == "lemh") %>%
  filter(geo_reach %in% ul_geos) %>%
  unite(scenario, life_stage, season, sep = "_") %>%
  mutate(scenario = recode(scenario,
                           `juv_sum` = "Juvenile Summer Rearing",
                           `juv_win` = "Juvenile Winter Rearing",
                           `spw_sum` = "Adult Spawning",
                           `spw_spr` = "Adult Spawning"))

# violin plot
# ul_vplot = ul_cs %>%
#   ggplot(aes(x = geo_reach, y = value)) +
#   geom_violin(fill = "cornflowerblue",
#               draw_quantiles = c(0.5)) +
#   # geom_boxplot(fill = "cornflowerblue") +
#   theme_bw() +
#   facet_wrap(species ~ scenario) +
#   labs(x = "Geomorphic Reach",
#        y = "Composite Suitability (Depth & Velocity",
#        title = "Upper Lemhi")
# ul_vplot

ul_vplot = ul_cs %>%
  ggplot(aes(x = geo_reach, 
             y = value,
             fill = scenario)) +
  geom_violin(draw_quantiles = c(0.5),
              scale = "width") +
  scale_fill_brewer(palette = "Set3") +
  # geom_boxplot(fill = "cornflowerblue") +
  theme_bw() +
  facet_wrap(~ species,
             nrow = 2) +
  labs(x = "Geomorphic Reach",
       y = "Composite Suitability (Depth & Velocity",
       title = "Upper Lemhi")

# geomorphic tiers
## read in the geomorph summary
load("output/geomorph/lemh_geomorph_summary.RData")
tier_p = tier_summary %>%
  filter(Name %in% unique(ul_cs$geo_reach)) %>%
  ggplot(aes(x = Name, y = p_Geo_Tier, fill = Geo_Tier)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(x = "Geomorphic Reach",
       y = "p(Geomorphic Tier)",
       fill = "Geomorphic Tier") +
  scale_fill_brewer(palette = "Set2") +
  theme(axis.text.x = element_text(angle = -45, vjust = 0))
tier_p

library(ggpubr)
ggarrange(plotlist = list(ul_vplot +
                            theme(axis.text.x = element_blank(),
                                  legend.position = 'top') +
                            labs(x = NULL),
                            tier_p +
                            theme(legend.position = "bottom")),
          nrow = 2,
          ncol = 1,
          heights = c(2, 1.5))

us_p = us_cs %>% 
  ggplot(aes(x = geo_reach, y = value)) +
  geom_violin(fill = "cornflowerblue") +
  theme_bw() +
  labs(x = "Geomorphic Reach",
       y = "Composite Suitability (Depth & Velocity)",
       title = "Chinook Juvenile Summer Rearing")
us_p


#############################
# SOME UPPER SALMON TESTING #
#############################
# upper salmon, chinook, juvenile, summer
us_cs = all_comp_suits %>%
  filter(watershed == "upsa",
         season == "sum")

# boxplot
us_p = us_cs %>% 
  ggplot(aes(x = geo_reach, y = value)) +
  geom_boxplot(fill = "cornflowerblue") +
  theme_bw() +
  labs(x = "Geomorphic Reach",
       y = "Composite Suitability (Depth & Velocity)",
       title = "Chinook Juvenile Summer Rearing")
us_p

# violin plot
us_p = us_cs %>% 
  ggplot(aes(x = geo_reach, y = value)) +
  geom_violin(fill = "cornflowerblue") +
  theme_bw() +
  labs(x = "Geomorphic Reach",
       y = "Composite Suitability (Depth & Velocity)",
       title = "Chinook Juvenile Summer Rearing")
us_p

# next, add faceted histogram
us_p = us_cs %>%
  ggplot(aes(x = value)) +
  geom_histogram() +
  theme_bw() +
  facet_wrap(~ geo_reach, ncol = 4) +
  labs(x = "Geomorphic Reach",
       y = "Composite Suitability (Depth & Velocity)",
       title = "Chinook Juvenile Summer Rearing")
us_p
  
