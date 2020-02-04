## Summarizing HSI results and geomorph results ##
#
# Created by: Mike Ackerman
# 
# This script reads in results from calculate_hsi_mets.R and geo_summary.R, 
# summarizes and plots them
#
##################################################

## load necessary libraries
library(tidyverse)
library(gridExtra)
library(ggmap)
library(sf)

## read in all of the HSI results
hsi_outputs = list.files(path = "output/hsi_csvs/", pattern = "*.csv", full.names = T)
hsi_df = sapply(hsi_outputs, read_csv, simplify = F) %>%
  bind_rows(.id = "id") %>%
  dplyr::select(-c(id, X1)) %>%
  mutate(geo_reach = paste0("GR_", str_pad(sub(".*_", "", ID), 2, pad = "0"))) %>%
  dplyr::select(-ID) %>%
  dplyr::select(species, life_stage, season, geo_reach, everything())

## read in the geomorph summary
load("output/geomorph/lemh_geomorph_summary.RData")

##################################
# Plot HHS for 1 Model at a time #
##################################
# Chinook juvenile summer rearing
chnk_juv_sum_p = hsi_df %>%
  filter(species == "chnk",
         life_stage  == "juv",
         season == "sum") %>%
  ggplot(aes(x = geo_reach, y = HHS)) +
  geom_bar(fill = "cornflowerblue", stat = "identity") +
  theme_bw() +
  labs(x = "Geomorphic Reach",
       y = "Hydraulic Habitat Suitability",
       title = "Chinook Juvenile Summer Rearing")
chnk_juv_sum_p

# Chinook juvenile winter rearing
chnk_juv_win_p = hsi_df %>%
  filter(species == "chnk",
         life_stage  == "juv",
         season == "win") %>%
  ggplot(aes(x = geo_reach, y = HHS)) +
  geom_bar(fill = "cornflowerblue", stat = "identity") +
  theme_bw() +
  labs(x = "Geomorphic Reach",
       y = "Hydraulic Habitat Suitability",
       title = "Chinook Juvenile Winter Rearing")
chnk_juv_win_p

# Chinook summer spawning
chnk_spw_sum_p = hsi_df %>%
  filter(species == "chnk",
         life_stage  == "spw",
         season == "sum") %>%
  ggplot(aes(x = geo_reach, y = HHS)) +
  geom_bar(fill = "cornflowerblue", stat = "identity") +
  theme_bw() +
  labs(x = "Geomorphic Reach",
       y = "Hydraulic Habitat Suitability",
       title = "Chinook Spawning")
chnk_spw_sum_p

# Steelhead juvenile summer rearing
sthd_juv_sum_p = hsi_df %>%
  filter(species == "sthd",
         life_stage  == "juv",
         season == "sum") %>%
  ggplot(aes(x = geo_reach, y = HHS)) +
  geom_bar(fill = "cornflowerblue", stat = "identity") +
  theme_bw() +
  labs(x = "Geomorphic Reach",
       y = "Hydraulic Habitat Suitability",
       title = "Steelhead Juvenile Summer Rearing")
sthd_juv_sum_p

# Steelhead juvenile winter rearing
sthd_juv_win_p = hsi_df %>%
  filter(species == "sthd",
         life_stage  == "juv",
         season == "win") %>%
  ggplot(aes(x = geo_reach, y = HHS)) +
  geom_bar(fill = "cornflowerblue", stat = "identity") +
  theme_bw() +
  labs(x = "Geomorphic Reach",
       y = "Hydraulic Habitat Suitability",
       title = "Steelhead Juvenile Winter Rearing")
sthd_juv_win_p

# Steelhead summer spawning
sthd_spw_win_p = hsi_df %>%
  filter(species == "sthd",
         life_stage  == "spw",
         season == "win") %>%
  ggplot(aes(x = geo_reach, y = HHS)) +
  geom_bar(fill = "cornflowerblue", stat = "identity") +
  theme_bw() +
  labs(x = "Geomorphic Reach",
       y = "Hydraulic Habitat Suitability",
       title = "Steelhead Spawning")
sthd_spw_win_p

#################################################
# 1 HHS plot for each species, 3 scenarios each #
#################################################
chnk_p = hsi_df %>%
  filter(species == "chnk") %>%
  mutate(Scenario = paste0(life_stage, "_", season)) %>%
  mutate(Scenario = recode(Scenario,
                           `juv_sum` = "Juvenile Summer Rearing",
                           `juv_win` = "Juvenile Winter Rearing",
                           `spw_sum` = "Summer Spawning")) %>%
  ggplot(aes(x = geo_reach, y = HHS, fill = Scenario)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_bw() +
  scale_y_continuous(limits = c(0,1),
                     breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
  #ylim(0, 1) +
  labs(x = "Geomorphic Reach",
       y = "Hydraulic Habitat Suitability",
       title = "Chinook salmon") +
  theme(axis.text.x = element_text(angle = -45, vjust = 0),
        plot.title = element_text(hjust = 0.025, vjust = -8))
chnk_p
ggsave("output/figures/chnk_hhs.pdf", chnk_p)

sthd_p = hsi_df %>%
  filter(species == "sthd") %>%
  mutate(Scenario = paste0(life_stage, "_", season)) %>%
  mutate(Scenario = recode(Scenario,
                           `juv_sum` = "Juvenile Summer Rearing",
                           `juv_win` = "Juvenile Winter Rearing",
                           `spw_win` = "Winter Spawning")) %>%
  ggplot(aes(x = geo_reach, y = HHS, fill = Scenario)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_bw() +
  scale_y_continuous(limits = c(0,1),
                     breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
  #ylim(0, 1) +
  labs(x = "Geomorphic Reach",
       y = "Hydraulic Habitat Suitability",
       title = "Steelhead") +
  theme(axis.text.x = element_text(angle = -45, vjust = 0),
        plot.title = element_text(hjust = 0.025, vjust = -8))
sthd_p
ggsave("output/figures/sthd_hhs.pdf", sthd_p)

################
# Both species #
################
species_p = hsi_df %>%
  mutate(Scenario = paste0(life_stage, "_", season)) %>%
  mutate(Scenario = recode(Scenario,
                           `juv_sum` = "Juvenile Summer Rearing",
                           `juv_win` = "Juvenile Winter Rearing",
                           `spw_sum` = "Spawning",
                           `spw_win` = "Spawning")) %>%
  mutate(species = recode(species,
                          `chnk` = "Chinook",
                          `sthd` = "Steelhead")) %>%
  ggplot(aes(x = geo_reach, y = HHS, fill = Scenario)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_bw() +
  scale_y_continuous(limits = c(0,1),
                     breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
  labs(x = "Geomorphic Reach",
       y = "Hydraulic Habitat Suitability") +
  theme(axis.text.x = element_text(angle = -45, vjust = 0)) +
  facet_grid(species ~ .)
species_p
ggsave("output/figures/both_species_hhs.pdf", species_p)

#############################
# All scenarios as 6 facets #
#############################
all_scenario_p = hsi_df %>%
  mutate(Scenario = paste0(life_stage, "_", season)) %>%
  mutate(Scenario = recode(Scenario,
                           `juv_sum` = "Juvenile Summer Rearing",
                           `juv_win` = "Juvenile Winter Rearing",
                           `spw_sum` = "Spawning",
                           `spw_win` = "Spawning")) %>%
  mutate(species = recode(species,
                          `chnk` = "Chinook",
                          `sthd` = "Steelhead")) %>%
  ggplot(aes(x = geo_reach, y = HHS)) +
  geom_bar(fill = "cornflowerblue", stat = "identity") +
  theme_bw() +
  scale_y_continuous(limits = c(0,1),
                     breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
  #ylim(0, 1) +
  labs(x = "Geomorphic Reach",
       y = "Hydraulic Habitat Suitability") +
  theme(axis.text.x = element_text(angle = -45, vjust = 0, size = 6)) +
  facet_grid(species ~ Scenario)
all_scenario_p
ggsave("output/figures/all_scenarios_hhs.pdf", all_scenario_p)

###########################
# Plot geomorph summaries #
###########################
# geomorphic tiers
tier_p = tier_summary %>%
  ggplot(aes(x = Name, y = p_Geo_Tier, fill = Geo_Tier)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(x = "Geomorphic Reach",
       y = "p(Geomorphic Tier)",
       fill = "Geomorphic Tier") +
  theme(axis.text.x = element_text(angle = -45, vjust = 0))
tier_p
ggsave("output/figures/geomorphic_tiers.pdf", tier_p)

# channel units; Rob and Sam say this data is junk
# cu_p = cu_summary %>%
#   ggplot(aes(x = Name, y = p_ChanUnit, fill = ChanUnits)) +
#   geom_bar(stat = "identity") +
#   theme_bw() +
#   labs(x = "Geomorphic Reach",
#        y = "p(Channel Unit)",
#        fill = "Channel Unit") +
#   theme(axis.text.x = element_text(angle = -45, vjust = 0))
# cu_p
#ggsave("output/figures/channel_units.pdf", cu_p)

# geomorph and cu
# geo_p = tier_summary %>%
#   mutate(Category = "Geomorphic Tiers") %>%
#   rename(ChanUnits = Geo_Tier,
#          p_ChanUnit = p_Geo_Tier) %>%
#   rbind(cu_summary %>%  mutate(Category = "Channel Units")) %>%
#   rename(Unit = ChanUnits,
#          p = p_ChanUnit) %>%
#   ggplot(aes(x = Name, y = p, fill = Unit)) +
#   geom_bar(stat = "identity") +
#   theme_bw() +
#   labs(x = "Geomorphic Reach",
#        y = "p(Unit)") +
#   theme(axis.text.x = element_text(angle = -45, vjust = 0)) +
#   facet_grid(Category ~ .)
# geo_p
#ggsave("output/figures/geomorph_summaries.pdf", geo_p)

#############################################################
# Plot species and geomorph summaries together; easy method #
#############################################################
grid.arrange(species_p +
               theme(axis.title.x = element_blank()),
                     #axis.text.x = element_blank(),
                     #axis.ticks.x = element_blank()),
             tier_p,
             ncol = 1)

g = arrangeGrob(species_p  +
                  theme(axis.title.x = element_blank()),
                        # axis.text.x = element_blank(),
                        # axis.ticks.x = element_blank()),
                tier_p,
                ncol = 1)
ggsave("output/figures/species_geomorph_summaries.pdf", g)

# BELOW IS MIKE ATTEMPTING TO MAKE THE ABOVE LOOK NICER. SPOILER ALERT: IT DIDN'T WORK
# # try 1
# library(grid)
# g.species = ggplotGrob(species_p)
# g.tier    = ggplotGrob(tier_p)
# species.widths = g.species$widths[1:3]
# tier.widths = g.tier$widths[1:3]
# max.widths = unit.pmax(species.widths, tier.widths)
# g.species$widths[1:3] = max.widths
# g.tier$widths[1:3] = max.widths
# plot_grid(g.species, g.tier, labels = "AUTO", 
#           ncol = 1,
#           rel_widths = c(2,1),
#           rel_heights = c(1.4, 1))
# 
# # try 2
# p1 = species_p + theme(axis.title.x = element_blank(), axis.text.x = element_blank())
# plot_grid(p1, tier_p, ncol = 1,
#           labels = "AUTO",
#           align = "v",
#           rel_widths = c(3, 1),
#           rel_heights = c(1.4, 1))
# 
# # try 3
# chnk.p = chnk_p + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), title = element_blank())
# sthd.p = sthd_p + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), title = element_blank())
# plot_grid(chnk.p, sthd.p, tier_p,
#           ncol = 1,
#           align = "v",
#           rel_heights = c(1, 1, 1.4))

###########################
# Map HHS values on a map #
###########################
# for a single scenario
chnk_juv_sum_map <- st_read("data/geomorph/geo_reaches/Lem_Poly_Label.shp") %>%
  mutate(Name = paste0("GR_", str_pad(sub(".*_", "", Name), 2, pad = "0"))) %>%
  left_join(hsi_df %>%
              filter(species == "chnk",
                     life_stage == "juv",
                     season == "sum"),
            by = c("Name" = "geo_reach")) %>%
  dplyr::select(Name, HHS) %>%
  ggplot(aes(fill = HHS)) +
  geom_sf() +
  theme_bw()
chnk_juv_sum_map

# map all scenarios
all_scenarios_map = hsi_df %>%
  left_join(st_read("data/geomorph/Lem_Poly_Label.shp") %>%
              mutate(Name = paste0("GR_", str_pad(sub(".*_", "", Name), 2, pad = "0"))),
            by = c("geo_reach" = "Name")) %>%
  mutate(Scenario = paste0(life_stage, "_", season)) %>%
  mutate(Scenario = recode(Scenario,
                           `juv_sum` = "Juvenile Summer Rearing",
                           `juv_win` = "Juvenile Winter Rearing",
                           `spw_sum` = "Spawning",
                           `spw_win` = "Spawning")) %>%
  mutate(species = recode(species,
                          `chnk` = "Chinook",
                          `sthd` = "Steelhead")) %>%
  st_as_sf() %>%
  ggplot(aes(fill = HHS)) +
  geom_sf() +
  scale_fill_viridis_c(option = "magma") +
  #geom_sf_label(aes(label = geo_reach)) +
  theme_bw() +
  facet_grid(species ~ Scenario)
all_scenarios_map
ggsave("output/figures/hhs_map.pdf", all_scenarios_map)

###########################
# save all plots and maps #
###########################
plot_list = lapply(ls(pattern = "\\_p$"), get)
save(plot_list, file = "output/hsi_plots.RData")

map_list = lapply(ls(pattern = "\\_map$"), get)
save(map_list, file = "output/hsi_maps.RData")

# End hsi_mets_summary.R
