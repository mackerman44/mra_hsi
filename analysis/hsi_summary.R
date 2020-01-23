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

## read in all of the HSI results
hsi_outputs = list.files(path = "output/hsi/", pattern = "*.csv", full.names = T)
hsi_df = sapply(hsi_outputs, read_csv, simplify = F) %>%
  bind_rows(.id = "id") %>%
  select(-c(id, X1)) %>%
  mutate(geo_reach = paste0("GR_", str_pad(sub(".*_", "", ID), 2, pad = "0"))) %>%
  select(-ID) %>%
  select(species, life_stage, season, geo_reach, everything())

## read in the geomorph summary
load("output/geomorph/geomorph_summary.RData")

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
       y = "Habitat Suitability",
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
       y = "Habitat Suitability",
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
       y = "Habitat Suitability",
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
       y = "Habitat Suitability",
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
       y = "Habitat Suitability",
       title = "Steelhead Juvenile Winter Rearing")
sthd_juv_win_p

# Steelhead summer spawning
sthd_spw_sum_p = hsi_df %>%
  filter(species == "sthd",
         life_stage  == "spw",
         season == "sum") %>%
  ggplot(aes(x = geo_reach, y = HHS)) +
  geom_bar(fill = "cornflowerblue", stat = "identity") +
  theme_bw() +
  labs(x = "Geomorphic Reach",
       y = "Habitat Suitability",
       title = "Steelhead Spawning")
sthd_spw_sum_p

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
       y = "Habitat Suitability",
       title = "Chinook salmon") +
  theme(axis.text.x = element_text(angle = -45, vjust = 0))
chnk_p

sthd_p = hsi_df %>%
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
       y = "Habitat Suitability",
       title = "Steelhead") +
  theme(axis.text.x = element_text(angle = -45, vjust = 0))
sthd_p

################
# Both species #
################
species_p = hsi_df %>%
  mutate(Scenario = paste0(life_stage, "_", season)) %>%
  mutate(Scenario = recode(Scenario,
                           `juv_sum` = "Juvenile Summer Rearing",
                           `juv_win` = "Juvenile Winter Rearing",
                           `spw_sum` = "Summer Spawning")) %>%
  mutate(species = recode(species,
                          `chnk` = "Chinook",
                          `sthd` = "Steelhead")) %>%
  ggplot(aes(x = geo_reach, y = HHS, fill = Scenario)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_bw() +
  scale_y_continuous(limits = c(0,1),
                     breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
  #ylim(0, 1) +
  labs(x = "Geomorphic Reach",
       y = "Habitat Suitability") +
  theme(axis.text.x = element_text(angle = -45, vjust = 0)) +
  facet_grid(species ~ .)
species_p

#############################
# All scenarios as 6 facets #
#############################
all_scenario_p = hsi_df %>%
  mutate(Scenario = paste0(life_stage, "_", season)) %>%
  mutate(Scenario = recode(Scenario,
                           `juv_sum` = "Juvenile Summer Rearing",
                           `juv_win` = "Juvenile Winter Rearing",
                           `spw_sum` = "Summer Spawning")) %>%
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
       y = "Habitat Suitability") +
  theme(axis.text.x = element_text(angle = -45, vjust = 0, size = 6)) +
  facet_grid(species ~ Scenario)
all_scenario_p

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

# channel units
cu_p = cu_summary %>%
  ggplot(aes(x = Name, y = p_ChanUnit, fill = ChanUnits)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(x = "Geomorphic Reach",
       y = "p(Channel Unit)",
       fill = "Channel Unit") +
  theme(axis.text.x = element_text(angle = -45, vjust = 0))
cu_p

# geomorph and cu
geo_p = tier_summary %>%
  mutate(Category = "Geomorphic Tiers") %>%
  rename(ChanUnits = Geo_Tier,
         p_ChanUnit = p_Geo_Tier) %>%
  rbind(cu_summary %>%  mutate(Category = "Channel Units")) %>%
  rename(Unit = ChanUnits,
         p = p_ChanUnit) %>%
  ggplot(aes(x = Name, y = p, fill = Unit)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(x = "Geomorphic Reach",
       y = "p(Unit)") +
  theme(axis.text.x = element_text(angle = -45, vjust = 0)) +
  facet_grid(Category ~ .)
geo_p

    
    

  

