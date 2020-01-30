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
  dplyr::select(-ID)

# just grab the upper salmon results for some testing (lemhi results are still wrong)

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
       y = "Hydraulic Habitat Suitability",
       title = "Chinook Juvenile Summer Rearing")
us_p

# violin plot
us_p = us_cs %>% 
  ggplot(aes(x = geo_reach, y = value)) +
  geom_violin(fill = "cornflowerblue") +
  theme_bw() +
  labs(x = "Geomorphic Reach",
       y = "Hydraulic Habitat Suitability",
       title = "Chinook Juvenile Summer Rearing")
us_p

# next, add faceted histogram

