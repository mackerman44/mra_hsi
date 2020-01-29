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
library(gridExtra)
library(ggmap)
library(sf)

## 

test = load("output/hsi_raw/lemh_chnk_juv_sum_hsi_values.RData")

## read in all of the HSI results
hsi_outputs = list.files(path = "output/hsi_csvs/", pattern = "*.csv", full.names = T)
hsi_df = sapply(hsi_outputs, read_csv, simplify = F) %>%
  bind_rows(.id = "id") %>%
  dplyr::select(-c(id, X1)) %>%
  mutate(geo_reach = paste0("GR_", str_pad(sub(".*_", "", ID), 2, pad = "0"))) %>%
  dplyr::select(-ID) %>%
  dplyr::select(species, life_stage, season, geo_reach, everything())

## read in the geomorph summary
load("output/geomorph/geomorph_summary.RData")
