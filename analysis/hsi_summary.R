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
  select(species, life_stage, season, everything())

## read in the geomorph summary
load("output/geomorph/geomorph_summary.RData")

