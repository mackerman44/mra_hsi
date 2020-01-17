##Code for calculating WUA and HHS for Depth and Velocity rasters##
library(tidyverse)
library(maptools)
library(raster)
library(sf)
library(sp)
library(tidyr)
library(rgdal)
library(lwgeom)
library(dplyr)

setwd("F:/LemhiHydraulicScenarios")
outdir <- "F:/LemhiHydraulicScenarios/HSI/Chinook/Juvenile"

##Read in depth and velocity rasters##
d_rast <- raster("D_Aug_All.tif")
v_rast <- raster("V_Aug_All.tif")
# summer_d <- raster("D_Aug_All.tif")
# summer_v <- raster("V_Aug_All.tif")
# winter_d <- raster("d_jan_v2")
# winter_v <- raster("v_jan_v2")

life_stage <- "juvenile" ##Either "juvenile" or "spawning" or "adult"
species <- "chinook" ##Either "chinook" or "steelhead"
watershed <- "Lemhi"

##short names
ls <- "sp" ##Either "sp" "juv" "ad"
spec <- "stl" ##Either "stl" or "chn"
watsh <- "lem"
d <- "_d.tif"
v <- "_v.tif"
name <- paste(spec, ls, watsh, sep = "_")

##Read in reach polygons.Create one object for each reach##
reaches <- st_read("F:/UpperSalmon_Pahsimeroi_Lemhi_BOR/GeoReaches/Lem_Poly_Label.shp") 

gr_6 <- filter(reaches, Name == "GR-6")
gr_7 <- filter(reaches, Name == "GR-7")
gr_8 <- filter(reaches, Name == "GR-8")
gr_9 <- filter(reaches, Name == "GR-9")
gr_10 <- filter(reaches, Name == "GR-10")
gr_11 <- filter(reaches, Name == "GR-11")
gr_12 <- filter(reaches, Name == "GR-12")
gr_13 <- filter(reaches, Name == "GR-13")
gr_14 <- filter(reaches, Name == "GR-14")
gr_15 <- filter(reaches, Name == "GR-15")
gr_16 <- filter(reaches, Name == "GR-16")
gr_17 <- filter(reaches, Name == "GR-17")
gr_18 <- filter(reaches, Name == "GR-18")
gr_19 <- filter(reaches, Name == "GR-19")
gr_20 <- filter(reaches, Name == "GR-20")
gr_21 <- filter(reaches, Name == "GR-21")

###Split rasters into reaches for processing, if your raster/river reach is too large##
# reach_1 <- st_read("F:/LemhiHydraulicScenarios/HSI/Reach_Mask/reach_1.shp")
# reach_2 <- st_read("F:/LemhiHydraulicScenarios/HSI/Reach_Mask/reach_2.shp")
# reach_3 <- st_read("F:/LemhiHydraulicScenarios/HSI/Reach_Mask/reach_3.shp")
# 
# 
# summer_d_r1 <- crop(summer_d, reach_1)
# summer_d_r2 <- crop(summer_d, reach_2)
# summer_d_r3 <- crop(summer_d, reach_3)
# 
# 
# summer_v_r1 <- crop(summer_v, reach_1)
# summer_v_r2 <- crop(summer_v, reach_2)
# summer_v_r3 <- crop(summer_v, reach_3)
# ##Read in centerline##
# 
# LemhiCenterline <- st_read("LemhiCenterline.shp") %>%
#   st_zm(drop = TRUE, what = "ZM") %>%
#   st_transform(crs = crs(summer_comp)) 
# ##Convert centerlines to spatial dataframe 'sp' object##
# 
# sp_centerline <- as(centerline, "Spatial") 
# 
# 
# ##Calculate total length of centerlines for number of points to construct##
# numOfPoints <- (round(as.numeric(sum(st_length(LemhiCenterline))))/1000)
# 
# ##Add points along centerlines and reproject to wgs84##
# cl_points_1000m <- spsample(sp_centerline, 
#                          n = numOfPoints, 
#                          type = "regular") %>%
#   st_as_sf()
# st_write(cl_points_1000m, "points_1000m.shp")
# 
# ##split the line at the points##
# 
# split_cl <- st_split(LemhiCenterline, cl_points_1000m)
# 
# plot(centerline)
# plot(st_geometry(cl_points_100m), add = TRUE)
# 
# plot(split_cl)
# st_write("split_cl.shp")
# 
# site_snap <- st_snap(cl_points_1000m, LemhiCenterline, tolerance = 0.0001)
# split_cl <- st_collection_extract(st_split(centerline$geometry, site_snap$geometry), "LINESTRING")
# 
# st_write(split_cl, "split_cl_1000m.shp", overwrite = TRUE)
# 
# 
# cl_split_1000m 

#################Read in all HSI curves##




                            

                            
##Functions for calculating 
##Chinook HSI 
##(maret curve)##

##juvenle depth
chnk_juv_d <- function(x) {
  y = if_else(x <= 0.061,
              0,
              if_else(x > 0.061 & x <= 0.1219,
                      0.8202 * x -0.03,
                      if_else(x > 0.1219 & x <= 0.1829,
                              1.4764 * x -0.11,
                              if_else(x > 0.1829 & x <= 0.2438,
                                      1.6404 * x -0.14,
                                      if_else(x > 0.2438 & x <= 0.3048, 
                                              1.8045 * x -0.18,
                                              if_else(x > 0.3048 & x <= 0.3658, 
                                                      1.8045 * x -0.18, 
                                                      if_else(x > 0.3658 & x <= 0.4267, 
                                                              1.8045 * x -0.18, 
                                                              if_else(x > 0.4267 & x <= 0.4877, 
                                                                      1.4764 * x -0.04,
                                                                      if_else(x > 0.4877 & x <= 0.5486, 
                                                                              1.3123 * x + 0.04, 
                                                                              if_else(x > 0.5486 & x <= 0.6096,
                                                                                      3.937 * x - 1.4,
                                                                                      if_else(x > 0.6096,
                                                                                              1,
                                                                                              as.numeric(NA))))))))))))
  return(y)
}

##juvenile velocity
chnk_juv_v <- function(x) {
  y = if_else(x >= 0 & x <= 0.0305,
              2.2966 * x + 0.88,
              if_else(x > 0.0305 & x <= 0.0610,
                      1.3123 * x + 0.91,
              if_else(x > 0.0610 & x <= 0.0914,
                      0.3281 * x + 0.97,
                      if_else(x > 0.0914 & x <= 0.1219,
                              -0.3281 * x + 1.03,
                              if_else(x > 0.1219 & x <= 0.1524,
                                      -1.3123 * x + 1.15,
                                      if_else(x > 0.1524 & x <= 0.3962,
                                               -2.9364 * x + 1.4205,
                                                if_else(x > 0.3962 & x <= 0.4572,
                                                        -1.8045 * x + 0.9733,
                                                        if_else( x > 0.4572 & x <= 0.5182,
                                                                -1.3123 * x + 0.75,
                                                                if_else(x > 0.5182 & x <= 0.5791,
                                                                       -0.6562 * x + 0.41,
                                                                        if_else(x > 0.5791 & x <= 0.6401,
                                                                                -0.3281 * x + 0.22,
                                                                                if_else(x > 0.6401 & x <= 0.6706,
                                                                                        0.01, 
                                                                                        if_else(x > 0.6706 & x <= 0.7010,
                                                                                                -0.3281 * x + 0.23,
                                                                                                if_else(x > 0.7010, 
                                                                                                0,
                                                                                as.numeric(NA))))))))))))))
  return(y)
}

##spawning depth
chnk_spwn_d <- function(x) {
  y = if_else( x < 0.06096,
               0,
               if_else(x > 0.06096 & x <= 0.09144,
                       6.5617 * x - 0.4,
                       if_else(x > 0.09144 & x <= 0.17618,
                               2.3435 * x - 0.0143,
                               if_else(x > 0.17618 & x <= 0.24384,
                                       5.9652 * x - 0.6546,
                                       if_else(x > 0.24384 & x <= 0.28956,
                                               4.3745 * x - 0.2667,
                                               if_else(x > 0.28956,
                                                       1,
                                                       
                                       as.numeric(NA)))))))
  return(y)
}

##spawning velocity 
chnk_spwn_v <- function(x) {
  y = if_else(x <= 0.15240,
              0,
              if_else(x > 0.15240 & x <= 0.3048,
                      6.5617 * x - 1,
                      if_else(x > 0.3048 & x <= 0.9144,
                              1,
                              if_else(x > 0.9144 & x <= 1.2192,
                                      -3.2808 * x + 4,
                                      if_else(x > 1.2192,
                                              0,
                              as.numeric(NA))))))
  return(y)
}


##Functions for calculating 
##steelhead HSI 
##(maret curve)##

##juvenile depth
stlh_juv_d <- function(x) {
  y = if_else( x >= 0 & x <= 0.0914,
               2.1872 * x,
               if_else(x > 0.0914 & x <= 0.1829,
                       4.9213 * x - 0.25,
                       if_else(x > 0.1829 & x <= 0.3048,
                               2.5427 * x + 0.185,
                               if_else(x > 0.3048 & x <= 0.3658,
                                       0.6562 * x + 0.76,
                                       if_else(x > 0.3658,
                                               1,
                                               as.numeric(NA))))))
  return(y)
}

##Juvenile velocity
stlh_juv_v <- function(x) {
  y = if_else( x >= 0 & x <= 0.061,
               2.4606 * x,
               if_else(x > 0.061 & x <= 0.0914,
                       24.606 * x - 1.35,
                       if_else(x > 0.0914 & x <= 0.1524,
                               1.1483 * x + 0.795,
                               if_else(x > 0.1524 & x <= 0.2134,
                                       0.4921 * x + 0.895,
                                       if_else(x > 0.2134 & x <= 0.3658,
                                               1,
                                               if_else(x > 0.3658 & x <= 0.6096,
                                                       -4.101 * x + 2.5,
                                                       if_else(x > 0.6096,
                                                               1,
                                                       as.numeric(NA))))))))
  return(y)
}

##spawning depth
stlh_spwn_d <- function(x) {
  y = if_else(x <= 0.061,
               0,
               if_else(x > 0.061 & x <= 0.0914,
                       6.5617 * x - 0.4,
                       if_else(x > 0.0914 & x <= 0.1768,
                               2.3435 * x + -0.0143,
                               if_else(x > 0.1768 & x <= 0.2438,
                                       5.9652 * x - 0.6545,
                                       if_else(x > 0.2438 & x <= 0.2896,
                                               4.3745 * x - 0.2667,
                                               if_else(x > 0.2896,
                                                       1,
                                                       as.numeric(NA)))))))
  return(y)
}

##spawning velocity
stlh_spwn_v <- function(x) {
  y = if_else( x <= 0.1524,
               0,
               if_else(x > 0.1524 & x <= 0.3048,
                       6.5617 * x - 1,
                       if_else(x > 03048 & x <= 0.9144,
                               1,
                               if_else(x > 0.9144 & x <= 1.2192,
                                       -3.2808 * x + 4,
                                       if_else(x < 1.2192,
                                               0,
                                               as.numeric(NA))))))
  return(y)
}

##Function for geometric mean composite of depth and velocity##
comp_hsi <- function(r1, r2) 
  {y = (sqrt(r1*r2))
  return(y)
}


##Calculate suitability insert appropriate fuction##

d_suit <- calc(d_rast, chnk_juv_d)

v_suit <- calc(v_rast, chnk_juv_v) 


##Write rasters if desired##
writeRaster(d_suit, paste(outdir, "d_suit.tif", sep = "/"))

writeRaster(v_suit, paste(outdir, "v_suit.tif", sep = "/"))



##Calculate geometric mean and create a new raster##
comp_suit <- overlay(d_suit, 
                     v_suit, 
                     fun = comp_hsi)


writeRaster(comp_suit, paste(outdir, "comp_suit.tif", sep = "/"))

##Applying Chinook juvenile function to depth raster##
# chnk_juv_d_calc <- calc(d_rast, chnk_juv_d)



####Calculate metrics at reach scale wetted area, WUA, HHS####

##Extract raster values at polygon 'reaches'from each object created
##from your reaches polygon##
hsi_extract_gr6 <- raster::extract(comp_suit, 
                               gr_6, fun = NULL,
                               df = TRUE,
                               na.rm = TRUE) %>%
  drop_na() 


hsi_extract_gr7 <- raster::extract(comp_suit, 
                                   gr_7, fun = NULL,
                                   df = TRUE,
                                   na.rm = TRUE) %>%
  drop_na()


hsi_extract_gr8 <- raster::extract(comp_suit, 
                                   gr_8, fun = NULL,
                                   df = TRUE,
                                   na.rm = TRUE) %>%
  drop_na() 


hsi_extract_gr9 <- raster::extract(comp_suit, 
                                   gr_9, fun = NULL,
                                   df = TRUE,
                                   na.rm = TRUE) %>%
  drop_na() 


hsi_extract_gr10 <- raster::extract(comp_suit, 
                                   gr_10, fun = NULL,
                                   df = TRUE,
                                   na.rm = TRUE) %>%
  drop_na() 


hsi_extract_gr11 <- raster::extract(comp_suit, 
                                   gr_11, fun = NULL,
                                   df = TRUE,
                                   na.rm = TRUE) %>%
  drop_na() 
hsi_extract_gr12 <- raster::extract(comp_suit, 
                                   gr_12, fun = NULL,
                                   df = TRUE,
                                   na.rm = TRUE) %>%
  drop_na() 
hsi_extract_gr13 <- raster::extract(comp_suit, 
                                   gr_13, fun = NULL,
                                   df = TRUE,
                                   na.rm = TRUE) %>%
  drop_na() 
hsi_extract_gr14 <- raster::extract(comp_suit, 
                                   gr_14, fun = NULL,
                                   df = TRUE,
                                   na.rm = TRUE) %>%
  drop_na() 
hsi_extract_gr15 <- raster::extract(comp_suit, 
                                   gr_15, fun = NULL,
                                   df = TRUE,
                                   na.rm = TRUE) %>%
  drop_na() 
hsi_extract_gr16 <- raster::extract(comp_suit, 
                                   gr_16, fun = NULL,
                                   df = TRUE,
                                   na.rm = TRUE) %>%
  drop_na() 
hsi_extract_gr17 <- raster::extract(comp_suit, 
                                   gr_17, fun = NULL,
                                   df = TRUE,
                                   na.rm = TRUE) %>%
  drop_na() 
hsi_extract_gr18 <- raster::extract(comp_suit, 
                                   gr_18, fun = NULL,
                                   df = TRUE,
                                   na.rm = TRUE) %>%
  drop_na() 
hsi_extract_gr19 <- raster::extract(comp_suit, 
                                   gr_19, fun = NULL,
                                   df = TRUE,
                                   na.rm = TRUE) %>%
  drop_na() 
hsi_extract_gr20 <- raster::extract(comp_suit, 
                                   gr_20, fun = NULL,
                                   df = TRUE,
                                   na.rm = TRUE) %>%
  drop_na() 
hsi_extract_gr21 <- raster::extract(comp_suit, 
                                   gr_21, fun = NULL,
                                   df = TRUE,
                                   na.rm = TRUE) %>%
  drop_na() 

##Renaming the ID column to match the reach polygon name##
hsi_extract_gr6 <- hsi_extract_gr6 %>%
  mutate(ID = replace(ID, ID == 1, "GR_6")) 
hsi_extract_gr7 <- hsi_extract_gr7 %>%
  mutate(ID = replace(ID, ID == 1, "GR_7"))
hsi_extract_gr8 <- hsi_extract_gr8 %>%
  mutate(ID = replace(ID, ID == 1, "GR_8"))
hsi_extract_gr9 <- hsi_extract_gr9 %>%
  mutate(ID = replace(ID, ID == 1, "GR_9"))
hsi_extract_gr10 <- hsi_extract_gr10 %>%
  mutate(ID = replace(ID, ID == 1, "GR_10"))
hsi_extract_gr11 <- hsi_extract_gr11 %>%
  mutate(ID = replace(ID, ID == 1, "GR_11"))
hsi_extract_gr12 <- hsi_extract_gr12 %>%
  mutate(ID = replace(ID, ID == 1, "GR_12"))
hsi_extract_gr13 <- hsi_extract_gr13 %>%
  mutate(ID = replace(ID, ID == 1, "GR_13"))
hsi_extract_gr14 <- hsi_extract_gr14 %>%
  mutate(ID = replace(ID, ID == 1, "GR_14"))
hsi_extract_gr15 <- hsi_extract_gr15 %>%
  mutate(ID = replace(ID, ID == 1, "GR_15"))
hsi_extract_gr16 <- hsi_extract_gr16 %>%
  mutate(ID = replace(ID, ID == 1, "GR_16"))
hsi_extract_gr17 <- hsi_extract_gr17 %>%
  mutate(ID = replace(ID, ID == 1, "GR_17"))
hsi_extract_gr18 <- hsi_extract_gr18 %>%
  mutate(ID = replace(ID, ID == 1, "GR_18"))
hsi_extract_gr19 <- hsi_extract_gr19 %>%
  mutate(ID = replace(ID, ID == 1, "GR_19"))
hsi_extract_gr20 <- hsi_extract_gr20 %>%
  mutate(ID = replace(ID, ID == 1, "GR_20"))
hsi_extract_gr21 <- hsi_extract_gr21 %>%
  mutate(ID = replace(ID, ID == 1, "GR_21"))

##Merge reach HSI's back together##

hsi_merge <- hsi_extract_gr6 %>%
  rbind(hsi_extract_gr7) %>%
  rbind(hsi_extract_gr8) %>%
  rbind(hsi_extract_gr9) %>%
  rbind(hsi_extract_gr10) %>%
  rbind(hsi_extract_gr11) %>%
  rbind(hsi_extract_gr12) %>%
  rbind(hsi_extract_gr13) %>%
  rbind(hsi_extract_gr14) %>%
  rbind(hsi_extract_gr15) %>%
  rbind(hsi_extract_gr16) %>%
  rbind(hsi_extract_gr17) %>%
  rbind(hsi_extract_gr18) %>%
  rbind(hsi_extract_gr19) %>%
  rbind(hsi_extract_gr20) %>%
  rbind(hsi_extract_gr21) 

names(hsi_merge) <- c("ID", "layer")


#Calculating HSI metrics##
# for lem the pixels are 1m x 1m = 1sq meter
# for pahs, us the pixels are 3' x 3' = 1sq yard

cell_size = res[1] * res[2]

hsi_mets <- hsi_merge %>%
  mutate(Cell_Area = 1) %>%
  group_by(ID) %>%
  summarise(Reach_Area = sum(Cell_Area),
            WUA = sum(layer),
            HHS = WUA/Reach_Area)



write.csv(hsi_mets, paste(outdir, "Lem_Juv_Chnk.csv", sep = "/"))



# ##Create plots for Wetted Area, Weighted Usable Area, Hydraulic Habitat Suitability##
# 
# scaleFactor <- max(hsi_mets$WUA/max(hsi_mets$HHS))
# 
# ggplot(hsi_mets, aes(x=ID)) +
#   geom_smooth(aes(y=WUA),  col="blue") +
#   geom_smooth(aes(y=HHS * scaleFactor),  col="red") +
#   scale_y_continuous(name="Weighted Usable Area (m2)", sec.axis=sec_axis(~./scaleFactor, name="Hydraulic Habitat Suitability (WUA/Area)")) +
#   theme(
#     axis.title.y.left=element_text(color="blue"),
#     axis.text.y.left=element_text(color="blue"),
#     axis.title.y.right=element_text(color="red"),
#     axis.text.y.right=element_text(color="red")) 


##Read in and merge all CSVs and reach designations##




