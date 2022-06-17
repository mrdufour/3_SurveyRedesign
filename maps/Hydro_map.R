####  Demonstration of mapping in R, using the sf and tmap packages   #####################
#     Nov 9 2018 - ODNR Sandusky - Zak Slagle
#
#     Most of this is based on the online book Geocomputation in R (Lovelace, Nowosad, and Muenchow 2017)
#       available: https://geocompr.robinlovelace.net/
#
#     This code demonstrates map building using the R packages sf() and tmap(), which allow users to import and plot 
#     shapefiles and user waypoints, project map features into different projections, and perform spatial operations.
#     Users can also create publication-quality maps that include scale bars and inset maps.
#
###############################################


#install packages if necessary:
#if (!require('tidyverse')) install.packages('tidyverse'); require('tidyverse') #base R sux
#if (!require('sf')) install.packages('sf'); require('sf')# Simple Features package
#if (!require('tmap')) install.packages('tmap'); require('tmap') # Thematic Maps plotting package 
#if (!require('grid')) install.packages('grid'); require('grid') # required for printing inset map on active graphics device
#require(readxl)

## load pachages
library(readr)
library(ggplot2)
library(dplyr)
library(magrittr)

library(sf)     #simple features - REMINDER - this loads library(raster), which masks dplyr::select!!!!!
library(measurements)
require(RColorBrewer)
require(gganimate)
library(ggsn)
library(magrittr) # fun pipes
library(mapview)  # easy interactive maps
library(readxl)
library(tidyverse) #obv
library(here)

#######################################################
## bring in data and do some subsetting
#######################################################
us_and_can = st_read("maps/Shapefiles/ne_10m_admin_1_states_provinces.shp") %>% filter(gu_a3 == "CAN"|gu_a3 == "USA") #filter out US and CAN only
great_lakes <- st_read("maps/Shapefiles/great lakes poly.shp") %>% st_set_crs(4326)
mus <- st_read("maps/Shapefiles/international_border_and_state_dd2.shp") %>% st_set_crs(4326)
int_boundary_main <- st_read("maps/Shapefiles/ne_10m_admin_0_boundary_lines_land.shp") # this works for LSC and Huron, but is off of MU lines
int_boundary_le <- st_read("maps/Shapefiles/international_border_p.shp") %>% st_set_crs(32617) #matches MU line
lake_erie <- st_read("maps/Shapefiles/Lake_Erie_Shoreline.shp") 
depth_contour = st_read("maps/Shapefiles/NOAA_1m_bathy_dd.shp")
five_min_grid <- st_read("maps/Shapefiles/5 minute grids.shp") %>% st_set_crs(4326)
LE_juris = st_read("maps/Shapefiles/lake erie polygon jurisdictions.shp") %>%  st_set_crs(4326)

wb_strata <- st_read("maps/Shapefiles/WB_strata.shp") %>% st_set_crs(4326)
cb_strata <- st_read("maps/Shapefiles/CB_strata_2.shp") %>% st_set_crs(4326)
eb_strata <- st_read("maps/Shapefiles/le_ac_strata_geo.shp") %>% st_transform(4326)

########################################################







## bounding box
whole_lake_bound_box <- c(xmin = -83.6649, ymin = 41.3235, xmax = -78.7869, ymax = 43.017)

levels = c(wb_strata$strata, 
           cb_strata$strata,
           as.character(eb_strata$STRATUM))

#make manual levels so we can group "not sampled"
ggplot()+
  scale_x_continuous(limits = c(whole_lake_bound_box["xmin"], whole_lake_bound_box["xmax"]))+
  scale_y_continuous(limits = c(whole_lake_bound_box["ymin"], whole_lake_bound_box["ymax"]))+
  geom_sf(data = wb_strata, aes(fill = strata), col = "gray50", size = .5, show.legend = FALSE)+
  geom_sf(data = cb_strata, aes(fill = strata), col = "gray50", show.legend = FALSE)+
  geom_sf(data = eb_strata, aes(fill = STRATUM), col = "gray50", show.legend = FALSE)+
  geom_sf(data = five_min_grid, col = "white", fill = NA, lty = 2, size = .3)+
  
  #geom_sf(data = ten_min_grid_clipped, col = "white", fill = NA, lty = 2, size = .3)+
  geom_sf(data = lake_erie, fill = NA, col = "gray50")+
  scale_fill_manual(limits = levels, 
                    values = c( sample(viridis::viridis(n = 20, direction = -1))))+ # randomize using sample()
  guides(fill=guide_legend(ncol=2))+
  theme_minimal()+
  theme(axis.title = element_blank(), axis.text = element_blank(), panel.grid = element_line(color = "white"))

ggsave("maps/LE_hydro_survey.png", width = 8, height = 5, units = "in", dpi = 400)

