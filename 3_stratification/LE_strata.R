# Thu Jun 16 16:45:26 2022 ------------------------------

## this script creates the east basin hydroacoustic strata

library(sf)     #simple features - REMINDER - this loads library(raster), which masks dplyr::select!!!!!
library(measurements)
library(magrittr) # fun pipes
library(readxl)
library(tidyverse) #obv
library(dplyr)
library(nngeo) # for st_remove_holes

#####  Read in shapefiles and data  ###############################
#read in shapefiles, many from http://www.naturalearthdata.com/downloads/
lake_erie <- st_read("maps/Shapefiles/Lake_Erie_Shoreline.shp") 
depth_contour = st_read("maps/Shapefiles/NOAA_1m_bathy_dd.shp")
five_min_grid <- st_read("maps/Shapefiles/5 minute grids.shp") %>% st_set_crs(4326)
LE_juris = st_read("maps/Shapefiles/lake erie polygon jurisdictions.shp") %>%  st_set_crs(4326)
wb_strata <- st_read("maps/Shapefiles/wb_strata.shp") %>% st_transform(4326)
cb_strata <- st_read("maps/Shapefiles/cb_strata.shp") %>% st_transform(4326)
eb_strata <- st_read("maps/Shapefiles/eb_strata.shp") %>% st_transform(4326)

LE_strata <- rbind(wb_strata,cb_strata,eb_strata)
LE_strata$ordered <- seq(1,dim(LE_strata)[1],by=1)



#bounding boxes   
LE_bound_box <- c(xmin = -83.6117, ymin = 41.3494, xmax = -78.85, ymax = 42.9)
LE_bounds <- LE_bound_box %>% st_bbox() %>% st_as_sfc() %>% st_set_crs(4326)

ggplot()+
  scale_x_continuous(limits = c(LE_bound_box["xmin"], LE_bound_box["xmax"]))+
  scale_y_continuous(limits = c(LE_bound_box["ymin"], LE_bound_box["ymax"]))+
  geom_sf(data = lake_erie, fill = NA, col = "grey30")+
  geom_sf(data = LE_strata, aes(fill = ordered), col = "black",  show.legend = "polygon") +
  #guides(fill=guide_legend("Ordered \nStratum")) +
  scale_fill_viridis_c()+
  theme_bw()



ggsave("3_stratification/LE_strata.png", width = 8, height = 5, units = "in", dpi = 400)


## save strata as shapefile
st_write(obj = LE_strata,
         dsn = "maps/Shapefiles/LE_strata.shp",
         append = FALSE)
