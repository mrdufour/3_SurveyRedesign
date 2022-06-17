# Thu Jun 16 16:12:24 2022 ------------------------------

## This script creates the west basin hydroacoustic strata

library(sf)     #simple features - REMINDER - this loads library(raster), which masks dplyr::select!!!!!
library(measurements)
library(magrittr) # fun pipes
library(readxl)
library(tidyverse) #obv
library(dplyr)

#####  Read in shapefiles and data  ###############################
#read in shapefiles, many from http://www.naturalearthdata.com/downloads/
lake_erie <- st_read("maps/Shapefiles/Lake_Erie_Shoreline.shp")  %>% st_set_crs(4326)
depth_contour = st_read("maps/Shapefiles/NOAA_1m_bathy_dd.shp")  %>% st_set_crs(4326)
#five_min_grid <- st_read("maps/Shapefiles/5 minute grids.shp") %>% st_set_crs(4326)
water_masses <- st_read("maps/Shapefiles/stu watermasses.shp") %>% st_set_crs(4326)

sf_use_s2(FALSE) #beware spherical degenerate duplicate vertex error.... 

#function to get rid of units, they suck
clean_units <- function(x){
  attr(x,"units") <- NULL
  class(x) <- setdiff(class(x),"units")
  x<- as.vector(x)
  x
}

### Read and format Excel/CSV data ##############################################
#read in polygons to cut up erie into basins
read_excel("maps/Shapefiles/basin cutter.xlsx") %>%
  st_as_sf(coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84") %>%
  filter(hydro_only == T) %>%
  group_by(line) %>%
  summarize(.) %>%
  st_cast("POLYGON")%>%
  st_convex_hull(.) %>% data.frame() %>% st_sf() -> basin_cutter



### West Basin - same process, but use water masses to divide.  ####################################
# need to clip out eastern portion of WB to add to water_masses, since the far east WB hydro line is near
#      the eastern border of water_masses. Assume our survey coverage width across hydro lines is similar for both 
#      surveys, so we're manually extending water_masses east by a distance that approximates the width between CB
#      survey lines
#
# get simple WB box


st_intersection(basin_cutter %>% filter(line == "west basin"), lake_erie) %>% 
  st_union() %>%
  st_sf() -> wb_outline  #this is the WB outline that we want....



# extract only that far east part from the polygon
st_difference(wb_outline, st_union(water_masses)) %>% st_cast("POLYGON") -> missing_part
missing_part[as.numeric(st_area(missing_part)) > 50000000,] -> missing_part

# now, merge missing_part with water masses
missing_part <- st_union(water_masses %>% filter(DISCHARGE == "Central Basin"), missing_part) # union missing_part with the existing CB water mass
water_masses %>% 
  filter(DISCHARGE != "Central Basin") %>%# remove the CB water mass that we just used from base water_masses
  rbind(missing_part) -> water_masses  # bind missing_part to remainder of water_masses

# identify unsampled area < 5 m
depth_contour %>% 
  st_transform(crs = 32617) %>%                   #reproject lake
  st_buffer(dist = 0) %>%                         #buffer(0) [this removes overlapping line errors later]
  st_transform(crs = 4326) %>%                    #project back
  rename("depth" = Z_VALUES) %>%
  st_intersection(water_masses) %>%                                  # add water masses
  mutate(strata = case_when(depth < 5  ~ "not sampled",          
                            T ~ as.character(DISCHARGE))) %>%
  select(-JUNK, -COUNT) %>%
  group_by(strata) %>%
  summarize(geometry = st_union(geometry)) %>%
  ungroup %>%
  #select(-4) %>% #random-ass column called "1"
  mutate(area_km2 = round(clean_units(st_area(.))/1000000)) %>%  
  filter(strata != "not sampled") %>% st_cast("POLYGON") -> wb_strata


## create sequential STRATUM category and arange/trim columns
wb_strata$basin <- "WB"
wb_strata$STRATUM <- c(5,1,4,2,3,6)
wb_strata %>% arrange(by_group=STRATUM) %>% 
  select(basin, STRATUM, strata, area_km2, geometry) %>%
  rename(name = strata) -> wb_strata
wb_strat

wb_strata$STRATUM <- as.factor(wb_strata$STRATUM)



### Plot the WB map!    ###################
centroids.df <- st_centroid(wb_strata)
y <- NULL
x <- NULL
for (i in 1:10) x[i] <- centroids.df$geometry[[i]][1]
for (i in 1:10) y[i] <- centroids.df$geometry[[i]][2]
names <- centroids.df$STRATUM

#bounding boxes   
wb_bound_box <- c(xmin = -83.6117, ymin = 41.3494, xmax = -82.513, ymax = 42.1053)
wb_bounds <- wb_bound_box %>% st_bbox() %>% st_as_sfc() %>% st_set_crs(4326)


#plot hydro lines and strata
ggplot()+
  scale_x_continuous(limits = c(wb_bound_box["xmin"], wb_bound_box["xmax"]))+
  scale_y_continuous(limits = c(wb_bound_box["ymin"], wb_bound_box["ymax"]))+
  geom_sf(data = lake_erie, fill = NA, col = "grey30")+
  geom_sf(data = wb_strata, aes(fill = STRATUM), col = "black", show.legend = "polygon")+
  geom_text( aes(label = names, x = x, y = y)) +
  scale_fill_viridis_d()+
  theme_bw()#+
  #theme(axis.title = element_blank(), axis.text = element_blank(), legend.spacing.y = unit(0.1, "cm"))

ggsave("3_stratification/wb_strata.png", width = 6, height = 5, units = "in", dpi = 400)


## write to file
st_write(obj = wb_strata,
         dsn = "maps/Shapefiles/wb_strata.shp",
         append = FALSE)

