# Thu Jun 16 16:45:26 2022 ------------------------------

## this script creates the central basin hydroacoustic strata

library(sf)     #simple features - REMINDER - this loads library(raster), which masks dplyr::select!!!!!
library(measurements)
library(magrittr) # fun pipes
library(readxl)
library(tidyverse) #obv
library(dplyr)

#####  Read in shapefiles and data  ###############################
#read in shapefiles, many from http://www.naturalearthdata.com/downloads/
lake_erie <- st_read("maps/Shapefiles/Lake_Erie_Shoreline.shp") 
depth_contour = st_read("maps/Shapefiles/NOAA_1m_bathy_dd.shp")
#five_min_grid <- st_read("maps/Shapefiles/5 minute grids.shp") %>% st_set_crs(4326)
LE_juris = st_read("maps/Shapefiles/lake erie polygon jurisdictions.shp") %>%  st_set_crs(4326)


### Read and format Excel/CSV data ##############################################
#read in polygons to cut up erie into basins
read_excel("maps/Shapefiles/basin cutter.xlsx") %>%
  st_as_sf(coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84") %>%
  filter(hydro_only == T) %>%
  group_by(line) %>%
  summarize(.) %>%
  st_cast("POLYGON")%>%
  st_convex_hull(.)-> basin_cutter


# clip 5 min grid to just WB
cb_five_min_grid <- five_min_grid %>%
          st_intersection(basin_cutter %>% filter(line != "west basin" | line != "east basin")) %>%
          st_intersection(LE_juris)


## function to get rid of units, they suck
clean_units <- function(x){
  attr(x,"units") <- NULL
  class(x) <- setdiff(class(x),"units")
  x<- as.vector(x)
  x
}

## calc area of each grid
five_min_grid %>%
  mutate(AREA = as.vector(st_area(.)/1000000)) -> five_min_grid



###################################################################################
## Central Basin Strata 
###################################################################################

## create strata by depth contours, east/west CB, and jurisdictions
depth_contour %>% 
  st_transform(crs = 32617) %>%                   #reproject lake
  st_buffer(dist = 0) %>%                         #buffer(0) [this removes overlapping line errors later]
  st_transform(crs = 4326) %>%                    #project back
  st_intersection(LE_juris) %>%
  st_intersection(basin_cutter %>% 
                    filter(line %in% c("east central basin", 
                                       "west central basin"))) %>%  # cut into basins and add basin names
  rename("depth" = Z_VALUES, "basin" = line) %>%
  mutate(depth_class = case_when(depth < 10 ~ "< 10",          # classify depths into strata by basin since DuFour is silly
                                 depth < 20  ~ "10-20",          
                                 T ~ ">= 20"),
         strata = case_when(          
                            depth_class == "10-20" & CNTRY_NAME == "Canada" & basin == "west central basin" ~ "NWS",
                            depth_class == ">= 20" & CNTRY_NAME == "Canada" & basin == "west central basin" ~ "NWD",
                            depth_class == ">= 20" & CNTRY_NAME == "United States" & basin == "west central basin" ~ "SWD", 
                            depth_class == "10-20" & CNTRY_NAME == "United States" & basin == "west central basin" ~ "SWS",
                            depth_class == "10-20" & CNTRY_NAME == "Canada" & basin == "east central basin" ~ "NES",
                            depth_class == ">= 20" & CNTRY_NAME == "Canada" & basin == "east central basin" ~ "NED",
                            depth_class == ">= 20" & CNTRY_NAME == "United States" & basin == "east central basin" ~ "SED", 
                            depth_class == "10-20" & CNTRY_NAME == "United States" & basin == "east central basin" ~ "SES",
                            T ~ "not sampled"))%>%
  select(-JUNK, -COUNT) %>%
  group_by(basin, depth_class, CNTRY_NAME, strata) %>%
    summarize(geometry = st_union(geometry)) %>%
  ungroup %>%
  #select(-4) %>% #random-ass column called "1"
  mutate(area_km2 = round(clean_units(st_area(.))/1000000)) %>%
  filter(strata != "not sampled") -> cb_strata


cb_strata$basin <- "CB"
cb_strata$STRATUM <- c(6,7,5,8,2,3,1,4)
cb_strata %>% arrange(by_group=STRATUM) %>% 
  select(basin, STRATUM, strata, area_km2, geometry) %>%
  rename(name = strata) -> cb_strata

cb_strata$STRATUM <- as.factor(cb_strata$STRATUM)



## Plot Central Basin strata
centroids.df <- st_centroid(cb_strata)
y <- NULL
x <- NULL
for (i in 1:10) x[i] <- centroids.df$geometry[[i]][1]
for (i in 1:10) y[i] <- centroids.df$geometry[[i]][2]
names <- centroids.df$STRATUM

## bounding boxes
cb_bound_box <- c(xmin = -82.4, ymin = 41.363, xmax = -80.4, ymax = 42.7205)
cb_bounds <- cb_bound_box %>% st_bbox() %>% st_as_sfc() %>% st_set_crs(4326)

ggplot()+
  scale_x_continuous(limits = c(cb_bound_box["xmin"], cb_bound_box["xmax"]))+
  scale_y_continuous(limits = c(cb_bound_box["ymin"], cb_bound_box["ymax"]))+
  geom_sf(data = lake_erie, fill = NA, col = "grey30")+
  geom_sf(data = cb_strata, aes(fill = STRATUM), col = "black", show.legend = "polygon")+
  #geom_text( aes(label = names, x = x, y = y)) +
  scale_fill_viridis_d()+
  theme_bw()


ggsave("3_stratification/cb_strata.png", width = 6, height = 5, units = "in", dpi = 400)


## save strata as shapefile
st_write(obj = cb_strata,
         dsn = "maps/Shapefiles/cb_strata.shp",
         append = FALSE)

###################################################################################



