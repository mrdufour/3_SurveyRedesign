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
#five_min_grid <- st_read("maps/Shapefiles/5 minute grids.shp") %>% st_set_crs(4326)
LE_juris = st_read("maps/Shapefiles/lake erie polygon jurisdictions.shp") %>%  st_set_crs(4326)
eb_strata_orig <- st_read("maps/Shapefiles/le_ac_strata_geo.shp") %>% st_transform(4326)


### Read and format Excel/CSV data ##############################################
#read in polygons to cut up erie into basins
read_excel("maps/Shapefiles/basin cutter.xlsx") %>%
  st_as_sf(coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84") %>%
  filter(hydro_only == T) %>%
  group_by(line) %>%
  summarize(.) %>%
  st_cast("POLYGON")%>%
  st_convex_hull(.)-> basin_cutter


## function to get rid of units, they suck
clean_units <- function(x){
  attr(x,"units") <- NULL
  class(x) <- setdiff(class(x),"units")
  x<- as.vector(x)
  x
}


## carve out main lake CA and US polygons
LE_juris %>%
  select(CNTRY_NAME, geometry) %>%
  group_by(CNTRY_NAME)%>%
  summarize(geometry = st_union(geometry)) %>%
  ungroup %>%
  #select(-4) %>% #random-ass column called "1"
  mutate(area_km2 = round(clean_units(st_area(.))/1000000)) %>%
  select(CNTRY_NAME, area_km2,geometry) -> LE_juris_by_cntry

ca <- LE_juris_by_cntry[LE_juris_by_cntry$CNTRY_NAME=="Canada",]
ca <-   st_cast(ca, "POLYGON")
ca <- ca[3,]


us <- LE_juris_by_cntry[LE_juris_by_cntry$CNTRY_NAME=="United States",]
us <-   st_cast(us, "POLYGON")
us <- us[5,]

LE_juris_by_cntry <- rbind(ca,us)



###################################################################################
## EB Basin Strata - 40 m
###################################################################################

## create strata by depth contour
depth_contour %>% 
  st_transform(crs = 32617) %>%                   #reproject lake
  st_buffer(dist = 0) %>%                         #buffer(0) [this removes overlapping line errors later]
  st_transform(crs = 4326) %>%                    #project back
  st_intersection(basin_cutter %>% 
                    filter(line %in% "east basin")) %>%  # cut into basins and add basin names
  rename("depth" = Z_VALUES, "basin" = line) %>%
  mutate(depth_class = case_when(depth < 40  ~ "< 40",          # classify depths into strata by basin since DuFour is silly
                                 depth >= 40 ~ ">= 40"),
         strata = case_when(depth_class == "< 40"  ~ "shallow",          
                            depth_class == ">= 40" ~ "deep")) %>%
  select(-JUNK, -COUNT) %>%
  group_by(basin, depth_class, strata) %>%
  summarize(geometry = st_union(geometry)) %>%
  ungroup %>%
  #select(-4) %>% #random-ass column called "1"
  mutate(area_km2 = round(clean_units(st_area(.))/1000000)) %>% 
  st_simplify(preserveTopology = T,dTolerance = 0) %>%
  st_remove_holes(max_area = 1000000) %>% st_cast("POLYGON") ->  eb_depths


eb_depths <- eb_depths[1:2,] ## nix little deep polygon
ggplot() + geom_sf(data = eb_depths, fill = NA, col = "grey30", alpha = 0.5)



# ## plot potential strata
deep <- eb_depths[eb_depths$strata == "deep",]
shallow <- st_difference(eb_strata_orig, deep)
eb_depth_strata40 <- st_intersection(eb_depths, eb_strata_orig)

## create proposed strata
shallow <- st_cast(shallow, "POLYGON")
shallow$new_strat <- seq(1,dim(shallow)[1], by=1)
shallow$new_strat
shallow$new_strat <- c(1,4,2,4,2,2,5,6)

ggplot() + geom_sf(data = shallow, aes(fill = new_strat), col = "grey30", alpha = 0.5)

## clean up and group shallow strata
shallow %>%
  select(-STRATUM, -AREA_M2, - basin, -depth_class, -strata, -area_km2) %>%
  group_by(new_strat) %>%
  summarize(geometry = st_union(geometry)) %>%
  ungroup %>%
  #select(-4) %>% #random-ass column called "1"
  mutate(area_km2 = round(clean_units(st_area(.))/1000000)) -> shallow

shallow$depth_class <- "shallow"
shallow %>% select(new_strat, depth_class, area_km2,geometry) -> shallow

## clean up and group deep strata  
deep <- select(deep, -basin, -strata)
deep$depth_class <- "deep"
deep$new_strat <- 3
deep <- select(deep, new_strat, depth_class, area_km2) 
deep <- rename(deep, geometry = geom)

## combine shallow and deep strata
eb_strata_40 <- rbind(shallow, deep)
eb_strata_40_cntry <- st_intersection(eb_strata_40,LE_juris_by_cntry)
eb_strata_40_cntry %>% select(new_strat, depth_class, CNTRY_NAME, geometry) -> eb_strata_40_cntry
eb_strata_40_cntry$area_km2 <- st_area(eb_strata_40_cntry)
eb_strata_40_cntry %>% mutate(area_km2 = round(clean_units(st_area(.))/1000000)) %>%
  select(new_strat, depth_class, CNTRY_NAME,area_km2,geometry) -> eb_strata_40_cntry
eb_strata_40_cntry$STRATUM <- c(1,3,6,7,9,4,
                                2,6,8,10,5)

eb_strata_40_cntry %>%
  select(STRATUM,new_strat, depth_class, CNTRY_NAME) %>%
  group_by(STRATUM) %>%
  summarize(geometry = st_union(geometry)) %>%
  ungroup %>%
  #select(-4) %>% #random-ass column called "1"
  mutate(area_km2 = round(clean_units(st_area(.))/1000000)) %>%
  select(STRATUM, area_km2,geometry) -> eb_strata

eb_strata$area_km2 <- st_area(eb_strata)
eb_strata %>% mutate(area_km2 = round(clean_units(st_area(.))/1000000)) %>%
  select(STRATUM, area_km2,geometry) -> eb_strata

eb_strata$basin <- "EB"
eb_strata$STRATUM <- as.factor(eb_strata$STRATUM)
eb_strata$strata <- eb_strata$STRATUM

eb_strata %>% select(basin, STRATUM, strata, area_km2,geometry) %>%
  rename(name = strata) -> eb_strata


## Plot East Basin strata
centroids.df <- st_centroid(eb_strata)
y <- NULL
x <- NULL
for (i in 1:10) x[i] <- centroids.df$geometry[[i]][1]
for (i in 1:10) y[i] <- centroids.df$geometry[[i]][2]
names <- centroids.df$STRATUM

#bounding boxes   
eb_bound_box <- c(xmin = -80.5, ymin = 42.1, xmax = -78.85, ymax = 42.9)
eb_bounds <- eb_bound_box %>% st_bbox() %>% st_as_sfc() %>% st_set_crs(4326)

ggplot()+
  scale_x_continuous(limits = c(eb_bound_box["xmin"], eb_bound_box["xmax"]))+
  scale_y_continuous(limits = c(eb_bound_box["ymin"], eb_bound_box["ymax"]))+
  geom_sf(data = lake_erie, fill = NA, col = "grey30")+
  geom_sf(data = eb_strata, aes(fill = STRATUM), col = "black",  show.legend = "polygon") +
  geom_text( aes(label = names, x = x, y = y)) +
  scale_fill_viridis_d()+
  theme_bw()

ggsave("3_stratification/eb_strata.png", width = 8, height = 5, units = "in", dpi = 400)


## save strata as shapefile
st_write(obj = eb_strata,
         dsn = "maps/Shapefiles/eb_strata.shp",
         append = FALSE)

###################################################################################


