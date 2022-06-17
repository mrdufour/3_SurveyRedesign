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
windowsFonts(Times=windowsFont("Times New Roman"))

#####  Read in shapefiles and data  ###############################
#read in shapefiles, many from http://www.naturalearthdata.com/downloads/
us_and_can = st_read("shapefiles/ne_10m_admin_1_states_provinces.shp") %>% filter(gu_a3 == "CAN"|gu_a3 == "USA") #filter out US and CAN only
great_lakes <- st_read("shapefiles/great lakes poly.shp") %>% st_set_crs(4326)
mus <- st_read("shapefiles/international_border_and_state_dd2.shp") %>% st_set_crs(4326)
int_boundary_main <- st_read("shapefiles/ne_10m_admin_0_boundary_lines_land.shp") # this works for LSC and Huron, but is off of MU lines
int_boundary_le <- st_read("shapefiles/international_border_p.shp") %>% st_set_crs(32617) #matches MU line
lake_erie <- st_read("shapefiles/Lake_Erie_Shoreline.shp") 
depth_contour = st_read("shapefiles/NOAA_1m_bathy_dd.shp")
ten_min_grid <- st_read("shapefiles/clipped 10 minute grids.shp") %>% st_set_crs(4326)
five_min_grid <- st_read("shapefiles/5 minute grids.shp") %>% st_set_crs(4326)
water_masses <- st_read("shapefiles/stu watermasses.shp") %>% st_set_crs(4326)
eb_strata <- st_read("shapefiles/le_ac_strata_geo.shp")
wb_trawls <- st_read("shapefiles/trawl_sites_2009-2016.shp")

### Read and format Excel/CSV data ##############################################
#read in polygons to cut up erie into basins
read_excel("data/basin cutter.xlsx") %>%
  st_as_sf(coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84") %>%
  filter(hydro_only == T) %>%
  group_by(line) %>%
  summarize(.) %>%
  st_cast("POLYGON")%>%
  st_convex_hull(.)-> basin_cutter


# read in hydro survey lines, create simple feature
read_excel("data/sampling locations for status report maps.xlsx") %>%
  filter(lat != ".", survey == "hydroacoustics endpoints") %>% #remove missing values
  st_as_sf(coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84")  %>% #project
  group_by(crn) %>%
    summarise() %>% #groups points by crn
  st_cast(to = "LINESTRING") -> hydro_lines

# read in midwater trawls
read_excel("data/sampling locations for status report maps.xlsx") %>%
  filter(lat != ".", survey == "hydroacoustics midwater", type == "permanent site") %>% #remove missing values
  st_as_sf(coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84") -> hydro_trawls
#mapview(hydro_lines)+mapview(basin_cutter)

# clip 5 min grid to just WB
five_min_grid <- five_min_grid %>%
          st_intersection(basin_cutter %>% filter(line == "west basin"))

# create new 10 min grid clipped to cb and eb
ten_min_grid_clipped <- ten_min_grid %>%
                        st_intersection(basin_cutter %>% filter(line != "west basin"))

### Read in East Basin data   ############################
# read in 2019 hydro survey lines, create simple feature
eb_hydro_lines <- read_csv("data/2019 east basin hydro data.csv") %>%
  st_as_sf(coords = c("Lon_M", "Lat_M"), crs = "+proj=longlat +datum=WGS84")  %>% 
  group_by(TRANSECT) %>%
  summarise() %>% #groups points by crn
  st_cast(to = "LINESTRING")

# 2019 trawls
eb_hydro_trawls <- read_csv("data/2019 east basin midwater trawl data.csv") %>%
  st_as_sf(coords = c("LON_dd", "LAT_dd"), 
           crs = "+proj=longlat +datum=WGS84")

# get 2020-only data
eb_hydro_2020 <- read_csv("data/2020 east basin hydro data.csv") %>%
  st_as_sf(coords = c("Lon_M", "Lat_M"), 
           crs = "+proj=longlat +datum=WGS84") %>%
  mutate(SurveyRandom = ifelse(SurveyRandom == "R","Random", "Regular"))

### Classify depths into strata by basin     #######################

#function to get rid of units, they suck
clean_units <- function(x){
  attr(x,"units") <- NULL
  class(x) <- setdiff(class(x),"units")
  x<- as.vector(x)
  x
}

#calc area of each grid
ten_min_grid %>%
  mutate(AREA = as.vector(st_area(.)/1000000)) -> ten_min_grid

# turn depth contour into strata by basin
depth_contour %>% 
  st_transform(crs = 32617) %>%                   #reproject lake
  st_buffer(dist = 0) %>%                         #buffer(0) [this removes overlapping line errors later]
  st_transform(crs = 4326) %>%                    #project back
  st_intersection(basin_cutter %>% 
                    filter(line %in% c("east central basin", 
                                       "west central basin"))) %>%  # cut into basins and add basin names
  rename("depth" = Z_VALUES, "basin" = line) %>%
  mutate(depth_class = case_when(depth < 10 & basin == "west central basin" ~ "< 10",          # classify depths into strata by basin since DuFour is silly
                                 depth < 20 & basin == "west central basin" ~ "10-20",          
                                 basin == "west central basin" ~ ">= 20",
                                 depth < 10 & basin == "east central basin" ~ "< 10",
                                 T ~ ">= 10"),
         strata = case_when(depth_class == "< 10" ~ "not sampled",          
                            depth_class == "10-20" ~ "west central nearshore",         
                            depth_class == ">= 20" ~ "west central offshore",
                            T ~ "east central"))%>%
  select(-JUNK, -COUNT) %>%
  group_by(basin, depth_class, strata) %>%
    summarize(geometry = st_union(geometry)) %>%
  ungroup %>%
  #select(-4) %>% #random-ass column called "1"
  mutate(area_km2 = round(clean_units(st_area(.))/1000000)) -> cb_strata

# transect lengths
hydro_lines %>%
  mutate(length_nmi = clean_units(st_length(.))/1852,
         basin = if_else(str_detect(crn, "T"), "west", "central")) %>%
  group_by(basin) %>%
  summarize(total_length_nmi = sum(length_nmi))

#mapview(cb_strata)

st_write(obj = cb_strata,
         dsn = "shapefiles created/central basin strata.shp",
         append = FALSE)
### Central Basin map - old survey w/ labelled transects    ###################
#bounding boxes   
cb_bound_box <- c(xmin = -82.7743, ymin = 41.363, xmax = -80.1288, ymax = 42.7205)
cb_bounds <- cb_bound_box %>% st_bbox() %>% st_as_sfc() %>% st_set_crs(4326)

#plot hydro lines and strata
ggplot()+
  scale_x_continuous(limits = c(cb_bound_box["xmin"], cb_bound_box["xmax"]))+
  scale_y_continuous(limits = c(cb_bound_box["ymin"], cb_bound_box["ymax"]))+
  geom_sf(data = lake_erie, fill = NA, col = "grey30")+
  geom_sf(data = cb_strata, aes(fill = strata), col = "black")+
  geom_sf(data = hydro_lines %>% filter(!(str_detect(crn, "T"))), 
          col = "white", lty = 9191, lwd = 1)+ # extract hydro lines from CB only (WB lines have "T" in name)
  scale_fill_manual(name = "Depth \nstrata", values = c("white", "#e1b59e",  "#3182bd", "#cf6d64"),
                    labels = c("not sampled", "west central nearshore", "east central", "west central offshore"),
                    breaks = c("not sampled", "west central nearshore", "east central", "west central offshore"))+
  theme_minimal()+
  theme(axis.title = element_blank(), axis.text = element_blank())

ggsave("figures/hydroacoustics/strata - central basin NEW.png", width = 8, height = 5, units = "in", dpi = 400)

#plot strata and 10 min grid
ggplot()+
  scale_x_continuous(limits = c(cb_bound_box["xmin"], cb_bound_box["xmax"]))+
  scale_y_continuous(limits = c(cb_bound_box["ymin"], cb_bound_box["ymax"]))+
  geom_sf(data = lake_erie, fill = NA, col = "grey30")+
  geom_sf(data = cb_strata, aes(fill = strata), col = "black")+
  geom_sf(data = ten_min_grid, fill = NA, col = "black")+
  #geom_sf_text(data = ten_min_grid %>% filter(AREA > 50),  #only label larger grids
  #             col = "grey80", aes(label = GRID), size = 3)+
  #geom_sf(data = hydro_lines, col = "grey30", lty = 9191, lwd = 1)+
  #geom_sf_label(data = hydro_lines, aes(label = crn), col = "black", size = 3, nudge_y = .05)+
  scale_fill_manual(name = "Depth \nstrata", values = c("white", "#e1b59e",  "#3182bd", "#cf6d64"),
                    labels = c("not sampled", "west central nearshore", "east central", "west central offshore"),
                    breaks = c("not sampled", "west central nearshore", "east central", "west central offshore"))+
  theme_minimal()+
  theme(axis.title = element_blank(), axis.text = element_blank())

ggsave("figures/hydroacoustics/strata - central basin 10min grid.png", width = 8, height = 5, units = "in", dpi = 400)

### New Central Basin map - old survey w/ labeled transects    ###################
# Fig 4 in redesign summary

#plot hydro lines and strata
ggplot()+
  scale_x_continuous(limits = c(cb_bound_box["xmin"], cb_bound_box["xmax"]))+
  scale_y_continuous(limits = c(cb_bound_box["ymin"], cb_bound_box["ymax"]))+
  geom_sf(data = lake_erie, fill = NA, col = "grey30")+
  geom_sf(data = cb_strata, aes(fill = strata), col = "black")+
  geom_sf(data = hydro_trawls, aes(col = "trawls"), fill = "white", shape = 21, show.legend = "point")+
  scale_fill_manual(name = "Depth strata", values = c("white", "#9ecae1","#e1b59e",  "#3182bd", "#cf6d64"),
                    labels = c("Not sampled", "East nearshore", "West nearshore", "East offshore", "West offshore"),
                    breaks = c("not sampled", "east central nearshore", "west central nearshore", "east central offshore", "west central offshore"))+
  scale_color_manual(name = "",
                     values = "black",
                     breaks = "trawls", 
                     labels = "Midwater trawls")+
  guides(fill = guide_legend(order = 1, override.aes = list(shape = NA)), 
         col = guide_legend(order = 2, override.aes = list(linetype = 0, fill = NA)))+
  theme_minimal()+
  theme(axis.title = element_blank(), axis.text = element_blank(), legend.spacing.y = unit(0.1, "cm"))

ggsave("figures/hydroacoustics/Fig 4 cb strata.png", width = 4, height = 2.5, units = "in", dpi = 400)



### Get areas of each strata by basin   ##################

cb_strata %>%
  st_drop_geometry() %>%     #don't need geometry
  filter(strata != "not sampled") %>%
  group_by(basin) %>%
    mutate(total_area_km2 = sum(area_km2)) %>% #total area by (sub)basin
  ungroup %>%
  mutate(percent_area = 100*area_km2/total_area_km2) %>% # percent area within each subbasin
  write_excel_csv("data derived/hydroacoustics basin strata areas v3.csv")


### make shapefile of different basins - not using hydroacoustics CB divide ######
#read in polygons to cut up erie into basins
read_excel("data/basin cutter.xlsx") %>%
  st_as_sf(coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84") %>%
  filter(basin_cutter_only == T) %>% #opposite of hydro_only - uses slightly different CB split
  group_by(line) %>%
  summarize(.) %>%
  st_cast("POLYGON")%>%
  st_convex_hull(.)-> basin_cutter_2

#mapview(basin_cutter_2)

# turn depth contour into strata by basin
depth_contour %>% 
  st_transform(crs = 32617) %>%                   #reproject lake
  st_buffer(dist = 0) %>%                         #buffer(0) [this removes overlapping line errors later]
  st_transform(crs = 4326) %>%                    #project back
  st_intersection(basin_cutter_2) %>%  # cut into basins and add basin names
  rename("depth" = Z_VALUES, "basin" = line) %>% 
  st_write(dsn = "shapefiles created/erie basins.shp", delete_dsn=TRUE)

#st_write(obj = hydro_lines, dsn = here("created shapefiles", "hydroacoustic transect lines.shp"), delete_dsn=TRUE)

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
st_difference(wb_outline, st_union(water_masses)) %>%
  st_cast("POLYGON") -> missing_part
missing_part[as.numeric(st_area(missing_part)) > 50000000,] -> missing_part

# now, merge missing_part with water masses
missing_part <- st_union(water_masses %>% filter(DISCHARGE == "Central Basin"), missing_part) # union missing_part with the existing CB water mass
water_masses %>% 
  filter(DISCHARGE != "Central Basin") %>%# remove the CB water mass that we just used from base water_masses
  rbind(missing_part) -> water_masses  # bind missing_part to remainder of water_masses
  
# turn depth contour into strata by basin
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
  mutate(area_km2 = round(clean_units(st_area(.))/1000000)) -> wb_strata



### Plot the WB map!    ###################
#bounding boxes   
wb_bound_box <- c(xmin = -83.6117, ymin = 41.3494, xmax = -82.513, ymax = 42.1053)
wb_bounds <- wb_bound_box %>% st_bbox() %>% st_as_sfc() %>% st_set_crs(4326)


#plot hydro lines and strata
ggplot()+
  scale_x_continuous(limits = c(wb_bound_box["xmin"], wb_bound_box["xmax"]))+
  scale_y_continuous(limits = c(wb_bound_box["ymin"], wb_bound_box["ymax"]))+
  geom_sf(data = lake_erie, fill = NA, col = "grey30")+
  geom_sf(data = wb_strata, aes(fill = strata), col = "black", show.legend = "polygon")+
  geom_sf(data = wb_trawls, aes(col = "trawls"), fill = "white", shape = 21, show.legend = "point")+
  scale_fill_manual(name = "Strata", 
                    values = c("white", "#e31a1c", "#a6cee3", "#1f78b4", "#fb9a99", "#fdbf6f", "#ff7f00"),
                    labels = c("Not sampled", "Central Basin", "Detroit R.", "Islands", "Maumee R.", "Pidgeon Bay", "Sandusky R."),
                    breaks = c("not sampled", "Central Basin", "Detroit R.", "Islands", "Maumee R.", "Pidgeon Bay", "Sandusky R."))+
  scale_color_manual(name = "",
                      values = "black",
                      breaks = "trawls", 
                      labels = "Bottom trawls")+
  guides(fill = guide_legend(order = 1, override.aes = list(shape = NA)), 
         col = guide_legend(order = 2, override.aes = list(linetype = 0, fill = NA)))+
  theme_minimal()+
  theme(axis.title = element_blank(), axis.text = element_blank(), legend.spacing.y = unit(0.1, "cm"))

ggsave("figures/hydroacoustics/strata - west basin.png", width = 6, height = 4, units = "in", dpi = 400)


### Get areas of each strata by water mass
wb_strata %>%
  st_drop_geometry() %>%     #don't need geometry
  filter(strata != "not_sampled") %>%
  mutate(total_area_km2 = sum(area_km2), #total area by (sub)basin
         percent_area = round(100*area_km2/total_area_km2,1)) %>% # percent area within each subbasin
  arrange(-percent_area) %>% write_excel_csv("data derived/west basin strata areas.csv")


### Both CB and WB survey maps with grid and strata   ################################################
wb_and_cb_bound_box <- c(xmin = -83.6849, ymin = 41.1825, xmax = -80.6, ymax = 42.9)

levels = c("not sampled", wb_strata$strata[-which(wb_strata$strata == "not sampled")], "nearshore", "offshore")

ggplot()+
  scale_x_continuous(limits = c(wb_and_cb_bound_box["xmin"], wb_and_cb_bound_box["xmax"]))+
  scale_y_continuous(limits = c(wb_and_cb_bound_box["ymin"], wb_and_cb_bound_box["ymax"]))+
  geom_sf(data = lake_erie, fill = NA, col = "grey50")+
  geom_sf(data = wb_strata, aes(fill = strata), col = "grey50")+
  geom_sf(data = cb_strata, aes(fill = strata), col = "grey50")+
  #geom_sf(data = hydro_lines, col = "grey70", lty = 9191, lwd = 1)+
  geom_sf(data = ten_min_grid, col = "grey70", lwd = 1, fill = NA)+
  scale_fill_manual(limits = levels, values = c("white", viridis::viridis(n = 8, direction = -1)), name = "Strata")+
  theme_minimal()+
  theme(axis.title = element_blank(), axis.text = element_blank(), panel.grid = element_line(color = "white"),
        legend.position = c(.32, .7), legend.key.size = unit(3, "mm"), legend.background = element_rect(color = "grey50"))

ggsave("figures/hydroacoustics/strata - wb and cb 10min grid.png", width = 10, height = 6, units = "in", dpi = 400)


### Entire lake - WB, CB, and EB with grid and strata   ################################################
# Fig 6 in survey redesign summary

whole_lake_bound_box <- c(xmin = -83.6649, ymin = 41.3235, xmax = -78.7869, ymax = 43.017)

#make manual levels so we can group "not sampled"
levels = c("not sampled", 
           wb_strata$strata[-which(wb_strata$strata == "not sampled")], 
           cb_strata$strata[-which(cb_strata$strata == "not sampled")],
           as.character(eb_strata$STRATUM))

ggplot()+
  scale_x_continuous(limits = c(whole_lake_bound_box["xmin"], whole_lake_bound_box["xmax"]))+
  scale_y_continuous(limits = c(whole_lake_bound_box["ymin"], whole_lake_bound_box["ymax"]))+
  geom_sf(data = wb_strata, aes(fill = strata), col = "grey50", size = .5, show.legend = FALSE)+
  geom_sf(data = cb_strata, aes(fill = strata), col = "grey50", show.legend = FALSE)+
  geom_sf(data = eb_strata, aes(fill = STRATUM), col = "grey50", show.legend = FALSE)+
  geom_sf(data = five_min_grid, col = "white", fill = NA, lty = 2, size = .3)+
  geom_sf(data = ten_min_grid_clipped, col = "white", fill = NA, lty = 2, size = .3)+
  geom_sf(data = lake_erie, fill = NA, col = "grey50")+
  scale_fill_manual(limits = levels, 
                    values = c("white", sample(viridis::viridis(n = 17, direction = -1))))+ # randomize using sample()
  guides(fill=guide_legend(ncol=2))+
  theme_minimal()+
  theme(axis.title = element_blank(), axis.text = element_blank(), panel.grid = element_line(color = "white"))

ggsave("figures/hydroacoustics/Fig 6 whole lake strata.png", 
       width = 7, height = 3.5, units = "in", dpi = 400)

### Entire lake - WB, CB, and EB NO GRID - "all basins old transects.png"   ################################################
# Fig 1 in survey redesign summary
whole_lake_bound_box <- c(xmin = -83.6649, ymin = 41.3235, xmax = -78.7869, ymax = 43.017)

ggplot()+
  scale_x_continuous(limits = c(whole_lake_bound_box["xmin"], whole_lake_bound_box["xmax"]))+
  scale_y_continuous(limits = c(whole_lake_bound_box["ymin"], whole_lake_bound_box["ymax"]))+
  geom_sf(data = hydro_lines, aes(col = "hydro_lines"), show.legend = "line", lwd = 1)+
  geom_sf(data = eb_hydro_lines, aes(col = "hydro_lines"),show.legend = "line", lwd = 1)+
  geom_sf(data = hydro_trawls, aes(col = "hydro_trawls"), show.legend = "point", alpha = .8)+
  geom_sf(data = eb_hydro_trawls, aes(col = "hydro_trawls"), show.legend = "point", alpha = .8)+
  geom_sf(data = eb_strata, fill = NA, aes(col = "strata"), show.legend = "point", lty = 3)+
  geom_sf(data = lake_erie, fill = NA, col = "grey50")+
  theme_minimal()+
  scale_color_manual(labels = c("East basin strata", "Hydroacoustic transect", "Midwater trawl"), 
                     values = c("grey25", "black", "grey30"),
                     guide = guide_legend(override.aes = list(linetype = c(3,1,0), 
                                                              shape = c(NA, NA, 19),
                                                              color = c("grey25", "grey30", "black"))))+
  theme(axis.title = element_blank(), axis.text = element_blank(), panel.grid = element_line(color = "white"),
        legend.position = c(.2, .75), legend.key.size = unit(5, "mm"), 
        legend.background = element_rect(color = "grey50"), legend.title = element_blank())+
  ggsn::scalebar(dist = 50, model = "WGS84",location = "bottomright", dist_unit = "km", 
                 x.min = whole_lake_bound_box["xmin"], y.min = whole_lake_bound_box["ymin"], 
                 x.max = whole_lake_bound_box["xmax"], y.max = whole_lake_bound_box["ymax"],
                 anchor = c(x = -79.1, y = 41.5), family = "Times", transform = TRUE,
                 border.size = 0.5, st.dist = .05)+
  ggsn::north(x.min = whole_lake_bound_box["xmin"], y.min = whole_lake_bound_box["ymin"], 
              x.max = whole_lake_bound_box["xmax"], y.max = whole_lake_bound_box["ymax"],
              location = "topright", symbol = 3, anchor = c(x = -78.6, y = 42),
              scale = .2)

ggsave("figures/hydroacoustics/all basins - old transects.png", width = 6.5, height = 3, units = "in", dpi = 400)

### EB strata map   ###################
# Fig 4.1 (or 5) in survey redesign map
eb_bound_box <- c(xmin = -80.5316, ymin = 41.9385, xmax = -78.7869, ymax = 42.9696)

ggplot()+
  scale_x_continuous(limits = c(eb_bound_box["xmin"], eb_bound_box["xmax"]))+
  scale_y_continuous(limits = c(eb_bound_box["ymin"], eb_bound_box["ymax"]))+
  geom_sf(data = lake_erie, fill = NA, col = "grey30")+
  geom_sf(data = eb_strata, aes(fill = STRATUM), col = "black")+
  geom_sf(data = eb_hydro_trawls, aes(col = "trawls"), fill = "white", shape = 21,show.legend = "point")+
  geom_sf(data = eb_strata, fill = NA, col = "black")+
  scale_fill_manual(limits = as.character(eb_strata$STRATUM), 
                    values = c(viridis::viridis(n = 6, direction = -1)), 
                    name = "Strata")+
  scale_color_manual(name = "",
                     values = "black",
                     breaks = "trawls", 
                     labels = "Midwater trawls")+
  guides(fill = guide_legend(order = 1, override.aes = list(shape = NA), title.vjust = 3), 
         col = guide_legend(order = 2, override.aes = list(linetype = 0, fill = NA)))+
  theme_minimal()+
  theme(axis.title = element_blank(), axis.text = element_blank(), panel.grid = element_line(color = "white"),
        legend.key.size = unit(5, "mm"), legend.position = c(.95, .25), legend.spacing.y = unit(-.1, "cm"))

ggsave("figures/hydroacoustics/Fig 5 eb strata and trawls.png", width = 5, height = 3, units = "in", dpi = 400)

### EB 10min grid map   ###################
eb_bound_box <- c(xmin = -80.5316, ymin = 41.9385, xmax = -78.7869, ymax = 42.9696)

ggplot()+
  scale_x_continuous(limits = c(eb_bound_box["xmin"], eb_bound_box["xmax"]))+
  scale_y_continuous(limits = c(eb_bound_box["ymin"], eb_bound_box["ymax"]))+
  geom_sf(data = lake_erie, fill = NA, col = "grey30")+
  geom_sf(data = eb_strata, aes(fill = STRATUM), col = "black")+
  geom_sf(data = ten_min_grid, fill = NA, col = "black")+
  geom_sf_text(data = ten_min_grid %>% filter(AREA > 50),  #only label larger grids
               col = "grey80", aes(label = GRID), size = 3)+
  scale_fill_manual(limits = as.character(eb_strata$STRATUM), values = c(viridis::viridis(n = 6, direction = -1)), 
                    name = "Stratum")+
  theme_minimal()+
  theme(axis.title = element_blank(), axis.text = element_blank(), legend.position = c(.8, .3))

ggsave("figures/hydroacoustics/strata - east basin 10min grid.png", width = 8, height = 5, units = "in", dpi = 400)

# calc center points for EB 10min grids
ten_min_grid %>%
  filter(AREANAME == "EB", AREA > 30) %>%
  st_transform(crs = 32617) %>%                   #reproject lake
  st_centroid(.) %>%
  st_transform(crs = 4326)  %>%                   #project back
  select(GRID, AREA, PERIMETER) -> eb_centerpoints
  #mapview(.)+mapview(ten_min_grid)
  
# write to CSV
eb_centerpoints %>%
  as_tibble(.) %>%
  select(-geometry) %>%
  cbind(st_coordinates(eb_centerpoints)) %>%
  rename("long" = X, "lat" = Y) %>%
  write_excel_csv("data derived/east basin 10min grid centerpoints.csv")


### EB Fish density map     #################################
# fig 8 in survey redesign
ggplot()+
  scale_x_continuous(limits = c(eb_bound_box["xmin"], eb_bound_box["xmax"]))+
  scale_y_continuous(limits = c(eb_bound_box["ymin"], eb_bound_box["ymax"]))+
  geom_sf(data = lake_erie, fill = NA, col = "grey30")+
  geom_sf(data = eb_hydro_2020, aes(fill = SurveyRandom, size = YAO_FishDens), 
          shape = 21, show.legend = "point", alpha = .7)+
  geom_sf(data = eb_strata, fill = NA, col = "black", lty = 3)+
  scale_fill_manual(breaks = c("Random", "Regular"),
                    labels = c("Random-grid", "Cross-strata"),
                    values = c("#3182bd", "#cf6d64"), 
                    name = "Transect type")+
  labs(size = paste(" Fish\n density\n (#/ha)"))+
  ggsn::scalebar(dist = 25, model = "WGS84",location = "bottomright", dist_unit = "km", 
                 x.min = eb_bound_box["xmin"], y.min = eb_bound_box["ymin"], 
                 x.max = eb_bound_box["xmax"], y.max = eb_bound_box["ymax"],
                 anchor = c(x = -79.45, y = 42), transform = TRUE,
                 border.size = 0.5, st.dist = .05)+
  ggsn::north(x.min = eb_bound_box["xmin"], y.min = eb_bound_box["ymin"], 
              x.max = eb_bound_box["xmax"], y.max = eb_bound_box["ymax"],
              location = "topright", symbol = 3, anchor = c(x = -79.1, y = 42.1),
              scale = .15)+
  guides(size = guide_legend(order = 1, override.aes = list(fill = "white")), 
         fill = guide_legend(order = 2, override.aes = list(size = 3)))+
  theme_minimal()+
  theme(axis.title = element_blank(), axis.text = element_blank(), panel.grid = element_line(color = "white"),
        legend.position = c(.9, .3), legend.spacing.y = unit(-.05, "cm"))

ggsave("figures/hydroacoustics/Fig 8 eb 2020 results.png", 
       width = 5, height = 4, units = "in", dpi = 400)
### EB Fish density map -2020 results ONLY     #################################
# fig 8 in survey redesign
ggplot()+
  scale_x_continuous(limits = c(eb_bound_box["xmin"], eb_bound_box["xmax"]))+
  scale_y_continuous(limits = c(eb_bound_box["ymin"], eb_bound_box["ymax"]))+
  geom_sf(data = lake_erie, fill = NA, col = "grey30")+
  geom_sf(data = eb_hydro_2020 %>% filter(SurveyRandom == "Regular"), 
          aes(size = YAO_FishDens), 
          shape = 21, show.legend = "point", alpha = .7, fill = "orange")+
  geom_sf(data = eb_strata, fill = NA, col = "black", lty = 2)+
  scale_size(range = c(1, 7))+
  labs(size = paste(" Fish\n density\n (#/ha)"),
       fill = paste(" Fish\n density\n (#/ha)"))+
  ggsn::scalebar(dist = 25, model = "WGS84",location = "bottomright", dist_unit = "km", 
                 x.min = eb_bound_box["xmin"], y.min = eb_bound_box["ymin"], 
                 x.max = eb_bound_box["xmax"], y.max = eb_bound_box["ymax"],
                 anchor = c(x = -79.45, y = 42), transform = TRUE,
                 border.size = 0.5, st.dist = .05)+
  ggsn::north(x.min = eb_bound_box["xmin"], y.min = eb_bound_box["ymin"], 
              x.max = eb_bound_box["xmax"], y.max = eb_bound_box["ymax"],
              location = "topright", symbol = 3, anchor = c(x = -79.1, y = 42.1),
              scale = .15)+
  theme_minimal()+
  theme(axis.title = element_blank(), axis.text = element_blank(), panel.grid = element_line(color = "white"),
        legend.position = c(.9, .3), legend.spacing.y = unit(-.05, "cm"))

ggsave("figures/hydroacoustics/East Basin 2020 FTG Report results.png", 
       width = 7, height = 4, units = "in", dpi = 400)
### CB map - overlay ESHIN densities over strata     #############################

#read in midwater trawl catches
read_csv("data/data_cpha.csv") %>%
  janitor::clean_names() %>%
  select(year, sdepth, "e_shin_catch_per_ha" = es, lat, long) %>%
  st_as_sf(coords = c("long", "lat"), crs = "+proj=longlat +datum=WGS84") -> midwater_catch

ggplot(midwater_catch, aes(x = e_shin_catch_per_ha))+
  geom_density(stat = log10())

summary(midwater_catch$e_shin_catch_per_ha)

midwater_catch %<>%
  mutate(catch_qual = factor(case_when(e_shin_catch_per_ha == 0 ~ "None",
                                e_shin_catch_per_ha < 10 ~ "Low",
                                e_shin_catch_per_ha < 50 ~ "Moderate",
                                T ~ "High"), levels = c("None", "Low", "Moderate", "High")))

#setup manual pallete
custom_palette <- colorRampPalette(brewer.pal(5, "YlOrRd"), bias = 100)
custom_scale <- scale_colour_gradientn(colours = c("white", custom_palette(30)), 
                                       limits=c(0, 
                                                max(midwater_catch$e_shin_catch_per_ha)),
                                       name = paste("E. Shiner \ncatch/ha"))

#Map - plot emerald shiner catches and strata, paneled (or turn off for no panelling across catch sizes)
ggplot()+
  scale_x_continuous(limits = c(cb_bound_box["xmin"], cb_bound_box["xmax"]))+
  scale_y_continuous(limits = c(cb_bound_box["ymin"], cb_bound_box["ymax"]))+
  geom_sf(data = lake_erie, fill = NA, col = "grey30")+
  geom_sf(data = cb_strata, aes(fill = strata), col = "black")+
  geom_sf(data = midwater_catch,#%>% filter(e_shin_catch_per_ha > 0), 
          aes(color = e_shin_catch_per_ha), size = 2, alpha = .5)+
  scale_fill_manual(name = "Depth \nstrata", values = c("#9ecae1", "white", "#3182bd"),
                    labels = c("not sampled", "nearshore", "offshore"),
                    breaks = c("not sampled", "nearshore", "offshore"))+
  theme_minimal()+
  facet_grid(catch_qual ~ .)+
  custom_scale +
  theme(axis.title = element_blank(), axis.text = element_blank()) -> paneled_catches

ggsave("figures/hydroacoustics/shiner catches - central basin paneled.png",
       paneled_catches,
       width = 8, height = 12, units = "in", dpi = 400)


#Animation - plot emerald shiner catches over years
ggplot()+
  scale_x_continuous(limits = c(cb_bound_box["xmin"], cb_bound_box["xmax"]))+
  scale_y_continuous(limits = c(cb_bound_box["ymin"], cb_bound_box["ymax"]))+
  geom_sf(data = lake_erie, fill = NA, col = "grey30")+
  geom_sf(data = cb_strata, aes(fill = strata), col = "black")+
  geom_sf(data = midwater_catch ,#%>% filter(e_shin_catch_per_ha > 0), 
          aes(color = e_shin_catch_per_ha, group = seq_along(year)), size = 2, alpha = .5)+ #group call means no transition b/t states
  transition_states(year)+
  scale_fill_manual(name = "Depth \nstrata", values = c("#9ecae1", "white", "#3182bd"),
                    labels = c("not sampled", "nearshore", "offshore"),
                    breaks = c("not sampled", "nearshore", "offshore"))+
  theme_minimal()+
  ggtitle("Year = {closest_state}")+
  custom_scale +
  theme(axis.title = element_blank(), axis.text = element_blank()) -> animated_shiners

anim_save(filename = "shiner catches by year animated.gif",
             path = "figures/hydroacoustics",
             animation = animated_shiners,
             width = 8, height = 5, units = "in", res = 300)
