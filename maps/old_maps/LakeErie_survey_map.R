# Wed Feb 23 10:37:51 2022 ------------------------------


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
us_and_can = st_read("maps/Shapefiles/ne_10m_admin_1_states_provinces.shp") %>% filter(gu_a3 == "CAN"|gu_a3 == "USA") #filter out US and CAN only
lake_erie <- st_read("maps/Shapefiles/Lake_Erie_Shoreline.shp") 
depth_contour = st_read("maps/Shapefiles/NOAA_1m_bathy_dd.shp")
ten_min_grid <- st_read("maps/Shapefiles/10 minute grids.shp") %>% st_set_crs(4326)
five_min_grid <- st_read("maps/Shapefiles/5 minute grids.shp") %>% st_set_crs(4326)
wb_strata <- st_read("maps/Shapefiles/stu watermasses.shp") %>% st_set_crs(4326) %>% rename("strata" = DISCHARGE)
cb_strata <- st_read("maps/Shapefiles/CB_strata_2.shp") %>% st_set_crs(4326)

#LE_juris = st_read("maps/Shapefiles/lake erie polygon jurisdictions.shp") %>%  st_set_crs(4326)


### Read and format Excel/CSV data ##############################################
#read in polygons to cut up erie into basins
read_excel("maps/Shapefiles/basin cutter.xlsx") %>%
  st_as_sf(coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84") %>%
  filter(hydro_only == T) %>%
  group_by(line) %>%
  summarize(.) %>%
  st_cast("POLYGON")%>%
  st_convex_hull(.)-> basin_cutter


# clip 5 min grid to just WB - clib to strata
# cb_five_min_grid <- five_min_grid %>%
#   st_intersection(basin_cutter %>% filter(line != "west basin" | line != "east basin")) %>%
#   st_intersection(LE_juris)



## function to get rid of units, they suck
clean_units <- function(x){
  attr(x,"units") <- NULL
  class(x) <- setdiff(class(x),"units")
  x<- as.vector(x)
  x
}





### Entire lake - WB, CB, and EB with grid and strata   ################################################
# Fig 6 in survey redesign summary

whole_lake_bound_box <- c(xmin = -83.6649, ymin = 41.3235, xmax = -78.7869, ymax = 43.017)

#make manual levels so we can group "not sampled"
# levels = c("not sampled", 
#            wb_strata$strata[-which(wb_strata$strata == "not sampled")], 
#            cb_strata$strata[-which(cb_strata$strata == "not sampled")],
#            as.character(eb_strata$STRATUM))

ggplot()+
  scale_x_continuous(limits = c(whole_lake_bound_box["xmin"], whole_lake_bound_box["xmax"]))+
  scale_y_continuous(limits = c(whole_lake_bound_box["ymin"], whole_lake_bound_box["ymax"]))+
  geom_sf(data = wb_strata, aes(fill = strata), col = "grey50", size = .5, show.legend = FALSE)+
  geom_sf(data = cb_strata, aes(fill = strata), col = "grey50", show.legend = FALSE)+
  #geom_sf(data = eb_strata, aes(fill = STRATUM), col = "grey50", show.legend = FALSE)+
  #geom_sf(data = five_min_grid, col = "white", fill = NA, lty = 2, size = .3)+
  #geom_sf(data = ten_min_grid_clipped, col = "white", fill = NA, lty = 2, size = .3)+
  geom_sf(data = lake_erie, fill = NA, col = "grey50")+
  #scale_fill_manual(limits = levels, 
  #                  values = c("white", sample(viridis::viridis(n = 17, direction = -1))))+ # randomize using sample()
  #guides(fill=guide_legend(ncol=2))+
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
