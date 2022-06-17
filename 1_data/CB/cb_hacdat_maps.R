# Wed Jun 15 14:56:36 2022 ------------------------------

## this script plots 2006-2019 and 2020-2021 data

# Load required libraries
library(readxl)
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(sf)


## bring in data
lake_erie <- st_read("maps/Shapefiles/Lake_Erie_Shoreline.shp") 
data_2010_2019 <- read.csv("1_data/CB/cb_hacdat_2010_2019.csv") %>%  st_as_sf(coords = c("Lon_M", "Lat_M"), crs = 4326)
data_2020_2021 <- read.csv("1_data/CB/cb_hacdat_2020_2021.csv") %>%  st_as_sf(coords = c("Lon_M", "Lat_M"), crs = 4326)


## Central Basin map    ###################
#bounding boxes   
cb_bound_box <- c(xmin = -82.4, ymin = 41.363, xmax = -80.55, ymax = 42.7205)
cb_bounds <- cb_bound_box %>% st_bbox() %>% st_as_sfc() %>% st_set_crs(4326)

#plot hydro lines and strata
map <- ggplot()+
  scale_x_continuous(limits = c(cb_bound_box["xmin"], cb_bound_box["xmax"]))+
  scale_y_continuous(limits = c(cb_bound_box["ymin"], cb_bound_box["ymax"]))+
  geom_sf(data = lake_erie, fill = NA, col = "grey30") +
  theme_minimal()+
  theme(axis.title = element_blank(), axis.text = element_blank(),
        panel.background = element_rect(fill = 'white', color = 'white'),
        plot.background = element_rect(fill = "white", color = "white"))


## map 2006-2019 data
map +
geom_sf(data = data_2010_2019, aes(size=NperHa), pch=16, col = "black", alpha = 0.15) +
facet_wrap(~Year, ncol=3)
ggsave("1_data/CB/cb_hacdat_2010_2019.png", width = 8, height = 10, units = "in", dpi = 400)


## map 2020-2021 data
map +
geom_sf(data = data_2020_2021, aes(size=NperHa, col=Type), pch=16,  alpha = 0.5) +
facet_wrap(~Year, ncol=1)
ggsave("1_data/CB/cb_hacdat_2020_2021.png", width = 8, height = 10, units = "in", dpi = 400)


