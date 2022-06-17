# Wed Jun 15 14:56:36 2022 ------------------------------

## this script calculates effort allocation

# Load required libraries
library(readxl)
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(sf)


## bring in data
lake_erie <- st_read("maps/Shapefiles/Lake_Erie_Shoreline.shp")  %>% st_set_crs(4326)
cb_data_2010_2019 <- read.csv("1_data/CB/cb_hacdat_2010_2019.csv") %>%  st_as_sf(coords = c("Lon_M", "Lat_M"), crs = 4326)
cb_strata <- st_read("maps/Shapefiles/cb_strata.shp")  %>% st_set_crs(4326)


## Central Basin map    ###################
#bounding boxes   
cb_bound_box <- c(xmin = -82.4, ymin = 41.363, xmax = -80.55, ymax = 42.7205)
cb_bounds <- cb_bound_box %>% st_bbox() %>% st_as_sfc() %>% st_set_crs(4326)

#plot hydro lines and strata
map <- ggplot()+
  scale_x_continuous(limits = c(cb_bound_box["xmin"], cb_bound_box["xmax"]))+
  scale_y_continuous(limits = c(cb_bound_box["ymin"], cb_bound_box["ymax"]))+
  geom_sf(data = lake_erie, fill = NA, col = "grey30") +
  geom_sf(data = cb_strata, aes(fill = STRATUM), col = "black", show.legend = "polygon", alpha = 0.5)+
  theme_minimal()+
  scale_fill_viridis_d()+
  theme(axis.title = element_blank(), axis.text = element_blank(),
        panel.background = element_rect(fill = 'white', color = 'white'),
        plot.background = element_rect(fill = "white", color = "white"))


## map 2006-2019 data
map +
  geom_sf(data = cb_data_2010_2019, aes(size=NperHa), pch=16, col = "black", alpha = 0.15) +
  facet_wrap(~Year, ncol=3)

ggsave("4_effort_allocation/cb_historical_data_strata.png", width = 6, height = 5, units = "in", dpi = 400)


## intersect data and strata
cb_data_2010_2019 <- st_intersection(cb_data_2010_2019,cb_strata) 
cb_data_2010_2019 <- select(cb_data_2010_2019,-X,-basin)
write.csv(cb_data_2010_2019, "4_effort_allocation/cb_historical_data_strata.csv")


## calculate effort proportional to mean STRATUM sd and STRATUM area
## Following Adams et al. 2006
## Ws = sd weighted area
## p = proportion of Ws
## eq = area based proportion
STRATUM_Year_sd <- aggregate(NperHa ~ STRATUM + Year, data=cb_data_2010_2019, FUN="sd")
mean_STRATUM_sd <- aggregate(NperHa ~ STRATUM, data=STRATUM_Year_sd, FUN="mean")

alloc <- left_join(mean_STRATUM_sd,cb_strata[,c(2,4)], by="STRATUM") %>% select(-geometry)
alloc$Ws <- alloc$NperHa*alloc$area_km2
alloc$p  <- round(alloc$Ws/sum(alloc$Ws),3)
alloc$eq <- round(alloc$area_km2/sum(alloc$area_km2),3)
n_trans <- 20
alloc$n_trans_p <- round(n_trans*alloc$p,0)
alloc$n_trans_eq <- round(n_trans*alloc$eq,0)
write.csv(alloc, "4_effort_allocation/cb_effort_allocation.csv")

