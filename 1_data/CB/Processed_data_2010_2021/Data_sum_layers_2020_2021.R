# Wed Jun 15 10:15:50 2022 ------------------------------

## this script sums layers within intervals for CB data 2010-2019
## exports data
## plots data

# Load required libraries
library(readxl)
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(sf)


data <- read.csv("1_data/CB/Processed_data_2010_2021/cb_hacdat_2020_2021_w_layers.csv")
head(data)


data %>% select(Year, type, TRANSECT,Interval, LAYER, Lat_M, Lon_M, NperHa) %>%
  group_by(Year, type, TRANSECT, Interval, Lat_M, Lon_M) %>%
  summarize( NperHa = sum(NperHa)) %>%
  rename(Transect = TRANSECT, Type = type) -> data_processed

data_processed$Basin <- "CB"
data_processed <- select(data_processed, Basin, Year, Type, Transect, Interval, Lat_M, Lon_M, NperHa)
data_processed$Type <- ifelse(data_processed$Type == "transect", "T", "R")

head(data_processed)
write.csv(data_processed, "1_data/CB/cb_hacdat_2020_2021.csv")



