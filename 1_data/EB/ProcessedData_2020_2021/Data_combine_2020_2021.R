# Wed Jun 15 14:08:41 2022 ------------------------------


# Load required libraries
library(readxl)
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(sf)


## combind eb 2020 and 2021 data and map

data_2020 <- read.csv("1_data/EB/ProcessedData_2020_2021/eb_hacdat_2020.csv")
data_2021 <- read.csv("1_data/EB/ProcessedData_2020_2021/eb_hacdat_2021.csv")

data <- rbind(data_2020, data_2021)

write.csv(data, "1_data/EB/eb_hacdat_2020_2021.csv")
