#####################################
# J. Holden
# Jan 25, 2021
# EBResample Project
# With analysis files from M. Dufour
#####################################

# Load required libraries
library(readxl)
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)

# MD code needs a data frame with names = c(Year, Transect, Density, Interval)

# Import analysis data
allfiles <- dir("1_data/EB/ProcessedData_2008_2021")
justcsv <- grep(allfiles, pattern = "csv$", value = T)
justxls <- grep(allfiles, pattern = "xls$", value = T)

csvdat <- lapply(file.path("1_data/EB/ProcessedData_2008_2021", justcsv), read_csv)
csvdat <- bind_rows(csvdat)
head(csvdat)
nrow(csvdat)
#SurveyRandom only a field for 2020
#csvdat$SurveyRandom[is.na(csvdat$SurveyRandom)] <- "T" 
#table(csvdat$SurveyRandom)
# remove random transect from 2020
# csvdat <- csvdat %>% filter(!(SurveyRandom == 'R')) # this line seems to be causing a warning
nrow(csvdat)
csvdat$Year <- year(ymd(csvdat$Date_M))
table(csvdat$Year)
names(csvdat)
csvdat <- csvdat %>% select(Year, TRANSECT, Lat_M, Lon_M, YAO_FishDens) %>% 
  rename(Transect = TRANSECT, NperHa = YAO_FishDens)
head(csvdat)

# Import XLS data
xlsdat <- lapply(file.path("1_data/EB/ProcessedData_2008_2021", justxls), read_xls)
xlsdat <- bind_rows(xlsdat)
head(xlsdat)
table(xlsdat$Thermal_layer) # use only the cold strata
xlsdat <- xlsdat %>% filter(Thermal_layer == 'Cold')
nrow(xlsdat)
table(xlsdat$year)
names(xlsdat)
head(xlsdat)
xlsdat <- xlsdat %>% select(year, transect, `_Lat_M`, `_Lon_M`,  YAO_NPha) %>% 
  rename(Transect = transect, NperHa = YAO_NPha, Year = year, Lat_M = `_Lat_M`, Lon_M= `_Lon_M`) %>% 
  mutate(Transect = as.character(Transect))

# Merge two data series together
analysisdat <- bind_rows(xlsdat, csvdat)
head(analysisdat)

# need to create unique interval sequence
a_split <- split(analysisdat, analysisdat$Year)
interval_renum <- function(x) {x$Interval <- 1:nrow(x); x}
a_split <- lapply(a_split, interval_renum)
analysis_dat <- bind_rows(a_split)
table(analysis_dat$Interval) # some strata only have 38 EDSU
analysis_dat 

data <- select(analysis_dat, Year, Transect, Interval, Lat_M, Lon_M, NperHa)
head(data)

str(data)
range(data$NperHa, na.rm = T)
data %>% filter(is.na(NperHa))
data <- data %>% filter(!is.na(NperHa))
table(data$Year)
ggplot(data, aes(Year)) + geom_histogram(binwidth = 1)
median(table(data$Year))
data$Basin <- "EB"
data <- select(data, Basin, Year, Transect, Interval, Lat_M, Lon_M, NperHa)
write.csv(data, file = "1_data/EB/eb_hacdat_2008_2019.csv")
#save(data, file = "1_data/EB/eb_hacdat_2008_2019.Rdata")


