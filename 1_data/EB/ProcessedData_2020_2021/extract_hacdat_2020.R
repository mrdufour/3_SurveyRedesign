# Thu Dec 09 15:51:27 2021 ------------------------------

## Ping data summary script
## Uses exports from erieacoustics package export_transect_evdata() function
## Transect specific exports are combine to generate whole survey summaries

## load packages
library(readr)
#library(plyr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(rstudioapi)
library(lattice)
library(reshape2)

## Gather ping data from transect folders and bind together
# allfiles<-dir('3_Ping_Data', recursive = T, full.names = T)
# intg<-grep(allfiles, pattern = "intg\\.csv$", value = T)
# ts<-grep(allfiles, pattern = "ts\\.csv$", value = T)
# histo<-grep(allfiles, pattern = "histo\\.csv$", value = T)
# 
# intg<-bind_rows(lapply(intg, read_csv))
# intg<-intg %>% filter(!(is.na(Region_name)))
# ts<-bind_rows(lapply(ts, read_csv))
# histo<-bind_rows(lapply(histo, read_csv))

load("1_data/EB/ProcessedData_2020_2021/2020_hacdat_export.Rdata")

head(hacdat2020)

intg  <- hacdat2020$intg
ts    <- hacdat2020$ts
histo <- hacdat2020$histo

## Merge/join integration and target strength data 
hacdat <- left_join(intg, ts, by=c('Region_name', 'Interval', 'Date_M', 'Time_M'))

## remove interval 6 - only a short segment if it exists
#hacdat <- filter(hacdat, Interval <= 5)

## Split Region_name into distinct STRATUM, TRANSECT, and LAYER for hacdat
hacdat$type <-substr(hacdat$Region_name, 1, 1)
hacdat$Region_name2 <- substr(hacdat$Region_name,2,1000)
hacdat$TRANSECT <- sapply(strsplit(hacdat$Region_name2, "_"), '[',1)
hacdat$LAYER <- sapply(strsplit(hacdat$Region_name2, "_"), '[',2)
hacdat$LAYER <- ifelse(hacdat$LAYER == "UP", "epi","hypo")
hacdat$STRATUM <- substr(hacdat$TRANSECT, 1,nchar(hacdat$TRANSECT)-1)


## Split Region_name into distinct STRATUM, TRANSECT, and LAYER for histo
histo$type <-substr(histo$Region_name, 1, 1)
histo$Region_name2 <- substr(histo$Region_name,2,1000)
histo$TRANSECT <- sapply(strsplit(histo$Region_name2, "_"), '[',1)
histo$LAYER <- sapply(strsplit(histo$Region_name2, "_"), '[',2)
histo$LAYER <- ifelse(histo$LAYER == "UP", "epi","hypo")
histo$STRATUM <- substr(histo$TRANSECT, 1,nchar(histo$TRANSECT)-1)



## replace missing TS values with average TS from INTERVALS within same TRANSECT and LAYER
hacdat$sigma_bs <- 10^(hacdat$TS_mean/10) # create sigma_bs
hacdat$sigma_bs <- ifelse(hacdat$TS_mean == 9999, NA,  hacdat$sigma_bs) # assign NAs
## impute missing sigma_bs_adj data - replace missing and Nv flagged sigma_bs values with mean of interval
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
hacdat <- plyr::ddply(hacdat, ~ STRATUM + TRANSECT + LAYER, transform, sigma_bs = impute.mean(sigma_bs))
hacdat$TS_mean <- 10*log10(hacdat$sigma_bs) # convert sigma_bs back to TS_mean using imputed values


## Calculate fish density:
## volume back scattering coefficient divided by backscattering cross-section = fish/cubic meter
## multiply by thickness (m) and 10,000 turns this into areal density (fish/hectare)
hacdat$FishDensity <- with(hacdat, (10^(Sv_mean/10))/(10^(TS_mean/10))*Thickness_mean.x*10000)
## Exporting and using ABC
# hacdat$FishDensity2 <- with(hacdat, (ABC/(10^(TS_mean/10)))*Thickness_mean.x*10000)

head(hacdat)

## Reduce hacdat to essential columns 
hacdat <-hacdat %>% select(type, STRATUM, TRANSECT, LAYER, Interval, Date_M, Time_M, Lat_M.x, Lon_M.x, 
                           FishDensity, Num_targets, Exclude_below_line_range_mean.x) %>%
                            rename(Lat_M = Lat_M.x) %>% rename(Lon_M = Lon_M.x) %>%
                            rename(BottomLine = Exclude_below_line_range_mean.x)

hacdat$Year <- 2020
hacdat$Basin <- "EB"
hacdat$type <- ifelse(hacdat$type=="T","transect","grid")

head(hacdat)


hacdat %>% filter(LAYER=="hypo") %>% 
  select(Basin, Year, type, TRANSECT, Interval, Lat_M, Lon_M, FishDensity) %>%
  rename(Transect = TRANSECT, Type = type, NperHa = FishDensity) -> hacdat 


head(hacdat)


# ## Reduce histo to essential columns and transform to long form
# ## TS columns to rows with replicated data 
# histo <- histo %>% select(type, STRATUM, TRANSECT, LAYER, Date_M, Time_M, Lat_M, Lon_M, Interval,
#                           Targets_Binned, `-64.500000`:`-20.500000`) %>%
#   gather(TS, N_Targets, `-64.500000`:`-20.500000`) %>%
#   mutate(TS = as.numeric(TS))
# 
# 
# ## Merge hacdat and histo
# histohac<-left_join(hacdat, histo, by = c("type","STRATUM", "TRANSECT", "LAYER", "Interval", "Date_M", "Time_M"))



write.csv(hacdat, "1_data/EB/ProcessedData_2020_2021/eb_hacdat_2020.csv")
# write.csv(histohac, "3_East_Basin/1_extract_data/histohac_2020.csv")
#write.csv(histo, "3_East_Basin/1_extract_data/histo_2020.csv")
