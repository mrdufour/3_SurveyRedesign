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

load("1_data/EB/ProcessedData_2020_2021/2021_hacdat_export.Rdata")
hacdat2021 <- hacdat

head(hacdat2021)

intg  <- hacdat2021$intg
ts    <- hacdat2021$ts
histo <- hacdat2021$histo

## Merge/join integration and target strength data 
hacdat <- left_join(intg, ts, by=c('Region_name', 'Interval', 'Date_M', 'Time_M'))

## remove interval 6 - only a short segment if it exists
#hacdat <- filter(hacdat, Interval <= 5)





## Split Region_name into distinct STRATUM, TRANSECT, and LAYER for hacdat
hacdat$STRATUM <- sapply(strsplit(hacdat$Region_name, "_"), '[',1)
hacdat$TRANSECT <- sapply(strsplit(hacdat$Region_name, "_"), '[',2)
#hacdat$TRANSECT <- as.numeric(hacdat$TRANSECT)
hacdat$LAYER <- sapply(strsplit(hacdat$Region_name, "_"), '[',3)

## Split TRANSECT into t Type and Transect
hacdat$Type <- sapply(strsplit(hacdat$TRANSECT, ""), '[', 1)
#hacdat$Transect <- sapply(strsplit(hacdat$TRANSECT, ""), '[', 2:3)



## Split Region_name into distinct STRATUM, TRANSECT, and LAYER for histo
histo$STRATUM <- sapply(strsplit(histo$Region_name, "_"), '[',1)
histo$TRANSECT <- sapply(strsplit(histo$Region_name, "_"), '[',2)
#histo$TRANSECT <- as.numeric(histo$TRANSECT)
histo$LAYER <- sapply(strsplit(histo$Region_name, "_"), '[',3)
## Split TRANSECT into t Type and Transect
histo$Type <- sapply(strsplit(histo$TRANSECT, ""), '[', 1)



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
hacdat <-hacdat %>% select(Type, STRATUM, TRANSECT, LAYER, Interval, Date_M, Time_M, Lat_M.x, Lon_M.x, 
                           FishDensity, Num_targets, Exclude_below_line_range_mean.x) %>%
                            rename(Lat_M = Lat_M.x) %>% rename(Lon_M = Lon_M.x) %>%
                            rename(BottomLine = Exclude_below_line_range_mean.x)

hacdat$Year <- 2021
hacdat$Basin <- "EB"

head(hacdat)

hacdat$LAYER

hacdat %>% filter(LAYER=="HYP") %>% 
  select(Basin, Year, Type, TRANSECT, Interval, Lat_M, Lon_M, FishDensity) %>%
  rename(Transect = TRANSECT, NperHa = FishDensity) -> hacdat 


head(hacdat)


# ## Reduce histo to essential columns and transform to long form
# ## TS columns to rows with replicated data 
# histo <- histo %>% select(STRATUM, TRANSECT, LAYER, Date_M, Time_M, Lat_M, Lon_M, Interval,
#                           Targets_Binned, `-64.500000`:`-20.500000`) %>%
#   gather(TS, N_Targets, `-64.500000`:`-20.500000`) %>%
#   mutate(TS = as.numeric(TS))
# 
# 
# ## Merge hacdat and histo
# histohac<-left_join(hacdat, histo, by = c("STRATUM", "TRANSECT", "LAYER", "Interval", "Date_M", "Time_M"))



write.csv(hacdat, "1_data/EB/ProcessedData_2020_2021/eb_hacdat_2021.csv")
# write.csv(histohac, "3_East_Basin/1_extract_data/histohac_2021.csv")
# write.csv(histo, "3_East_Basin/1_extract_data/histo_2021.csv")
