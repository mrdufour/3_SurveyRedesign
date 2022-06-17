# Wed Jun 15 17:05:21 2022 ------------------------------

# Bring in data
library(tidyverse)
library(rstudioapi)
library(magrittr)
library(plotrix)
library(gridExtra) 


## bring in data
data_wb <- read.csv("1_data/WB/wb_hacdat_2006_2019.csv", sep=",",header=T)
data_cb <- read.csv("1_data/CB/cb_hacdat_2010_2019.csv", sep=",",header=T)
data_eb <- read.csv("1_data/EB/Eb_hacdat_2008_2019.csv", sep=",",header=T)

source("2_efficiency_analyses/eff_func.R")
source("2_efficiency_analyses/plot_funcs.R")


## efficiency analysis by year for west basin
eff_func(data_wb); head(eff_out)
write.csv(eff_out, "2_efficiency_analyses/wb_efficiency_by_year.csv")
#saveRDS(eff_out,  "2_efficiency_analyses/wb_efficiency_by_year.rds")


## allow updating of title and x-axis labels
plot_func_perc_er(data_wb,"West Basin","1-km")  ## add ablines for 5 and 10%
ggsave("2_efficiency_analyses/wb_efficiency_by_year_perc_err.png", plot = last_plot(), width = 5, height = 4, units = "in", dpi = 400)

plot_func_rse(data_wb,"West Basin","1-km")  ## add ablines for 15 RSE
ggsave("2_efficiency_analyses/wb_efficiency_by_year_rse.png", width = 5, height = 4, units = "in", dpi = 400)



## efficiency analysis by year for central basin
eff_func(data_cb); head(eff_out)
write.csv(eff_out, "2_efficiency_analyses/cb_efficiency_by_year.csv")
#saveRDS(eff_out,   "2_efficiency_analyses/cb_efficiency_by_year.rds")

plot_func_perc_er(data_cb,"Central Basin","0.5-km")  
ggsave("2_efficiency_analyses/cb_efficiency_by_year_perc_err.png", plot = last_plot(), width = 5, height = 4, units = "in", dpi = 400)

plot_func_rse(data_cb,"Central Basin","0.5-km")  
ggsave("2_efficiency_analyses/cb_efficiency_by_year_rse.png", width = 5, height = 4, units = "in", dpi = 400)



## efficiency analysis by year for east basin
eff_func(data_eb); head(eff_out)
write.csv(eff_out, "2_efficiency_analyses/eb_efficiency_by_year.csv")
#saveRDS(eff_out,   "2_efficiency_analyseseb_efficiency_by_year.rds")

plot_func_perc_er(data_eb,"East Basin","0.8-km")  ## add ablines for 5 and 10%
ggsave("2_efficiency_analyses/eb_efficiency_by_year_perc_err.png", plot = last_plot(), width = 5, height = 4, units = "in", dpi = 400)

plot_func_rse(data_eb,"East Basin","0.8-km")  ## add ablines for 15 RSE
ggsave("2_efficiency_analyses/eb_efficiency_by_year_rse.png", width = 5, height = 4, units = "in", dpi = 400)

