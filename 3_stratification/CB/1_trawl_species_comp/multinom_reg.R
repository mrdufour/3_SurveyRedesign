# Sat Mar 21 19:33:26 2020 ------------------------------

# Bring in data
library(rstudioapi)
library(nnet)
library(effects)
library(ggplot2)
library(magrittr)
library(reshape2)
library(dplyr)


## Bring in data
data <- read.csv("2_Central_Basin/1_trawl_species_comp/data_cat.csv", sep=",",header=T) 
head(data)

#############################################################################################
## multinomial with continuous variables (longitude and depth)
#############################################################################################
res <- as.matrix(data[,1:5])

## predictors
long <- as.numeric(data$LONG)
depth <- as.numeric(data$Sdepth)
ew <- data$EW
dc <- data$depthcat



mod <- multinom(res~long * depth)
summary(mod)

dses <- data.frame(long=seq(min(long),max(long),length.out=1000), depth = rep(mean(data$Sdepth),1000))
new <- cbind(dses, predict(mod, newdata = dses, "probs"))

par(mfrow=c(2,1), mar=c(4,4,2,1))
plot(new$long,new$ES, ylim=c(0,0.8), typ="l", col="blue", lwd=2, ylab="Proportion of Catch", xlab="Longitude (dd)")
lines(new$long,new$RSYAO, col="gray", lwd=2)
lines(new$long,new$RSYOY, col="gray", lty=3, lwd=2)
lines(new$long,new$YPYOY, col="yellow", lty=1, lwd=2)
lines(new$long,new$OTHER, col="red", lty=1, lwd=2)
legend("topleft",horiz=F,legend=c("RSYAO","RSYOY","ES","YPYOY","OTHER"), cex=0.65,
       lty=c(1,3,1,1,1), lwd=2,col=c("darkgray","lightgray","blue","yellow","red"), bty="n")
abline(v=-81.65, col="black", lwd=3,lty=2)


dses <- data.frame(long=rep(mean(long),1000), depth = seq(min(depth),max(depth),length.out=1000))
new <- cbind(dses, predict(mod, newdata = dses, "probs"))

plot(new$depth,new$ES, ylim=c(0,0.8), typ="l", col="blue", lwd=2, ylab="Proportion of Catch", xlab="Depth (m)")
lines(new$depth,new$RSYAO, col="gray", lwd=2)
lines(new$depth,new$RSYOY, col="gray", lty=3, lwd=2)
lines(new$depth,new$YPYOY, col="yellow", lty=1, lwd=2)
lines(new$depth,new$OTHER, col="red", lty=1, lwd=2)
 legend("topleft",horiz=T,legend=c("RSYAO","RSYOY","ES","YPYOY","OTHER"), cex=0.65,
        lty=c(1,3,1,1,1), lwd=2,col=c("darkgray","lightgray","blue","yellow","red"), bty="n")
abline(v=14, col="black", lwd=3,lty=2)
#############################################################################################


#############################################################################################
## multinomial with factor variables (longitude and depth) - based on above results
#############################################################################################
## addin interaction plot
long_c <- ifelse(long<=-81.65,1,0)
depth_c <- ifelse(depth<=20,1,0)

mod <- multinom(res~as.factor(long_c) * as.factor(depth_c))
fits <- data.frame(cbind(fitted(mod),long_c,depth_c))
head(fits)

RSYOY <- aggregate(RSYOY~long_c+depth_c,data=fits,FUN="mean")
RSYAO <- aggregate(RSYAO~long_c+depth_c,data=fits,FUN="mean")
ES    <- aggregate(ES~long_c+depth_c,data=fits,FUN="mean")
YPYOY <- aggregate(YPYOY~long_c+depth_c,data=fits,FUN="mean")
OTHER <- aggregate(OTHER~long_c+depth_c,data=fits,FUN="mean")

mat <- matrix(c(RSYOY[,3],
              RSYAO[,3],
              ES[,3],
              YPYOY[,3],
              OTHER[,3]),nrow=4,byrow=F)

par(mfrow=c(2,2), oma=c(2,2,1,1))
sp_col <- c("darkgray","lightgray","blue","yellow","red")
nam <- c("RSYOY","RSYAO","ES","YPYOY","OTHER")
barplot(t(mat)[,2], col=sp_col, names=nam, main="West-Offshore", ylim=c(0,0.6))
barplot(t(mat)[,1], col=sp_col, names=nam, main="East-Offshore", ylim=c(0,0.6))
barplot(t(mat)[,4], col=sp_col, names=nam, main="West-Inshore", ylim=c(0,0.6))
barplot(t(mat)[,3], col=sp_col, names=nam, main="East-Inshore", ylim=c(0,0.6))
mtext("Proportion of Catch",2,0.5,outer=T, cex=1.2)
mtext("Species Groups",1,0.5,outer=T, cex=1.2)
#############################################################################################


#############################################################################################
## multinomial with longitude as factor and depth as continuous - based on above results 
#############################################################################################
## addin interaction plot
long_c <- as.factor(ifelse(long<=-81.65,0,1))
depth <- as.numeric(data$Sdepth)

mod <- multinom(res~long_c * depth)

model.matrix(mod)

dses <- data.frame(long_c=as.factor(rep(0,500)), depth = seq(min(depth),max(depth),length.out=500))
new.0 <- cbind(dses, predict(mod, newdata = dses, "probs"))

dses <- data.frame(long_c=as.factor(rep(1,500)), depth = seq(min(depth),max(depth),length.out=500))
new.1 <- cbind(dses, predict(mod, newdata = dses, "probs"))

png("2_Central_Basin/1_trawl_species_comp/1_Catch_comp_by_depth_EW.png", units="in",pointsize=12,width=6,height=6,res=600)

par(mfrow=c(2,1))
plot(new.0$depth,new.0$ES, ylim=c(0,0.8), typ="l", col="blue", lwd=2, ylab="Proportion of Catch", xlab="Depth (m)")
lines(new.0$depth,new.0$RSYAO, col="gray", lwd=2)
lines(new.0$depth,new.0$RSYOY, col="gray", lty=3, lwd=2)
lines(new.0$depth,new.0$YPYOY, col="yellow", lty=1, lwd=2)
lines(new.0$depth,new.0$OTHER, col="red", lty=1, lwd=2)
legend("topleft",horiz=T,legend=c("RSYAO","RSYOY","ES","YPYOY","OTHER"), cex=0.65,
       lty=c(1,3,1,1,1), lwd=2,col=c("darkgray","lightgray","blue","yellow","red"), bty="n")
abline(v=20, col="black", lwd=3,lty=2)
text(23,0.65,"West strata", cex=1.5)


plot(new.1$depth,new.1$ES, ylim=c(0,0.8), typ="l", col="blue", lwd=2, ylab="Proportion of Catch", xlab="Depth (m)")
lines(new.1$depth,new.1$RSYAO, col="gray", lwd=2)
lines(new.1$depth,new.1$RSYOY, col="gray", lty=3, lwd=2)
lines(new.1$depth,new.1$YPYOY, col="yellow", lty=1, lwd=2)
lines(new.1$depth,new.1$OTHER, col="red", lty=1, lwd=2)
legend("topleft",horiz=T,legend=c("RSYAO","RSYOY","ES","YPYOY","OTHER"), cex=0.65,
       lty=c(1,3,1,1,1), lwd=2,col=c("darkgray","lightgray","blue","yellow","red"), bty="n")
abline(v=20, col="black", lwd=3,lty=2)
text(23,0.65,"East strata", cex=1.5)

dev.off()
#############################################################################################


#############################################################################################
## multinomial with factor variables (longitude and depth) - based on above results (updated)
#############################################################################################
## addin interaction plot
head(data)

data$long_c <- ifelse(data$LONG<=-81.65,"west","east")
data$depth_c <- ifelse(data$long_c=="west" & data$Sdepth <=20,"near",ifelse(data$long_c=="east" & data$Sdepth <=20,"near","off"))
table(data$long_c,data$depth_c)

long_c <- as.factor(data$long_c)
depth_c <- as.factor(data$depth_c)


mod <- multinom(res~long_c*depth_c)
fits <- data.frame(cbind(fitted(mod),long_c,depth_c))
head(fits)

model.frame(mod)
summary(mod)

RSYOY <- aggregate(RSYOY~long_c+depth_c,data=fits,FUN="mean")
RSYAO <- aggregate(RSYAO~long_c+depth_c,data=fits,FUN="mean")
ES    <- aggregate(ES~long_c+depth_c,data=fits,FUN="mean")
YPYOY <- aggregate(YPYOY~long_c+depth_c,data=fits,FUN="mean")
OTHER <- aggregate(OTHER~long_c+depth_c,data=fits,FUN="mean")

mat <- matrix(c(RSYOY[,3],
                RSYAO[,3],
                ES[,3],
                YPYOY[,3],
                OTHER[,3]),nrow=4,byrow=F)

png("2_Central_Basin/1_trawl_species_comp/2_Catch_comp_by_strata.png", units="in",pointsize=12,width=6,height=6,res=600)

par(mfrow=c(2,2), mar=c(3,3,1,1), oma=c(2,2,1,1))
sp_col <- c("darkgray","lightgray","blue","yellow","red")
nam <- c("RSYOY","RSYAO","ES","YPYOY","OTHER")
barplot(t(mat)[,4], col=sp_col, names=nam, main="West-Deep (_WD) combine", ylim=c(0,0.6), cex.names=0.65)
barplot(t(mat)[,3], col=sp_col, names=nam, main="East-Deep (_ED) combine", ylim=c(0,0.6), cex.names=0.65)
barplot(t(mat)[,2], col=sp_col, names=nam, main="West-Shallow (_WS) combine", ylim=c(0,0.6), cex.names=0.65)
barplot(t(mat)[,1], col=sp_col, names=nam, main="East-Shallow (_ES) combine", ylim=c(0,0.6), cex.names=0.65)
mtext("Proportion of Catch",2,0.5,outer=T, cex=1.2)
mtext("Species Groups",1,0.5,outer=T, cex=1.2)

dev.off()
#############################################################################################


#############################################################################################
## SpcGrp density by site depth and trawl foot rope depth
#############################################################################################
data %>% melt(id.vars=c("LAT","LONG","Sdepth","Fdepth","Layer","Year","long_c","depth_c"),
              variable.name="SpcGrp", value.name="catch") %>% filter(SpcGrp != "OTHER") -> data_melt
head(data_melt)
summary(data_melt)

(p <- data_melt %>% ggplot(aes(x=Sdepth, y=-Fdepth, size = catch), ) +
        geom_point(alpha=0.5) + xlim(10,25) + ylim(-25,0) +
        scale_size(range = c(.1, 24), name="Catch (N)") +
        facet_wrap(~SpcGrp ))

ggsave("2_Central_Basin/1_trawl_species_comp/3_SpcGrp_cat_by_depth.png", plot = p,  width = 7, height = 7, units = "in", dpi = 400)
#############################################################################################




#############################################################################################
## Map with final strata and trawls
#############################################################################################
source("2_Central_Basin/2_define_strata/central_basin_strata.R")


data_map <- data %>% st_as_sf(coords = c("LONG", "LAT"), crs = 4326) 

## bounding boxes   
cb_bound_box <- c(xmin = -82.40, ymin = 41.363, xmax = -80.5, ymax = 42.7205)
cb_bounds <- cb_bound_box %>% st_bbox() %>% st_as_sfc() %>% st_set_crs(4326)

## plot strata and 5 min grid - add historic transects
ggplot()+
        scale_x_continuous(limits = c(cb_bound_box["xmin"], cb_bound_box["xmax"]))+
        scale_y_continuous(limits = c(cb_bound_box["ymin"], cb_bound_box["ymax"]))+
        geom_sf(data = lake_erie, fill = NA, col = "grey30")+
        geom_sf(data = cb_strata_2, aes(fill = strata), col = "black", alpha=0.5)+
        geom_sf(data = cb_five_min_grid, fill = NA, lty=3, col = gray(0,0.2), alpha=0.05)+
        geom_sf(data =  data_map, fill = NA, pch = 16,  col = gray(1,1), size = 3) +
        geom_sf(data =  data_map, fill = NA, pch = 1,  col = gray(0,0.2), size=3) +
        scale_fill_manual(name = "Depth \nstrata", values = c("red", "orange", "yellow", "green",  "blue", "darkblue", "purple", "violet"),
                          labels = c("NWS", "NWD", "SWD", "SWS", "NES", "NED", "SED", "SES"),
                          breaks = c("NWS", "NWD", "SWD", "SWS", "NES", "NED", "SED", "SES")) +

        theme_minimal()+
        theme(axis.title = element_blank(), axis.text = element_blank())

ggsave("2_Central_Basin/1_trawl_species_comp/trawl_samples_by_strata.png", width = 8, height = 5, units = "in", dpi = 400)

#############################################################################################
