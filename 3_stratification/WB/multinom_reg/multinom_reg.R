# Sat Mar 21 19:33:26 2020 ------------------------------

# Bring in data
library(rstudioapi)
library(nnet)
library(effects)

## set directory to current folder (or manually set working directory with 'setwd')
## This folder should hold .tpl, .dat, reptoRlist1.R, and Automate.R files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

data <- read.csv("data_c_strata.csv", sep=",",header=T) 
head(data)



#############################################################################################
## multinomial with Stu strata
#############################################################################################
## addin interaction plot
res <- as.matrix(data[,10:15])
head(res)
strata <- as.factor(data$strata)
head(strata)

mod <- multinom(res~strata)
fits <- data.frame(cbind(fitted(mod),strata))
head(fits)

ALEWIFN   <- aggregate(ALEWIFN~strata,data=fits,FUN="mean")
GSHADN    <- aggregate(GSHADN~strata,data=fits,FUN="mean")
ESHIN     <- aggregate(ESHIN~strata,data=fits,FUN="mean")
MiscSHIN  <- aggregate(MiscSHIN~strata,data=fits,FUN="mean")
OtherSOFT <- aggregate(OtherSOFT~strata,data=fits,FUN="mean")
SPINCAT   <- aggregate(SPINCAT~strata,data=fits,FUN="mean")

mat <- matrix(c(ALEWIFN[,2],
                GSHADN[,2],
                ESHIN[,2],
                MiscSHIN[,2],
                OtherSOFT[,2],
                SPINCAT[,2]),nrow=6,byrow=F)


par(mfrow=c(2,3), mar=c(6,1.5,4,1.5),oma=c(4,4,1,1))
sp_col <- c("darkgray","lightgray","blue","yellow","red","orange")
nam <- c("ALEWIFN","GSHADN","ESHIN","MiscSHIN","OtherSOFT","SPINCAT")
lim=1
barplot(t(mat)[,1], col=sp_col, names=nam, main="Detroit", ylim=c(0,lim), las=2)
barplot(t(mat)[,2], col=sp_col, names=nam, main="Pigeon Bay", ylim=c(0,lim), las=2)
barplot(t(mat)[,3], col=sp_col, names=nam, main="Central Basin", ylim=c(0,lim), las=2)
barplot(t(mat)[,4], col=sp_col, names=nam, main="Maumee", ylim=c(0,lim), las=2)
barplot(t(mat)[,5], col=sp_col, names=nam, main="Islands", ylim=c(0,lim), las=2)
barplot(t(mat)[,6], col=sp_col, names=nam, main="Sandusky", ylim=c(0,lim), las=2)
mtext("Proportion of Catch",2,2,outer=T, cex=1.2)
mtext("Species Groups",1,1.5,outer=T, cex=1.2)






mat <- matrix(c(ALEWIFN[,2],
                GSHADN[,2],
                ESHIN[,2],
                MiscSHIN[,2],
                OtherSOFT[,2]),nrow=6,byrow=F)


par(mfrow=c(2,3), mar=c(6,1.5,4,1.5),oma=c(4,4,1,1))
sp_col <- c("darkgray","lightgray","blue","yellow","red")
nam <- c("ALEWIFN","GSHADN","ESHIN","MiscSHIN","OtherSOFT")
lim=0.35
barplot(t(mat)[,1], col=sp_col, names=nam, main="Detroit", ylim=c(0,lim), las=2)
barplot(t(mat)[,2], col=sp_col, names=nam, main="Pigeon Bay", ylim=c(0,lim), las=2)
barplot(t(mat)[,3], col=sp_col, names=nam, main="Central Basin", ylim=c(0,lim), las=2)
barplot(t(mat)[,4], col=sp_col, names=nam, main="Maumee", ylim=c(0,lim), las=2)
barplot(t(mat)[,5], col=sp_col, names=nam, main="Islands", ylim=c(0,lim), las=2)
barplot(t(mat)[,6], col=sp_col, names=nam, main="Sandusky", ylim=c(0,lim), las=2)
mtext("Proportion of Catch",2,2,outer=T, cex=1.2)
mtext("Species Groups",1,1.5,outer=T, cex=1.2)
#############################################################################################












#############################################################################################
## multinomial with contiuous variables (longitude and depth)
#############################################################################################
res <- as.matrix(data[,12:17])
head(res)
## predictors
long <- as.numeric(data$Longitude)
lat <- as.numeric(data$Latitude)
depth <- as.numeric(data$Depth)
btemp <- as.numeric(data$Btemp)
secchi  <- as.numeric(data$secchi)

mod <- multinom(res~long * lat * depth)
summary(mod)

dses <- data.frame(long=seq(min(long),max(long),length.out=1000), lat = rep(mean(lat),1000), depth = rep(mean(depth),1000))
new <- cbind(dses, predict(mod, newdata = dses, "probs"))
head(new)

par(mfrow=c(2,1), mar=c(4,4,2,1))
plot(new$long,new$ALEWIFN, ylim=c(0,1), typ="l", col="blue", lwd=2, ylab="Proportion of Catch", xlab="Longitude (dd)")
lines(new$long,new$GSHADN, col="gray", lwd=2)
lines(new$long,new$ESHIN, col="gray", lty=3, lwd=2)
lines(new$long,new$MiscSHIN, col="yellow", lty=1, lwd=2)
lines(new$long,new$OtherSOFT, col="red", lty=1, lwd=2)
lines(new$long,new$SPINCAT, col="orange", lty=1, lwd=2)
legend("topleft",horiz=F,legend=c("ALEWIFN","GSHADN","ESHIN","MiscSHIN","OtherSOFT","SPINCAT"), cex=0.65,
       lty=c(1,3,1,1,1), lwd=2,col=c("blue","darkgray","lightgray","yellow","red","orange"), bty="n")
abline(v=-82.8, col="black", lwd=3,lty=2)


dses <- data.frame(long=rep(mean(long),1000), lat = seq(min(lat),max(lat),length.out=1000), depth = rep(mean(depth),1000))
new <- cbind(dses, predict(mod, newdata = dses, "probs"))

plot(new$lat,new$ALEWIFN, ylim=c(0,1), typ="l", col="blue", lwd=2, ylab="Proportion of Catch", xlab="Lattitude (dd)")
lines(new$lat,new$GSHADN, col="gray", lwd=2)
lines(new$lat,new$ESHIN, col="gray", lty=3, lwd=2)
lines(new$lat,new$MiscSHIN, col="yellow", lty=1, lwd=2)
lines(new$lat,new$OtherSOFT, col="red", lty=1, lwd=2)
lines(new$lat,new$SPINCAT, col="orange", lty=1, lwd=2)
legend("topleft",horiz=F,legend=c("ALEWIFN","GSHADN","ESHIN","MiscSHIN","OtherSOFT","SPINCAT"), cex=0.65,
       lty=c(1,3,1,1,1), lwd=2,col=c("blue","darkgray","lightgray","yellow","red","orange"), bty="n")
abline(v=8, col="black", lwd=3,lty=2)


dses <- data.frame(long=rep(mean(long),1000), lat = rep(mean(lat),1000),depth = seq(min(depth),max(depth),length.out=1000))
new <- cbind(dses, predict(mod, newdata = dses, "probs"))

plot(new$depth,new$ALEWIFN, ylim=c(0,1), typ="l", col="blue", lwd=2, ylab="Proportion of Catch", xlab="Depth (m)")
lines(new$depth,new$GSHADN, col="gray", lwd=2)
lines(new$depth,new$ESHIN, col="gray", lty=3, lwd=2)
lines(new$depth,new$MiscSHIN, col="yellow", lty=1, lwd=2)
lines(new$depth,new$OtherSOFT, col="red", lty=1, lwd=2)
lines(new$depth,new$SPINCAT, col="orange", lty=1, lwd=2)
legend("topleft",horiz=F,legend=c("ALEWIFN","GSHADN","ESHIN","MiscSHIN","OtherSOFT","SPINCAT"), cex=0.65,
       lty=c(1,3,1,1,1), lwd=2,col=c("blue","darkgray","lightgray","yellow","red","orange"), bty="n")
abline(v=8, col="black", lwd=3,lty=2)
#############################################################################################


#############################################################################################
## multinomial with factor variables (longitude and depth) - based on above results
#############################################################################################
## addin interaction plot
long_c <- ifelse(long<=-82.8,1,0)
depth_c <- ifelse(depth<=8,1,0)

mod <- multinom(res~as.factor(long_c) * as.factor(depth_c))
fits <- data.frame(cbind(fitted(mod),long_c,depth_c))
head(fits)

ALEWIFN   <- aggregate(ALEWIFN~long_c+depth_c,data=fits,FUN="mean")
GSHADN    <- aggregate(GSHADN~long_c+depth_c,data=fits,FUN="mean")
ESHIN     <- aggregate(ESHIN~long_c+depth_c,data=fits,FUN="mean")
MiscSHIN  <- aggregate(MiscSHIN~long_c+depth_c,data=fits,FUN="mean")
OtherSOFT <- aggregate(OtherSOFT~long_c+depth_c,data=fits,FUN="mean")
SPINCAT   <- aggregate(SPINCAT~long_c+depth_c,data=fits,FUN="mean")

mat <- matrix(c(ALEWIFN[,3],
                GSHADN[,3],
                ESHIN[,3],
                MiscSHIN[,3],
                OtherSOFT[,3],
                SPINCAT[,3]),nrow=4,byrow=F)

par(mfrow=c(2,2), oma=c(2,2,1,1))
sp_col <- c("darkgray","lightgray","blue","yellow","red","orange")
nam <- c("ALEWIFN","GSHADN","ESHIN","MiscSHIN","OtherSOFT","SPINCAT")
barplot(t(mat)[,2], col=sp_col, names=nam, main="West-Offshore", ylim=c(0,1))
barplot(t(mat)[,1], col=sp_col, names=nam, main="East-Offshore", ylim=c(0,1))
barplot(t(mat)[,4], col=sp_col, names=nam, main="West-Inshore", ylim=c(0,1))
barplot(t(mat)[,3], col=sp_col, names=nam, main="East-Inshore", ylim=c(0,1))
mtext("Proportion of Catch",2,0.5,outer=T, cex=1.2)
mtext("Species Groups",1,0.5,outer=T, cex=1.2)
#############################################################################################


#############################################################################################
## multinomial with longitude as factor and depth as continuous - based on above results 
#############################################################################################
## addin interaction plot
long_c <- as.factor(ifelse(data$Longitude<=-82.8,0,1))
depth <- as.numeric(data$Depth)

mod <- multinom(res~long_c * depth)

model.matrix(mod)

dses <- data.frame(long_c=as.factor(rep(0,500)), depth = seq(min(depth),max(depth),length.out=500))
new.0 <- cbind(dses, predict(mod, newdata = dses, "probs"))

dses <- data.frame(long_c=as.factor(rep(1,500)), depth = seq(min(depth),max(depth),length.out=500))
new.1 <- cbind(dses, predict(mod, newdata = dses, "probs"))


par(mfrow=c(2,1))
plot(new.0$depth,new.0$ALEWIFN, ylim=c(0,1), typ="l", col="blue", lwd=2, ylab="Proportion of Catch", xlab="Depth (m)")
lines(new.0$depth,new.0$GSHADN, col="gray", lwd=2)
lines(new.0$depth,new.0$ESHIN, col="gray", lty=3, lwd=2)
lines(new.0$depth,new.0$MiscSHIN, col="yellow", lty=1, lwd=2)
lines(new.0$depth,new.0$OtherSOFT, col="red", lty=1, lwd=2)
lines(new.0$depth,new.0$SPINCAT, col="orange", lty=1, lwd=2)
legend("topleft",horiz=F,legend=c("ALEWIFN","GSHADN","ESHIN","MiscSHIN","OtherSOFT","SPINCAT"), cex=0.65,
       lty=c(1,3,1,1,1), lwd=2,col=c("blue","darkgray","lightgray","yellow","red","orange"), bty="n")
abline(v=8, col="black", lwd=3,lty=2)
text(15,0.5,"West strata", cex=1.5)


plot(new.1$depth,new.1$ALEWIFN, ylim=c(0,1), typ="l", col="blue", lwd=2, ylab="Proportion of Catch", xlab="Depth (m)")
lines(new.1$depth,new.1$GSHADN, col="gray", lwd=2)
lines(new.1$depth,new.1$ESHIN, col="gray", lty=3, lwd=2)
lines(new.1$depth,new.1$MiscSHIN, col="yellow", lty=1, lwd=2)
lines(new.1$depth,new.1$OtherSOFT, col="red", lty=1, lwd=2)
lines(new.1$depth,new.1$SPINCAT, col="orange", lty=1, lwd=2)
legend("topleft",horiz=F,legend=c("ALEWIFN","GSHADN","ESHIN","MiscSHIN","OtherSOFT","SPINCAT"), cex=0.65,
       lty=c(1,3,1,1,1), lwd=2,col=c("blue","darkgray","lightgray","yellow","red","orange"), bty="n")
abline(v=8, col="black", lwd=3,lty=2)
text(15,0.85,"East strata", cex=1.5)

#############################################################################################



#############################################################################################
## multinomial with factor variables (longitude and depth) - based on above results (updated)
#############################################################################################
## addin interaction plot
head(data)

data$long_c <- ifelse(data$LONG<=-81.65,"west","east")
data$depth_c <- ifelse(data$long_c=="west" & data$Sdepth <=17,"near",ifelse(data$long_c=="east" & data$Sdepth <=15,"near","off"))
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

par(mfrow=c(2,2), oma=c(2,2,1,1))
sp_col <- c("darkgray","lightgray","blue","yellow","red")
nam <- c("RSYOY","RSYAO","ES","YPYOY","OTHER")
barplot(t(mat)[,4], col=sp_col, names=nam, main="West-Offshore", ylim=c(0,0.6))
barplot(t(mat)[,3], col=sp_col, names=nam, main="East-Offshore", ylim=c(0,0.6))
barplot(t(mat)[,2], col=sp_col, names=nam, main="West-Inshore", ylim=c(0,0.6))
barplot(t(mat)[,1], col=sp_col, names=nam, main="East-Inshore", ylim=c(0,0.6))
mtext("Proportion of Catch",2,0.5,outer=T, cex=1.2)
mtext("Species Groups",1,0.5,outer=T, cex=1.2)
#############################################################################################














############
## hide
############
plot(data$Longitude,data$Btemp)
plot(data$Longitude,data$Stemp)

plot(data$Latitude,data$Btemp)
plot(data$Latitude,data$Stemp)

plot(data$Btemp,data$Depth)
plot(data$Stemp,data$Depth)

plot(data$year,data$Btemp)
plot(data$year,data$Stemp)

plot(data$Btemp,data$Stemp)



plot(data$Longitude,data$secchi)
plot(data$Latitude,data$secchi)

plot(data$secchi,data$Depth)
plot(data$year,data$secchi)
plot(data$secchi,data$Stemp)
############


