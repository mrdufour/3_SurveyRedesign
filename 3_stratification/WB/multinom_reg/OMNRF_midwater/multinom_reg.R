# Tue Jun 02 12:28:06 2020 ------------------------------

# Bring in data
library(rstudioapi)
library(nnet)
library(effects)

## set directory to current folder (or manually set working directory with 'setwd')
## This folder should hold .tpl, .dat, reptoRlist1.R, and Automate.R files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

data <- read.csv("data_c_strata.csv", sep=",",header=T) 
data <- data[data$Total >0,]
data$TL_MODE2 <- ifelse(data$TL_MODE=="BOTTOM","BOTTOM","TOP-MIDDLE")
head(data)



#############################################################################################
## multinomial with Stu strata - upper middle and bottom of water column
#############################################################################################
## addin interaction plot
res <- as.matrix(data[,29:34])
head(res)
strata <- as.factor(data$strata)
head(strata)
levels(strata)
mode <- as.factor(data$TL_MODE2)
levels(mode)

mod <- multinom(res~strata+mode)
fits <- data.frame(cbind(fitted(mod),strata,mode))
head(fits)

ALEWIFN   <- aggregate(ALEWIFN~strata+mode,data=fits,FUN="mean")
GSHADN    <- aggregate(GSHADN~strata+mode,data=fits,FUN="mean")
ESHIN     <- aggregate(ESHIN~strata+mode,data=fits,FUN="mean")
MiscSHIN  <- aggregate(MiscSHIN~strata+mode,data=fits,FUN="mean")
OtherSOFT <- aggregate(OtherSOFT~strata+mode,data=fits,FUN="mean")
SPINCAT   <- aggregate(SPINCAT~strata+mode,data=fits,FUN="mean")



############################################################
## TOP
############################################################
# mat3 <- matrix(c(ALEWIFN[ALEWIFN$mode==3,3],
#                  GSHADN[GSHADN$mode==3,3],
#                  ESHIN[ESHIN$mode==3,3],
#                  MiscSHIN[MiscSHIN$mode==3,3],
#                  OtherSOFT[OtherSOFT$mode==3,3],
#                  SPINCAT[SPINCAT$mode==3,3]),nrow=4,byrow=F)
# 
# 
# par(mfrow=c(2,2), mar=c(4,1.5,2,1.5),oma=c(4,4,1,1))
# sp_col <- c("darkgray","lightgray","blue","yellow","red","orange")
# nam <- c("ALEWIFN","GSHADN","ESHIN","MiscSHIN","OtherSOFT","SPINCAT")
# lim=1
# barplot(t(mat3)[,1], col=sp_col, names=nam, main="Central Basin", ylim=c(0,lim), las=2)
# barplot(t(mat3)[,2], col=sp_col, names=nam, main="Detroit R", ylim=c(0,lim), las=2)
# barplot(t(mat3)[,3], col=sp_col, names=nam, main="Islands", ylim=c(0,lim), las=2)
# barplot(t(mat3)[,4], col=sp_col, names=nam, main="Pidgeon Bay", ylim=c(0,lim), las=2)
# mtext("Proportion of Catch",2,2,outer=T, cex=1.2)
# mtext("Species Groups",1,1.5,outer=T, cex=1.2)

############################################################



############################################################
## TOP-MIDDLE
############################################################
mat2 <- matrix(c(ALEWIFN[ALEWIFN$mode==2,3],
                 GSHADN[GSHADN$mode==2,3],
                 ESHIN[ESHIN$mode==2,3],
                 MiscSHIN[MiscSHIN$mode==2,3],
                 OtherSOFT[OtherSOFT$mode==2,3],
                 SPINCAT[SPINCAT$mode==2,3]),nrow=4,byrow=F)


par(mfrow=c(2,2), mar=c(4,1.5,2,1.5),oma=c(4,4,1,1))
sp_col <- c("darkgray","lightgray","blue","yellow","red","orange")
nam <- c("ALEWIFN","GSHADN","ESHIN","MiscSHIN","OtherSOFT","SPINCAT")
lim=1
barplot(t(mat2)[,1], col=sp_col, names=nam, main="Central Basin", ylim=c(0,lim), las=2)
barplot(t(mat2)[,2], col=sp_col, names=nam, main="Detroit R", ylim=c(0,lim), las=2)
barplot(t(mat2)[,3], col=sp_col, names=nam, main="Islands", ylim=c(0,lim), las=2)
barplot(t(mat2)[,4], col=sp_col, names=nam, main="Pidgeon Bay", ylim=c(0,lim), las=2)
mtext("Proportion of Catch",2,2,outer=T, cex=1.2)
mtext("Species Groups",1,1.5,outer=T, cex=1.2)

############################################################


############################################################
## BOTTOM
############################################################
mat1 <- matrix(c(ALEWIFN[ALEWIFN$mode==1,3],
                GSHADN[GSHADN$mode==1,3],
                ESHIN[ESHIN$mode==1,3],
                MiscSHIN[MiscSHIN$mode==1,3],
                OtherSOFT[OtherSOFT$mode==1,3],
                SPINCAT[SPINCAT$mode==1,3]),nrow=4,byrow=F)


par(mfrow=c(2,2), mar=c(4,1.5,2,1.5),oma=c(4,4,1,1))
sp_col <- c("darkgray","lightgray","blue","yellow","red","orange")
nam <- c("ALEWIFN","GSHADN","ESHIN","MiscSHIN","OtherSOFT","SPINCAT")
lim=1
barplot(t(mat1)[,1], col=sp_col, names=nam, main="Central Basin", ylim=c(0,lim), las=2)
barplot(t(mat1)[,2], col=sp_col, names=nam, main="Detroit R", ylim=c(0,lim), las=2)
barplot(t(mat1)[,3], col=sp_col, names=nam, main="Islands", ylim=c(0,lim), las=2)
barplot(t(mat1)[,4], col=sp_col, names=nam, main="Pidgeon Bay", ylim=c(0,lim), las=2)
mtext("Proportion of Catch",2,2,outer=T, cex=1.2)
mtext("Species Groups",1,1.5,outer=T, cex=1.2)

############################################################