# Wed Jun 15 17:01:11 2022 ------------------------------

## function for efficiency analysis
eff_func <- function(data.x){

data.x$yr <- as.numeric(as.factor(data.x$Year))
labs <- unique(data.x$Year)

total_full <- NULL

for (k in length(labs):1){
  dat <- data.x[data.x$yr==k,]
  head(dat)
  dim(dat)
  length(dat$NperHa)
  table(dat$Transect)
  
m_tot <- NULL
se_tot <- NULL
rse_tot <- NULL
for(i in 0:(dim(dat)[1]-1)){
m_sims <- NULL
se_sims <- NULL
rse_sims <- NULL
for(j in 1:100){
  samp <- dat[sample(1:nrow(dat), dim(dat)[1]-i, replace=F),]
  m <- mean(samp$NperHa)
  m_sims <- c(m_sims,m)
  
  se <- (sd(samp$NperHa)/sqrt(length(samp$NperHa)))
  se_sims <- c(se_sims,se)
  
  rse <- 100*(se/m)
  rse_sims <- c(rse_sims,rse)
}

m_tot <- c(m_tot,m_sims) 
se_tot <- c(se_tot,se_sims)
rse_tot <- c(rse_tot,rse_sims)
}  



##  create matrix of replicate samples
sim.mat.m <- NULL
sim.mat.se <- NULL
sim.mat.rse <- NULL

sim.mat.m <- matrix(m_tot,nrow=100,ncol=dim(dat)[1], byrow=F)
sim.mat.se <- matrix(se_tot,nrow=100,ncol=dim(dat)[1], byrow=F)
sim.mat.rse <- matrix(rse_tot,nrow=100,ncol=dim(dat)[1],byrow=F)

## summaries matrix
mu <- NULL
se <- NULL
rse <- NULL

mu <- apply(sim.mat.m,2,mean)
se <- apply(sim.mat.se,2,mean)
rse <- apply(sim.mat.rse,2,mean)

## build dataset
summary_full <- NULL
summary_full <- data.frame(cbind(mu,se,rse,rep(100,dim(dat)[1]),seq(dim(dat)[1],1,by=-1)))
colnames(summary_full)[4] <- "Reps"
colnames(summary_full)[5] <- "EDSUs"
summary_full$perc_er <- abs(((summary_full$mu-summary_full$mu[1])/summary_full$mu[1]))*100
summary_full$perc <- summary_full$EDSUs/dim(dat)[1]
summary_full$Year <- k

total_full <- rbind(total_full,summary_full)

}

rep_yr <- data.frame(cbind(c(1:length(labs)),labs))
colnames(rep_yr) <- c("Year","Year_labs")
eff_out <<- merge(total_full,rep_yr,by="Year")
}

