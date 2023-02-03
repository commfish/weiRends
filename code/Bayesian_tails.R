#  Weir missing passage estimator program  3 Jan 2022     
#  This program is designed to estimate missing run passage using hierarchical Bayesian method
#  The model specifies daily variance 
#  This is a R-code with R2jags Package and call for JAGS.
#  Before running the code, download and install JAGS.

# This code was developed by Hamachan Hamazaki. See equations and methods here:
# Matter, A. N., and M. Tyers. 2020. Chinook salmon escapement in the Chena and Salcha Rivers and coho salmon
# escapement in the Delta Clearwater River, 2017. Alaska Department of Fish and Game, Fishery Data Series
# No. 20-01, Anchorage.

# model assumption:  Normal distribution of Escapement Counts 
# load packages
library(coda)
library(lattice)
library(R2jags)
library(MCMCpack)
library(tidyverse)

# name of results file
results_file = 'Chilkoot_sockeye_results.csv'
out.path <- paste0("output/Bayesian_tails/Chilkoot/test/")
if(!exists(out.path)){dir.create(out.path)}

# Specify x-axis of graph and title.
# start day:  First day of the data entry
start_day = 'Days from 5/27'
# gtitle: title name appears on the graph
gtitle = 'Chilkoot_sockeye'

# import csv file 
# read table and put into data escape
read.csv('data/Chilkoot_transform_sockeye.csv', header =TRUE) -> escape

# dimensions of the data set
dim(escape)
# d is days vector
x2 <- as.Date(escape$Date,"%d-%b")
# nyrs is the number of years data has
nyrs <- dim(escape)[2] -1
# ndays is the number of days  
ndays <- dim(escape)[1]
# Set an empty matrix y 
n.sigma = 50

# data transformation  (change data from columns to rows); adds 0.01 to each value; keeps NAs                          
y <- matrix(0,nyrs,ndays)
for (i in 1:ndays){
  for (j in 1:nyrs){
    # Add 0.01 so that the model will not fail. 
    y[j,i]<-  ifelse(escape[i,j+1]<=0,0.1,escape[i,j+1])
  }
}
dat <-list(nyrs=nyrs, ndays=ndays, y=y)
# create initial values
# calculate initial value used for Bayesian method   

nyrs <- dim(escape)[2] -1
ndays <- dim(escape)[1]
d <- seq(1,ndays)
sigma <- rep(0,ndays)
mu <- rep(0,nyrs)
a <- rep(0,nyrs)
b <- rep(0,nyrs)
  
for (j in 1:nyrs){
  a[j]<-  (max(escape[,j+1],na.rm=TRUE))    #can change to force not to use max, e.g., maxx1.5
  mu[j]<-  sum(d*escape[,j+1],na.rm=TRUE)/sum(escape[,j+1],na.rm=TRUE)
  b[j]<-   1/sqrt(log(sum(escape[,j+1],na.rm=TRUE)))
}

inits<- list(
    a =(a)*rnorm(nyrs,1,0.25), 
    mu = mu*rnorm(nyrs,1,0.25),
    b = b*rnorm(nyrs,1,0.1),
    a0 = median(a),
    b0 = median(b),
    mu0 = median(mu),
    a0.sigma = sd(a),
    mu0.sigma = sd(mu),
    b0.sigma = sd(b)
  )

inits

hyper<- list(
    a0m = median(inits$a),
    a0tau = 2/var(inits$a),
    b0m = median(inits$b),
    b0tau = 2/var(inits$b),
    mu0m = median(inits$mu),
    mu0tau = 2/var(inits$mu),
    sigma0 = n.sigma
  ) 
hyper

# create JAGS model code					   
jag.model<- function(){
for(j in 1:nyrs) {
    for(i in 1:ndays){
    y[j,i] ~ dnorm(theta[j,i],tausqd[j]) # normal error assumption
#    y[j,i] ~ dpois(theta[j,i])
# Assume that run timing distribution takes log normal distribution 
    theta[j,i] <- exp(a[j])*exp(-0.5*pow(log(i/mu[j])/b[j],2))
# Assume that run timing distribution takes Extreme value distribution 
#   theta[j,i] <- a[j]*exp(-exp(-(x[i]-mu[j])/b[j])-(x[i]-mu[j])/b[j]+1)
# Assume that run timing distribution takes log-logistic distribution 
#   theta[j,i] <- (a[j]*(b[j]/mu[j])*pow((x[i]/mu[j]),b[j]-1))/pow(1+pow((x[i]/mu[j]),b[j]),2)   
 }
}
# a[] indicates the maximum height (amplitude) of the function a>0
# mu[] indicates the function peaks when x = mu mu>0 : Peak timing
# b[] indicates peak width of the function b>0 standard deviation

# priors 
for(j in 1:nyrs) {
# Normal distribution positive only 
#  a: is independent not hierarchical 
  a[j] ~ dnorm(a0,a0.prec)%_%T(1,) # amplitude of the run peak parameter; normal distribution with mean mu                            # and precision tau; 
  b[j] ~ dnorm(b0,b0.prec)%_%T(0.2,) # width of the run peak
  mu[j] ~ dnorm(mu0,mu0.prec)%_%T(1,) # location by date of the run peak; https://www.scribbr.com/statistics/normal-distribution/
     }  
  # Rule of thumb prior
  # a log of the highest passage 
  # b 1/(log(total passage))
  # m peak passage date. 
  
  a0 ~ dnorm(a0m,a0tau)
  b0 ~ dnorm(b0m,b0tau)
  mu0 ~ dnorm(mu0m,mu0tau)
  a0.prec <-1/pow(a0.sigma,2)
  a0.sigma ~ dunif(0,2)  	
  b0.prec <-1/pow(b0.sigma,2)
  b0.sigma ~ dunif(0,2)  
  mu0.prec <-1/pow(mu0.sigma,2)
  mu0.sigma ~ dunif(0,10) 
    
## This assumes that variance of each year is independent.     
  for(i in 1:nyrs) {    
    sigmad[i] ~ dunif(0,sigma0)  
    tausqd[i] <-pow(sigmad[i],-2)
  }            
  
}

# create JAGS data file                      
# initial values can change based on your data and model; y is transformed data          
datnew<-list(nyrs=nyrs, ndays=ndays, y=y)

initss<-function(){ list(
  a =(a)*rnorm(nyrs,1,0.25), 
  mu = mu*rnorm(nyrs,1,0.25),
  b = b*rnorm(nyrs,1,0.1),
  a0 = median(a),
  b0 = median(b),
  mu0 = median(mu),
  a0.sigma = sd(a),
  mu0.sigma = sd(mu),
  b0.sigma = sd(b)
)
}
hyper<-hyper
# save initial value as list file   
parameters <- c('a','b','mu','y') 

# run JAGS using the bugs() function
starttime=Sys.time()
# full run
sim <- jags(data=append(datnew,hyper), inits=initss, parameters.to.save=parameters, model.file= jag.model,n.chains=1, #
	n.iter=5000,n.burnin=1000,n.thin=10,DIC=TRUE)
# test run
# sim <- jags(data=datnew, inits=inits, parameters.to.save=parameters, model.file= jag.model,n.chains=1, 
#            n.iter=500,n.burnin=100,n.thin=2,DIC=TRUE)
#sim <- autojags(sim)   
Sys.time()-starttime

# output summary
sink(paste0(out.path,results_file,sep=''))
print(sim)
sink()
sim_sum <- print(sim)
   
# data outputs
mcmc <- as.mcmc(sim)
post.samp <- as.matrix(mcmc)

# Graphics 
# plot estimates of each parameter: optional 
post.samp1 <-(post.samp[,substr(colnames(post.samp),1,1)!='y'])
nvars<-dim(post.samp1)[2]
nsamps<-dim(post.samp1)[1]
int<-(nyrs*3)+1
pdf(file=paste0(out.path,"/parameters.pdf"),height=6, width=12,onefile=T)
for(j in seq(1,nvars,int)){
par(mfrow=c(5,10),mai=c(0.2,0.2,0.2,0.2))
# trace plots for Chain1
for(i in 0:(int-1)){
	mindat<-min(post.samp1[,i+j])
	maxdat<-max(post.samp1[,i+j])
# plot density 
	plot(density(post.samp1[1:(nsamps),i+j]),col='blue',main=colnames(post.samp1)[i+j],xlim=c(mindat,maxdat))
#	lines(density(post.samp[1:nsamps,i+j]),col='red')
# plot trace plot
#	plot(post.samp[1:(nsamps),i+j],col='blue',main=names(post.samp)[i+j],ylim=c(mindat,maxdat),type='l')
#	lines(post.samp[1:nsamps,i+j],col='red')
}
}
dev.off()

# Extract NA data											  
na.list <- matrix(NA,nyrs,ndays)
for (i in 1:ndays){
  for (j in 1:nyrs){
     na.list[j,i]<- ifelse(is.na(y[j,i]),paste0('[',j,',',i,']'),NA) 
     }
    }
navector <- na.list[which(!is.na(na.list))]

# extract predicted observed data                                                                                                                    
# create data with expected value
t1 <-(post.samp[,substr(colnames(post.samp),1,1)=='y'])
# extract names:  this extracts only bracket part of the theta
t1.name <- substr(colnames(t1),2,15)
# change names:   this changes column name only bracket part of the theta
colnames(t1) <- t1.name
# extract predicted data based on y2's bracket 
y2 <- t1[,navector]
write.csv(y2, paste0(out.path,"y2med(1).csv",sep='')) # full iterations for each day for each year (e.g., 2000 for year 2003 day 30)
# extract names: this extracts only first part of bracket (year id)
tyear <- substr(navector,2,3)
tyear <- ifelse(substr(tyear,2,2)== ',',substr(tyear,1,1),tyear)

# calculate median, 95% CI of missing dates           
y2med <- apply(y2[,1:dim(y2)[2]],2,median)
y2med <- ifelse(y2med<0,0,y2med)
y2low <- apply(y2[,1:dim(y2)[2]],2,function(x) quantile(x, 0.025))
y2low <- ifelse(y2low<0,0,y2low)
y2up <- apply(y2[,1:dim(y2)[2]],2,function(x) quantile(x, 0.975))
write.csv(y2med, paste0(out.path,"y2med(2).csv",sep='')) # median of the iterations for that year and day (e.g., median of 2000 iterations; if < 0, then 0 for an iteration)

# calculate missing value for each year total:              
# change names to year id 
colnames(y2) <- tyear
y2 <- ifelse(y2<0,0,y2) # added this (Sara Miller 1_5_2023); I think the negative missing counts need to be replaced with 0 in the data set; IS THIS CORRECT???????????
# combine columns based on year id 
t3 <- as.data.frame(sapply(unique(colnames(y2)), function(x) rowSums(y2[, colnames(y2) == x, drop = FALSE])))
 write.csv(t3, paste0(out.path,"t3.csv",sep='')) # sum of the missing counts per iteration (e.g., t1 is the sum of the missing counts for iteration 1; includes negative values that should not be there)
# calculate median, 95% CI of missing dates  
# ym, ylow, and yup are median, 95% CI of annual passage of missing dates
ym <- round(apply(t3[,1:dim(t3)[2]],2,median),0)
ym <- ifelse(ym<0,0,ym) # ym is the median of the 2000 iterations (i.e., 2000 iterations for each missing value are computed and then all the missing values for one iteration are summed; then the median is applied)
ylow <- round(apply(t3[,1:dim(t3)[2]],2,function(x) quantile(x, 0.025)),0)
ylow<- ifelse(ylow<0,0,ylow)
yup <- round(apply(t3[,1:dim(t3)[2]],2,function(x) quantile(x, 0.975)),0)

# create data for graphs and outputs             
# extract years from ym
tname <- names(ym)
tname <- as.numeric(tname)
names(ym) <- tname

# create vector that will include missing years
ym2 <- vector('numeric',nyrs)
yl2 <- vector('numeric',nyrs)
yu2 <- vector('numeric',nyrs)

for(i in tname){
  ym2[i] <- ym[as.numeric(names(ym))==i] 
  yu2[i] <- yup[as.numeric(names(yup))==i] 
  yl2[i] <- ylow[as.numeric(names(ylow))==i] 
}

# plot run timing:  
Modelesc <- matrix(0,ndays,nyrs)
bug_summary <- sim_sum$summary 
am <- bug_summary[substr(row.names(bug_summary),1,2)=='a[',5]
bm <- bug_summary[substr(row.names(bug_summary),1,2)=='b[',5]
mum <- bug_summary[substr(row.names(bug_summary),1,2)=='mu',5]

for (i in 1:ndays){
  for (j in 1:nyrs){
#     Expected log normal run timing   
      Modelesc[i,j]<- (am[j]*exp(-0.5*(log(x[i]/mum[j])/bm[j])^2))
#     Expected Extreme value normal run timing   
#      Modelesc[i,j] <-am[j]*exp(-exp(-(x[i]-mum[j])/bm[j])-(x[i]-mum[j])/bm[j]+1)
#     Expected log logistic run timing
#     Modelesc[i,j] <- (am[j]*(bm[j]/mum[j])*((x[i]/mum[j])^(bm[j]-1))/(1+(x[i]/mum[j])^bm[j])^2)-1   
     }
    }
# output summary
est.esc <- matrix(0,ndays,nyrs)
for (i in 1:ndays){
  for (j in 1:nyrs){
     est.esc[i,j]<- ifelse(is.na(escape[i,j+2]), Modelesc[i,j],escape[i,j+2]) 
     }
    }
colSums(est.esc) # observed escapement and modeled escapement to fill in blanks

# y2m is a matrix of median passage estimates of all missing passage
y2m <- matrix(0,ndays,nyrs)
# y2u is a matrix of upper 95% CI passage estimates of all missing passage    
y2u <- matrix(0,ndays,nyrs)
# y2l is a matrix of upper 95% CI passage estimates of all missing passage    
y2l <- matrix(0,ndays,nyrs)

# enter data to matrix
final <- matrix(0,ndays,nyrs)
for (j in 1:nyrs){
  for (i in 1:ndays){
    if (is.na(na.list[j,i])){
        y2m[i,j] <- NA
        y2u[i,j] <- NA
        y2l[i,j] <- NA
        } else {
		    y2m[i,j] <- y2med[na.list[j,i]]
        y2u[i,j] <- y2up[na.list[j,i]]
        y2l[i,j] <- y2low[na.list[j,i]]
        }
    }
  }

write.csv(y2m, paste0(out.path,"y2m.csv",sep='')) # median of the 2000 iterations for each day for each year

# plot graph
pdf(file=paste0(out.path,"plots.pdf"),height=6, width=12,onefile=T)
#windows(h=6,w=12,record=TRUE)
par(mfrow=c(3,3),mai=c(.6,.5,.1,.1))
for(i in 1:nyrs){
	maxdat<-max(max(escape[,i+2],na.rm=T),max(y2u[,i],na.rm=T))
# plot observed passage
	plot(x2,escape[,i+2],col='blue', ylim = c(0,maxdat), xlab= start_day, ylab='escapement')
	legend('topright',bty ='n', c(gtitle,paste(substr(names(escape)[i+2],2,5)),paste('missing counts',ym2[i]),paste('95%CI ',yl2[i],' - ',yu2[i])))
# plot modeled run timing
	lines(x2,Modelesc[,i],col='red')
# plot 95% CI lines    
    arrows(x2,y0=y2u[,i],y1=y2l[,i],code=0)
# plot median passage estimate    
    points(x2,y2m[,i], pch=21, col='black',bg='white')
}
dev.off()

# data outputs        
# annual Observed and Estimated Missing Count         
# extract total observed counts 
esc.ob <- colSums(escape[,3:(nyrs+2)],na.rm = TRUE)
# extract year column 
year <- substr(names(esc.ob),2,5)
# create data frame with year, observed escapement, estimated escapement (median), lower 95%CI, and upper 95%CI 
esc.sum <- data.frame(year,esc.ob, ym2,yl2,yu2)
# calculate fraction of missing passage
esc.sum$p.est <- ym2/(esc.ob+ym2)
# rename column name
names(esc.sum) <- c('year','observed','estimated','low.95%CI','upper.95%CI','% estimated')
# write csv file to working directory (summary is based on t3 output; median of the 2000 iterations w/o negative values included)
write.csv(esc.sum,paste0(out.path, paste0(gtitle,'_summary.csv')),na = '',row.names = FALSE)    

# daily observed and estimated missing count by year     
for(i in 1:nyrs){
# create a data.frame with date, observed escapement, estimated (median), lower 95% CI, upper 95% CI, and the full modeled runtiming curve
esc.daily <- data.frame(escape$Date,escape[,2+i],round(y2m[,i],0),round(y2l[,i],0),round(y2u[,i],), round(Modelesc[,i],))
# rename the column name 
names(esc.daily) <- c('date','observed','estimated','low.95%CI','upper.95%CI', 'run_timing_curve')
# write csv file to working directory
write.csv(esc.daily,paste0(out.path, paste0(gtitle,'_',year[i],'.csv')),na = '',row.names = FALSE)    
}
# yearly csv files are the median of the 2000 iterations for the missing days of each year
