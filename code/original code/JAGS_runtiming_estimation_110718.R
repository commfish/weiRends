############################################################################
#  Weir missing passage estimator program   12/01/2016      
#  This program is designed to estimate missing run passage
#  using Hierarchical Bayesian method
#  The model specifies daily variance 
#  This is a R-code with R2jags Package and call for JAGS
#  You need to: 
#   1) Install R, 
#   2) Install Packages: coda R2jags and SDMTools
#   3) Download and Instal JAGS
#   4) Prepare csv file with csv format
############################################################################

############################################################################
#  Model assumption:  Normal distribution of Escapement Counts 
############################################################################

############################################################################
# 1.0  Set working environment                                 
############################################################################
rm(list=ls(all=TRUE))
library(coda)
library(lattice)
library(R2jags)
library(MCMCpack)

#library(R2WinBUGS)
library(SDMTools)

# Set working directory: Where your file is located 
working_dir='S:/REG3/Kuskokwim Research/data/Weir/Kogrukluk/passage estimates/2022/Supporting materials/'
# Input file is the one you created 
input_file = 'Chinook master.csv'
# results file is the one you created 
results_file = '2022_KOG_Chinook_results.csv'
# Specify directory where WinBUGS program is located


# Specify x-axis of graph and title.
# start day:  First day of the data enntry
start_day = 'Days from 6/26'
# gtitle: title name appears on the graph
gtitle = '2022_KOG_Chinook'

############################################################################
# 1.1   Import csv data to R                                   
############################################################################
# read table and put into data escape
escape <- read.csv(paste0(working_dir,input_file),header = TRUE)
# Find dimention of the dataset
dim(escape)
# x is days vector
x <- as.vector(escape$Day)
x2 <- as.Date(escape$Date,"%d-%b")
# nyrs is the number of years data have 
nyrs <- dim(escape)[2] -2
# ndays is the number of days  
ndays <- dim(escape)[1]
# Set an empty matrix y 


############################################################################
# 1.3  Data tarnsformation                             
############################################################################
y <- matrix(0,nyrs,ndays)
for (i in 1:ndays){
  for (j in 1:nyrs){
#     y[j,i]<-  log(escape[i,j+2]+0.01)
     y[j,i]<-  escape[i,j+2]+0.01  
     }
    }

############################################################################
#  2.0: Create JAGS Model code					   
############################################################################
############################################################################
#  2.0: Create JAGS Model code					   
############################################################################
jag.model<- function(){
for(j in 1:nyrs) {
    for(i in 1:ndays){
    y[j,i] ~ dnorm(theta[j,i],tausq[i])
#    y[j,i] ~ dpois(theta[j,i])
# Assume that run timing distribution takes log normal distribution 
    theta[j,i] <- a[j]*exp(-0.5*pow(log(x[i]/mu[j])/b[j],2))
# Assume that run timing distribution takes Extreme value distribution 
#   theta[j,i] <- a[j]*exp(-exp(-(x[i]-mu[j])/b[j])-(x[i]-mu[j])/b[j]+1)
# Assume that run timing distribution takes log-logistic distribution 
#   theta[j,i] <- (a[j]*(b[j]/mu[j])*pow((x[i]/mu[j]),b[j]-1))/pow(1+pow((x[i]/mu[j]),b[j]),2)   
 }
}
# a[] indicates the maximum height (amplitude) of the function a>0
# mu[] indicates the function peaks when x = mu mu>0 : Peak timing
# b[] indicates peak width of the function b>0 standard deviation

# Priors
for(j in 1:nyrs) {
# Normal distribution Positive only 
#  a: is independent not hierarhical 
   a[j] ~ dnorm(100,0.00001)%_%T(100,)
   b[j] ~ dnorm(b0,b0.prec)%_%T(0.1,)
   mu[j] ~ dnorm(mu0,mu0.prec)%_%T(10,)   
     }  
		b0 ~ dnorm(0.2,0.1)%_%T(0,)
		mu0 ~ dnorm(50,0.001)%_%T(0,)
		b0.prec <-1/b0.ssq 
    b0.ssq <- b0.sigma*b0.sigma
    b0.sigma ~ dunif(0,100)  
		mu0.prec <-1/mu0.ssq 
    mu0.ssq <- mu0.sigma*mu0.sigma
    mu0.sigma ~ dunif(0,100)  
    
## This assumes that variance of each year is independent.     
 for(i in 1:ndays) {    
	   tausq[i] <- pow(sigma[i],-2)
		sigma[i] ~ dunif(0,100) 
     }
}

############################################################################
#  3.0: Create JAGS data file                      
#  Initial value an change based on your data and model          
############################################################################

datnew<-list(nyrs=nyrs, ndays=ndays, x=x, y=y)

############################################################################
# 3.1   Calculate initial value used for Bayesian method       
############################################################################
# zs is empirical standard deviation for each 


mu <- rep(0,nyrs)
a <- rep(0,nyrs)
  for (j in 1:nyrs){
     a[j]<-  (max(escape[,j+2],na.rm=TRUE))    #can change to force not to use max, eg maxx1.5
     mu[j]<-  sum(x*escape[,j+2],na.rm=TRUE)/sum(escape[,j+2],na.rm=TRUE)
    }
    
# Calculate Initial Values 
zs <- rep(0,ndays)
for (i in 1:ndays){
     zs[i]<-  sd(y[,i],na.rm=TRUE)
    }
# add 0 when zs is NA 
zs[is.na(zs)] <- 0  

b <- rep(0.3,nyrs)
sigma <- zs
b0 <- mean(b)
mu0 <- median(mu)
b0.sigma <- 1
mu0.sigma <- 5
z <- rep(0,nyrs)

	
################################################################
# 3.2   Save Initial value as list file                        #
################################################################
inits <- function(){list(
a = a, 
mu = mu,
b = b,
b0 = b0,
mu0 = mu0,
b0.sigma = b0.sigma,
mu0.sigma = mu0.sigma
)
}

############################################################################
#  4.0: Run JAGS                                              
############################################################################
#Define the parameters (nodes) of interest 
parameters <- c('a','b','mu','y') 
	
#Run JAGS using the bugs() function
starttime=Sys.time()
sim <- jags(data=datnew, inits=inits, parameters.to.save=parameters, model.file= jag.model,n.chains=1, 
	n.iter=5000,n.burnin=1000,n.thin=2,DIC=TRUE, working.directory=working_dir)
#sim <- autojags(sim)   
   Sys.time()-starttime

# output summary
sink(paste(working_dir,results_file,sep=''))
print(sim)
sink()
sim_sum <- print(sim)
   
# data outputs
mcmc <- as.mcmc(sim)
post.samp <- as.matrix(mcmc)

############################################################################
# 5.0 Graphics 
############################################################################

############################################################################
#  Plot estimates of each parameter: Optional 
############################################################################
post.samp1 <-(post.samp[,substr(colnames(post.samp),1,1)!='y'])
nvars<-dim(post.samp1)[2]
nsamps<-dim(post.samp1)[1]
int<-50
for(j in seq(1,nvars,int)){
windows(h=6,w=12)
par(mfrow=c(5,10),mai=c(0.2,0.2,0.2,0.2))

# Trace plost for Chain1
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


############################################################################
#  5.1 Extract NA data											  #
############################################################################
na.list <- matrix(NA,nyrs,ndays)
for (i in 1:ndays){
  for (j in 1:nyrs){
     na.list[j,i]<- ifelse(is.na(y[j,i]),paste0('[',j,',',i,']'),NA) 
     }
    }
navector <- na.list[which(!is.na(na.list))]

############################################################################
#  5.2 Extract predicted observed data                                                                                                                    
############################################################################
#create data with expected value
t1 <-(post.samp[,substr(colnames(post.samp),1,1)=='y'])
#extract names:  this extracts only bracket part of the theta
t1.name <- substr(colnames(t1),2,15)
#change names:   this changes column name only bracket part of the theta
colnames(t1) <- t1.name
#Extract predicted data based on y2's blacket 
y2 <- t1[,navector]

#extract names: this extracts only First part of bracket (year id)
tyear <- substr(navector,2,3)
tyear <- ifelse(substr(tyear,2,2)== ',',substr(tyear,1,1),tyear)

############################################################################
#  5.3 Calculate median, 95% CI of missing dates           
############################################################################
y2med <- apply(y2[,1:dim(y2)[2]],2,median)
y2med <- ifelse(y2med<0,0,y2med)
y2low <- apply(y2[,1:dim(y2)[2]],2,function(x) quantile(x, 0.025))
y2low <- ifelse(y2low<0,0,y2low)
y2up <- apply(y2[,1:dim(y2)[2]],2,function(x) quantile(x, 0.975))

############################################################################
#  5.4 Calculate missing value for each year total:              
############################################################################
# Change names to year id 
colnames(y2) <- tyear
# Combine columns based on year id 
t3 <- as.data.frame(sapply(unique(colnames(y2)), function(x) rowSums(y2[, colnames(y2) == x, drop = FALSE])))
# Calculate median, 95% CI of missing dates  
# ym, ylow, and yup are median, 95% CI of annual passage of missing dates
ym <- round(apply(t3[,1:dim(t3)[2]],2,median),0)
ym <- ifelse(ym<0,0,ym)
ylow <- round(apply(t3[,1:dim(t3)[2]],2,function(x) quantile(x, 0.025)),0)
ylow<- ifelse(ylow<0,0,ylow)
yup <- round(apply(t3[,1:dim(t3)[2]],2,function(x) quantile(x, 0.975)),0)

############################################################################
#  5.5 Create data for graphs and outputs             
############################################################################
# 1.0: Extract years from ym
tname <- names(ym)
tname <- as.numeric(tname)
names(ym) <- tname

# Create vector that will include missing years
ym2 <- vector('numeric',nyrs)
yl2 <- vector('numeric',nyrs)
yu2 <- vector('numeric',nyrs)

for(i in tname){
  ym2[i] <- ym[as.numeric(names(ym))==i] 
  yu2[i] <- yup[as.numeric(names(yup))==i] 
  yl2[i] <- ylow[as.numeric(names(ylow))==i] 
}


############################################################################
# 5.1 Plot Run timing:  
############################################################################

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

est.esc <- matrix(0,ndays,nyrs)
for (i in 1:ndays){
  for (j in 1:nyrs){
     est.esc[i,j]<- ifelse(is.na(escape[i,j+2]), Modelesc[i,j],escape[i,j+2]) 
     }
    }
colSums(est.esc)

# y2m is a matrix of median passage estimates of all missing passage
y2m <- matrix(0,ndays,nyrs)
# y2u is a matrix of upper 95% CI passage estimates of all missing passage    
y2u <- matrix(0,ndays,nyrs)
# y2l is a matrix of upper 95% CI passage estimates of all missing passage    
y2l <- matrix(0,ndays,nyrs)

# Enter data to matrix

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

# plot graph
windows(h=6,w=12,record=TRUE)
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
# plot median passage stimate    
    points(x2,y2m[,i], pch=21, col='black',bg='white')
}

############################################################################
# 6.0 Data outputs        
############################################################################
############################################################################
# 6.1 Annual Observed and Esimated Missing Count         
############################################################################
# Extract Total observed counts 
esc.ob <- colSums(escape[,3:(nyrs+2)],na.rm = TRUE)
# Extract Year column 
year <- substr(names(esc.ob),2,5)
# Create data.frame with: year, observed, estimated, lower 95%CI, and upper 95%CI 
esc.sum <- data.frame(year,esc.ob, ym2,yl2,yu2)
# Calculate fraction of missing passage
esc.sum$p.est <- ym2/(esc.ob+ym2)
# Rename column name
names(esc.sum) <- c('year','observed','estimated','low.95%CI','upper.95%CI','% esimated')
# Write CSV file to working directory
write.csv(esc.sum,paste0(working_dir,paste0(gtitle,'_summary.csv')),na = '',row.names = FALSE)    

############################################################################
# 6.2  Daily Observed and Esimated Missing Count by year     
############################################################################
for(i in 1:nyrs){
# Create a data.frame with observed escapement, estimated, lower 95% CI, and upper 95% CI
esc.daily <- data.frame(escape$Date,escape[,2+i],round(y2m[,i],0),round(y2l[,i],0),round(y2u[,i],))
# Rename the column name 
names(esc.daily) <- c('Date','observed','estimated','low.95%CI','upper.95%CI')
# Write CSV file to working directory
write.csv(esc.daily,paste0(working_dir,paste0(gtitle,'_',year[i],'.csv')),na = '',row.names = FALSE)    
}
