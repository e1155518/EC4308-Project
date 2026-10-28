
rm(list=ls())

#Auxiliary function to compute root MSE (same as MSE before, but with square root):
RMSE <- function(pred, truth){ #start and end body of the function by { } - same as a loop 
  return(sqrt(mean((truth - pred)^2)))
} #end function with a return(output) statement. Here we can go straight to return because the object of interest is a simple function of inputs


#Install the HDeconometrics package used in Medeiros et al. (2019) for convenient estimation
#of LASSO and ElNet using information criteria (basically uses glmnet, and selects on criterion)

install.packages("githubinstall") #this package is needed to install packages from GitHub (a popular code repository)
library(githubinstall)

#Here, some of you may encounter the following issue:
#You may see an error like this when trying to load the "githubinstall" library:
#Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) :  namespace 'cli' 3.x.x is already loaded, but >= 3.x.x is required

#If you see this, the following should fix it (reinstall the "cli" package):
#remove.packages("cli")
#install.packages("cli")


 #install Medeiros et al's package, you will be prompted to say "Yes" to confirm the name of the installed package:

githubinstall("HDeconometrics")

#You will see something like:
#Suggestion:
#  - gabrielrvsc/HDeconometrics  Set of R functions for high-dimensional econometrics
#Do you want to install the package (Y/n)?  
#Type "Y" in the command line and press Enter

library(HDeconometrics)
#install.packages("sandwich")
library(sandwich) #library to estimate variance for DM test regression using NeweyWest()
library(randomForest)
library(hdm)
###########################
###########################
###########################


#Set working directory and load data:

#setwd("D:/data") #set your working directory here - or load data directly via RStudio interface

load("rawdata.RData") #load the FRED-MD pre-processed data used in Medeiros et al (2019) 


#We keep file/object names unchanged for ease of reference to the paper's replication materials (in case someone want to dig deeper for future projects/theses)

### Some preliminary data manipulation
Y=dados #this is the matrix of all variables, Y and X. It is in a format of a matrix with added column names.
dum=rep(0,nrow(Y)) # 1st step in creating the 2008 outlier dummy - a vector of zeros of conformable length to rest of the data
dum[which.min(Y[,1])]=1 #create dummy for outlier in 2008 - corresponds to the minimum of the first column, which is CPI inflation
Y=cbind(Y,dum=dum) #add dummy to data matrix

yy=Y[,1] #get the y variable - CPI inflation

nprev=180 #number of out-of-sample observations (test window )

#Timing convention used in all the forecasting functions: the rolling window passes the data up to period T to the
#function, and the resulting forecast is compared with y_{T+1}. For an h-step forecast the forecaster only has data
#up to T-h+1, so each function first drops the last h-1 observations of the window. The estimation window is
#therefore 491 observations for h=1, 489 for h=3, 486 for h=6 and 480 for h=12.

oosy=tail(yy,nprev) #auxiliary:get the out-of-sample true values (last 180 obs. using tail())

#create lags
rwtemp=embed(yy,13)
#Simple RW forecast:
rw1c=tail(rwtemp[,2],nprev)
rw3c=tail(rwtemp[,4],nprev)
rw6c=tail(rwtemp[,7],nprev)
rw12c=tail(rwtemp[,13],nprev)

#Collect RMSE's for randomw walk:
rw.rmse1=RMSE(oosy,rw1c)
rw.rmse3=RMSE(oosy,rw3c)
rw.rmse6=RMSE(oosy,rw6c)
rw.rmse12=RMSE(oosy,rw12c)

######################################
#Benchmark 2: AR(p) forecast
######################################

#Perform AR(p) forecasts using rolling window.
#See the file func-ar.R for the forecast construction details there

#Add the functions  in func-ar.R (must be in your working directory)
#Or simply open up func-ar.R and execute the function commands there
source("func-ar.R")


bar1c=ar.rolling.window(Y,nprev,1,1,type="fixed") #1-step AR forecast
bar3c=ar.rolling.window(Y,nprev,1,3,type="fixed") #3-step AR forecast
bar6c=ar.rolling.window(Y,nprev,1,6,type="fixed") #6-step AR forecast
bar12c=ar.rolling.window(Y,nprev,1,12,type="fixed") #12-step AR forecast

#Benchmark forecast graphics:

#Plot benchmark coefficients
#Here I use plot.ts(), which plots time series objects that are dated

#First, I make a coefficient matrix a time-series object by using ts() function
#Main reason: I want dates on the X-axis

#Syntax: ts(object, start=startdate, end=enddate, freq=frequency (periods per year))
arcoef.ts=ts(bar1c$coef, start=c(2001,1), end=c(2015,12), freq=12)
colnames(arcoef.ts)=c("Constant","Phi1","Phi2","Phi3","Phi4") #name columns to distinguish plots

#Plot all the coefficients over time (plot.ts() same as plot, but for tme series objects):
windows()
plot.ts(arcoef.ts, main="AR regression coefficients", cex.axis=1.5)

#Similarly, I create ts objects out of 1-step and 12-step benchmark forecasts
bench1.ts=ts(cbind(rw1c,bar1c$pred,oosy), start=c(2001,1), end=c(2015,12), freq=12)
colnames(bench1.ts)=c("RW","AR(4)","True Value")

bench12.ts=ts(cbind(rw12c,bar12c$pred,oosy), start=c(2001,1), end=c(2015,12), freq=12)
colnames(bench12.ts)=c("RW","AR(4)","True Value")

#Plot 1-step forecasts:

windows()
plot.ts(bench1.ts[,1], main="1-step Benchmark forecasts", cex.axis=1.5, lwd=1.8, col="blue", ylab="CPI Inflation")
points(bench1.ts[,2], type="l", col="red",lwd=1.8)
points(bench1.ts[,3], type="l", col="black",lwd=2)
legend("bottomleft",legend=c("RW","AR(4)","CPI Inflation"))

#Plot 12-step forecasts:

windows()
plot.ts(bench12.ts[,1], main="12-step Benchmark forecasts", cex.axis=1.5, lwd=1.8, col="blue", ylab="CPI Inflation")
points(bench12.ts[,2], type="l", col="red",lwd=1.8)
points(bench12.ts[,3], type="l", col="black",lwd=2)
legend("bottomleft",legend=c("RW","AR(4)","CPI Inflation"))

#AR forecasts RMSE:

ar.rmse1=bar1c$errors[1]
ar.rmse3=bar3c$errors[1]
ar.rmse6=bar6c$errors[1]
ar.rmse12=bar12c$errors[1]

############################################################################
#Penalized regression: LASSO forecasts (BIC, AIC, AICc, rlasso, post-LASSO)
############################################################################

#Add the functions  in func-lasso.R (must be in your working directory)
#Or simply open up func-lasso.R and execute the function commands there
source("func-lasso.R")
source("func-rlasso.R")

alpha=1 #set alpha=1 for LASSO

#Run forecasts for LASSO (BIC)
lasso1c=lasso.rolling.window(Y,nprev,1,1,alpha,IC="bic")
lasso3c=lasso.rolling.window(Y,nprev,1,3,alpha,IC="bic")
lasso6c=lasso.rolling.window(Y,nprev,1,6,alpha,IC="bic")
lasso12c=lasso.rolling.window(Y,nprev,1,12,alpha,IC="bic")

#LASSO(BIC) RMSE's
lasso.rmse1=lasso1c$errors[1]
lasso.rmse3=lasso3c$errors[1]
lasso.rmse6=lasso6c$errors[1]
lasso.rmse12=lasso12c$errors[1]


#Run forecasts for LASSO (AIC)
lasso1ca=lasso.rolling.window(Y,nprev,1,1,alpha,IC="aic")
lasso3ca=lasso.rolling.window(Y,nprev,1,3,alpha,IC="aic")
lasso6ca=lasso.rolling.window(Y,nprev,1,6,alpha,IC="aic")
lasso12ca=lasso.rolling.window(Y,nprev,1,12,alpha,IC="aic")

#LASSO(AIC) RMSE's
lassoa.rmse1=lasso1ca$errors[1]
lassoa.rmse3=lasso3ca$errors[1]
lassoa.rmse6=lasso6ca$errors[1]
lassoa.rmse12=lasso12ca$errors[1]



#Run forecasts for LASSO (AICc)
lasso1caic=lasso.rolling.window(Y,nprev,1,1,alpha,IC="aicc")
lasso3caic=lasso.rolling.window(Y,nprev,1,3,alpha,IC="aicc")
lasso6caic=lasso.rolling.window(Y,nprev,1,6,alpha,IC="aicc")
lasso12caic=lasso.rolling.window(Y,nprev,1,12,alpha,IC="aicc")

#LASSO(AICc) RMSE's
lassoac.rmse1=lasso1caic$errors[1]
lassoac.rmse3=lasso3caic$errors[1]
lassoac.rmse6=lasso6caic$errors[1]
lassoac.rmse12=lasso12caic$errors[1]

# rLASSO #
#Note the final input (FALSE) means no post-LASSO step:
rlasso1c=rlasso.rolling.window(Y,nprev,1,1,FALSE)
rlasso3c=rlasso.rolling.window(Y,nprev,1,3,FALSE)
rlasso6c=rlasso.rolling.window(Y,nprev,1,6,FALSE)
rlasso12c=rlasso.rolling.window(Y,nprev,1,12,FALSE)

# rLASSO RMSE's
rlasso.rmse1=rlasso1c$errors[1]
rlasso.rmse3=rlasso3c$errors[1]
rlasso.rmse6=rlasso6c$errors[1]
rlasso.rmse12=rlasso12c$errors[1]



# post-LASSO #
#Note the final input (TRUE) requests the post-LASSO step:
prlasso1c=rlasso.rolling.window(Y,nprev,1,1,TRUE)
prlasso3c=rlasso.rolling.window(Y,nprev,1,3,TRUE)
prlasso6c=rlasso.rolling.window(Y,nprev,1,6,TRUE)
prlasso12c=rlasso.rolling.window(Y,nprev,1,12,TRUE)

# post-LASSO RMSE's
prlasso.rmse1=prlasso1c$errors[1]
prlasso.rmse3=prlasso3c$errors[1]
prlasso.rmse6=prlasso6c$errors[1]
prlasso.rmse12=prlasso12c$errors[1]







########################################################################
#Penalized regression: Elastic Net (BIC, alpha=0.5)
########################################################################

alpha=0.5 #set alpha to 0.5 for elastic net

elasticnet1c=lasso.rolling.window(Y,nprev,1,1,alpha,IC="bic")
elasticnet3c=lasso.rolling.window(Y,nprev,1,3,alpha,IC="bic")
elasticnet6c=lasso.rolling.window(Y,nprev,1,6,alpha,IC="bic")
elasticnet12c=lasso.rolling.window(Y,nprev,1,12,alpha,IC="bic")

#See the RMSE
elnet.rmse1=elasticnet1c$errors[1]
elnet.rmse3=elasticnet3c$errors[1]
elnet.rmse6=elasticnet6c$errors[1]
elnet.rmse12=elasticnet12c$errors[1]

#######################################################
##Sparsity analysis over time plots for LASSO and ElNet
#######################################################

#Get nonzero coefficient numbers for different horizons (rLASSO)
c1c=rowSums(rlasso1c$coef != 0)
c3c=rowSums(rlasso3c$coef != 0)
c6c=rowSums(rlasso6c$coef != 0)
c12c=rowSums(rlasso12c$coef != 0)

#Create a ts object for the plot
lcoef.ts=ts(cbind(c1c,c3c,c6c,c12c), start=c(2001,1), end=c(2015,12), freq=12)
colnames(lcoef.ts)=c("1-step","3-step","6-step","12-step")
#Plot numbers of nonzero coefficients across the test window
windows()
plot.ts(lcoef.ts, main="Sparsity Analysis for rLASSO",cex.axis=1.5)

#Get nonzero coefficient numbers for different horizons (Elastic Net)
ce1c=rowSums(elasticnet1c$coef != 0)
ce3c=rowSums(elasticnet3c$coef != 0)
ce6c=rowSums(elasticnet6c$coef != 0)
ce12c=rowSums(elasticnet12c$coef != 0)

#Create a respective ts object for the plot
elcoef.ts=ts(cbind(ce1c,ce3c,ce6c,ce12c), start=c(2001,1), end=c(2015,12), freq=12)
colnames(elcoef.ts)=c("1-step","3-step","6-step","12-step")
#Plot numbers of nonzero coefficients across the test window:
windows()
plot.ts(elcoef.ts, main="Sparsity Analysis for ElNet",cex.axis=1.5)


########################################################################
#Random forest forecasts (Attention: these will take A WHILE to run)
#You may be better off loading the provided results file if you want
#to explore the results
########################################################################

#Add the functions  in func-rf.R (must be in your working directory)
#Or simply open up func-rf.R and execute the function commands there
source("func-rf.R")

rf1c=rf.rolling.window(Y,nprev,1,1)
rf3c=rf.rolling.window(Y,nprev,1,3)
rf6c=rf.rolling.window(Y,nprev,1,6)
rf12c=rf.rolling.window(Y,nprev,1,12)

#See the RMSE:
rf.rmse1=rf1c$errors[1]
rf.rmse3=rf3c$errors[1]
rf.rmse6=rf6c$errors[1]
rf.rmse12=rf12c$errors[1]



################################################
##Plot ML forecasts for 1 and 12-steps
##############################################

#Create the time series object collecting 1-step best=performing ML forecasts
ml1.ts=ts(cbind(oosy,rlasso1c$pred,rf1c$pred), start=c(2001,1), end=c(2015,12), freq=12)
colnames(ml1.ts)=c("True Value","rLASSO","RF")

#Create the time series object collecting 12-step best=performing ML forecasts
ml12.ts=ts(cbind(oosy,rlasso12c$pred,rf12c$pred), start=c(2001,1), end=c(2015,12), freq=12)
colnames(ml12.ts)=c("True Value","rLASSO","RF")

#Plot the graph for 1-step forecasts
windows()
plot.ts(ml1.ts[,1], main="1-step ML forecasts", cex.axis=1.5, lwd=2, ylab="CPI Inflation")
points(ml1.ts[,2], type="l", col="blue",lwd=2.8)
points(ml1.ts[,3], type="l", col="red",lwd=2.8)
legend("bottomleft", c("Inflation","rLASSO","RF"), lty=c(1,1,1) ,col=c("black","blue","red"))


#Plot the graph for 12-step forecasts
windows()
plot.ts(ml12.ts[,1], main="12-step ML forecasts", cex.axis=1.5, lwd=2, ylab="CPI Inflation")
points(ml12.ts[,2], type="l", col="blue",lwd=2.8)
points(ml12.ts[,3], type="l", col="red",lwd=2.8)
legend("bottomleft", c("Inflation","rLASSO","RF"), lty=c(1,1,1) ,col=c("black","blue","red"))



########################################################
###It's the final showdown: Diebold-Mariano (DM) tests
#########################################################

#####################################################
#Set up for the tests of Random Forest vs. AR benchmark
#####################################################

#Compute squared loss for different horizons (RF)
lrf1c=(oosy-rf1c$pred)^2
lrf3c=(oosy-rf3c$pred)^2
lrf6c=(oosy-rf6c$pred)^2
lrf12c=(oosy-rf12c$pred)^2

#Compute squared loss for different horizons (AR)
lar1c=(oosy-bar1c$pred)^2
lar3c=(oosy-bar3c$pred)^2
lar6c=(oosy-bar6c$pred)^2
lar12c=(oosy-bar12c$pred)^2

#Compute loss differentials (d_t) for different horizons (AR-RF)
darrf1=lar1c-lrf1c
darrf3=lar3c-lrf3c
darrf6=lar6c-lrf6c
darrf12=lar12c-lrf12c

#Create ts object containing loss differentials
dtarrf.ts=ts(cbind(darrf1,darrf3,darrf6,darrf12), start=c(2001,1), end=c(2015,12), freq=12)
colnames(dtarrf.ts)=c("1-step dt","3-step dt","6-step dt","12-step dt")
#Plot them to examine stationarity:
windows()
plot.ts(dtarrf.ts, main="Loss differential AR-RF",cex.axis=1.8)

#####################################################
#Set up for the tests of rLASSO vs. Random Forest
#####################################################

#Compute squared loss for different horizons (rLASSO)
lpl1c=(oosy-rlasso1c$pred)^2
lpl3c=(oosy-rlasso3c$pred)^2
lpl6c=(oosy-rlasso6c$pred)^2
lpl12c=(oosy-rlasso12c$pred)^2

#Compute loss differentials (d_t) for different horizons (rLASSO-RF)
dplrf1=lpl1c-lrf1c
dplrf3=lpl3c-lrf3c
dplrf6=lpl6c-lrf6c
dplrf12=lpl12c-lrf12c

#Create ts object containing loss differentials
dtplrf.ts=ts(cbind(dplrf1,dplrf3,dplrf6,dplrf12), start=c(2001,1), end=c(2015,12), freq=12)
#Plot them to examine stationarity:
colnames(dtplrf.ts)=c("1-step dt","3-step dt","6-step dt","12-step dt")
windows()
plot.ts(dtplrf.ts, main="Loss differential rLASSO-RF",cex.axis=1.8)

#DM regressions

#Regress d_t (AR-RF) for 1-step forecasts on a constant - get estimate of mean(d_t)
dmarrf1=lm(darrf1~1) #regression
acf(dmarrf1$residuals) #check serial correlation of residuals - number of significant autocorrelations is a good guess for number lags included in the HAC variance estimator
dmarrf1$coefficients/sqrt(NeweyWest(dmarrf1,lag=6)) #form the DM t-statistic

#NeweyWest(model,lag=p) computes the HAC variance estimator of Newey-West (1987), where model is the regresion output from lm, lag is the number of lags of serial correlation we want to correct for.
#Here examining the ACF above it seems like 6 lags are significant in the residual.
#A note of caution: these confidence bands for the ACF are not the typical Bartlett bands we see in ,e.g., Stata.
#As such, these will give you more significant lags than there probably is. 
#You can verify robustness of results by using the (nprev)^(1/3) rule of thumb for lag choice.
#This is what I do here: I use lag=6 since 180^(1/3)=5.64 - almost 6.

#For the rest of DM regressions, the structure is exactly the same, so I omit detailed comments:

#3-step forecast test
dmarrf3=lm(darrf3~1)
acf(dmarrf3$residuals)
dmarrf3$coefficients/sqrt(NeweyWest(dmarrf3,lag=6))

#6-step forecast test
dmarrf6=lm(darrf6~1)
acf(dmarrf6$residuals)
dmarrf6$coefficients/sqrt(NeweyWest(dmarrf6,lag=6))

#12-step forecast test
dmarrf12=lm(darrf12~1)
acf(dmarrf12$residuals)
dmarrf12$coefficients/sqrt(NeweyWest(dmarrf12,lag=6))



#rLASSO-RF

#1-step forecast test
dmplrf1=lm(dplrf1~1)
acf(dmplrf1$residuals)
dmplrf1$coefficients/sqrt(NeweyWest(dmplrf1,lag=6))

#3-step forecast test
dmplrf3=lm(dplrf3~1)
acf(dmplrf3$residuals)
dmplrf3$coefficients/sqrt(NeweyWest(dmplrf3,lag=6))

#6-step forecast test
dmplrf6=lm(dplrf6~1)
acf(dmplrf6$residuals)
dmplrf6$coefficients/sqrt(NeweyWest(dmplrf6,lag=6))

#12-step forecast test
dmplrf12=lm(dplrf12~1)
acf(dmplrf12$residuals)
dmplrf12$coefficients/sqrt(NeweyWest(dmplrf12,lag=6))

#Save results to workspace file for later use:
save.image("Lecture4_results.RData")