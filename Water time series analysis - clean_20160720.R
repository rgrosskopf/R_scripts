#-------------------------------------------------------------------------------------#
#Author: Ryan Grosskopf
#Date: July 20, 2016
#Description: Perform multi-seasonal decomposition of water demand time series to determine base demand
#pattern and to evaluate the distribution of the model residuals. This distribution will be used
#in a Monte Carlo model to perturb the base demand pattern. DMA = Demand Management Area.
#-------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------#
####Fit time series model####
#-------------------------------------------------------------------------------------#
#install.packages("forecast", "fitdistrplus", "tseries", "actuar")

#load required packages
library(forecast)
library(fitdistrplus)
library(tseries)
library(actuar)

#read in data file
DMA.df<-read.csv(file="TSExt_TWS-PMA05_10120_02-hourly.csv", header=T, sep=",", stringsAsFactors=F)

#setup time series objects
DMA.ts<-ts(data=DMA.df$Units, frequency=24)

boxplot(DMA.ts~cycle(DMA.ts)) #examine daily seasonality

DMA.ts.train <- window(DMA.ts,1,137)
DMA.msts <- msts(DMA.ts.train, seasonal.periods=c(24, 168)) #time step in weeks instead of days.

DMA.ts.test <- window(ts(data=DMA.df$Units, frequency=168),20.5,22) #time step in weeks to match msts output

#fit trend-less TBATS model
DMAtrain.fit.tbats <- tbats(DMA.msts, use.trend=FALSE)
summary(DMAtrain.fit.tbats)
plot(DMAtrain.fit.tbats)

#plot forecast and witheld test set
plot(forecast(DMAtrain.fit.tbats, 192, level=c(97.5)), main="TBATS model fit", sub="16 Sep 2012 through 22 Sept 2012", ylab="Flow", xlab="Weeks", xlim=c(19,22))
lines(DMA.ts.test)
legend("bottomright",inset=.01, c("Model", "Actual"), lty=1, col=c("blue","black"))

#calculate numeric fit statistics and accuracy summary metrics
accuracy(forecast(DMAtrain.fit.tbats, h=192),DMA.ts.test)
accuracy(forecast(DMAtrain80.fit.tbats, h=(145*.2*24)),DMA.ts.test)


#-------------------------------------------------------------------------------------#
####Determine distirbution and parameters for stochastic component of demand model####
#-------------------------------------------------------------------------------------#
#calculate normalized and raw errors
res.raw<-as.numeric(DMAfull.fit.tbats$errors)
res.norm<-as.numeric(DMAfull.fit.tbats$errors) / mean(DMA.msts)

#check error distribution
hist(res.norm, 100, col="black", main="TWS-PM A05_10120_02, TBATS Residuals")

#examine fit plots
descdist(res.norm, boot=1000)
plotdist(DMA.df$Units)

qqnorm(res.norm)
qqline(res.norm)
shapiro.test(residuals(DMA.fit.tbats))

#fit distributions, review results and plot
dist.norm<-fitdist(res.norm, "norm")
dist.logistic<-fitdist(res.norm, "logis")
plot(dist.norm)
plot(dist.logistic)
summary(dist.norm)
summary(dist.logistic)

#generate goodness of fit statistics
gofstat(list(dist.norm, dist.logistic), fitnames = c("norm","logistic"))

#generate comparison plots
denscomp(list(dist.norm,dist.logistic),addlegend=TRUE, xlab="Normalized Residual Magnitude", legendtext=c("Normal","Logistic"),xlegend = "topright", breaks=40)
qqcomp(list(dist.norm,dist.logistic),legendtext=c("Normal","Logistic"), main="Residuals Distribution Fitting",xlegend = "bottomright",line01 = TRUE, fitpch=16)
ppcomp(list(dist.norm,dist.logistic),legendtext=c("Normal","Logistic"), main="Residuals Distribution Fitting",xlegend = "bottomright",line01 = TRUE, fitpch=16)

#Export TBATS model component output if desired.
write.table(as.data.frame(tbats.components(DMA.fit.tbats)), file = "C:\\Users\\USER\\FILEPATH\\TBATS_components.csv", sep = ",", col.names = NA, qmethod = "double")

#-------------------------------------------------------------------------------------#
####FIT EVENT PARAMETERS####
#-------------------------------------------------------------------------------------#
##water required for fires, uses log-logistic distribution
FireWaterReq.dist<-fitdist(DMA.df$Units, "llogis", start = list(shape = 1, scale = 500))
summary(FireWaterReq.dist)

##break duration distribution, discrete distributions
#fit Poisson distribution
BreakDuration.pois <- fitdist(BreakDuration,"pois")
summary(BreakDuration.pois)
plot(BreakDuration.pois)
#fit Geometric distribution
BreakDuration.geom <- fitdist(BreakDuration,"geom")
summary(BreakDuration.geom)
plot(BreakDuration.geom)
#compare fit statistics
gofstat(list(BreakDuration.pois, BreakDuration.geom),fitnames = c("Poisson","Geometric"))


