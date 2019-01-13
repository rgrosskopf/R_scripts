#-------------------------------------------------------------------------------------#
#Author: Ryan Grosskopf
#Date: April 2018
#Description: Perform quick-turnaround exploratory analysis on EV charging data of poor quality 
#and unknown origin. Lots and lots of clean-up tricks.
#-------------------------------------------------------------------------------------#


library(ggplot2)
library(lubridate)
library(tidyr)
library(zoo)
library(sqldf)
library(forecast)
library(corrplot)

library(tseries)
library(actuar)


####data loading and clean up####
#read in data file
EV<-read.csv(file="ElectricVehicleData.csv", header=T, sep=",", stringsAsFactors=F)
EV_raw<-read.csv(file="ElectricVehicleData.csv", header=T, sep=",", stringsAsFactors=F)

#remove duplicate rows
EV = EV[!duplicated(EV),]

#clean up names
EV$CustomerID = paste0("C",formatC(EV$CustomerID, width = 2, format = "d", flag = "0")) #zero pad and add prefix
EV$VehicleCom = EV$Vehicle
EV$VehicleCom = gsub("Ford Cmax Energi", "Ford Cmax", EV$VehicleCom)
EV$VehicleCom = gsub("Ford Fusion Energi", "Ford Fusion", EV$VehicleCom)
EV$VehicleCom = gsub("Leaf 3.3 kWand Ford Cmax", "Leaf 3.3 kW, Ford Cmax", EV$VehicleCom)
EV$VehicleCom = gsub("Tesla 60 kW$", "Tesla 60 kWh", EV$VehicleCom)
EV$VehicleCom = gsub("Tesla 85 kW$", "Tesla 85 kWh", EV$VehicleCom)
EV$VehicleCom = gsub("Tesla 85 kW $", "Tesla 85 kWh", EV$VehicleCom, ignore.case=T)
EV$VehicleCom = gsub("Tesla 85kWh", "Tesla 85 kWh", EV$VehicleCom)
EV$VehicleCom = gsub("Tesla 85 KW Performance", "Tesla 85 kWh Performance", EV$VehicleCom, ignore.case=T)

#group by Tesla, Leaf to VehicleComCom
EV$VehicleComCom = EV$VehicleCom
EV[grep("Tesla", EV$Vehicle), ]$VehicleComCom = "Tesla"
EV[grepl("Leaf", EV$VehicleCom) & !grepl(",", EV$VehicleCom), ]$VehicleComCom = "Leaf"

#add column indicating number of vehicles
EV$VehicleCt = 0
EV[grepl(",", EV$VehicleCom), ]$VehicleCt = 2
EV[!grepl(",", EV$VehicleCom), ]$VehicleCt = 1
EV[is.na(EV$Vehicle), ]$VehicleCt = 0

#add average kW draw over 5 minute intervals
EV$Energy_AvgkW = EV$Energy_WattHours * 12 / 1000 # convert to average kW over each 5 minute interval

table(EV$VehicleCt, EV$Vehicle)

#edit data types
EV$CustomerID = as.factor(EV$CustomerID)
EV$Vehicle = as.factor(EV$Vehicle)
EV$VehicleCom = as.factor(EV$VehicleCom)
EV$VehicleComCom = as.factor(EV$VehicleComCom)

#create time stamp with lubridate
EV$TimeStamp = mdy_hms(paste(EV$Date, EV$Time), tz = "US/Eastern")
#round time stamp to nearest 5 minute interval
EV$TimeStampRnd = round_date(EV$TimeStamp, unit="5 minutes")
summary(EV)


#Assume only one reading per customer per time stamp, select max per duplicate entry.
EV = sqldf("select CustomerID, max(Energy_WattHours), Date, Time, Vehicle, VehicleCom, VehicleComCom, VehicleCt, TimeStamp, TimeStampRnd from EV group by CustomerID, Date, Time, Vehicle, VehicleCom, VehicleComCom, VehicleCt, TimeStamp, TimeStampRnd")
#rename max(Energy_WattHours) back to standard
names(EV)[names(EV) == 'max(Energy_WattHours)'] = 'Energy_WattHours'
str(EV)
#raw: 2004178
#EV, duplicate rows removed: 1879153
#EV duplicate customer/time stamps removed with max consumption kept: 1753763

#check for outliers
max(EV$Energy_WattHours)
boxplot(EV$Energy_WattHours, ylim = c(0, 7000))
EV[EV$Energy_WattHours == 6267.30,] #outlier identified
test = subset(EV, CustomerID == "C35" & Date == "08-20-2015")
ggplot(subset(EV, CustomerID == "C35" & Date == "08-20-2015"),aes(x=TimeStampRnd,y=Energy_WattHours)) + geom_line()

#spike is momentary, remove
EV[EV$Energy_WattHours == 6267.30,]$Energy_WattHours = 0


####Create Contiguous TS for analysis####
##Spread / Recast TS data w/tidyr##
head(EV)
EV_melt = data.frame(EV$TimeStampRnd, EV$CustomerID, EV$Energy_WattHours)

#remove duplicate rows < NONE FOUND, same count as EV
#EV_melt = EV_melt[!duplicated(EV_melt),]

EV_cast = spread(EV_melt, EV.CustomerID, EV.Energy_WattHours)

#convert each customer TS to a zooreg object and then to a ts for fitting with forecast and longest segment ID, possibly also hts
EV_z <- read.zoo(EV_cast, format = "%Y-%m-%d %H:%M:%S", tz = "US/Eastern")
EV_ts = as.ts(EV_z)

#fill gaps in TS using linear interpolation with a maximum number of filled contiguous NAs
EV_ts.fill = na.approx(EV_ts, na.rm=FALSE, maxgap = 10)

#watt hours of consumption by CustomerID
sort(tapply(EV_melt$EV.Energy_WattHours, EV_melt$EV.CustomerID, FUN=sum))

#Count number of NAs for each customer
sort(colSums(is.na(EV_cast)))
colSums(is.na(EV_ts))

CustNAs=data.frame(CustomerID = colnames(EV_ts.fill), NAs = colSums(is.na(EV_ts.fill))) 

#Create multivariate TS with contiguous non-NA entries (37 filled, 31 not filled), NAs < 1000
EV_ts_cont = na.contiguous(EV_ts.fill[,subset(CustNAs, NAs < 1000)$CustomerID])
EV_z_cont = zoo(EV_ts_cont) #add times
head(time(EV_z_cont))


as_datetime(1440874200, tz="US/Eastern") #"2015-08-29 14:50:00 EDT", both filled and not filled
as_datetime(1443671700, tz="US/Eastern") #"2015-10-01", not filled end
as_datetime(1443153300, tz="US/Eastern") #"2015-09-25 01:20:00 EDT", filled end


#create total zoo/ts for contig period
EV.cont.df = data.frame(coredata(EV_z_cont))
EV_cont_tot = rowSums(EV.cont.df)
EV_total_z = zoo(EV_cont_tot, time(EV_z_cont))

plot(EV_total_z)

#trim to complete days for ease of use/display, Note 8/30/2015 is a Sunday.
EV_total_z = window(EV_total_z, start = as.integer(as.POSIXct("2015-08-30 00:00:00 EDT", tz = "US/Eastern")), end = as.integer(as.POSIXct("2015-09-24 23:55:00 EDT", tz = "US/Eastern")))
EV_z_cont = window(EV_z_cont, start = as.integer(as.POSIXct("2015-08-30 00:00:00 EDT", tz = "US/Eastern")), end = as.integer(as.POSIXct("2015-09-24 23:55:00 EDT", tz = "US/Eastern")))

plot(EV_z_cont*12/1000, main = "Selected Customers and Time Period, 5 min avg kWh", xlab = "2015-08-30 through 2015-09-24", xaxt="n", ylim = c(0,9))


plot(EV_total_z)

as_datetime(start(EV_total_z), tz="US/Eastern")
as_datetime(end(EV_total_z), tz="US/Eastern")
(1443153300-1440907200)/60/60/24
####Exploratory Analysis####

#calculate correlation among users...
corr_df = subset(data.frame(coredata(EV_z_cont)), select = -c(C48, C11, C20, C52, C54)) #remove Customers with zero consumption during observation period.
corrplot(cor(corr_df), method = "color")
hclust(cor(corr_df))
head(cor(corr_df))

#cluster users by correlation
dissimilarity <- 1 - abs(cor(corr_df))
distance <- as.dist(dissimilarity)
plot(hclust(distance), main="Dissimilarity = 1 - Abs(Correlation)", xlab="")


#Plot of total, min overall user, max overall user from continuous set and from EV (total set)

#use forecast to fit seasonal model to total consumption, w/ intervals
#Autocorrelation Function for Total (ID seasonality)
acf(EV_total_z) #shows high but decreasing correlation at all lags up to 30+
pacf(EV_total_z) #partial ACF, leaves off


#generate boxplots over 24 hour daily cycle; watt hours
EV_total_z = zoo(EV_total_z, frequency=288)
head(time(EV_total_z))
EV_total_ts<-ts(EV_total_z, frequency=288)
boxplot(EV_total_ts~cycle(EV_total_ts), xaxt="n", xlab = "Hour", ylab = "Consumption (Watt-Hours)", main = "Daily Distribution of Watt-Hours Consumed, 5 min Intervals") #examine daily seasonality
axis(1, at=seq(1, 288, by=12), labels=seq(1:24))

#generate boxplots over 24 hour daily cycle; average kw over 5 minute intervals
EV_total_z_kw = EV_total_z * (12 / 1000) #convert from w-h to avg kw draw over 5 minute intervals
head(time(EV_total_z_kw))
head(EV_total_z_kw)
EV_total_ts_kw<-ts(EV_total_z_kw, frequency=288)
boxplot(EV_total_ts_kw~cycle(EV_total_ts_kw), xaxt="n", xlab = "Hour", ylab = "Avg Demand, 5 min Intervals (kW)", 
        main = "Distribution of Avg Total kW Demand Over 24-hour Cycle, 5 min Intervals", sub = "Data Range: Aug 30 through Sep 24, 2015, 36 Customers") #examine daily seasonality
axis(1, at=seq(1, 288, by=12), labels=seq(1:24))

?boxplot

##Fit multi-seasonal TBATS model
#create multi-seasonal time series object
EV_total_kw.msts = msts(EV_total_ts_kw, seasonal.periods=c(288, 2016)) #time step in weeks instead of days.
EV_total_kw.msts <- window(EV_total_kw.msts,1.5) #time step in weeks to match msts output

#fit trend-less TBATS model
EV_total_kw.fit.tbats <- tbats(EV_total_kw.msts, use.trend=FALSE)
summary(EV_total_kw.fit.tbats)
plot(EV_total_kw.fit.tbats, xlab = "Weeks")

#plot forecast and witheld test set
plot(forecast(EV_total_kw.fit.tbats, 2016, level=FALSE), main="Total Sample Consumption Forecast", sub="Data Range: Aug 30 through Sep 24, 2015, 37 Customers", ylab="Demand, 5 min Avg (kW)", xlab="Weeks")
axis(1, at=seq(1, 288, by=12), labels=seq(1:24))

lines(DMA.ts.test)
legend("bottomright",inset=.01, c("Model", "Actual"), lty=1, col=c("blue","black"))

#calculate numeric fit statistics and accuracy summary metrics
accuracy(forecast(DMAtrain.fit.tbats, h=192),DMA.ts.test)
accuracy(forecast(DMAtrain80.fit.tbats, h=(145*.2*24)),DMA.ts.test)

?plot.forecast



#use hts for other graphics?

#Do customers have consistent vehicles? Yes. 7 customers do not have/charge EVs.
cars = as.data.frame.matrix(table(EV$VehicleComCom, EV$CustomerID)) 
cars[cars>0] = 1
cars$count = rowSums(cars)
cars = cars[order(-cars$count),]
cars$Vehicle = row.names(cars)
cars
cars_samp = 

#plot of number of customers with each (consolidated) car configuration (original)
ggplot(data=cars, aes(reorder(Vehicle, count), y=count)) +  geom_bar(stat="identity") + 
  labs(y = "Customer Count", x = "Cars", title = "Number of Customers by Car Configuration") + 
  coord_flip() + theme(plot.title = element_text(hjust = 0.5))

#plot of number of customers with each (consolidated) car configuration (studied sample)
ggplot(data=cars, aes(reorder(Vehicle, count), y=count)) +  geom_bar(stat="identity") + 
  labs(y = "Customer Count", x = "Cars", title = "Number of Customers by Car Configuration") + 
  coord_flip() + theme(plot.title = element_text(hjust = 0.5))


#Total customer consumption over observed period
cons = as.data.frame(tapply(EV$Energy_WattHours, EV$CustomerID, sum))
colnames(cons) = "Total_kWh"
cons$Total_kWh = cons$Total_kWh/1000 #convert watt hours to kWh
cons$CustomerID = row.names(cons)
#normalize consumption as not all customers have records at all times
cust_count = as.data.frame(table(EV$CustomerID)) #interval record count per customer
cons$Avg_kWh_p5min = cons$Total_kWh/cust_count$Freq
cons$Avg_kWh_pday = cons$Avg_kWh_p5min * 12 * 24

#plot of Average kWh Consumed per Monitored Day by Customer
ggplot(data=cons, aes(x=reorder(CustomerID, -Avg_kWh_pday), y=Avg_kWh_pday)) +  geom_bar(stat="identity") + 
  labs(y = "Avg kWh per Day", x = "Customer ID", title = "Average kWh Consumed per Monitored Day", subtitle = "Jun 1 - Sep 30, 2015") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Customer consumption over modeled period
cons.const = data.frame(CustomerID = names(colSums(EV.cont.df)), TotalWh = colSums(EV.cont.df))
cons.const = as.data.frame(cons.const)
cons.const$TotalkWh = cons.const$TotalWh/1000
cons.const$CustomerID = as.character(cons.const$CustomerID)
str(cons.const)

ggplot(data=cons.const, aes(reorder(CustomerID, -TotalkWh), y=TotalkWh)) +  geom_bar(stat="identity") + 
  labs(y = "Total kWh", x = "Customer ID", title = "Total kWh by Customer", subtitle = "Aug 29 - Sep 25, 2015") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  geom_vline(xintercept=8.5, linetype="dashed") +
  annotate("text", x=12, y=550, label= "50% of Cumulative Demand")


#create cumulative statistics for Pareto plot
library(dplyr)
d <- arrange(cons.const, desc(TotalkWh)) %>%
  mutate(
    cumsum = cumsum(TotalkWh),
    freq = round(TotalkWh / sum(TotalkWh), 5),
    cum_freq = cumsum(freq)
  )
d
#copy table to clipboard
write.table(d, "clipboard", sep="\t", row.names=FALSE)


#TS plot of consumption by customer
ggplot(EV, aes(x=TimeStampRnd,y=Energy_WattHours,colour=CustomerID,group=CustomerID)) + geom_line()

ggplot(subset(EV, TimeStamp > as.POSIXct("2015-06-02 11:00:00") & TimeStamp < as.POSIXct("2015-06-03 12:00:00")), 
       aes(x=TimeStampRnd,y=Energy_WattHours,colour=CustomerID,group=CustomerID)) + geom_line()

ggplot(subset(EV, TimeStamp > as.POSIXct("2015-08-05 00:00:00") & TimeStamp < as.POSIXct("2015-08-08 12:00:00")), 
       aes(x=TimeStampRnd,y=Energy_AvgkW,colour=CustomerID,group=CustomerID)) + geom_point()


table(EV_raw$CustomerID)








#----------------------------------
#Scratch work
#----------------------------------

#Generate sequence of time-date stamps
format( seq.POSIXt(as.POSIXct(Sys.Date()), as.POSIXct(Sys.Date()+1), by = "60 min"), "%H%M", tz="GMT")



test = subset(EV, TimeStampRnd >= as.POSIXct("2015-08-06 11:00:00", tz = "US/Eastern") & TimeStampRnd <= as.POSIXct("2015-08-08 21:00:00", tz = "US/Eastern"))
summary(test)
subset(EV_raw, Date == "08-08-2015")
EV_ts[, 'C04']

test=EV_cast[rowSums(is.na(EV_cast[,2:57])) != ncol(EV_cast)-2,2:57]
head(EV_cast[,2:57])
test = rowSums(is.na(EV_cast[,2:57]))
table(test)



dat_y<-(dat[,c(2:1130)])
dat_x<-(dat[,c(1)])
models <- list()
#
for(i in names(dat_y)){
  y <- dat_y[i]
  model[[i]] = lm( y~dat_x )
}


summary(EV_ts_df)
plot(EV_ts[,"total"])

test = rowSums(EV_cast[,2:ncol(EV_cast)], na.rm = FALSE)
head(EV_ts)
summary(test)


#Identify segments of continuous data
EV_ts_cont = na.contiguous(EV_ts[, c('C04',"C05","C01")])

summary(EV_ts_cont)
sum(is.na(EV_ts_cont))

?read.zoo
str(EV_cast)

?strptime

####TESTING####


#create pareto plot
library(qcc)
Avg_kWh_pday = cons$Avg_kWh_pday
names(Avg_kWh_pday) = cons$CustomerID
pareto.chart(Avg_kWh_pday, ylab = "Avg kWh per Day")










nrow(EV_raw)-nrow(EV)

EV_melt[15839,]
EV_melt[16010,]

EV[c(15839, 16010, 15896,16067),]
test = subset(EV, TimeStamp > as.POSIXct("2015-06-02 11:00:00") & TimeStamp < as.POSIXct("2015-06-03 12:00:00") & CustomerID == "C02")
test[order(test$TimeStamp),]
test[order(test$Energy_WattHours),]

install.packages("sqldf")
library(sqldf)
test2 = sqldf("select *, max(Energy_WattHours) from test group by CustomerID, Date, Time, Vehicle, VehicleCom, VehicleComCom, Vehicle Ct, TimeStamp, TimeStampRnd")
head(test[order(test$TimeStamp),])
head(test2[order(test2$TimeStamp),])

table(EV$Vehicle, EV$CustomerID)


#TEST tidyr spread() function#
set.seed(14)
stocks <- data.frame(time = as.Date('2009-01-01') + 0:9,
                     X = rnorm(10, 0, 1),
                     Y = rnorm(10, 0, 2),
                     Z = rnorm(10, 0, 4))
stocksm <- gather(stocks, stock, price, -time)
spread.stock <- spread(stocksm, stock, price)
head(spread.stock)

test = stocksm[-c(21:23),]
rownames(test) = c() #remove row names
spread.stock <- spread(test, stock, price)
#End spread() test SUCCESS!

str(cons)


cons = as.data.frame(tapply(EV$TimeStamp, EV$CustomerID, count))

table(EV$CustomerID)




cars[order(-cars$count),]
ggplot(cars, aes(x=reorder(Seller, Num), y=Avg_Cost)) +
  geom_bar(stat='identity') +
  coord_flip()

head(EV)
summary(cars)

summary(EV)

table(EV$Vehicle)

#setup time series objects

##RUN ACF!!
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
