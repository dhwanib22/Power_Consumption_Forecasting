library(zoo)
library(dplyr) 
library(TTR) 
library(forecast)
library(DMwR)
library(tseries) 
library(ggplot2)
library(lubridate)

setwd("D:/vu/terms/term 7/Probabiltiy and Statistics in Decision Modelling-II/End-Course_Hackathon")

#Data Understanding ====
## Load the file into R
ts_data= read.csv("Tetuan City power consumption.csv")

View(ts_data)

dim(ts_data)
str(ts_data)
summary(ts_data)                    

#Check for missing values
sum(is.na(ts_data))

#Basic Pre-processing ====

# Converting the date into Data format

# convert to date/time and retain as a new field
# date in the format:MonthDayYear Hour:Minute
ts_data$DateTime= as.POSIXlt(ts_data$DateTime, format="%m/%d/%Y %H:%M")
summary(ts_data$DateTime)

minute_data= ts_data$DateTime$min #0–59: minutes.
hourly_data= ts_data$DateTime$hour #0–23: hours.
wday_data= ts_data$DateTime$wday #0–6 day of the week, starting on Sunday.
yday_data= ts_data$DateTime$yday #0–365: day of the year (365 only in leap years).
mday_data= ts_data$DateTime$mday #1–31: day of the month
monthly_data=ts_data$DateTime$mon #0–11: months after the first of the year.
year_data= ts_data$DateTime$year #years since 1900.
date= date(data$DateTime)


################################ZONE1#####################################################
#Time-Series related Pre-processing ====
#Focusing on a particular time zone (Let's use only Time Zone 1 for analysis)

zone1= ts_data$Zone.1.Power.Consumption
DateTime= ts_data$DateTime

data1 = data.frame(DateTime, zone1)
View(data1)

# On the given date, power consumption has multiple values, 
#so one way is to consider the average power consumption.

hour_group_data= data.frame( zone1,hourly_data)

# Aggregate Data at month level from day level
# Derive Year and Month attribute 
hour_group_data$Month = as.numeric(format(monthly_data, format="%m"))
hour_group_data$week = as.numeric(wday_data)
hour_group_data$Day = as.numeric(format(yday_data, format="%d"))
head(hour_group_data)
View(hour_group_data)

pwr_cnsm_daily_avg = hour_group_data %>% 
  group_by(Day) %>% 
  summarise("zone1" = mean(zone1))
pwr_cnsm_daily_avg$week_day= as.factor(rep(c(1:7),52))
pwr_cnsm_daily_avg$Day= NULL

# Creating sequence Time variable.
pwr_cnsm_daily_avg$Time = 1:nrow(pwr_cnsm_daily_avg)

daily=pwr_cnsm_daily_avg
View(daily)
head(daily)
str(daily)

par(mfrow=c (1,1))
plot(daily$zone1, type = 'l')


# Splitting the Dataset into Train and Test
train = daily[1:357,]
val = daily[358:nrow(daily),]


#Regression on Time====
# Simple Linear Regression
lm1 = lm(zone1~Time, data = train)
summary(lm1)

# get predictions on train and validation 
pred_Train = predict(lm1)
pred_Val  = predict(lm1, val)

# plot the train data along with predictions
plot(train$zone1, type="l")
lines(train$Time, pred_Train, col="red", lwd=2)

# Evaluate model performance
lm1TrainError = regr.eval(train$zone1, pred_Train)
lm1ValError  = regr.eval(val$zone1, pred_Val)
lm1TrainError
lm1ValError

# Linear Regression Model using week_day Variable (to capture Seasonality)
str(train)
slm1 <- lm(zone1~., data=train)
summary(slm1)

#get predictions on train and test
pred_Train = predict(slm1)
pred_Val  = predict(slm1, val)

# plot the train data along with predictions
plot(train$zone1, type="l")
points(train$Time, pred_Train, type="l", col="red", lwd=2)

# Evaluate model performance
slm1TrainError = regr.eval(train$zone1, pred_Train)
slm1ValError = regr.eval(val$zone1, pred_Val)
slm1TrainError
slm1ValError

#Creating Time-Series Object====

# Converting data into R time series object 
findfrequency(daily)
train_TS <- ts(train$zone1, frequency =7 )
#train_TS
val_TS <- ts(val$zone1, frequency = 7)
#val_TS

# Visualize the time series Data
plot(train_TS, 
     type="l", lwd=3, col="blue", 
     xlab="Daily", ylab="Average Power Zone 1",
     main="Aggregated Daily Power Time series plot of Zone 1")
plot(val_TS, col="red", lwd=3)


#Moving Averages====
# Modelling  the time series using simple moving averages

fitsma = SMA(train_TS, n=3)
plot(train_TS, col='red',lwd=3)
lines(fitsma2,  col='blue',lwd=3)

fitsma_val = SMA(val_TS, n=3)
fitsma_val


# Weighted Moving Averages
fitwma = WMA(train_TS, n=2, 1:2)

plot(train_TS, col='red',lwd=3)
lines(fitwma, col='black',lwd=3)

#Simple Exponential Smoothing====
# Simple Exponential Smoothing Model
fitses = ses(train_TS, h=7, level = c(80,95))
plot(fitses, tcol="black", lwd=1.5, xlab="Weeks", ylab="Avg power consumption Zone 1")
lines(fitses$fitted, type="l", lwd=1.5, col="blue")
summary(fitses)

pred_Train = fitses$fitted
pred_Val = forecast(fitses, h=7)

sesTrainError = regr.eval(train$zone1, pred_Train)
sesValError = regr.eval(val$zone1, pred_Val$mean)
sesTrainError
sesValError

plot(fitses$fitted, col="black", lwd=2, xlab="Weeks", ylab="Mean Power Zone 1",
     main="Aggregated Daily Power Time series plot of Zone 1")
lines(train_TS, type="l", lwd=1, col="blue" )

lines(val_TS, col="red", lwd=1)

plot(decompose(train_TS))

#ETS Models=====
# ETS Models
fitets = ets(train_TS)
plot(fitets)
summary(fitets)

#fitets$fitted
#train_TS
head(data.frame(train_TS,fitets$fitted),10) #to compare original and model values

forecast_ets = forecast(fitets, h=7, level = c(80,95))
forecast_ets
val_pred = forecast_ets$mean
val_pred=as.numeric(val_pred)

plot(forecast_ets, col="black", lwd=1.5)
lines(train_TS, type="l", lwd=1, col="blue")

checkresiduals(fitets)
##𝐻_0:Zero correlation among residuals
##𝐻_1:Significant correlation among residuals

regr.eval(train_TS, fitets$fitted)
regr.eval(val_TS, val_pred)

### Auto Arima====
ARIMA_auto = auto.arima(train_TS)
summary(ARIMA_auto)

### Check the acf and pacf plots
par(mfrow=c(1,2))
Acf(ARIMA_auto$residuals)
Pacf(ARIMA_auto$residuals)

### KPSS test
#Null Hypothesis       : The series is trend (or level) stationary
# Alternative Hypothesis: The series is not stationary
kpss.test(ARIMA_auto$residuals, null="Trend")

## Predictions  
### Prediction on the Train  
pred_Train = fitted(ARIMA_auto)
pred_Test = forecast(ARIMA_auto, h = 7, level = c(80,95))

par(mfrow=c(1,1))
plot(pred_Test, col="black",lwd=1.5)
lines(pred_Train, type="l", lwd=1, col="blue")

### Find error for Arima_1 on both Test and Train data
regr.eval(train_TS, pred_Train)
regr.eval(val_TS, data.frame(pred_Test)$Point.Forecast)


################################ZONE2####################################################
#Time-Series related Pre-processing ====
#Focusing on a particular time zone (Let's use only Time Zone 1 for analysis)
zone2=ts_data$Zone.2..Power.Consumption
DateTime= ts_data$DateTime

data2= data.frame(DateTime, zone2)
View(data2)

hour_group_data2= data.frame(zone2, hourly_data)
View(hour_group_data2)

# Aggregate Data at month level from day level
# Derive Year and Month attribute 

hour_group_data2$Month = as.numeric(format(monthly_data, format="%m"))
hour_group_data2$week = as.numeric(wday_data)
hour_group_data2$Day = as.numeric(format(yday_data, format="%d"))
head(hour_group_data2)
View(hour_group_data2)

pwr_cnsm_daily_avg2 = hour_group_data2 %>% 
  group_by(Day) %>% 
  summarise("zone2" = mean(zone2))
pwr_cnsm_daily_avg2$week_day= as.factor(rep(c(1:7),52))
pwr_cnsm_daily_avg2$Day= NULL

# Creating sequence Time variable.
pwr_cnsm_daily_avg2$Time = 1:nrow(pwr_cnsm_daily_avg2)

daily2=pwr_cnsm_daily_avg2
View(daily2)
head(daily2)
str(daily2)

par(mfrow=c (1,1))
plot(daily2$zone2, type = 'l')

# Splitting the Dataset into train2 and Test
train2 = daily2[1:357,]
val2 = daily2[358:nrow(daily2),]

#Regression on Time====
# Simple Linear Regression
lm2 = lm(zone2~Time, data = train2)
summary(lm2)

# get predictions on train2 and validation 
pred_Train2 = predict(lm2)
pred_Val2  = predict(lm2, val2)

# plot the train2 data along with predictions
plot(train2$zone2, type="l")
lines(train2$Time, pred_Train2, col="red", lwd=2)

# Evaluate model performance
lm2TrainError = regr.eval(train2$zone2, pred_Train2)
lm2ValError  = regr.eval(val2$zone2, pred_Val2)
lm2TrainError
lm2ValError

# Linear Regression Model using week_day Variable (to capture Seasonality)
str(train2)
slm2 <- lm(zone2~., data=train2)
summary(slm2)

#get predictions on train and test
pred_Train2 = predict(slm2)
pred_Val2  = predict(slm2, val2)

# plot the train data along with predictions
plot(train2$zone2, type="l")
points(train2$Time, pred_Train2, type="l", col="blue", lwd=2)

# Evaluate model performance
slm2TrainError = regr.eval(train2$zone2, pred_Train2)
slm2ValError = regr.eval(val2$zone2, pred_Val2)
slm2TrainError
slm2ValError


#Creating Time-Series Object====

# Converting data into R time series object 
findfrequency(daily2)
train_TS2 <- ts(train2$zone2, frequency =7 )
# train_TS2
val_TS2 <- ts(val2$zone2, frequency = 7, start=c(358))
#val_TS2


# Visualize the time series Data
plot(train_TS2, 
     type="l", lwd=3, col="blue", 
     xlab="daily2", ylab="Mean Power Zone 1",
     main="Aggregated daily2 Power Time series plot of Zone 1")
plot(val_TS2, col="red", lwd=3)


#Moving Averages====
# Modelling  the time series using simple moving averages

fitsma2 = SMA(train_TS2, n=3)
plot(train_TS2, col='red',lwd=3)
lines(fitsma2,  col='blue',lwd=3)

fitsma_val2 = SMA(val_TS2, n=3)
fitsma_val2


# Weighted Moving Averages
fitwma2 = WMA(train_TS2, n=2, 1:2)

plot(train_TS2, col='red',lwd=3)
lines(fitwma2, col='black',lwd=3)

#Simple Exponential Smoothing====
# Simple Exponential Smoothing Model
fitses2 = ses(train_TS2, h=7, level = c(80,95))
plot(fitses2, col="black", lwd=1.5, xlab="Weeks", ylab="Avg power consumption Zone 2")
lines(fitses2$fitted, type="l", lwd=1.5, col="blue")
summary(fitses2)

pred_Train2 = fitses2$fitted
pred_Val2 = forecast(fitses2, h=7)
# Find error for slm2 on both Test and train2 data
sesTrainError2 = regr.eval(train2$zone2, pred_Train2)
sesValError2 = regr.eval(val2$zone2, pred_Val2$mean)
sesTrainError2
sesValError2

plot(fitses2$fitted, col="black", lwd=2, xlab="Weeks", ylab="Mean Power Zone 2",
     main="Aggregated Daily Power Time series plot of Zone 2")
lines(train_TS2, type="l", lwd=1, col="blue" )
plot(val_TS2, col="red", lwd=1)

plot(decompose(train_TS2))


#ETS Models=====
# ETS Models
fitets2 = ets(train_TS2)
plot(fitets2)
summary(fitets2)

#fitets2$fitted
#train_TS2
head(data.frame(train_TS2,fitets2$fitted),10) #to compare original and model values

forecast_ets2 = forecast(fitets2, h=7, level = c(80,95))
forecast_ets2
val_pred2 = forecast_ets2$mean
val_pred2=as.numeric(val_pred2)

plot(forecast_ets2, col="black", lwd=1.5)
lines(forecast_ets2$fitted, type="l", lwd=1, col="blue")

checkresiduals(fitets2)
##𝐻_0:Zero correlation among residuals
##𝐻_1:Significant correlation among residuals

regr.eval(train_TS2, fitets2$fitted)
regr.eval(val_TS2, val_pred2)

### Auto Arima====
ARIMA_auto2 = auto.arima(train_TS2)
summary(ARIMA_auto2)

### Check the acf and pacf plots
par(mfrow=c(1,2))
Acf(ARIMA_auto2$residuals)
Pacf(ARIMA_auto2$residuals)

### KPSS test
#Null Hypothesis       : The series is trend (or level) stationary
# Alternative Hypothesis: The series is not stationary
kpss.test(ARIMA_auto2$residuals,null="Trend")

## Predictions  
### Prediction on the train2  
pred_Train2 = fitted(ARIMA_auto2)
pred_Test2 = forecast(ARIMA_auto2, h = 7, level = c(80,95))

par(mfrow=c(1,1))
plot(pred_Test2, col="black", lwd=1.5)
lines(pred_Train2, type="l", lwd=1, col="blue")

### Find error for Arima2 on both Test and train2 data
regr.eval(train_TS2, pred_Train2)
regr.eval(val_TS2, data.frame(pred_Test2)$Point.Forecast)



################################ZONE3########################################################
#Time-Series related Pre-processing ====
#Focusing on a particular time zone (Let's use only Time Zone 1 for analysis)

zone3=ts_data$Zone.3..Power.Consumption
DateTime= ts_data$DateTime

data3= data.frame(DateTime, zone3)
View(data3)

hour_group_data3= data.frame(zone3, hourly_data)
View(hour_group_data3)

# Aggregate data3 at month level from day level
# Derive Year and Month attribute 
hour_group_data3$Month = as.numeric(format(monthly_data, format="%m"))
hour_group_data3$Day = as.numeric(format(yday_data, format="%d"))
head(hour_group_data3)
View(hour_group_data3)

pwr_cnsm_daily_avg3 = hour_group_data3 %>% 
  group_by(Day) %>%
  summarise("zone3" = mean(zone3))

# Creating sequence Time variable.
pwr_cnsm_daily_avg3$Time = 1:nrow(pwr_cnsm_daily_avg3)

daily3=pwr_cnsm_daily_avg3
View(daily3)
head(daily3)
str(daily3)

par(mfrow=c (1,1))
plot(daily3$zone3, type = 'l')

# Splitting the Dataset into train3 and Test
train3 = daily3[1:357,]
val3 = daily3[358:nrow(daily3),]

#Regression on Time====
# Simple Linear Regression
lm3 = lm(zone3 ~ Time, data = train3)
summary(lm3)

# get predictions on train3 and validation 
pred_Train3 = predict(lm3)
pred_val3  = predict(lm3, val3)

# plot the train3 data along with predictions
plot(train3$zone3, type="l")
lines(train3$Time, pred_Train3, col="red", lwd=2)

# Evaluate model performance
lm3TrainError = regr.eval(train3$zone3, pred_Train3)
lm3ValError  = regr.eval(val3$zone3, pred_val3)
lm3TrainError
lm3ValError

#Creating Time-Series Object====

# Converting data3 into R time series object 
findfrequency(daily3)
train_TS3 <- ts(train3$zone3, frequency =1 )
val_TS3 <- ts(val3$zone3, frequency = 1, start=c(358))

# Visualize the time series data3

plot(train_TS3, 
     type="l", lwd=3, col="blue", 
     xlab="daily3", ylab="Mean Power Zone 1",
     main="Aggregated daily3 Power Time series plot of Zone 1")
plot(val_TS3, col="red", lwd=3)


#Moving Averages====
# Modelling  the time series using simple moving averages

fitsma3 = SMA(train_TS3, n=3)
plot(train_TS3, col='red',lwd=3)
lines(fitsma3,  col='blue',lwd=3)

fitsma_val3 = SMA(val_TS3, n=3)
fitsma_val3


# Weighted Moving Averages
fitwma3 = WMA(train_TS3, n=2, 1:2)

plot(train_TS3, col='red',lwd=3)
lines(fitwma3, col='black',lwd=3)

#Simple Exponential Smoothing====
# Simple Exponential Smoothing Model
fitses3 = ses(train_TS3, h=7, level = c(80,95))
plot(fitses3, col="black", lwd=1.5, xlab="Weeks", ylab="Avg power consumption Zone 3")
lines(fitses3$fitted, type="l", lwd=1.5, col="blue")
summary(fitses3)

pred_Train3 = fitses3$fitted
pred_val3 = forecast(fitses3, h=7)

# Find error for slm3 on both Test and train3 data3
sesTrainError3 = regr.eval(train3$zone3, pred_Train3)
sesValError3 = regr.eval(val3$zone3, pred_val3$mean)
sesTrainError3
sesValError3

plot(fitses3$fitted, col="black", lwd=2, xlab="Weeks", ylab="Mean Power Zone 3",
     main="Aggregated Daily Power Time series plot of Zone 3")
lines(train_TS3, type="l", lwd=1, col="blue" )
plot(val_TS3, col="red", lwd=1)

plot(decompose(train_TS3))

#ETS Models=====
# ETS Models
fitets3 = ets(train_TS3)
plot(fitets3)

summary(fitets3)

#fitets$fitted
#train_TS3
head(data.frame(train_TS3,fitets3$fitted),10) #to compare original and model values

forecast_ets3 = forecast(fitets3,  h=7, level = c(80,95))
forecast_ets3
val_pred3= forecast_ets3$mean
val_pred3=as.numeric(val_pred3)

plot(forecast_ets3, col="black", lwd=1.5)
lines(forecast_ets3$fitted, type="l", lwd=1, col="blue")

checkresiduals(fitets3)
##𝐻_0:Zero correlation among residuals
##𝐻_1:Significant correlation among residuals

regr.eval(train_TS3, fitets3$fitted)
regr.eval(val_TS3, val_pred3)


### Auto Arima====
ARIMA_auto3 = auto.arima(train_TS3)
summary(ARIMA_auto3)

### Check the acf and pacf plots
par(mfrow=c(1,2))
Acf(ARIMA_auto3$residuals)
Pacf(ARIMA_auto3$residuals)

### KPSS test
#Null Hypothesis       : The series is trend (or level) stationary
# Alternative Hypothesis: The series is not stationary
kpss.test(ARIMA_auto3$residuals,null="Trend")

## Predictions  
### Prediction on the train3  
pred_Train3 = fitted(ARIMA_auto3)
pred_Test3 = forecast(ARIMA_auto3, h = 7, level = c(80,95))

par(mfrow=c(1,1))
plot(pred_Test3, col="black", lwd=1.5)
lines(pred_Train3, type="l", lwd=1, col="blue")

### Find error for Arima3 on both Test and train3 data3
regr.eval(train_TS3, pred_Train3)
regr.eval(val_TS3, data.frame(pred_Test3)$Point.Forecast)

