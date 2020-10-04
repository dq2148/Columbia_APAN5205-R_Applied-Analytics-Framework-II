library(ggplot2);library(ggthemes);library(gridExtra)  # For plots 
library(quantmod);library(xts);library(zoo) # For using xts class objects
library(forecast) # Set of forecasting functions
library(fpp); library(fpp2) # Datasets from Forecasting text by Rob Hyndman
library(tseries) # for a statistical test
library(dplyr) # Data wrangling

####################Section 1
#1 type of data strcture of goog
setwd('~/Desktop/APAN5205notes')
goog = readRDS('goog.RDS')
class(goog)

#2 what was google stock price for 2010/6
library(xts)
goog_xts = as.xts(goog)
goog_xts['2010-06',]

#3 using mothly stock price, what is the average stock price for the year 2010?
#method 1
mean(goog_xts['2010',])
#method 2
average_2010 = ts(goog,start=c(2007,01),frequency=12)
average_2010_price = window(average_2010,start=c(2010,01),end=c(2010,12))
mean(average_2010_price)

#4 how many months of data are includued
str(goog_xts)
nmonths(goog_xts)

#5 correlationo between google stock price and one-month lagged stock price
cor(goog_xts,lag(goog_xts),use='complete.obs')

#6 train and test
google = ts(goog,start=c(2007,01),frequency=12)
train = window(google,start=c(2007,01),end=c(2015,12))
test = window(google,start=c(2016,01),end=c(2018,10))
nmonths(train)

#7 Autocorrelation exmaines, which lag has the stronogest autocorrelation
#method1
library(forecast)
acf(x = goog,lag.max = 1,plot=F)
ggAcf(x = goog)
acf(x = goog,lag.max = 2,plot=F)
ggAcf(x = goog)
acf(x = goog,lag.max = 3,plot=F)
ggAcf(x = goog)
acf(x = goog,lag.max = 4,plot=F)
ggAcf(x = goog)
#method2
ggAcf(train)


####################Section 2
#1
#Use the average to make a prediction for the stock price over the 34 months of the test sample. 
#Let's call this average_model. What is the point forecast of the stock price for October 2018?
average_model=meanf(train,h=34)
window(average_model$mean, c(2018,10))

#2 RMSE of the predicition in train smaple
accuracy(average_model)

#3 RMSE of average_model on test sample
accuracy(average_model, x=test)

#4 use naive_model to forecast stoock price over next 34 months of the test sample
naive_model=naive(train,h=34)
naive_model
window(naive_model$mean,c(2018,10))

#5 RMSE of naive_model on test sample
accuracy(naive_model, x=test)


####################Section 3
#1-3 exponential smoothing models
ets_aaa=ets(train,model = 'AAA') 
summary(ets_aaa)

#4 residuals do not look like white noise
checkresiduals(ets_aaa)

#5 use ets_model to forecast stock price 
ets_aaa_forecast = forecast(ets_aaa,h=34)
ets_aaa_forecast
window(ets_aaa_forecast$mean,c(2018,10))

#6 RNSE if ets_model on test sample
accuracy(ets_aaa_forecast, x=test)

####################Section 4
#1-4
auto_arima_model = auto.arima(train)
summary(auto_arima_model)

#5 resideuals resemble white nose
checkresiduals(auto_arima_model)

#6 Auto_ARIMA_Model to forecast
auto_arima1_model = forecast(auto_arima_model,h=34)
window(auto_arima1_model$mean,c(2018,10))

#7 RMSE
accuracy(auto_arima1_model,x=test)

#8 Improve Arima moel by variance stabilizing transformation. 
BoxCox.lambda(train)

#9 Specify an ARIMA model
arima_model = Arima(train,order = c(1,1,1),seasonal = c(3,1,0),lambda=BoxCox.lambda(train))
arima_model

#10 residuals resemble white nose
checkresiduals(arima_model)

#11 arima_model
arima1_model = forecast(arima_model,h=34)
window(arima1_model$mean,c(2018,10))

#12 RMSE oof arima_model on test sample
accuracy(arima1_model,x=test)