
setwd("C:\\Users\\PHANI KUMAR\\Desktop\\Time series analysis")

#Loading required libraries
library(readxl)
library(tseries)
library(forecast)

#Reading data
mydata <- read_xls("UK Outward Passengers Movement.xls",range = "A6:G46")

#Converting the dataset index by time
timeseries_df <- ts(mydata$Total,start = c(1996,1),end = c(2005,4),frequency = 4)

plot(timeseries_df)  

#Identifying Seasonal,trend,random,observed (decomposition)
seasonal_decomp <-decompose(timeseries_df,type = c("multiplicative"))
plot(seasonal_decomp)

#checking for if the data is stationary
adf.test(timeseries_df,alternative = "stationary")

#As the p value is low it is not stationary so now we need to find the values of p,d,q
#Checking manually will give the best result

#no of diffesrences - d
ndiffs(timeseries_df)

#auto corelation - p
acf(timeseries_df)

#order of moving averages - q
pacf(timeseries_df) 

#Exponential Models using Holtwinters
#double exponential
fit2 <- HoltWinters(timeseries_df, gamma=FALSE)
accuracy(fit2$fitted, timeseries_df)
# triple exponential - models level, trend, and seasonal components
fit3 <- HoltWinters(timeseries_df)
accuracy(fit3$fitted, timeseries_df)

forecast(fit3, 4)

fit<-ets(timeseries_df)
accuracy(fit$fitted, timeseries_df)
summary(fit)

# predict next three future values
forecast(fit, 4)
plot(forecast(fit, 4))

# fit an ARIMA model of order P, D, Q
fit <- arima(timeseries_df, order=c(2,1,1))
summary(fit)
#Very high mape

#Creating auto arima model
fit <- auto.arima(timeseries_df)
summary(fit)

forecast(fit,h=12,level = 95)
plot(forecast(fit,h=12,level = 95))

#Of all the above models ,I would like to go ahead with Auto arima model because it works well
#with both seasonal and non seasonal data.Models are chosen on the basis of fit criteria.

