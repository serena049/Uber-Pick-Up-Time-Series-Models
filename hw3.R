# set working directory
#setwd("~/Documents/uber")

# import libraries
library(zoo)
library(forecast)

# read in the uber data
uber14 <- read.csv("uber14.csv") # 2014 Apr - Sept

# convert to ts obj; frequency is the number of obs per year 
#2014 data
uber14ts<-ts(uber14$freq, frequency = 7)

# ARIMA identification

acf(uber14ts) # ACF for MA(q)
acf(diff(uber14ts,7)) # weekly seasonal diff
acf(diff(diff(uber14ts,7))) # diff of the weekly seasonal diff

pacf(uber14ts) # PCF for AR(q)
pacf(diff(uber14ts,7))
pacf(diff(diff(uber14ts,7)))

etsuber14<-ets(uber14ts)
summary(etsuber14)
plot(etsuber14)

# ARIMA Models (this auto.arima model finds the best fitted arima model based on AIC/BIC, I set the seasonal diff D = 7 to capture the weekly seasonal trend)
cbest1 = auto.arima(uber14ts, D=1) # short cut version, much faster
summary(cbest1)

cbest2 = auto.arima(uber14ts, D=1,stepwise = FALSE, approximation = FALSE) #full version
summary(cbest2)

cbest3 = Arima(uber14ts, order=c(0,0,4), seasonal=c(4,1,2), lambda=0) 
summary(cbest3)




# Ljung-Box tests (non significant statistics proves the assumption of white noise)
Box.test(residuals(cbest2), lag = 7, type = c("Ljung-Box"), fitdf = 0)

# forecasting
pred1 <- forecast(cbest1,h=21) #3 weeks
plot(pred1)

pred2 <- forecast(cbest2,h=21)
plot(pred2)

pred3 <- forecast(cbest3,h=21)
plot(pred3)



