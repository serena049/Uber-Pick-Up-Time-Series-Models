# set working directory
setwd("~/Documents/uber")

# import libraries
library(forecast)
library(tis)
library(lmtest)

# read in the uber data
uber14 <- read.csv("uber14.csv") # 2014 Apr - Sept
uber14$date<-strptime(as.character(uber14$DATE),"%m/%d/%Y")
format(uber14$date,"%Y-%m-%$d")

# convert to ts obj; frequency is the number of obs per year 
#2014 data
startWeek <- as.numeric(strftime(head(uber14$date,1),format ="%W"))
startDay <- as.numeric(strftime(head(uber14$date,1)+1,format ="%w"))
duber14<-diff(uber14$freq)
uber14ts<-ts(duber14, frequency = 7, start = c(startWeek, startDay))
prcp14 <-ts(uber14$PRCP, freq=7, start = c(startWeek, startDay))
tmax14 <-ts(uber14$TMAX, freq=7, start = c(startWeek, startDay))
tmin14<-ts(uber14$TMIN, freq=7, start = c(startWeek, startDay))
holiday14<-ts(uber14$holiday, freq=7, start = c(startWeek, startDay))
weekend14<-ts(uber14$weekend, freq=7, start = c(startWeek, startDay))
mon14<-ts(uber14$mon, freq=7, start = c(startWeek, startDay))
tue14<-ts(uber14$tue, freq=7, start = c(startWeek, startDay))
wed14<-ts(uber14$wed, freq=7, start = c(startWeek, startDay))
thurs14<-ts(uber14$thurs, freq=7, start = c(startWeek, startDay))
fri14<-ts(uber14$fri, freq=7, start = c(startWeek, startDay))
sat14<-ts(uber14$sat, freq=7, start = c(startWeek, startDay))
sun14<-ts(uber14$sun, freq=7, start = c(startWeek, startDay))


# Best linear regression model with daily patterns (full sample)
lag14<-lag(uber14ts, -1)
data1<-ts.union(uber14ts,lag14,prcp14,tmax14,tmin14,weekend14,holiday14,mon14,tue14,wed14,thurs14,fri14,sat14,sun14)
model1<-lm(uber14ts~lag14+prcp14+tmin14+mon14+tue14+wed14+thurs14+fri14+sat14+sun14-1, data = data1)
summary(model1)
aicmodel1<-AIC(model1)

# Best seasonal ARIMA Model (full sample)
model2 = Arima(uber14ts, order=c(1,0,1), seasonal=c(4,1,0), lambda=1) 
summary(model2)

# Divide sample for forecast and validation
endWeek <- startWeek+10
endDay <- startDay+7
data2 <- window(data1, start = c(startWeek+1, startDay), end = c(endWeek, endDay))
#mod1 <- lm(uber14ts~lag14+prcp14+mon14+tue14+wed14+fri14+sat14+sun14, data = data2)
#summary(mod1)
mod1 <- lm(uber14ts~prcp14+mon14+tue14+wed14+fri14+sat14+sun14, data = data2)
summary(mod1)

uber14tsw <- window(uber14ts, start = c(startWeek+1, startDay), end = c(endWeek, endDay))
mod2 <- Arima(uber14tsw, order=c(0,0,1), seasonal=c(0,1,1), lambda=1) 
summary(mod2)
#mod2<- auto.arima(uber14tsw) #full version
#summary(mod2)
# check residuals are white noise
uber14mod1fit<-ts(fitted(mod1), freq=7,start = c(startWeek+1, startDay))
e1<-residuals(mod1)
plot(e1)
abline(h=0)
e2<-residuals(mod2)
Box.test(e2, lag = 7, type = c("Ljung-Box"), fitdf = 0)

# in sample evaluations
E <- cbind(e1,e2)
ne <- nrow(E)
mE <- colMeans(E)
mEsq <- mE*mE
E2 <- E*E
mseE <- colMeans(E2)
varE <- mseE-mEsq
maeE <- colMeans(abs(E))
parE <- c(8,3)
aicE <- 2/ne*parE + log(mseE)
sicE <- log(ne)/ne*parE + log(mseE)
# display
varE
mseE
maeE
aicE
sicE
# plots
barplot(mE,main="mean")
barplot(varE,main="variance")
barplot(mseE,main="MSE")
barplot(maeE,main="MAE")
barplot(aicE,main="AIC")
barplot(sicE,main="SIC")
# forecasts
# mod1
c1 <- coefficients(mod1)
f1 <- c1[1]+c1[2]*prcp14+c1[3]*mon14+c1[4]*tue14+c1[5]*wed14+c1[6]*fri14+c1[7]*sat14+c1[8]*sun14
u1 <- window(uber14ts-f1,start=c(startWeek+11, startDay))
# mod2
uber14tsh <- window(uber14ts,freq=7, start=c(startWeek+11, startDay+1))
fit2 <- Arima(c(uber14tsw,uber14tsh), model=mod2)
res2 <- ts(residuals(fit2),freq =7,start=c(startWeek+1, startDay))
u2 <- window(res2,freq =7,start=c(startWeek+11, startDay))

# plot residuals
ts.plot(u1,u2,lty=1:4)
#nberShade()
abline(h=0, col = "gray60")
par(new=TRUE)
ts.plot(u1,u2,lty=1:4)
legend("bottomleft",lty=1:2,legend=c("u1","u2"))
# out of sample evaluation
U <- cbind(u1,u2)
nu <- nrow(U)
colMeans(U)
dath <- window(data1,freq=7,start=c(startWeek+11, startDay+1))
coeftest(lm(u1~dath[,3]))
coeftest(lm(u1~dath[,8]))
coeftest(lm(u1~dath[,9]))
coeftest(lm(u1~dath[,10]))
coeftest(lm(u2~dath[,12]))
coeftest(lm(u2~dath[,13]))
coeftest(lm(u2~dath[,14]))
#plot(as.data.frame(cbind(dath[,5],u1)))
#plot(as.data.frame(cbind(dath[,5],u2)))

mU <- colMeans(U)
mUsq <- mU*mU
U2 <- U*U
mseU <- colMeans(U2)
varU <- mseU-mUsq
maeU <- colMeans(abs(U))
parU <- c(8,3)
aicU <- 2/nu*parU + log(mseU)
sicU <- log(nu)/nu*parU + log(mseU)
# display
mU
varU
mseU
maeU
aicU
sicU
# plots
barplot(mU,main="mean")
barplot(varU,main="variance")
barplot(mseU,main="MSE")
barplot(maeU,main="MAE")
barplot(aicU,main="AIC")
barplot(sicU,main="SIC")

