# import libraries
#library(ts)
library(lmtest)
library(sandwich)
#library(plyr)
library(tseries)



# read in the uber data
uber14 <- read.csv("uber14.csv") # 2014 Apr - Sept


# convert to ts obj; frequency is the number of obs per year 
#2014 data
uber14ts<-ts(uber14$freq, freq = 7)
prcp14 <-ts(uber14$PRCP, freq=7)
tmax14 <-ts(uber14$TMAX, freq=7)
tmin14<-ts(uber14$TMIN, freq=7)

holiday14<-ts(uber14$holiday, freq=7)
weekend14<-ts(uber14$weekend, freq=7)
mon14<-ts(uber14$mon, freq=7)
tue14<-ts(uber14$tue, freq=7)
wed14<-ts(uber14$wed, freq=7)
thurs14<-ts(uber14$thurs, freq=7)
fri14<-ts(uber14$fri, freq=7)
sat14<-ts(uber14$sat, freq=7)
sun14<-ts(uber14$sun, freq=7)


## 3. Linear Regression Analysis

# LR for 2014 pickups

model1<-lm(uber14ts~prcp14+tmax14+tmin14+holiday14+mon14+tue14+thurs14+fri14+sat14+sun14) # use wed as the base
summary(model1)


lag14<-lag(uber14ts,-1)
data4<-ts.union(uber14ts, lag14, prcp14,tmax14,tmin14,weekend14,holiday14,mon14,tue14,wed14,thurs14,fri14,sat14,sun14)
model4<-lm(uber14ts~lag14+prcp14+tmax14+tmin14+holiday14+mon14+tue14+wed14+thurs14+fri14+sat14+sun14-1, data = data4)
summary(model4)

uber14fit<-ts(fitted(model4), freq=7)
uber14residue<-ts(residuals(model4), freq=7)

# HAC std errs

#coeftest(model4)
#coeftest(model4,NeweyWest(model1,lag = 1, prewhite = FALSE))


# fitted
df0<-ts.union(uber14ts, uber14fit)
df<-ts.union(df0,uber14residue)
head(df)

# ts plot of fitted
plot(df[,1:2],plot.type = "single", col = c("red", "blue"), lwd = 2)
# residuals
plot(df[,2],df[,3])
abline(h=0)



# search for best lag
# by R^2

for(i in 1:8)
{
        if(i==1) cat("horizon, R squared","\n")
        uber14tsi<-lag(uber14ts,i)
        mon14i<-lag(mon14, i)
        tue14i<-lag(tue14, i)
        thurs14i<-lag(thurs14, i)
        fri14i<-lag(fri14,i)
        sat14i<-lag(sat14,i)
        sun14i<-lag(sun14,i)
        datai<-ts.union(uber14tsi, lag14,prcp14, tmax14, holiday14,mon14i,tue14i,thurs14i,fri14i,sat14i,sun14i)
        fiti<-lm(uber14tsi~lag14+prcp14+tmax14+tmax14+holiday14+mon14i+tue14i+thurs14i+fri14i+sat14i+sun14i, data = datai)
        cat(sprintf("%7d %9.4f \n", i, summary(fiti)$r.squared))
}

# by HAC t stat
for(i in 1:8)
{
        if(i==1) cat("horizon, HAC t stat","\n")
        uber14tsi<-lag(uber14ts,i)
        mon14i<-lag(mon14, i)
        tue14i<-lag(tue14, i)
        thurs14i<-lag(thurs14, i)
        fri14i<-lag(fri14,i)
        sat14i<-lag(sat14,i)
        sun14i<-lag(sun14,i)
        datai<-ts.union(uber14tsi, lag14,prcp14, tmax14, holiday14,mon14i,tue14i,thurs14i,fri14i,sat14i,sun14i)
        fiti<-lm(uber14tsi~lag14+prcp14+tmax14+tmax14+holiday14+mon14i+tue14i+thurs14i+fri14i+sat14i+sun14i, data = datai)
        ct<-coeftest(fiti,NeweyWest(fiti,lag = i, prewhite = FALSE))
        cat(sprintf("%7d %9.4f \n", i, ct[2,3]))
}