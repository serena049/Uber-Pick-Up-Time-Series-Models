# predict real GDP growth with yield curve
library(tis)
library(sandwich)
library(lmtest)
# yield curve data
yc <- read.csv("ycdata.csv")
ycts <- ts(yc[,2:3], start = c(1959, 1), frequency = 12)
# convert monthly to quarterly data
ycq<-aggregate(ycts,nfrequency = 4,FUN = mean)
# GDP data
yp <- read.csv("ycQ.csv")
yts <- ts(yp[,2], start = c(1947, 1), frequency = 4)
# GDP growth over next year
dy4<-100*(lag(log(yts),4)-log(yts))
# GDP growth over last year
dy4led<-lag(dy4,-4)
# yield curve spread
spr<-ycq[,2]-ycq[,1]
# OLS
#trend<-ts(1:length(yts), start = c(1947, 1), frequency = 4)
d<-ts.union(dy4,spr)
fit<-lm(dy4~spr,dat=d)
summary(fit)
dy4f<-ts(fitted(fit), start = c(1959, 1), frequency = 4)
dy4r<-ts(residuals(fit), start = c(1959, 1), frequency = 4)
# HAC std errs
coeftest(fit)
coeftest(fit,NeweyWest(fit,lag = 4, prewhite = FALSE))
# fitted
df<-ts.union(dy4,dy4f,dy4r,spr)
head(df)
dt<-window(df,1959,c(2014,4))
head(dt)
plot(dt[,"spr"],dt[,"dy4"])
abline(fit)
abline(v=0, col = "gray60")
abline(h=0, col = "gray60")
# ts plot of fitted
ts.plot(dt[,1:2], lty = 1:2)
nberShade()
abline(h=0, col = "gray60")
par(new=TRUE)
ts.plot(dt[,1:2], lty = 1:2)
# residuals
ts.plot(dt[,"dy4"],dt[,"dy4r"], lty = 1:2)
nberShade()
abline(h=0, col = "gray60")
par(new=TRUE)
ts.plot(dt[,"dy4"],dt[,"dy4r"], lty = 1:2)
# search for best lag
# by R^2
for(i in 1:8)
{
  if(i==1) cat("horizon, R squared","\n")
  dy4i<-400/i*(lag(log(yts),i)-log(yts))
  di<-ts.union(dy4i,spr)
  fiti<-lm(dy4i~spr,dat=di)
  cat(sprintf("%7d %9.4f \n", i, summary(fiti)$r.squared))
}
# by t stat
for(i in 1:8)
{
  if(i==1) cat("horizon, t statistic","\n")
  dy4i<-400/i*(lag(log(yts),i)-log(yts))
  di<-ts.union(dy4i,spr)
  fiti<-lm(dy4i~spr,dat=di)
  cat(sprintf("%7d %9.4f \n", i, summary(fiti)$coefficients[2,3]))
}
# by HAC t stat
for(i in 1:8)
{
  if(i==1) cat("horizon, HAC t stat","\n")
  dy4i<-400/i*(lag(log(yts),i)-log(yts))
  di<-ts.union(dy4i,spr)
  fiti<-lm(dy4i~spr,dat=di)
  ct<-coeftest(fiti,NeweyWest(fiti,lag = i, prewhite = FALSE))
  cat(sprintf("%7d %9.4f \n", i, ct[2,3]))
}
