
# Additive:
# Y = trend + seasonality + remainder

# Multiplicative:
# Y = trend * seasonality * remainder

USbeer=read.csv(file.choose())   # choose US beer dataset
beer.ts=ts(USbeer[,2], frequency=12, start=c(1970,1), end=c(1977,12))
ts_info(beer.ts)

### 1 Decompose

# decompose the time series
ts_decompose(beer.ts, type="additive")

ts_decompose(beer.ts, type="multiplicative")

# decomposition based on Loess smoother (STL)
dc <- stl(beer.ts, s.window="per") # periodic seasonality
plot(dc)

dc <- stl(beer.ts, s.window=13) # seasonality extraction with specified window length
plot(dc)

dc <- stl(beer.ts, s.window=13, t.window=121) # seasonality extraction with specified window length
plot(dc)


### 2 Stationarize

# Detrending : Here, we simply remove the trend component from the time series.
# Differencing : This is the commonly used technique to remove non-stationarity.
# Seasonality : Seasonality can easily be incorporated in the ARIMA model directly.

# Dickey-Fuller Test
library(tseries)
adf.test(diff(log(beer.ts)), alternative="stationary", k=0)
## see if it is stationary enough to do time series modelling

### 3 ACF plots

acf(log(beer.ts))
acf(diff(log(beer.ts)))
pacf(diff(log(beer.ts)))

# Clearly, ACF plot cuts off after the first lag. Hence, we understood that value of p # should be 0 as the ACF is the curve getting a cut off. While value of q should be 1 # or 2. 
# After a few iterations, we found that (0,1,1) as (p,d,q) comes out to be the # combination with least AIC and BIC.

### 4 & 5
library(forecast)
fit <- auto.arima(beer.ts, ic="bic", approximation=T, trace=FALSE, allowdrift=F)
summary(fit)
accuracy(fit)
pred <- predict(fit, n.ahead = 50)
ts.plot(beer.ts, pred$pred, lty = c(1,3), col=c(5,2))


##################### Explonential Smoothing ####################


# simple moving average. 
# -------------------------

library("TTR")
par(mfrow=c(1,1))
plot.ts(beer.ts)
lines(SMA(beer.ts,n=3), col='red')
lines(SMA(beer.ts,n=8), col="blue")


# smoothing with splines
#-----------------------

beer.ts.spl <- smooth.spline(beer.ts)
beer.ts.spl # this is not a time series any more
plot(beer.ts.spl)

beer.ts.spl <- ts(beer.ts.spl$y, frequency=12, start=c(1970,1), end=c(1977,12))
plot(beer.ts)
lines(beer.ts.spl, col="red")


# smoothing with running windows
#-------------------------------

# rolling median
beer.ts.rmed <- rollmedian(beer.ts, 3)
plot(beer.ts)
lines(beer.ts.rmed, col="red")

# rolling max
beer.ts.rmed <- rollmax(beer.ts, 3)
plot(beer.ts)
lines(beer.ts.rmed, col="red")

# use a own function, e.g. quantile 0.9
beer.ts.q09 <- rollapply(beer.ts, 3, FUN=function(x) {
  quantile(x, 0.9, na.rm=TRUE)
})
plot(beer.ts)
lines(beer.ts.q09, col="red")


############################### Forecast models ##################################

# Simple Average - simple average of all data points

# Naive Method - the last observation value

# Seasonal Navie - the last observation value from previous seasonal cycle

# Drift Method - forecast value increase or decrease over time based on average change in historical data

beer.fit.a <- meanf(beer.ts, h = 120)
beer.fit.n <- naive(beer.ts, h = 120)
beer.fit.sn <- snaive(beer.ts, h = 120)
beer.fit.dri <- rwf(beer.ts, h = 120, drift = TRUE)

par(mfrow=c(1,1))

plot.ts(beer.ts, main = "Monthly Beer Production in Australia", xlab = "Year", ylab = "ML", xlim = c(1970, 1980))
lines(beer.fit.a$mean, col = "blue")
lines(beer.fit.n$mean, col = "yellow4")
lines(beer.fit.dri$mean, col = "seagreen4")
lines(beer.fit.sn$mean, col = "red")
legend("topleft",lty=1,col=c("blue","yellow4","seagreen4", "red"), cex = 0.75,
       legend=c("Mean method","Naive method","Drift Naive method", "Seasonal naive method"))


#### regression analysis

# without seasonlity
beer.fit.lm <- tslm(beer.ts ~ trend)
summary(beer.fit.lm)
f <- forecast(beer.fit.lm, h = 120, level = c(80,95))
plot.ts(beer.ts, main = "Monthly Beer Production in Australia", xlab = "Year", ylab = "ML", xlim = c(1970,1978))
lines(f$fitted, col = "blue")
lines(f$mean, col = "blue")


# with seasonality
beer.fit.lm2 <- tslm(beer.ts ~ trend + season)
summary(beer.fit.lm2)
f2 <- forecast(beer.fit.lm2, h = 120, level = c(80,95))
plot.ts(beer.ts, main = "Monthly Beer Production in Australia", xlab = "Year", ylab = "ML", xlim = c(1970,1978))
lines(f2$fitted, col = "blue")
lines(f2$mean, col = "blue")


f3 <- forecast(beer.fit.lm2, h = 120, level = c(80,95))
plot.ts(beer.ts, main = "Monthly Beer Production in Australia", xlab = "Year", ylab = "ML", xlim = c(1970,1980))
lines(f3$mean, col = "blue")


#### simple explonential smoothing
# >  used for the time seriese without trend and seasonality

plot.ts(beer.ts, main = "Monthly Beer Production in Australia", xlab = "Year", ylab = "ML", xlim = c(1970,1978))
beer.fit.ses1 <- ses(beer.ts2, alpha = 0.2, initial = "simple", h = 12)
beer.fit.ses2 <- ses(beer.ts2, alpha = 0.6, initial = "simple", h = 12)
beer.fit.ses3 <- ses(beer.ts2, h = 12)
plot(beer.fit.ses1, plot.conf=FALSE, type="o", main = "Monthly Beer Production in Australia", xlab = "Year", ylab = "ML")
lines(beer.fit.ses1$fitted, col = "blue", type="o")
lines(beer.fit.ses2$fitted, col = "green", type="o")
lines(beer.fit.ses3$fitted, col = "red", type="o")
lines(beer.fit.ses1$mean, col = "blue", type="o")
lines(beer.fit.ses2$mean, col = "green", type="o")
lines(beer.fit.ses3$mean, col = "red", type="o")
legend("topleft",lty=1, col=c(1,"blue","green","red"), 
       c("data", expression(alpha == 0.2), expression(alpha == 0.6),
         expression(alpha == 0.87)), pch = 1)


### Holt's Linear Trend Method

# In the Holt's linear trend models, there are 3 variations.

# Holt's Linear Trend
# Expontential Linear Trend - The level and slop are mutipled instead of added in Holt's model
# Damped Trend - The trend became flat after a period increase or decrease. It is usually very useful in the business world since typical growth or decline will stop after a certain period of time.


beer.ts.yrl <- aggregate(beer.ts, nfrequency=1)
beer.ts.yrl <- window(beer.ts.yrl , start = 1970, end = 1975)
beer.fit.holt1 <- holt(beer.ts.yrl, alpha = 0.2, beta = 0.2, initial = "simple", h = 6)
beer.fit.holt2 <- holt(beer.ts.yrl, alpha = 0.2, beta = 0.2, initial = "simple", exponential = TRUE, h = 6)
beer.fit.holt3 <- holt(beer.ts.yrl, alpha = 0.2, beta = 0.2, initial = "simple", damped = TRUE, h = 6)

plot(beer.fit.holt1, type="o", fcol="white", main = "Yearly Beer Production in Australia", xlab = "Year", ylab = "ML", plot.conf=FALSE)
lines(beer.fit.holt1$fitted, col = "blue")
lines(beer.fit.holt2$fitted, col = "green")
lines(beer.fit.holt3$fitted, col = "red")
lines(beer.fit.holt1$mean, col = "blue", type="o")
lines(beer.fit.holt2$mean, col = "green", type="o")
lines(beer.fit.holt3$mean, col = "red", type="o")
legend("topleft", lty = 1, col = c("black", "blue", "green", "red"), 
       c("Data", "Holt's Linear Trend", "Exponential Trend", "Damped Trend"))

## Holt-Winters' Seasonal Trend

beer.ts3 <- window(beer.ts, start = 1960, end = 1975)
beer.ts.qtr <- aggregate(beer.ts3, nfrequency=4)
beer.fit.hw1 <- hw(beer.ts.qtr, h = 20, seasonal = "additive")
beer.fit.hw2 <- hw(beer.ts.qtr, h = 20, seasonal = "multiplicative")

plot(beer.fit.hw1, type="o", fcol="white", main = "Quarterly Beer Production in Australia", xlab = "Year", ylab = "ML", plot.conf=FALSE)
lines(beer.fit.hw1$fitted, col = "blue", lty=2)
lines(beer.fit.hw2$fitted, col = "red", lty=2)
lines(beer.fit.hw1$mean, col = "blue", type="o")
lines(beer.fit.hw2$mean, col = "red", type="o")
legend("topleft", lty = 1, pch = 1, col = c("black", "blue", "red"),
       c("Data", "Holt Winters' Additive", "Holt Winters' Multiplicative"))


beer.fit.ets <- ets(beer.ts.qtr)
plot(forecast((beer.fit.ets), h = 8), xlab = "Year", ylab = "ML")


###### ARIMA

# Autoccorrelation is basically to see if the consecutive observations are correlated. 
# If there is correlation, we can run a regression using current observation as depending variable and previous observation as indenpending variable.

lag.plot(beer.ts, lags = 9, do.lines = FALSE)
ts_acf(beer.ts)
ts_pacf(beer.ts)
##Differencing
# Differencing computes the differences between consecutive observations. 
# By differencing the time series data, we can remove the trend and seasonality.

d_beer <- diff(log(beer.ts.qtr))
plot(d_beer, main = "Differencing logged Quarterly Beer Production")
ts_acf(d_beer)
ts_pacf(d_beer)


dd_beer <- diff(d_beer, lag = 4)
plot(dd_beer, main = "Differencing the difference logged Quarterly Beer Production")
ts_acf(dd_beer)
ts_pacf(dd_beer)


beer.arima=auto.arima(beer.ts)
summary(beer.fit)
fcst.auto <- forecast(beer.arima, h = 20)
plot(fcst.auto)


# Neural nework is probably one of the hottest machine learning algorithms which models human brain 
#and neural system. The lagged value can be used as inputs to a neural network similar to autoregression model

beer.fit.nn <- nnetar(beer.ts)
summary(beer.fit.nn)
plot(forecast(beer.fit.nn, h = 20), xlab = "Year", ylab = "ML", 
     xlim = c(1970, 1980), ylim = c(0, 20))


######### comparing our models

tsTrain <-beer.ts[1:85]
tsTest <- beer.ts[86:length(beer.ts)]

# lower RMSE the better the model, the rest of the reading denote error in model

par(mfrow=c(1,1))

m1=accuracy(forecast(beer.fit.a,tsTrain), tsTest)   # simple average
plot_forecast(forecast(beer.fit.a,tsTrain), tsTest)

m2=accuracy(forecast(beer.fit.n,tsTrain), tsTest)   # naive method
plot_forecast(forecast(beer.fit.n,tsTrain), tsTest)


m3=accuracy(forecast(beer.fit.lm2,tsTrain), tsTest)  # linear model w seasonlaity
plot_forecast(forecast(beer.fit.lm2,tsTrain), tsTest)


m4=accuracy(forecast(beer.fit.ses3,tsTrain), tsTest)  # simple exponential smoothing
plot_forecast(forecast(beer.fit.ses3,tsTrain), tsTest)


m5=accuracy(forecast(beer.fit.hw2,tsTrain), tsTest)  # holts winter
plot_forecast(forecast(beer.fit.hw2,tsTrain), tsTest)


m6=accuracy(forecast(beer.arima,tsTrain), tsTest)  # auto arima
plot_forecast(forecast(beer.arima,tsTrain), tsTest)

m1;m2;m3;m4;m5;m6

res=data.frame(name=c("average","naive","linear","simple ex","holts winter","arima"),
               rmse=c(m1[1,2], m2[1,2], m3[1,2], m4[1,2], m5[1,2], m6[1,2])
               )
barplot(res$rmse, names.arg=res$name)


############################### Examining forecast errors #############################

fit=auto.arima(beer.ts, ic="bic")
plot.ts(fitted(fit))
plot.ts(residuals(fit))

acf(residuals(fit), lag.max=20)
# To test whether there is significant evidence for non-zero correlations at lags 1-20, 
# we can carry out a Ljung-Box test. 

Box.test(residuals(fit), lag=20, type="Ljung-Box")

## examining errors

plotForecastErrors <- function(forecasterrors)
  {
     # make a histogram of the forecast errors:
     mybinsize <- IQR(forecasterrors)/4
     mysd   <- sd(forecasterrors)
     mymin  <- min(forecasterrors) - mysd*5
     mymax  <- max(forecasterrors) + mysd*3
     # generate normally distributed data with mean 0 and standard deviation mysd
     mynorm <- rnorm(10000, mean=0, sd=mysd)
     mymin2 <- min(mynorm)
     mymax2 <- max(mynorm)
     if (mymin2 < mymin) { mymin <- mymin2 }
     if (mymax2 > mymax) { mymax <- mymax2 }
     # make a red histogram of the forecast errors, with the normally distributed data overlaid:
     mybins <- seq(mymin, mymax, mybinsize)
     hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
     # freq=FALSE ensures the area under the histogram = 1
     # generate normally distributed data with mean 0 and standard deviation mysd
     myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
     # plot the normal curve as a blue line on top of the histogram of forecast errors:
     points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
  }

plotForecastErrors(residuals(fit))