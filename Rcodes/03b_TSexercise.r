library(astsa)

data(jj)#  Johnson and Johnson data are quarterly earnings, # hence it has frequency=4

time(jj)
cycle(jj) 

plot(jj, ylab="Earnings per Share", main="J & J")
plot(jj, type="o", col="blue", lty="dashed")
plot(diff(log(jj)), main="logged and diffed")

#  filtering/smoothing the Johnson & Johnson series
# using a two-sided moving average

k = c(.5,1,1,1,.5)        # k is the vector of weights
(k = k/sum(k))

fjj = filter(jj, sides=2, k)  # ?filter for help [but you knew that already]
plot(jj)
lines(fjj, col="red")         # adds a line to the existing plot
lines(lowess(jj), col="blue", lty="dashed")

##  difference the logged data and call it

dljj = diff(log(jj))        # difference the logged data
plot(dljj)                  # plot it (not shown)

shapiro.test(dljj)          # test for normality

## histogram and a Q-Q plot

par(mfrow=c(2,1))        # set up the graphics 
hist(dljj, prob=TRUE, 12)   # histogram    
lines(density(dljj))     # smooth it - ?density for details 
qqnorm(dljj)             # normal Q-Q plot  
qqline(dljj)             # add a line 

lag1.plot(dljj, 4)       # correlation structure

# ACF and PACF

acf2(dljj)


# structural decomposition using lowess.

plot(dog <- stl(log(jj), "per"))


### fit regression

Q = factor(cycle(jj))        # make (Q)uarter factors
trend  = time(jj) - 1970          # not necessary to "center" time, but the results look nicer
reg = lm(log(jj)~ 0 + trend + Q, na.action=NULL)  # run the regression without an intercept

model.matrix(reg)

# plot of the observations and their fitted values:
plot(log(jj), type="o")    # the data in black 
lines(fitted(reg), col=2)  # the fitted values in red

par( mfrow = c(2,1) )
plot( resid(reg) )       # residuals
acf( resid(reg), 20 )    # acf of the resids

### ARIMA forecasting

sarima.for(jj, 10, 1, 1, 1)   
# forecast gtemp for 10 years ahead based on an ARIMA(1,1,1)
