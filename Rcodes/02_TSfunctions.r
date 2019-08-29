############################ interpolation > missing values ##################################

# replace all NAs with last observation
na.locf(data, fromLast=TRUE)
# replace all NAs with next observation
na.locf(data)

na.fill(data, fill=0)  # fill all NAs with 0

na.trim(data)  # trim data at beginning and end of dataset

na.omit(data)  # gets rid of all the NAs

na.approx(data) # approx the data that should be by looking before and after

############################# correlation ##########################################

# Measure of the internal correlation within a time series. 
# It is a way of measuring and explaining internal association between 
# observations in a time series


plot(ndvi)

#### compute the auto-correlation function
acor <- acf(ndvi[,1])
ts_acf(ndvi)

# auto-correlation of a monotone trend
x <- ts(1:60, start=c(2000, 1), frequency=12)
ts_acf(x)

# auto-correlation of random numbers
x <- ts(rnorm(60), start=c(2000, 1), frequency=12)
ts_acf(x)


##### analyze the cross-correlation between air and soil temperature

# load the soil and air temperature data
load(file.choose())   # load the air_temperature.RData
load(file.choose())  # load the soil_temperature.RData

plot(merge(tair, tsoil))

# subset the data for a chamber
names(tair)
tair.ia <- tair[,1]
tsoil.ia <- tsoil$IA
tas <- merge(tair.ia, tsoil.ia)
plot(tas)

# aggregate the data to hourly values
hours <- format(time(tas), "%Y-%m-%d %H")
tas.h <- aggregate(tas, by=hours, FUN=mean)
index(tas.h) <- as.POSIXct(paste0(index(tas.h), ":00:00"))
plot(tas.h, plot.type="single", col=c("blue", "red"))

# check if air and soil temperature are cross-correlated
ccf(tas.h[,1], tas.h[,2], na.action=na.pass) 



############################### calculate lags and differences in data #######################

# lagging in time series > align time series for comparison and shift observations in time

lag(ndvi, k=1, na.pad=TRUE)  # k is number of lags forward(+) or backward(-) in time

# work with ndvi dataset
lead_xts=lag(ndvi, k=-1)
lag_xts=lag(ndvi, k=1)
new=cbind(lead_xts,ndvi,lag_xts)  # combine columns for comaprison

# differencing series

data-lag(ndvi) # calculate first diff using lag and subtraction

# calculate the first order 12 months difference
diff(ndvi, lag=12, differences=1)


#################################### rolling windows #####################################

######### rolling value given a fixed observation of time

# there is cumsum (sum up); sum product (multiply); 
#            cummin (minimum); cummax(maximum)


# go back to stock1 data created earlier
stock1

stock1p2=cbind(stock1, cumsum(stock1))
stock1p2


stock1p3=cbind(data=stock1, sum=cumsum(stock1), max=cummax(stock1))
stock1p3  # i addded a maximum column and gave some column names for easier understanding


############### rollapply function

# we calculate roliing 3 month mean

rollapply(ndvi[,1],3,mean) # somthing like in stocks data graphs 3month average


#####################################################################################
#                       aggregation of data
####################################################################################

par(mfrow=c(1,2))

ndvi2mon=aggregate(ndvi[,1], as.Date(as.yearmon(time(ndvi))), mean)
plot(ndvi2mon)


ndvi2qtr=aggregate(ndvi[,1], as.Date(as.yearqtr(time(ndvi))), mean)
plot(ndvi2qtr)

par(mfrow=c(1,1))

## for xts format

apply.quarterly(ndvi, FUN="mean")
apply.yearly(ndvi, FUN="mean")
apply.weekly()
apply.month()
apply.daily()


