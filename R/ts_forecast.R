#############################################################
## Forecasting Montreal Cycling Collisions 2011-2013 using
## historical data from 2006-2010
## Corey Chivers, corey.chivers@mail.mcgill.ca
## August 2013
#############################################################

library(lubridate)
library(forecast)

## Load pre-computed collision rates ##
if(file.exists('data/time_mat.Rdata'))
   load('data/time_mat.Rdata')

startday <-(ymd("2006-01-01",tz='EST'))
all_days <- startday + c(0:(365*5)) * days(1)

par(lwd=2,cex=1.5)
ir <- ts(incident_rate,frequency=365)
plot(stl(ir,365))
seasonal_decomp <- stlf(ir,365*3)


## Plot the seasonal averages ##
oneyear <- ymd("2006-01-01",tz='EST') + c(1:(365)) * days(1)
plot(oneyear,
    seasonal_decomp$seasonal,
    type='l',
    xlab='Time of Year',
    ylab='Seasonal Deviation from Average',
    main='Seasonal Average Collision Rates')
## Note the mid-summer dip ##

## Plot Historical rates plus projections
plot(all_days,
    incident_rate,
    type='l',
    xlim=c(ymd("2006-01-01",tz='EST'),
        ymd("2013-12-01",tz='EST')),
    ylim=c(0,200),
    ylab='Incident Rate (# per month)',
    xlab='Date',
    main='Projected bicycle collision rates in Montreal')

## Add projections ##
forecast_days <- ymd("2011-01-01",tz='EST') + c(1:(365*3)) * days(1)
lines(forecast_days,seasonal_decomp$mean,col='red')
seasonal_decomp$lower[seasonal_decomp$lower[,1]<0,1] <- 0 ## can't have negative rates
lines(forecast_days,seasonal_decomp$upper[,1],lty=2)
lines(forecast_days,seasonal_decomp$lower[,1],lty=2)

