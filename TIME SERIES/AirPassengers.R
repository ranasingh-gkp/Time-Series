#AirPassengers
#-------------------------------
data("AirPassengers")
AirPassengers
par(mfrow=c(3,1))
plot(AirPassengers, main="no of monthly air passenger")
acf(AirPassengers, main="ACF for no of monthly air passenger")
acf(AirPassengers, type="partial", main="PACF of no of monthly air passenger")
#A plot of the time series reveals strong seasonality as well as trend. 
# The data set also seems to be heteroscedastic, with increasing variability in later years.
# We transformed our data set and instead we considering log10(number of passengers).
#We saw that SES essentially led us to naïve forecasting.
AirPassengers.SES = HoltWinters( log10(AirPassengers), beta=FALSE, gamma=FALSE )
AirPassengers.SES$SSE
#Additive seasonality is the default. (Standard R documentation tells us that additive is the default since it is the first possibility           listed for the seasonal argument: seasonal = c("additive", "multiplicative")
AirPassengers.HW = HoltWinters( log10(AirPassengers) )
AirPassengers.HW$SSE
par(mfrow=c(2,1))
plot(AirPassengers.SES, main="SES for airline data")
plot(AirPassengers.HW, main="HoltWinter for trend and seasionality for airline data")
AirPassengers.HW$alpha
AirPassengers.HW$beta 
AirPassengers.HW$gamma 
AirPassengers.HW$coefficients 
# please follow pdf courseera
#Rather than build the explicit forecast ourselves from the return values, let's make a quick all to the routine forecast.HoltWinters () in the library forecast.
rm(list=ls(all=TRUE))

install.packages("forecast")
library("forecast")
AirPassengers.hw <- HoltWinters(log10(AirPassengers))
AirPassengers.forecast <- forecast.Holtwinter(AirPassengers.hw)
AirPassengers.forecast
plot(AirPassengers.forecast, xlim=c(1949, 1963))
forecast:::forecast.HoltWinters(AirPassengers.hw)
