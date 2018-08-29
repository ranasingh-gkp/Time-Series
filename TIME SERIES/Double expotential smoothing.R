#Double expotential smoothing
#--------------------------------
#level by alpha
#trend by bita
#seasional by gama
rm(list=ls(all=TRUE))
# use your appropriate directory!
#setwd("the directory where you have your data")
#setwd("C:/Users/yourname/Desktop") #for example
#money.data = read.table("volume-of-money-abs-definition-m.txt")
money.data.ts = ts(volume_of_money[,2],start=c(1960,2) , frequency=12)
par(mfrow=c(3,1))
plot(money.data.ts, main="Time Plot of Volume of Money")
acf(money.data.ts, main="ACF of Volume of Money")
acf(money.data.ts, type="partial", main="PACF of Volume of Money")
#acf have very slow decay, means there is some trend
# Instead of developing a SARIMA model, at the moment we are interested in forecasting
#Double Exponential, or Exponential Smoothing with Trend
#forecast = level + trend
#leveln = ??xn + (1 ??? ??) (leveln???1 + trendn???1)
#trendn = ?? ??? (leveln ??? leveln???1) + (1 ??? ?? )trendn???1
#????^n+1 = leveln + trendn
HoltWinters(money.data.ts, gamma = FALSE)
#set up our transformed data and smoothing parameters
data = volume_of_money[,2]
N = length(data)
alpha = 0.7
beta = 0.5
##prepare empty arrays so we can store values
forecast = NULL
level = NULL
trend = NULL
#initialize level and trend in a very simple way
level[1] = data [1]
trend[1] = data [2]- data [1]
#initialize forecast to get started
forecast[1] = data [1]
forecast[2] = data [2]
#loop to build forecasts
for( n in 2:N ) {
  level[n] = alpha* data [n] +
    (1-alpha)*(level[n-1]+trend[n-1])
  trend[n] = beta*(level[n] - level[n-1]) +
    (1-beta)*trend[n-1]
  forecast[n+1] = level[n] + trend[n]
}
#display your calculated forecast values
forecast[3:N]
#verify that we have recovered HoltWinters() output
m = HoltWinters(data, alpha = 0.7, beta = 0.5, gamma = FALSE)
m$fitted[,1]
plot(m, main="Holt Winters Fitting of Money Volume with Bogus Parameters")
m=HoltWinters(data, gamma = FALSE)
plot(m, main="Holt Winters Fitting of Money Volume with Optimal Parameters")
