#time series model
#https://www.datascience.com/blog/introduction-to-forecasting-with-arima-in-r-learn-data-science-tutorials
#-------------------------------
install.packages("forecast")
install.packages("tseries")
library('ggplot2')
library('forecast')
library('tseries')
data(day)
daily_data = read.csv('day.csv', header=TRUE, stringsAsFactors=FALSE)
day$Date = as.Date(day$dteday)
ggplot(day, aes(Date, cnt)) + geom_line() + scale_x_date('month')  + ylab("Daily Bike Checkouts") +
  xlab("")
count_ts = ts(day[, c('cnt')])
count_ts
day$clean_cnt = tsclean(count_ts)
ggplot() +
  geom_line(data = day, aes(x = Date, y = clean_cnt)) + ylab('Cleaned Bicycle Count')
day$cnt_ma = ma(day$clean_cnt, order=7) # using the clean count with no outliers
day$cnt_ma30 = ma(day$clean_cnt, order=30)
ggplot() +
  geom_line(data = day, aes(x = Date, y = clean_cnt, colour = "Counts")) +
  geom_line(data = day, aes(x = Date, y = cnt_ma,   colour = "Weekly Moving Average"))  +
  geom_line(data = day, aes(x = Date, y = cnt_ma30, colour = "Monthly Moving Average"))  +
  ylab('Bicycle Count')
#----------------------Decompose Your Data--------------
count_ma = ts(na.omit(day$cnt_ma), frequency=30)
decomp = stl(count_ma, s.window="periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp)
#--------------------------Stationarity--------------------
adf.test(count_ma, alternative = "stationary")
#--------------------------Autocorrelations and Choosing Model Order--------------------
Acf(count_ma, main='')

Pacf(count_ma, main='')

count_d1 = diff(deseasonal_cnt, differences = 1)
plot(count_d1)
adf.test(count_d1, alternative = "stationary")
Acf(count_d1, main='ACF for Differenced Series')
Pacf(count_d1, main='PACF for Differenced Series')
#---------------Fitting an ARIMA model---------------------
auto.arima(deseasonal_cnt, seasonal=FALSE)
#------------------Evaluate and Iterate--------------------
fit<-auto.arima(deseasonal_cnt, seasonal=FALSE)
tsdisplay(residuals(fit), lag.max=45, main='(1,1,1) Model Residuals')

fit2 = arima(deseasonal_cnt, order=c(1,1,7))

fit2

tsdisplay(residuals(fit2), lag.max=15, main='Seasonal Model Residuals')
fcast <- forecast(fit2, h=30)
plot(fcast)

hold <- window(ts(deseasonal_cnt), start=700)
fit_no_holdout = arima(ts(deseasonal_cnt[-c(700:725)]), order=c(1,1,7))
fcast_no_holdout <- forecast(fit_no_holdout,h=25)
plot(fcast_no_holdout, main=" ")
lines(ts(deseasonal_cnt))
fit_w_seasonality = auto.arima(deseasonal_cnt, seasonal=TRUE)
fit_w_seasonality
seas_fcast <- forecast(fit_w_seasonality, h=30)
plot(seas_fcast)
