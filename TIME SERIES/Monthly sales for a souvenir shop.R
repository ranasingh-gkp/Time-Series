#Monthly sales for a souvenir shop
#---------------------------------------
install.packages("astsa")
library(astsa)
suv<-ts(SUV$Sales)
sales<-ts(Monthly_sales_for_souvenir_shop$sale)
par(mfrow=c(3,1))
plot(sales, main= 'monthly sale', col='blue', lwd=3)
# SO THIS data is not stationary so we will transform it
acf(sales, main='acf', col='red', lwd=3,80)
pacf(sales, main='pacf', col='green', lwd=3,80)
#so data is not stationary
#non seasional trend also because all value almost increasing
#there is change in variation
# there is seasionality also
# if there is any variation in variance we will go for transformation
#so log transformation, non seasional/seasional differencing
# span of seasionality is 12 month
#------------one by one check all model
par(mfrow=c(4,1))
plot(sales, main= 'monthly sales', col='blue', lwd=3)
sales1= log((sales),12) # D=1,d=1 ONE SEASIONALITY AND ONE NON SEASIONALITY
plot(sales1, main= 'monthly sales', col='red', lwd=3)
sales2= diff(log((sales),12)) # D=1,d=1 ONE SEASIONALITY AND ONE NON SEASIONALITY
plot(sales2, main= 'monthly sales', col='green', lwd=3)
sales3= diff(diff(log((sales),12))) # D=1,d=1 ONE SEASIONALITY AND ONE NON SEASIONALITY
plot(sales3, main= 'monthly sales', col='black', lwd=3)
#so we assume that sales3 is stationary time series
par(mfrow=c(3,1))
plot(sales3, main= 'monthly sales', col='black', lwd=3)
acf(sales3, main='acf', col='red', lwd=3,50)
pacf(sales3, main='pacf', col='green', lwd=3,50)
#ACF  q=0,1, Q=0,1,2,3
#PACF  p=0,1  P=0,1
#plot acf abd pacf both use acf2(sales3, 50)
#--------------check model value
d=1
DD=1
per=12
for(p in 1:2){
  for(q in 1:2){
    for(i in 1:2){
      for(j in 1:4){
        if(p+d+q+i+DD+j<=10){
          model<-arima(x=log(sales), order = c((p-1),d,(q-1)), seasonal = list(order=c((i-1),DD,(j-1)), period=per))
          pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
          sse<-sum(model$residuals^2)
          cat(p-1,d,q-1,i-1,DD,j-1,per, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
        }
      }
    }
  }
}
#----------residual analysis
sarima(sales, 1,1,0,0,1,1,12)
# we can see that its almost stationary except at one point after 100, we can ignore as outlier
# there is no significant autocorrelation of residual
# there is systematic departure departure in normality.
#p value is more then 10% so we can say that there is significant autocorrelation
# we can assume that residual at this point is white noise
model<- arima(x=log(sales), order = c(1,1,0), seasonal = list(order=c(0,1,1), period=12))
plot(forecast(model))
forecast(model)
a<-sarima.for(log(sales),12,1,1,0,0,1,1,12)
plot.ts(c(sales,exp(a$pred)), main='Monthly sales + Forecast', ylab='', col='blue', lwd=3)

