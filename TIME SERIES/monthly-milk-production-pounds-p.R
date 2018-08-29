#monthly-milk-production-pounds-p
#-------------------------------------------
install.packages("astsa")
library(astsa)
Milk<-monthly_milk_production_pounds$Milk
par(mfrow=c(3,1))
plot(Milk, main= 'monthly milk prod', col='blue', lwd=3)
# SO THIS data is not stationary so we will transform it
acf(Milk, main='acf', col='red', lwd=3)
pacf(Milk, main='pacf', col='green', lwd=3)
# so there is seasionality in data and pacf also show that there is some autocorrelation in data
# so we will remove this seasionality by using difference of differencing
milk_return= diff(diff(Milk),12) # D=1,d=1 ONE SEASIONALITY AND ONE NON SEASIONALITY
par(mfrow=c(3,1))
plot(milk_return, main= 'monthly milk prod', col='blue', lwd=3)
acf(milk_return, main='acf', col='red', lwd=3)
pacf(milk_return, main='pacf', col='green', lwd=3)
# we found two spike near 100 in first graph, we are assuming these are outlier
#ACF   q=0; Q=0,1,2,3
#PACF  p=0; P=0,1,2
#---------------
library(astsa)
install.packages("forecast")
library(forecast)

d=NULL
DD=NULL
d=1
DD=1

per=12
for(p in 1:1){
  for(q in 1:1){
    for(i in 1:3){
      for(j in 1:4){
        if(p+d+q+i+DD+j<=10){     #the parsimony assumption
          model<-arima(x=Milk, order = c((p-1),d,(q-1)), seasonal = list(order=c((i-1),DD,(j-1)), period=per))
          pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
          sse<-sum(model$residuals^2)
          cat(p-1,d,q-1,i-1,DD,j-1,per, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
        }
      }
    }
  }
}
sarima(Milk, 0,1,0,0,1,1,12)
# we can see that its almost stationary except at one point after 100, we can ignore as outlier
# there is no significant autocorrelation of residual
# there is systematic departure departure in normality.
#p value is more then 10% so we can say that there is significant autocorrelation
# we can assume that residual at this point is white noise

# WE SELECT smallest AIC value: which means second row, no segenfecnt autocorrelation left in resudual , so model is going to be (0 1 0 0 1 1) with span of seasionality 12
model<- arima(x=Milk, order = c(0,1,0), seasonal = list(order=c(0,1,1), period=12))
plot(model)
plot(forecast(model))
forecast(model)
