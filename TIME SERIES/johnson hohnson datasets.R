#johnson hohnson datasets
#----------------------------------
install.packages("astsa")
library(astsa)
data("JohnsonJohnson")
JohnsonJohnson
plot(JohnsonJohnson, main= 'earning per share', col='blue', lwd=3)
# SO THIS data is not stationary so we will transform it
#-----------------------------transformation
# so here we will take difference logerthmic to stabilize data 
jj.log.return=diff(diff(log(JohnsonJohnson)),4)  # non-seasional and seasional defferenced logarithmic of earning

#---------plot log return
par(mfrow=c(3,1))
plot(jj.log.return, main= 'log return(mean zero) of jj earning per share', col='blue', lwd=3)
acf(jj.log.return, main='acf', col='red', lwd=3)
# here there is very much autocorrelation in data at lag 4,8 i.e because os seasonality
pacf(jj.log.return, main='pacf', col='green', lwd=3)#acf show variation but pacf show there might be of order 2
# here there is very much autocorrelation in data at lag 4,8 i.e because os seasonality
#------Ljung-Box test
library(astsa)

d=1
DD=1

per=4

for(p in 1:2){
  for(q in 1:2){
    for(i in 1:2){
      for(j in 1:2){
        if(p+d+q+i+DD+j<=10){
          model<-arima(x=log(jj), order = c((p-1),d,(q-1)), seasonal = list(order=c((i-1),DD,(j-1)), period=per))
          pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
          sse<-sum(model$residuals^2)
          cat(p-1,d,q-1,i-1,DD,j-1,per, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
        }
      }
    }
  }
}

Box.test(jj.log.return, lag= log(length(jj.log.return)))
p-value # is very small so reject null hypothesis , there is no autocorrelation between previous lags of seasonal and non-seasonal differenced logarithm of earning per j&j share


















