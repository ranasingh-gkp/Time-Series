#Daily_birth_rate_in_california_1959
#---------------------------------------
install.packages("astsa")
library(astsa)
number_of_births <- Births_California_1959$`female birth`
#use date formet for dates
Births_California_1959$Date <- as.Date(Births_California_1959$Date, "%m/%d/%Y")
#---PLOT THE SERIES-
plot(number_of_births ~ Births_California_1959$Date, type='l', main= "daily total female birth in california 1959", ylab = "no of births")
#this must not be stationary because some trend going on
#--------type of correlation---check statistics
Box.test(number_of_births, lag = log(length(number_of_births)))
# p value is very very small so reject null hypothesis means there is some correlation in model,

#----plot the differenced time series
# there is some trend going on so in order to remove trend we will use diff() function
plot(diff(number_of_births) ~ Births_California_1959$Date[1:364],type= 'l',  main = "differenced series", ylab = '', xlab='date')
#there is some peak in sepetember so we can ignore that outlier, so we have stationary condition
# again test
Box.test(diff(number_of_births), lag = log(length(diff(number_of_births))))
#----------acf and pacf---
# that may also suggest some autocorrelation
acf(diff(number_of_births), main= "acf of difference data", 50)
# acf suggest moving average correlation relation, here we can ignore one outlier which come at lag 21, so finally we have order 2.
pacf(diff(number_of_births), main= "acf of difference data", 50)
# we can see lot of signficant lag upto lag 7, we can ignore lag 21.

# ---fit various ARIMA model--
model1 <- arima(number_of_births, order=c(0,1,1))# MA(1) for difference data, acf have 2 node so 1,2 model
SEE1 <- sum(model1$residuals^2)  # WE WANt SEE as small as possible
model1.test <- Box.test(model1$residuals,  lag = log(length(model1$residuals)))

model2 <- arima(number_of_births, order=c(0,1,2))# MA(2) for difference data
SEE2 <- sum(model2$residuals^2)
model2.test <- Box.test(model2$residuals,  lag = log(length(model2$residuals)))

model3 <- arima(number_of_births, order=c(7,1,1))# MA(1) for difference data, pacf show 7 order AC(7),MA(1)
SEE3 <- sum(model3$residuals^2)
model3.test <- Box.test(model3$residuals,  lag = log(length(model3$residuals)))
                
model4 <- arima(number_of_births, order=c(7,1,2))# MA(1) for difference data,   AC(7),MA(2)
SEE4 <- sum(model4$residuals^2)
model4.test <- Box.test(model4$residuals,  lag = log(length(model4$residuals)))

df <- data.frame(row.names = c('AIC','SEE','p-value'), c(model1$aic,SEE1,model1.test$p.value),c(model2$aic,SEE2,model2.test$p.value),c(model3$aic,SEE3,model3.test$p.value),c(model4$aic,SEE4,model4.test$p.value))
colnames(df) <- c('arima(0,1,1)','arima(0,1,2)','arima(7,1,1)','arima(7,1,2)')
format(df, scientific= FALSE)
#result::
#1)we can not reject null hypothesis (all p value are more then 10%) so there is no autocorrelation
#2)  if you are going for minimum AIC value so model will be according to arima(0,1,2)
#3)if you are going for minimum SEE value so model will be according to arima(7,1,2)
# finally we are going to use simplest model (0+1+2=3 parameter),(7+1+2=10 parameter), so we are going to use (0,1,2) modelhave 3 parameter
#----run sarima model
sarima(number_of_births, 0,1,2,0,0,0)   #Seasonal ARIMA
#so constant term is not significant, still we will accept this model
#here difference is 1.
#X(t)= X(t-1)+0.0150-0.8511Z(t-1) -0.1113Z(t-2)  where Z(t) ~ Normal(0,49.08)
