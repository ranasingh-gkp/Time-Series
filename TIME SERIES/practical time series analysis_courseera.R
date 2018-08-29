install.packages("astsa")
library(astsa)
help(jj)
#-------------
data(jj)  #not tequire plot.ts(), its already time series data
plot(jj)  #there is sesional variation, beginning variation not so high but later variation more
#-----
help(flu)
data(flu)
plot(flu)  #not tequire plot.ts(), its already time series data
#overall going down
#--------------------
help(globtemp)
data(globtemp)
plot(globtemp)
#---------------
help(globtempl)
data(globtempl)
plot(globtempl)
#------------------
help(star)
data(star)
plot(star)
#-------------------------------
#----------------stationarity
#we do not want any chance in statismatic mean, variance, periodic fluctuation(periodic variance)
#in stationary, one property of time series is same as the another part of time series.
# if we have non-stationary time series, we will do some transformation to get stationary time series
#strictly stationary if joint distribution of X(t) and X(t+1) is same.
#---------------auto-coveriance--------
#recall random variables and covariance of two random variables
#??? characterize time series as a realization ofa stochastic process
#??? define autocovariance functi
# define as taking coveriance of different sequence of coveriance
#gama= gama(t,t+k)~c
#gama said same when coverience of (1,1+k) is same as the (10+10+k)
#acf() auto-variance coefficient function
#acf(time_series, type='covariance')
purely_random_process= ts(rnorm(100))# generate 100 dataset and put them into time series
print(purely_random_process)
(acf(purely_random_process, type='covariance'))
(acf(purely_random_process, main='correlogram plot for purely_random_process'))# it always start with "1" 
#plots autocorrelation coefficient at different lags::correlogram
# above plot does not show enough variation because we already chosen random no, above two line show significance level so value are under range at different lag
#-----------------------------random walk---------
x=NULL
x[1]=0
for (i in 2:1000) {
  x[i]=x[i-1]+rnorm(1)  #residual(mean=0,deviation=1)
}
print(x)  #does not have time series structure
random_walk=ts(x) # transform data into time series
random_walk
plot(random_walk,main='a random walk', xlab='', ylab='', col='blue', lwd=2)
# we can see high trend (high coveriance)
# so data is not stationary, can we remove it
# by using difference
#----------difference
plot(diff(random_walk))  # diff produce difference of x2-x1,x3-x2....we will not find x0
acf(diff(random_walk))
#========moving average process
#MA(q) MODEL....q is order of moving average model
#X(t)= Z(t)+ theta1*z(t-1)+theta2*Z(t-2)+.....(theta1,theta2 are noise of t-1, and t-2 time respectivelly)
#Z(i)is noise is normal random no with mean and variance
# we are using noise with standard distribution
noise =rnorm(10000)
ma_2=NULL
for (i in 3:10000) {
  ma_2[i]=noise[i]+0.7*noise[i-1]+0.2*noise[i-2]
}
moving_average_process=ma_2[3:10000]
moving_average_process=ts(moving_average_process)
par(mfrow=c(2,1))
plot(moving_average_process, main = 'a moving_average_process of order 2', ylab = '',col='blue')
acf(moving_average_process, main = 'corellagram of moving_average_process of order 2')
# in acf noise comming from two day back
# in MA(q=2) process acf has to cut at lag 2(in acf diagram third line is lag2 line)
