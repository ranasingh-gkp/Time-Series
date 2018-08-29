beveridge.ts=ts(beveridge[,2], start = 1500)
plot(beveridge.ts,ylab='price', main="beveridge wheat price data")
beveridge.MA= filter(beveridge.ts, rep(1/31,31), sides=2)
lines(beveridge.MA, col="red")
par(mfrow=c(3,1))
y=beveridge.ts/beveridge.MA
y
plot(y, ylab="scaled price", main="transformed beveridge wheat price data")
acf(na.omit(y), main="autocorrelation function of transformed beveridge data")
acf(na.omit(y), type="partial",main="partial autocorrelation function of transformed beveridge data")
ar(na.omit(y),order.max=5) #ar() to estimate cofficient of ar(p) process

