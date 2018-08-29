#recrument
#-----------------------------
install.packages("astsa")
library(astsa)
my.data= rec
plot(rec, main="recrument time series", col='blue', lwd='2')
# subtract mean to get time series with mean=0
ar.process=my.data-mean(my.data)
#acf and pacf of process
par(mfrow=c(2,1))
acf(ar.process, main='recrument', col='red', lwd=3)
pacf(ar.process, main='recrument', col='green', lwd=3)#acf show variation but pacf show there might be of order 2
#order
p=2
#sample autocorrelation function
r=NULL
r[1:p]=acf(ar.process, plot = F)$acf[2:(p+1)]
cat('r-',r,'\n')
#matrix r
R=matrix(1,p,p)#matrix of dimension p*p with entries 1's
#define non-diagonal entries of r
for(i in 1:p){
  for (j in 1:p){
    if(i!=j)
      R[i,j]=r[abs(i-j)]
    
  }
}
R
#B-COLUMN VECTOR ON RIGHT
b=NULL
b=matrix(r,p,1)# b-column matrix with no entries
b
# solve(R,b) solve Rx=b , and gives x=R^-1b vector
phi.hat=NULL
phi.hat=solve(R,b)[,1]
phi.hat
# variance estimation using Yule-walker estimation
c0=acf(ar.process, type = 'covariance', plot = F)$acf[1]
c0
var.hat=c0*(1-sum(phi.hat*r))
var.hat
#constant term in model
ph10.hat=mean(my.data)*(1-sum(phi.hat))
ph10.hat
cat("constant:", ph10.hat,"coefficient:", phi.hat, "and variance:", var.hat, '\n')
