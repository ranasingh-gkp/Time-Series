#johnson hohnson datasets
#----------------------------------
install.packages("astsa")
library(astsa)
data("JohnsonJohnson")
JohnsonJohnson
plot(JohnsonJohnson, main= 'earning per share', col='blue', lwd=3)
# SO THIS data is not stationary so we will transform it
#------#log return of JohnsonJohnson
#if u want to use Yule-walker estimation equation , you have to shift dataset to get mean=0
jj.log.return=diff(log(JohnsonJohnson))
jj.log.return.mean.zero=jj.log.return-mean(jj.log.return)#subtract by mean so that we will get mean=0

# ----------plot for log return
par(mfrow=c(3,1))
plot(jj.log.return.mean.zero, main= 'log return(mean zero) of jj earning per share', col='blue', lwd=3)
acf(jj.log.return.mean.zero, main='acf', col='red', lwd=3)
pacf(jj.log.return.mean.zero, main='pacf', col='green', lwd=3)#acf show variation but pacf show there might be of order 2
# from pacf we can infer that p=4 after that all lag value have insignificant range
#--------order
p=4
#----sample auto correlation function
r=NULL
r[1:p]=acf(jj.log.return.mean.zero, plot = F)$acf[2:(p+1)]
cat('r-',r,'\n')
#-----matrix R
R=matrix(1,p,p)#matrix of dimension p*p with entries 1's
#define non-diagonal entries of r
for(i in 1:p){
  for (j in 1:p){
    if(i!=j)
      R[i,j]=r[abs(i-j)]
    
  }
}
R
#-------b-column VECTOR ON RIGHT
b=NULL
b=matrix(r,p,1)# b-column matrix with no entries
b
#--------- solve(R,b) solve Rx=b , and gives x=R^-1b vector
phi.hat=NULL
phi.hat=solve(R,b)[,1]
phi.hat
# ---------variance estimation using Yule-walker estimation
c0=acf(jj.log.return.mean.zero, type = 'covariance', plot = F)$acf[1]
c0
var.hat=c0*(1-sum(phi.hat*r))
var.hat
#------------constant term in model
ph10.hat=mean(jj.log.return)*(1-sum(phi.hat))
ph10.hat
cat("constant:", ph10.hat,"coefficient:", phi.hat, "and variance:", var.hat, '\n')
