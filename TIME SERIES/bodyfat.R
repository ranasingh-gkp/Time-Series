#bodyfat
#---------------------
install.packages("isdals")
library(isdals)
data(bodyfat)
attach(bodyfat)
pairs(cbind(Fat,Triceps,Thigh,Midarm))
cor(cbind(Fat,Triceps,Thigh,Midarm))
#--------------
fat.hat= predict(lm(Fat~  Thigh))
Triceps.hat = predict(lm(Triceps~ Thigh))
cor((Fat-fat.hat),(Triceps-Triceps.hat))
library(ppcor)
install.packages("ppcor")
pcor(cbind(Fat,Triceps,Thigh))$estimate
