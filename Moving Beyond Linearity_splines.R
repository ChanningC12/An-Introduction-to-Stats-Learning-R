# 7.8.2 Splines
library(splines)
# the bs() function generates the entire matrix of basis functions for splines with the specified set of knots
# By default, cubic splines are produced
fit = lm(wage~bs(age,knots=c(25,40,60)),data=Wage)
pred = predict(fit,newdata=list(age=age.grid),se=T)
plot(age,wage,col="grey")
lines(age.grid,pred$fit,lwd=2)
lines(age.grid,pred$fit+2*pred$se,lty="dashed")
lines(age.grid,pred$fit-2*pred$se,lty="dashed")

dim(bs(age,knots=c(25,40,60)))
dim(bs(age,df=6))
attr(bs(age,df=6),"knots") # R choose the knots to be 33.75, 42.00 and 51.00 which represents the 25th, 50th and 75th percentiles

# ns() to fit natural splines
fit2 = lm(wage~ns(age,df=4),data=Wage)
pred2 = predict(fit2,newdata=list(age=age.grid),se=T)
lines(age.grid,pred2$fit,col="red",lwd=2)

# In order to fit a smoothing splines, we use the smooth.splines() function
plot(age,wage,xlim=agelims,cex=0.5,col="darkgrey")
title("Smoothing Splines")
fit = smooth.spline(age,wage,df=16)
fit2 = smooth.spline(age,wage,cv=TRUE)
fit2$df
lines(fit,col="red",lwd=2)
lines(fit2,col="blue",lwd=2)
legend("topright",legend=c("16 DF", "6.8 DF",col=c("red","blue"),lty=1,lwd=2,cex=0.8))

# local regression: loess
# Here we have performed local linear regression using spans of 0.2 and 0.5: that is, each neighborhood consists of 20% or 50% of the observations
# The larger the span, the smoother the fit. The locfit library can also be used for fitting local regression models in R
plot(age,wage,xlim=agelims,cex=0.5,col="darkgrey")
title("Local Regression")
fit = loess(wage~age,span=0.2,data=Wage)
fit2 = loess(wage~age,span=0.5,data=Wage)
lines(age.grid,predict(fit,data.frame(age=age.grid)),col="red",lwd=2)
lines(age.grid,predict(fit2,data.frame(age=age.grid)),col="blue",lwd=2)
legend("topright",legend=c("Span=0.2", "Span=0.5",col=c("red","blue"),lty=1,lwd=2,cex=0.8))

















