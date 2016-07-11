# Chapter 7 - Moving beyond linearity
# examining very simple extensions of linear models like polynomial regression and step functions, splines, local regression, and generalized additive models

# Polynomial regression
# Step function: cut the range of a variable into K distinct regions in order to produce a qualitative variable
# Regression splines: involve dividing the range of X into K distinct regions. Within each region, a polynomial function is fit to the data. They join smoothly at the region boundaries or knots
# Smoothing splines: Smoothing splines result from minimizing a residual sum of squares criterion subject to a smoothness penalty
# Local regression: The regions are allowed to overlap
# GAM: allow us to extend the methods above to deal with multiple predictors

# A cubic spline with K knots uses a total of 4+K degreee of freedom
# backfitting: fits a model involving multiple predictors by repeatedly updating the fit for each predictor in turn

library(ISLR)
attach(Wage)

# Polynominal regeression and step functions

fit = lm(wage~poly(age,4),data = Wage)  # poly() command allows us to avoid having to write out a long formula with powers of age. 
coef(summary(fit))

fit2 = lm(wage~poly(age,4,raw=T),data=Wage)
coef(summary(fit2))

fit2a = lm(wage~age+I(age^2)+I(age^3)+I(age^4),data=Wage) # I() wrapper function
coef(fit2a)

fit2b = lm(wage~cbind(age,age^2,age^3,age^4),data=Wage) # cbind() build a matrix from a collention of vectors

# create a grid of values of age
agelims = range(age) # min 18 max 80
age.grid = seq(from=agelims[1],to=agelims[2]) # create a vector range from 18 to 80 by 1
preds = predict(fit,newdata=list(age=age.grid),se=T)
se.bands = cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)

# plot the data and add the fit from the degree-4 polynomial
par(mfrow=c(1,2),mar=c(4.5,4.5,1,1),oma=c(0,0,4,0)) # mar, oma and par allow us to control the margins of the plot
plot(age,wage,xlim=agelims,cex=0.5,col="darkgrey")
title("Degree-4 Polynomial",outer=T)
lines(age.grid,preds$fit,lwd=2,col="blue")
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)

# anova(), M1 and M2 must be nested models
# anova performs an analysis of variance in order to test the null hypothesis that a model M1 is sufficient to explain the data against the alternative hypothesis that a more complex model M2 is required
fit.1 = lm(wage~age,data=Wage)
fit.2 = lm(wage~poly(age,2),data=Wage)
fit.3 = lm(wage~poly(age,3),data=Wage)
fit.4 = lm(wage~poly(age,4),data=Wage)
fit.5 = lm(wage~poly(age,5),data=Wage)
anova(fit.1,fit.2,fit.3,fit.4,fit.5)
# The p-value comparing the linear Model 1 to the quadratic Model 2 is essentially zero (<10−15), indicating that a linear fit is not sufficient.
# Similarly the p-value comparing the quadratic Model 2 to the cubic Model 3 is very low (0.0017), so the quadratic fit is also insufficient.
# either a cubic or a quartic polynomial appear to provide a reasonable fit to the data

# ANOVA method works whether or not we used orthogonal polynomial
fit.1 = lm(wage~education+age,data=Wage)
fit.2 = lm(wage~education+poly(age,2),data=Wage)
fit.3 = lm(wage~education+poly(age,3),data=Wage)
anova(fit.1,fit.2,fit.3)


fit = glm(I(wage>250)~poly(age,4),data=Wage,family=binomial) # T equals 1, F equals 0
preds = predict(fit,newdata=list(age=age.grid),se=T)

pfit = exp(preds$fit)/(1+exp(preds$fit))
se.bands.logit = cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)
preds = predict(fit,newdata=list(age=age,grid),type="response")

plot(age,I(wage>250),xlim=agelims,type="n",ylim=c(0,0.2))
points(jitter(age),I((wage>250)/5),cex=0.5,pch="1",col="darkgrey")

table(cut(age,4))
fit = lm(wage~cut(age,4),data=Wage)
coef(summary(fit))
