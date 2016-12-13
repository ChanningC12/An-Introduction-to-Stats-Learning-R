# PCR and PLS Regression
# Principal Components Regression
# pcr() is part of the library pls
library(pls)
set.seed(2)
pcr.fit = pcr(Salary~.,data=Hitters,scale=TRUE,validation="CV")
# scale=TRUE has the effect of standardizing each predictor
# Setting validation="CV" causes pcr() to compute the ten-fold cross-validation error for each possible value of M
summary(pcr.fit)
# a root mean squared error of 352.8 corresponds to an MSE of 352.82 = 124,468
validationplot(pcr.fit,val.type="MSEP")
pcr.fit = pcr(Salary~.,data=Hitters,subset=train,scale=TRUE,validation="CV")
validationplot(pcr.fit,val.type="MSEP")
pcr.pred = predict(pcr.fit,x[test,],ncomp=7)
mean((pcr.pred-y.test)^2)
summary(pcr.fit)

pcr.fit = pcr(y~x,scale=TRUE,ncomp=7)
summary(pcr.fit)

# Partial Least Squares: plsr()
set.seed(1)
pls.fit = plsr(Salary~.,data=Hitters,subset=train,scale=T,validation="CV")
summary(pls.fit)
validationplot(pls.fit)
pls.pred = predict(pls.fit,x[test,],ncomp=2)
mean((pls.pred - y.test)^2)
÷# perform pls on full dataset
pls.fit = plsr(Salary~.,data=Hitters,scale=T,ncomp=2)
summary(pls.fit)
# the percentage of variance in Salary that the two-component PLS fit explains, 46.40%