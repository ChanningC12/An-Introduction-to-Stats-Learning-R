# some ways in which the simple linear model can be improved, by replacing plain least squares fitting with some alternative fitting procedures
# better prediction accuracy and model interpretability.

# Accuracy

# if n is not much larger than p
## there can be a lot of variability in the least squares fit,
### resulting in overfitting and consequently poor predictions on future
#### observations not used in model training.

# if p > n, then there
## is no longer a unique least squares coefficient estimate: the variance
### is infinite so the method cannot be used at all.

# Interpretability
# automatically performing feature selection or variable selection
## excluding irrelevant variables from a multiple regression model




# 6.5.1 Best Subset Selection
# Hitter data: predict a baseball player’s Salary on the basis of various statistics associated with performance in the previous year
library(ISLR)
names(Hitters)
dim(Hitters)
head(Hitters)

sum(is.na(Hitters$Salary)) # how many salary is missing
Hitters = na.omit(Hitters) # remove all the NAs
dim(Hitters)
sum(is.na(Hitters))

# regsubsets() performs the best subset selection, best is quantified using RSS
library(leaps)
regfit.full = regsubsets(Salary~.,Hitters)
summary(regfit.full) # the best two-variable model contains only Hits and CRBI
# By default, regsubsets() only reports results up to the best eight-variable model
# use nvmax() to specify the best n models
regfit.full = regsubsets(Salary~.,Hitters, nvmax=19)
summary(regfit.full)
reg.summary = summary(regfit.full)
names(reg.summary)
reg.summary$rsq
par(mfrow=c(2,2))
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adj R Square",type="l") # type="l" option tells R to connect the plotted points with lines

which.max(reg.summary$adjr2)
points(11,reg.summary$adjr2[11],col="red",cex=2,pch=20)

# CP
plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
which.min(reg.summary$cp)
points(10,reg.summary$cp[10],col="red",cex=2, pch=20)

# BIC
plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
which.min(reg.summary$bic)
points(6,reg.summary$bic[6],col="red",cex=2, pch=20)
# same as
points(which.min(reg.summary$bic),reg.summary$bic[which.min(reg.summary$bic)],col="red",cex=2, pch=20)

# see coef within best model
coef(regfit.full,6)



