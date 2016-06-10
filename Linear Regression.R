# 1. Is there a relationship between advertising and sales and budget?
# -- fitting multiple regression and test the hypothsis, b1 = b2 = 0
# 2. How strong is the relationship
# -- RSE and R square
# 3. Which media contribute to sales?
# -- p-value
# 4. How large is the effect of each medium on sales?
# -- 95% of the interval of the coeffienct should be far from zero to indicate strong effect
# -- Test VIF on each predictor variable, low suggest no collinearity
# 5. How accurate can we predict the sales?
# -- RSS, notice R-square = 1 - RSS/TSS
# 6. Is the relationship linear?
# -- residual plot should have no pattern, if there is a pattern, then use tranformation on predictor variable
# 7. Is the synergy among the advertising media?
# -- Additive model

# KNN (K-nearest neighbors regression)

library(MASS)
library(ISLR)
# Dataset: Boston which records medv (median house value) for 506 neighborhoods around Boston
names(Boston)
dim(Boston)
head(Boston)
?Boston
lm.fit = lm(medv~lstat,data = Boston)
summary(lm.fit)
names(lm.fit)
coef(lm.fit)
confint(lm.fit)
predict(lm.fit,data.frame(lstat=c(c(5,10,15))),interval="confidence")

plot(Boston$lstat,Boston$medv)
abline(lm.fit,lwd=3,col="red") # lwd: line width, pch: plotting symbol

plot(predict(lm.fit),residuals(lm.fit))
plot(predict(lm.fit),rstudent(lm.fit))

plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

# Multiple linear regression
lm.fit = lm(medv~lstat + age, data = Boston)
summary(lm.fit)
lm.fit = lm(medv~.,data=Boston)
summary(lm.fit)
lm.fit1 = lm(medv~.-age,data=Boston)
summary(lm.fit1)

# Interaction Terms
# lstat*age = lstat + age + lstat:age
summary(lm(medv~lstat*age,data=Boston))

# Non-linear tranformation of the predictors
# I()
lm.fit2 = lm(medv~lstat + I(lstat^2),data=Boston)
summary(lm.fit2)


lm.fit = lm(medv~lstat,data=Boston)
anova(lm.fit,lm.fit2) # The anova() function performs a hypothesis test comparing the two models

par(mfrow=c(2,2))
plot(lm.fit2)

# use poly() to create polynomial within lm()
lm.fit5 = lm(medv~poly(lstat,5),data=Boston)
summary(lm.fit5)

# use log
summary(lm(medv~log(rm),data=Boston))

# Qualitative Predictors
names(Carseats)
lm.fit = lm(Sales ~. + Income:Advertising + Price:Age, data = Carseats)
summary(lm.fit)

# contrasts() function returns the coding that R uses for the dummy variables
attach(Carseats)
contrasts(ShelveLoc)

# Exercise
View(Auto)
names(Auto)
str(Auto)
fit.lm = lm(mpg~as.numeric(horsepower),data=Auto)
summary(fit.lm)
predict(fit.lm,data.frame(horsepower=98))
confint(fit.lm)
par(mfrow=c(1,1))
Auto$horsepower_num = as.numeric(Auto$horsepower)
plot(Auto$horsepower_num,Auto$mpg)
abline(fit.lm,lwd=3,col="red")
plot(fit.lm)





     