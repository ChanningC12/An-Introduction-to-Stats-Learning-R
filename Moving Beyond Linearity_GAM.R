# 7.8.3 GAM
gam1 = lm(wage~ns(year,4)+ns(age,5)+education,data=Wage)

library(gam)
# s() function is used to indicate that we would like to use a smoothing spline
gam.m3 = gam(wage~s(year,4)+s(age,5)+education,data=Wage)
par(mfrow=c(1,3))
plot.gam(gam.m3,se=TRUE,col="blue")
plot.gam(gam1,se=TRUE,col="blue")
# ANOVA to determine which ofthese three models is best: 
# M1: GAM that excludes year
# M2: GAM that uses a linear function of year
# M3: GAM that uses a spline function of year
gam.m1 = gam(wage~s(age,5)+education,data=Wage)
gam.m2 = gam(wage~year+s(age,5)+education,data=Wage)
anova(gam.m1,gam.m2,gam.m3,test="F")
# We find that there is compelling evidence that a GAM with a linear function
## of year is better than a GAM that does not include year at all
### (p-value=0.00014). However, there is no evidence that a non-linear function
#### of year is needed (p-value=0.349). In other words, based on the results
##### of this ANOVA, M2 is preferred.

summary(gam.m3)

preds = predict(gam.m2,newdata=Wage)
# we can also use local regression fits as building blocks in a GAM using lo()
gam.lo = gam(wage~s(year,df=4)+lo(age,span=0.7)+education,data=Wage)
# we can also use the lo() to create interaction between year and age
library(akima)

# logistic regression GAM
gam.lr = gam(I(wage>250)~year+s(age,df=5)+education,family=binomial,data=Wage)
par(mfrow=c(1,3))
plot(gam.lr,se=T,col="green")
table(education,I(wage>250))

gam.lr.s = gam(I(wage>250)~year+s(age,df=5)+education,family=binomial,data=Wage,subset=(education!="1. < HS Grad"))
plot(gam.lr.s,se=T,col="green")
