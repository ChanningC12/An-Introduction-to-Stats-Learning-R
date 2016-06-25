# use the set.seed() function in order to set a seed for R’s random number generator
library(ISLR)
set.seed(1)
train = sample(392,196) # 196 out of 392 in train set
?sample

lm.fit = lm(mpg~horsepower,data=Auto,subset=train)
attach(Auto)
mean((mpg-predict(lm.fit,Auto))[-train]^2)

lm.fit2=lm(mpg∼poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2 ,Auto))[-train]^2)
lm.fit3=lm(mpg∼poly(horsepower,3),data=Auto,subset=train )
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

# LOOCV
# The LOOCV estimate can be automatically computed for any generalized, linear model using the glm() and cv.glm() functions
glm.fit = glm(mpg~horsepower_num,data=Auto)
coef(glm.fit)

library(boot)
glm.fit = glm(mpg~horsepower_num,data=Auto)
cv.err = cv.glm(Auto,glm.fit)
cv.err$delta
names(cv.err) # delta contains cross-validation results

cv.error=rep(0,5)
for (i in 1:5){
    glm.fit = glm(mpg~poly(horsepower,i),data=Auto)
    cv.error[i] = cv.glm(Auto,glm.fit)$delta[1]
}
cv.error

# k-fold Cross Validation
set.seed(17)
cv.error.10 = rep(0,10)
for (i in 1:10){
    glm.fit = glm(mpg~poly(horsepower,i),data=Auto)
    cv.error.10[i] = cv.glm(Auto,glm.fit,K=10)$delta[1]
}
cv.error.10


# Bootstrap
alpha.fn = function(data,index){
    X=data$X[index]
    Y=data$Y[index]
    return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}

alpha.fn(Portfolio,1:100) # tells R to estimate α using all 100 observations

set.seed(1)
alpha.fn(Portfolio,sample(100,100,replace=T))
boot(Portfolio,alpha.fn,R=1000)


boot.fn = function(data,index)
    return(coef(lm(mpg∼horsepower,data=data,subset=index)))
boot.fn(Auto,1:392)
set.seed(1)
boot.fn(Auto,sample(392,392,replace=T))


