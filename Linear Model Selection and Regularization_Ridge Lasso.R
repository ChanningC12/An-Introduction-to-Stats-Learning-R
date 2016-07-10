# 6.6 Lab 2: Ridge Regression and the Lasso
# glmnet package
View(Hitters)
x = model.matrix(Salary~.,data=Hitters)[,-1]
y = Hitters$Salary
library(glmnet)
grid = 10^seq(10,-2,length=100)
grid
ridge.mod = glmnet(x,y,alpha=0,lambda=grid)
dim(coef(ridge.mod))
ridge.mod$lambda[50] # when lambda = 11,498
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))

ridge.mod$lambda[60] # when lambda = 705
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))

predict(ridge.mod,s=50,type="coefficients")[1:20,] # predict when lambda = 50

# split to train / test
set.seed(1)
train = sample(1:nrow(x),nrow(x)/2)
test=(-train)
y.test=y[test]

ridge.mod = glmnet(x[train,],y[train],alpha=0,lambda=grid,thresh=1e-12) # build model using train
ridge.pred = predict(ridge.mod,s=4,newx=x[test,]) # lambda = 4, ridge test predict
mean((ridge.pred-y.test)^2) # MSE
mean((mean(y[train])-y.test)^2)

ridge.pred = predict(ridge.mod,s=0,newx=x[test,],exact=T)
mean((ridge.pred-y.test)^2) # MSE
lm(y~x,subset=train)
predict(ridge.mod,s=0,exact=T,type="coefficients")[1:20,] # lambda = 0, LM model

# with cross validation
set.seed(1)
cv.out = cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam = cv.out$lambda.min # best lambda with the minimum train errors
bestlam
# test error
ridge.pred = predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2)

# same as
out = glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:20,]

# none of the coefficients is zero

# Lasso
lasso.mod = glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)

# cross validation
set.seed(1)
cv.out = cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam = cv.out$lambda.min
lasso.pred = predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred - y.test)^2)
# lasso has a substantial advantage over ridge regression in that the resulting coefficient estimates are sparse
out = glmnet(x,y,alpha=1,lambda=grid)
lasso.coef = predict(out,type="coefficients",s=bestlam)[1:20,]
lasso.coef
