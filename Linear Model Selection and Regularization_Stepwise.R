# 6.5.2 Forward and Backward Stepwise Selection
# forward
regfit.fwd = regsubsets(Salary~.,data=Hitters,nvmax=19,method="forward")
summary(regfit.fwd)
names(regfit.fwd)

# backward
regfit.bwd = regsubsets(Salary~.,data=Hitters,nvmax=19,method="backward")
summary(regfit.bwd)

coef(regfit.full,7)
coef(regfit.fwd,7)
coef(regfit.bwd,7)

# 6.5.3 Choosing Among Models Using the Validation Set Approach and Cross-Validation
# use only the training observations to perform all aspects of model-fitting
# split into training set and test set, create random vector, train
set.seed(1)
train = sample(c(TRUE,FALSE),nrow(Hitters),rep=TRUE)
test = (!train)

# apply to training set to perform best subset selection
regfit.best = regsubsets(Salary~.,data=Hitters[train,],nvmax=19)
test.mat = model.matrix(Salary~.,data=Hitters[test,])
val.errors = rep(NA,19)
for (i in 1:19){
    coefi = coef(regfit.best,id=i)
    pred = test.mat[,names(coefi)]%*%coefi
    val.errors[i] = mean((Hitters$Salary[test]-pred)^2)
}
val.errors
which.min(val.errors)
coef(regfit.best,10)


predict.regsubsets = function(object,newdata,id,...){
    form = as.formula(object$call[[2]])
    mat = model.matrix(form,newdata)
    coefi = coef(object,id=id)
    xvars = names(coefi)
    mat[,xvars]%*%coefi
}

# full data set and select the best tenvariable model
regfit.best = regsubsets(Salary~.,data=Hitters,nvmax=19)
coef(regfit.best,10)

# cross validation
k = 10
set.seed(1)
folds = sample(1:k,nrow(Hitters),replace=T)
cv.errors = matrix(NA,k,19,dimnames = list(NULL,paste(1:19)))
# In the jth fold, the elements of folds that equal j are in the test set
for (j in 1:k){
    best.fit = regsubsets(Salary~.,data = Hitters[folds!=j,], nvmax = 19)
    for (i in 1:19){
        pred = predict(best.fit,Hitters[folds==j,],id=j)
        cv.errors[j,i] = mean((Hitters$Salary[folds==j]-pred)^2)
    }
}

mean.cv.errors=apply(cv.errors,1,mean)
mean.cv.errors

