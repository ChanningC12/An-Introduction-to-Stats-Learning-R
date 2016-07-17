# Maximal Margin Classifier
# Support Vector Classifier: natural approach for classification in the two-class setting, if the boundary between the two classes is linear
# Support Vector Machine: An extension of the support vector classifier that results from enlarging the feature space in a specific way using kernels
library(e1071) # LiblineaR for very large linear problems
# svm() function can be used to fit a support vector classifier when argument kernel="linear"
# cost argument allows to specify cost of a violation to the margin
set.seed(1)
x=matrix(rnorm(20*2),ncol=2)
y=c(rep(-1,10),rep(1,10))
x[y==1,]=x[y==1,]+1

# check whether the classes are linearly separable
plot(x,col=(3-y)) # color = 3-y
dat = data.frame(x=x,y=as.factor(y))
library(e1071)
svmfit = svm(y~.,data=dat, kernal="linear",cost=10,scale=F) # scale=F tells svm not to scale each feature to have mean 0 and sd 1
plot(svmfit,dat)
# support vectors
svmfit$index

summary(svmfit)

# try a smaller value of the cost parameter
svmfit = svm(y~.,data=dat,kernel="linear",cost=0.1,scale=F)
plot(svmfit,dat)
svmfit$index
# the svm() function does not explicitly output the coefficients of the linear decision boundary obtained when the support vector classifier is fit, nor does it output the width of the margin.
# tune() performs cross-validation
set.seed(1)
tune.out = tune(svm,y~.,data=dat,kernel="linear",ranges=(list(cost=c(0.001,0.01,0.1,1,5,10,100))))
summary(tune.out)
# best performance when cost=0.1
bestmod = tune.out$best.model
summary(bestmod)

# generate test data
xtest = matrix(rnorm(20*2),ncol=2)
ytest = sample(c(-1,1),20,rep=T)
xtest[ytest==1,]=xtest[ytest==1,]+1
testdat = data.frame(x=xtest,y=as.factor(ytest))

ypred = predict(bestmod,testdat)
table(predict=ypred,truth=testdat$y)

# when cost is 0.01
svmfit=svm(y~.,data=dat,kernel="linear",cost=0.01,scale=F)
ypred=predict(svmfit,testdat)
table(predict=ypred,truth=testdat$y)


# Consider a situation in which the two classes are linearly separable
x[y==1,]=x[y==1,]+0.5
plot(x,col=(y+5)/2,pch=19)
dat = data.frame(x=x,y=as.factor(y))
svmfit = svm(y~.,data=dat,kernel="linear",cost=1e5)
summary(svmfit)
plot(svmfit,dat)
svmfit$index
# cost = 1
svmfit = svm(y~.,data=dat,kernel="linear",cost=1)
summary(svmfit)
plot(svmfit,dat)
svmfit$index










