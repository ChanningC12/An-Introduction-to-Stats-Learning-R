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



# Support Vector Machine
set.seed(1)
x=matrix(rnorm(200*2),ncol=2)
x[1:100,]=x[1:100,]+2
x[101:150,]=x[101:150,2]-2
y = c(rep(1,150),rep(2,50))
dat = data.frame(x=x,y=as.factor(y))

plot(x,col=y)
# The data is randomly split into training and testing groups. Fit the training data using the svm()
train = sample(200,100)
svmfit = svm(y~.,data=dat[train,],kernel="radial",gamma=1,cost=1)
plot(svmfit,dat[train,])

summary(svmfit)
# increase the cost, reduce the training error but at the expense of overfitting the data
svmfit = svm(y~.,data=dat[train,],kernel="radial",gamma=1,cost=1e5)
plot(svmfit,dat[train,])

# Perform cross-validation using tune()
set.seed(1)
tune.out = tune(svm,y~.,data=dat[train,],kernel="radial",ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))
summary(tune.out) # get the best model with best parameter values for cost, gamma
table(true=dat[-train,"y"],pred=predict(tune.out$best.model,newx=dat[-train,])) # determine the ratio of test observations being misclassified

# ROC Curves
library(ROCR)
rocplot = function(pred,truth, ...){
    predob = prediction(pred,truth)
    perf = performance(predob,"tpr","fpr")
    plot(perf,...)
}

svmfit.opt = svm(y~.,data=dat[train,],kernel="radial",gamma=2,cost=1,decision.values=T)
fitted = attributes(predict(svmfit.opt,dat[train,],decision.values=T))$decision.values
par(mfrow=c(1,2))
rocplot(fitted,dat[train,"y"],main="Training Data")

svmfit.flex = svm(y~.,data=dat[train,],kernel="radial",gamma=50,cost=1,decision.values=T)
fitted = attributes(predict(svmfit.flex,dat[train,],decision.values=T))$decision.values
rocplot(fitted,dat[train,"y"],main="Training Data")

# Interested in ROC curve on the test data
fitted = attributes(predict(svmfit.flex,dat[-train,],decision.values=T))$decision.values
rocplot(fitted,dat[-train,"y"],main="Test Data")

fitted = attributes(predict(svmfit.opt,dat[-train,],decision.values=T))$decision.values
rocplot(fitted,dat[-train,"y"],add=T,col="red")

# SVM with multiple classes
set.seed(1)
x=rbind(x,matrix(rnorm(50*2),ncol=2))
y=c(y,rep(0,50))
x[y==0,2]=x[y==0,2]+2
dat = data.frame(x=x,y=as.factor(y))
par(mfrow=c(1,1))
plot(x,col=(y+1))

# fit an SVM to the data
svmfit = svm(y~.,data=dat,kernel="radial",cost=10,gamma=1)
plot(svmfit,dat)

# Application to Gene Expression Data
library(ISLR)
names(Khan)
dim(Khan$xtrain)
length(Khan$ytrain)
dim(Khan$xtest)
length(Khan$ytest)

table(Khan$ytrain)
table(Khan$ytest)

# use the support vector approach to predict cancer subtype using gene expression measurements
dat = data.frame(x=Khan$xtrain,y=as.factor(Khan$ytrain))
out = svm(y~.,data=dat,kernel="linear",cost=10)
summary(out)
table(out$fitted,dat$y)

# performance on the test data
dat.te = data.frame(x=Khan$xtest,y=as.factor(Khan$ytest))
pred.te=predict(out,newdata=dat.te)
table(pred.te,dat.te$y) # 2 errors when cost equals 10

