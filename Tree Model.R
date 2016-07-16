# Fitting a classification tree
library(tree)
library(ISLR)
attach(Carseats)
High = ifelse(Sales<=8,"No","Yes")
Carseats = data.frame(Carseats,High)

# use tree() to fit a classification tree
tree.carseats = tree(High~.-Sales,data=Carseats)
summary(tree.carseats)
# The residual mean deviance reported is simply the deviance divided by n−|T0|
par(mfrow=c(1,1))
plot(tree.carseats)
text(tree.carseats,pretty=0,cex=0.6)
tree.carseats

# estimate the test error rather than simply computing the training error.
set.seed(2)
dim(Carseats)
train = sample(1:nrow(Carseats),200)
Carseats.test = Carseats[-train,]
High.test = High[-train]
tree.carseats = tree(High~.-Sales,data = Carseats, subset = train)
tree.pred = predict(tree.carseats,Carseats.test,type = "class")
table(tree.pred,High.test)

# Pruning the tree
set.seed(3)
cv.carseats = cv.tree(tree.carseats,FUN = prune.misclass)
names(cv.carseats)
cv.carseats
# dev corresponds to the cross-validation error rate
par(mfrow = c(1,2))
plot(cv.carseats$size,cv.carseats$dev,type="b")
plot(cv.carseats$k,cv.carseats$dev,type="b")

# prune.misclass() to prune the tree
prune.carseats = prune.misclass(tree.carseats,best=10)
par(mfrow = c(1,1))
plot(prune.carseats)
text(prune.carseats,pretty=0,cex=0.6)
tree.pred = predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)

# Fitting Regression Tree
library(MASS)
set.seed(1)
train = sample(1:nrow(Boston),nrow(Boston)/2)
tree.boston = tree(medv~.,data=Boston,subset=train)
summary(tree.boston)
# only tree of the variables (lstat,rm,dis) have been used
plot(tree.boston)
text(tree.boston,pretty=0,cex=0.7)
# use cv.tree() to improve the performance
cv.boston = cv.tree(tree.boston)
cv.boston
plot(cv.boston$size,cv.boston$dev,type="b")
prune.boston = prune.tree(tree.boston,best=5)
plot(prune.boston)
text(prune.boston,pretty=0,cex=0.8)

yhat = predict(tree.boston,newdata=Boston[-train,])
boston.test = Boston[-train,"medv"]
plot(yhat,boston.test)
abline(0,1)
mean((yhat-boston.test)^2)












