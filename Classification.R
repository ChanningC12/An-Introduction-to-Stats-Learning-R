library(ISLR)
View(Smarket)
names(Smarket)
dim(Smarket)
head(Smarket)
summary(Smarket)
str(Smarket)

# See correlation except for the Direction
round(cor(Smarket[,-9]),2)
# Year vs Volume
attach(Smarket)
plot(Volume)

# Logistic Regression
# Predict Direction using Lag1 through Lag5 and Volume
glm.fit = glm(Direction~Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, family = binomial, data = Smarket)
summary(glm.fit)
# p-value is still relatively large, and so there is no clear evidence of a real association
coef(glm.fit)
summary(glm.fit$coef)

# output probabilities
glm.probs = predict(glm.fit,type="response")
# predict the first ten obs
glm.probs[1:10]
# assign predicted variable
glm.pred = rep("Down",1250)
glm.pred[glm.probs>0.5] = "Up"
# cross tab to see predictions against actual
table(glm.pred,Direction)
# accuracy
mean(glm.pred == Direction)

# Train
train = (Year<2005)
Smarket.2005 = Smarket[!train,]
dim(Smarket.2005)
Direction.2005 = Direction[!train]
glm.fit = glm(Direction~Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, family = binomial, data = Smarket, subset = train)
glm.probs = predict(glm.fit,Smarket.2005,type="response")

glm.pred = rep("Down",252)
glm.pred[glm.probs<0.5] = "Up"
table(glm.pred,Direction.2005)
mean(glm.pred == Direction.2005)
mean(glm.pred!=Direction.2005)

# Use Lag1 and Lag2
glm.fit = glm(Direction~Lag1 + Lag2 , family = binomial, data = Smarket, subset = train)
glm.probs = predict(glm.fit, Smarket, type="response")
glm.pred = rep("Down",252)
glm.pred[glm.probs>0.5] = "Up"
table(glm.pred,Direction.2005)

predict(glm.fit,newdata=data.frame(Lag1=c(1.2,1.5),Lag2=c(1.1 , -0.8) ),type =" response ")


# Linear Discriminant Analysis
library(MASS)
lda.fit = lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
lda.fit
plot(lda.fit)
# The coefficients of linear discriminants output provides the linear combination of Lag1 and Lag2 that are used to form the LDA decision rule.
# If −0.642×Lag1−0.514×Lag2 is large, then the LDA classifier will predict a market increase, and if it is small, then the LDA classifier will predict a market decline.

lda.pred = predict(lda.fit, Smarket.2005)
names(lda.pred)
lda.class = lda.pred$class
table(lda.class,Direction.2005)
mean(lda.class == Direction.2005)

# LDA example in iris
data(iris)
head(iris,3)
dim(iris)
table(iris$Species)
tapply(iris$Sepal.Length,iris$Species,mean)
r = lda(Species~.,data=iris,prior=c(1,1,1)/3) #The prior argument sets the prior probabilities of class membership
r$prior
r$counts
r$means
r$scaling # linear combination coefficients (scaling)
r$svd # singular values (svd) that gives the ratio of the between- and within-group standard deviations on the linear discriminant variables

# use the singular values to compute the amount of the between-group variance that is explained by each linear discriminant
# we see that the first linear discriminant explains more than {99\%} of the between-group variance in the iris dataset
prop = r$svd^2/sum(r$svd^2)
prop

# predict
train = sample(1:150,75)
r3 = lda(Species~.,iris,prior=c(1,1,1)/3,subset=train)
plda = predict(object=r,newdata=iris[-train,])
head(plda$class)

# http://www.r-bloggers.com/computing-and-visualizing-lda-in-r/

# QDA
train = (Year<2005)
qda.fit = qda(Direction~Lag1+Lag2,data=Smarket,subset=train)
qda.fit
qda.class = predict(qda.fit,Smarket.2005)$class
table(qda.class,Direction.2005)
mean(qda.class == Direction.2005)


# K-Nearest Neighbors
library(class)
train.X = cbind(Lag1,Lag2)[train,]
test.X = cbind(Lag1,Lag2)[!train,]
train.Direction=Direction[train]
set.seed(1)
knn.pred = knn(train.X,test.X,train.Direction,k=1)
table(knn.pred,Direction.2005)
mean(knn.pred==Direction.2005)

# repeat with k=3
knn.pred = knn(train.X,test.X,train.Direction,k=3)
table(knn.pred,Direction.2005)
mean(knn.pred==Direction.2005)

# An Application to Caravan Insurance Data
dim(Caravan)
attach(Caravan)
summary(Purchase)
# need to scale the KNN predictors, $1,000 and 50 year old
standardize.X = scale(Caravan[,-86])
var(Caravan[,1])
var(Caravan[,2])
var(standardize.X[,1])
var(standardize.X[,2])

test = 1:1000
train.X = standardize.X[-test,]
test.X = standardize.X[test,]
train.Y = Purchase[-test]
test.Y = Purchase[test]
set.seed(1)
knn.pred = knn(train.X,test.X,train.Y,k=1)
mean(test.Y!=knn.pred)
mean(test.Y!="No")
table(knn.pred,test.Y)

# k = 3
knn.pred = knn(train.X,test.X,train.Y,k=3)
table(knn.pred,test.Y)
mean(knn.pred == test.Y)

knn.pred = knn(train.X,test.X,train.Y,k=5)
table(knn.pred,test.Y)
mean(knn.pred == test.Y)

# Compare to Logistic Regression
glm.fit = glm(Purchase~.,data=Caravan,family=binomial,subset=-test)
glm.probs = predict(glm.fit,Caravan[test,],type="response")
glm.pred = rep("No",1000)
glm.pred[glm.probs>0.5]="Yes"
table(glm.pred,test.Y)
mean(glm.pred == test.Y)

glm.pred = rep("No",1000)
glm.pred[glm.probs>0.25]="Yes"
table(glm.pred,test.Y)
mean(glm.pred == test.Y)


