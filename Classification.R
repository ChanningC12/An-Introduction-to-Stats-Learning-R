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

glm.probs = predict(glm.fit,type="response")
glm.probs[1:10]
glm.pred = rep("Down",1250)
glm.pred[glm.probs>0.5] = "Up"
table(glm.pred,Direction)
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


