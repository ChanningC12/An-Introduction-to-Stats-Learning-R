# Fitting a classification tree
library(tree)
library(ISLR)
attach(Carseats)
High = ifelse(Sales<=8,"No","Yes")
Carseats = data.frame(Carseats,High)

# use tree() to fit a classification tree
tree.carseats = tree(High~.-Sales,data=Carseats)
summary(tree.carseats)
