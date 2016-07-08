# some ways in which the simple linear model can be improved, by replacing plain least squares fitting with some alternative fitting procedures
# better prediction accuracy and model interpretability.

# Accuracy

# if n is not much larger than p
## there can be a lot of variability in the least squares fit,
### resulting in overfitting and consequently poor predictions on future
#### observations not used in model training.

# if p > n, then there
## is no longer a unique least squares coefficient estimate: the variance
### is infinite so the method cannot be used at all.

# Interpretability
# automatically performing feature selection or variable selection
## excluding irrelevant variables from a multiple regression model




# 6.5.1 Best Subset Selection
# Hitter data: predict a baseball player’s Salary on the basis of various statistics associated with performance in the previous year
    
