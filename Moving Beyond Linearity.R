# Chapter 7 - Moving beyond linearity
# examining very simple extensions of linear models like polynomial regression and step functions, splines, local regression, and generalized additive models

# Polynomial regression
# Step function: cut the range of a variable into K distinct regions in order to produce a qualitative variable
# Regression splines: involve dividing the range of X into K distinct regions. Within each region, a polynomial function is fit to the data. They join smoothly at the region boundaries or knots
# Smoothing splines: Smoothing splines result from minimizing a residual sum of squares criterion subject to a smoothness penalty
# Local regression: The regions are allowed to overlap
# GAM: allow us to extend the methods above to deal with multiple predictors

