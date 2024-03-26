library(xgboost)
library(shapr)

# x train 


# Fitting a basic xgboost model to the training data
model <- xgboost(
  data = x,
  label = y,
  nround = 20,
  verbose = FALSE
)

# Prepare the data for explanation
explainer <- shapr(x, model)
#> The specified model provides feature classes that are NA. The classes of data are taken as the truth.

# Specifying the phi_0, i.e. the expected prediction without any features
p <- mean(y)

# Computing the actual Shapley values with kernelSHAP accounting for feature dependence using
# the empirical (conditional) distribution approach with bandwidth parameter sigma = 0.1 (default)
explanation <- explain(
  x_test,
  approach = "empirical",
  explainer = explainer,
  prediction_zero = p
)