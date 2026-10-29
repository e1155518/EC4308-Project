library(dplyr)


runAR <- function(Y, p) {
  
  # Y contains:
  # Column 1: Employ_Rate_diff_h1 (forward difference)
  # Column 2: Employ_Rate_diff_lag1 (backward difference)
  
  # Create AR(p) regression data
  # The forward target at t is explained by
  # historical monthly changes at t, t-1, ..., t-p+1
  
  aux <- embed(Y[, 2], p + 1)
  
  # Historical changes at t, t-1, ..., t-p+1
  X <- aux[, 1:p, drop = FALSE]
  
  # Align forward target with historical predictors
  y <- tail(Y[, 1], nrow(X))
  
  # Last observation is the forecast origin
  # Its forward target is not yet observed
  X.out <- tail(X, 1)
  
  # Exclude the last observation from estimation
  y <- head(y, -1)
  X <- head(X, -1)
  
  # Estimate AR(p) by OLS
  model <- lm(y ~ X)
  
  # Extract coefficients
  coef <- coef(model)
  
  # Generate one-month-ahead forecast
  pred <- c(1, X.out) %*% coef
  
  return(list(
    model = model,
    pred = as.numeric(pred),
    coef = coef
  ))
}
