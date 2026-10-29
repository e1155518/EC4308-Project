library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)

can <- read_excel("data/CAN_data_cleaned.xlsx")

can$Date <- as.Date(can$Date)
can <- can[order(can$Date), ]

# Check required variables
required_cols <- c(
  "Date",
  "Employ_Rate",
  "Employ_Rate_diff_lag1",
  paste0("Employ_Rate_diff_h", c(1, 3, 6, 12))
)

stopifnot(all(required_cols %in% names(can)))

# Confirm monthly dates are consecutive
expected_dates <- seq(
  min(can$Date),
  max(can$Date),
  by = "month"
)

stopifnot(identical(can$Date, expected_dates))

runAR <- function(Y, p, horizon = 1) {
  
  # Y columns:
  # 1 = forward difference target for selected horizon
  # 2 = backward one-month employment-rate difference
  
  n <- nrow(Y)
  
  # Construct lagged historical changes using embed()
  # Rows correspond to forecast origins p, ..., n
  X.all <- embed(Y[, 2], p)
  
  # Align forward target with the predictor rows
  y.all <- Y[p:n, 1]
  
  # Last row is the current forecast origin
  X.out <- matrix(
    tail(X.all, 1),
    nrow = 1
  )
  
  # Only training targets observable by the forecast origin
  # are permitted.
  #
  # Target at s uses R_(s+h), so require s+h <= n.
  
  n.train <- nrow(X.all) - horizon
  
  if (n.train <= p + 1) {
    stop("Insufficient observations for estimation.")
  }
  
  X <- X.all[1:n.train, , drop = FALSE]
  y <- y.all[1:n.train]
  
  # Check for missing values in estimation sample
  if (anyNA(X) || anyNA(y) || anyNA(X.out)) {
    stop("Missing values found in AR estimation or forecast predictors.")
  }
  
  # Estimate AR(p) using OLS
  model <- lm(y ~ X)
  
  # Extract coefficients
  coef <- coef(model)
  
  # Generate direct h-month-ahead forecast
  pred <- c(1, X.out) %*% coef
  
  return(list(
    model = model,
    pred = as.numeric(pred),
    coef = coef
  ))
}

ar.rolling.window <- function(
    data,
    nprev = 100,
    p = 1,
    horizon = 1,
    test_end = as.Date("2019-12-01")
) {
  
  target_col <- paste0("Employ_Rate_diff_h", horizon)
  
  # Identify the final outcome date
  test_end_idx <- which(data$Date == test_end)
  
  if (length(test_end_idx) != 1) {
    stop("test_end must match exactly one date.")
  }
  
  # Identify the 100 OOS outcome dates
  outcome_idx <- seq(
    from = test_end_idx - nprev + 1,
    to = test_end_idx
  )
  
  # Forecast origins are h months before outcome dates
  origin_idx <- outcome_idx - horizon
  
  # Determine fixed window length automatically
  # using the first forecast origin
  window_size <- min(origin_idx)
  
  # Blank matrices for forecasts and coefficients
  save.coef <- matrix(NA_real_, nprev, p + 1)
  save.pred <- matrix(NA_real_, nprev, 1)
  
  # Professor's backwards rolling loop
  for (i in nprev:1) {
    
    j <- 1 + nprev - i
    
    origin <- origin_idx[j]
    
    # Fixed rolling window:
    # drop oldest observation and add newest
    Y.window <- as.matrix(
      data[
        (origin - window_size + 1):origin,
        c(target_col, "Employ_Rate_diff_lag1")
      ]
    )
    
    # Fit AR(p) and generate forecast
    fact <- runAR(
      Y = Y.window,
      p = p,
      horizon = horizon
    )
    
    save.coef[j, ] <- fact$coef
    save.pred[j, ] <- fact$pred
  }
  
  # Actual cumulative employment-rate changes
  actual <- data[[target_col]][origin_idx]
  
  # Calculate RMSE only
  forecast_errors <- actual - as.numeric(save.pred)
  
  rmse <- sqrt(mean(forecast_errors^2))
  
  return(list(
    pred = save.pred,
    coef = save.coef,
    errors = c(rmse = rmse),
    forecasts = data.frame(
      Forecast_Origin = data$Date[origin_idx],
      Outcome_Date = data$Date[outcome_idx],
      Actual = actual,
      Predicted = as.numeric(save.pred),
      Forecast_Error = forecast_errors
    )
  ))
}


#########################################

horizons <- c(1, 3, 6, 12)
ar_orders <- 1:12

# Store all model results
ar_results <- list()

# Store RMSE for every model-horizon combination
rmse_results <- data.frame()

for (h in horizons) {
  
  for (p in ar_orders) {
    
    cat("Running AR(", p, "), horizon =", h, "\n")
    
    result <- ar.rolling.window(
      data = can,
      nprev = 100,
      p = p,
      horizon = h,
      test_end = as.Date("2019-12-01")
    )
    
    # Save full results for plotting later
    ar_results[[paste0("h", h, "_AR", p)]] <- result
    
    # Save RMSE
    rmse_results <- rbind(
      rmse_results,
      data.frame(
        Horizon = h,
        AR_Order = p,
        RMSE = unname(result$errors["rmse"])
      )
    )
  }
}

#####3333333333333333333333333

rmse_table <- rmse_results %>%
  mutate(
    Model = paste0("AR(", AR_Order, ")"),
    Horizon = paste0("h=", Horizon)
  ) %>%
  select(Model, Horizon, RMSE) %>%
  pivot_wider(
    names_from = Horizon,
    values_from = RMSE
  ) %>%
  arrange(as.integer(gsub("[^0-9]", "", Model)))

print(rmse_table, n = 12)

best_ar_table <- rmse_results %>%
  group_by(Horizon) %>%
  slice_min(RMSE, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    Best_Model = paste0("AR(", AR_Order, ")")
  ) %>%
  select(Horizon, AR_Order, Best_Model, RMSE)

print(best_ar_table)

library(ggplot2)

plot_AR <- function(h) {
  
  # Find the best AR model for the selected horizon
  selected <- best_ar_table[
    best_ar_table$Horizon == h,
  ]
  
  p <- selected$AR_Order[1]
  rmse <- selected$RMSE[1]
  
  # Retrieve the saved forecasting results
  result <- ar_results[[paste0("h", h, "_AR", p)]]
  
  forecast_df <- result$forecasts
  
  # Plot actual vs predicted
  ggplot(forecast_df, aes(x = Outcome_Date)) +
    geom_line(
      aes(y = Actual, color = "Actual"),
      linewidth = 0.8
    ) +
    geom_line(
      aes(y = Predicted, color = "Predicted"),
      linewidth = 0.8
    ) +
    scale_color_manual(
      values = c(
        "Actual" = "black",
        "Predicted" = "blue"
      )
    ) +
    labs(
      title = paste0("Horizon ", h, ": AR(", p, ")"),
      subtitle = paste0("RMSE = ", round(rmse, 4)),
      x = "Outcome Date",
      y = "Employment Rate Change (percentage points)",
      color = NULL
    ) +
    theme_minimal()
}

plot_AR(1)   # 1-month forecast
plot_AR(3)   # 3-month forecast
plot_AR(6)   # 6-month forecast
plot_AR(12)  # 12-month forecast
