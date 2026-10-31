library(readxl)
library(dplyr)

can <- read_excel("data/CAN_data_cleaned.xlsx")

can <- can %>%
  mutate(Date = as.Date(Date)) %>%
  arrange(Date)

runAR <- function(window, max_lag = 6, IC = "bic") {
  
  # Create regression dataset
  df <- data.frame(
    y = window$Employ_Rate_diff_h1
  )
  
  # Add historical employment-rate changes
  for (p in 1:max_lag) {
    df[[paste0("L", p)]] <- dplyr::lag(
      window$Employ_Rate_diff_lag1 , p - 1
    )
  }
  
  # The last row is the forecast origin.
  # Its forward target is not available yet.
  X_out <- df[nrow(df), paste0("L", 1:max_lag)]
  
  # Remove rows with unavailable targets or lags
  train <- df[-nrow(df), ]
  train <- na.omit(train)
  
  # Select optimal AR order
  best_IC <- Inf
  best_model <- NULL
  best_p <- NA_integer_
  
  for (p in 1:max_lag) {
    
    predictors <- paste0("L", 1:p)
    
    formula <- reformulate(
      predictors,
      response = "y"
    )
    
    model <- lm(formula, data = train)
    
    criterion <- if (IC == "bic") {
      BIC(model)
    } else if (IC == "aic") {
      AIC(model)
    } else {
      stop("IC must be 'bic' or 'aic'")
    }
    
    if (criterion < best_IC) {
      best_IC <- criterion
      best_model <- model
      best_p <- p
    }
  }
  
  # Forecast one-month-ahead change
  pred <- predict(
    best_model,
    newdata = X_out
  )
  
  return(list(
    model = best_model,
    pred = as.numeric(pred),
    best_p = best_p,
    IC = best_IC,
    coef = coef(best_model)
  ))
}

ar.rolling.window <- function(
    data,
    test_start = as.Date("2011-09-01"),
    test_end = as.Date("2019-12-01"),
    window_size = 367,
    max_lag = 6,
    IC = "bic"
) {
  
  data <- data[order(data$Date), ]
  
  test_dates <- data$Date[
    data$Date >= test_start &
      data$Date <= test_end
  ]
  
  results <- data.frame(
    Date = test_dates,
    Actual = NA_real_,
    Predicted = NA_real_,
    Best_AR = NA_integer_,
    BIC = NA_real_
  )
  
  for (i in seq_along(test_dates)) {
    
    # Forecast origin: one month before test date
    test_idx <- which(data$Date == test_dates[i])
    origin_idx <- test_idx - 1
    
    # Fixed-length rolling window
    start_idx <- origin_idx - window_size + 1
    
    if (start_idx < 1) {
      stop("Not enough observations for rolling window")
    }
    
    window <- data[start_idx:origin_idx, ]
    
    # Fit AR model and forecast
    fit <- runAR(
      window,
      max_lag = max_lag,
      IC = IC
    )
    
    results$Actual[i] <-
      data$Employ_Rate[test_idx] -
      data$Employ_Rate[origin_idx]
    
    results$Predicted[i] <- fit$pred
    
    results$Best_AR[i] <- fit$best_p
    results$BIC[i] <- fit$IC
  }
  
  # Forecast accuracy
  errors <- results$Actual - results$Predicted
  
  mse <- mean(errors^2)
  rmse <- sqrt(mse)
  mae <- mean(abs(errors))
  
  return(list(
    forecasts = results,
    RMSE = rmse
  ))
}

ar1_results <- ar.rolling.window(
  data = can,
  test_start = as.Date("2011-09-01"),
  test_end = as.Date("2019-12-01"),
  window_size = 367,
  max_lag = 6,
  IC = "bic"
)

ar1_results$RMSE

head(ar1_results$forecasts)

library(ggplot2)

ggplot(ar1_results$forecasts, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual")) +  
  geom_line(aes(y = Predicted, color = "Predicted")) +
  labs(
    title = "AR Forecasts: 1-Month Employment Rate Change",
    subtitle = "Fixed Rolling Window, AR(1)-AR(6) Selected by BIC",
    x = "Date",
    y = "Change in employment rate (percentage points)",
    color = ""
  ) +
  theme_minimal()

table(ar1_results$forecasts$Best_AR)

ggplot(ar1_results$forecasts,
       aes(x = Date, y = Best_AR)) +
  
  geom_step(color = "steelblue", linewidth = 0.8) +
  geom_point(color = "steelblue", size = 1) +
  
  scale_y_continuous(
    breaks = 1:6,
    labels = paste0("AR(", 1:6, ")"),
    limits = c(1, 6)
  ) +
  
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  
  labs(
    title = "Optimal AR Order Over Time",
    subtitle = "1-Month-Ahead Forecast | Fixed Rolling Window | BIC Selection",
    x = "Forecast Date",
    y = "BIC-Selected AR Order"
  ) +
  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

