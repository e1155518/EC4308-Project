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

############ runAR and ar.rolling.window functions ##########

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
    test_end = as.Date("2019-12-01"),
    window_size = NULL
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
  
  if (is.null(window_size)) {
    window_size <- min(origin_idx)
  }
  
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


##################  Running AR(1)- AR(12) for all 4 horizons #######################

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

############ presenting the results #################

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


############# plotting actual vs predicted ###########

library(ggplot2)
library(dplyr)

plot_AR <- function(h, period = "all") {
  
  # Validate inputs
  if (!h %in% c(1, 3, 6, 12)) {
    stop("h must be 1, 3, 6, or 12")
  }
  
  period <- match.arg(period, c("all", "pre", "post"))
  
  # Retrieve best AR model for this horizon
  selected <- best_ar_table[
    best_ar_table$Horizon == h,
  ]
  
  p <- selected$AR_Order[1]
  
  # Retrieve pre-COVID forecasts
  pre <- ar_results[[
    paste0("h", h, "_AR", p)
  ]]$forecasts
  
  # Retrieve post-COVID forecasts
  post <- ar_postcovid_results[[
    paste0("h", h)
  ]]$forecasts
  
  # Select period
  if (period == "pre") {
    forecast_df <- pre
    period_label <- "Pre-COVID (2011–2019)"
  } else if (period == "post") {
    forecast_df <- post
    period_label <- "COVID/Post-COVID (2020–2025)"
  } else {
    forecast_df <- bind_rows(pre, post)
    period_label <- "Full OOS Period (2011–2025)"
  }
  
  # Calculate RMSE for selected period
  rmse <- sqrt(mean(
    (forecast_df$Actual - forecast_df$Predicted)^2
  ))
  
  # Plot actual vs predicted
  plot <- ggplot(forecast_df, aes(x = Outcome_Date)) +
    
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
      title = paste0(
        h, "-Month Forecast: AR(", p, ")"
      ),
      subtitle = paste0(
        period_label,
        " | RMSE = ", round(rmse, 4)
      ),
      x = "Outcome Date",
      y = "Employment Rate Change (percentage points)",
      color = NULL
    ) +
    
    theme_minimal()
  
  print(plot)
}

plot_AR(1, "pre")
plot_AR(3, "pre")
plot_AR(6, "pre")
plot_AR(12, "pre")

# ============================================================
# Post-COVID AR Forecasting (2020 - September 2025)
# ============================================================

# Store post-COVID results
ar_postcovid_results <- list()

# Comparison table
ar_period_comparison <- data.frame()

for (i in 1:nrow(best_ar_table)) {
  
  h <- best_ar_table$Horizon[i]
  p <- best_ar_table$AR_Order[i]
  
  # Recover the window length used in the pre-COVID analysis
  # (based on the first forecast origin)
  window_size <- which(
    can$Date ==
      as.Date(ar_results[[paste0("h", h, "_AR", p)]]$
                forecasts$Forecast_Origin[1])
  )
  
  # Run post-COVID forecasts
  result <- ar.rolling.window(
    data = can,
    nprev = 69,
    p = p,
    horizon = h,
    test_end = as.Date("2025-09-01"),
    window_size = window_size
  )
  
  # Save full predictions
  ar_postcovid_results[[paste0("h", h)]] <- result
  
  # Retrieve pre-COVID RMSE
  pre_rmse <- best_ar_table$RMSE[i]
  
  # Retrieve post-COVID RMSE
  post_rmse <- unname(result$errors["rmse"])
  
  # Store comparison
  ar_period_comparison <- rbind(
    ar_period_comparison,
    data.frame(
      Horizon = h,
      Model = paste0("AR(", p, ")"),
      Pre_COVID_RMSE = pre_rmse,
      Post_COVID_RMSE = post_rmse
    )
  )
}

print(ar_period_comparison)


# ============================================================
# Export complete AR predictions (to calculate loss differential later on)
# ============================================================

library(dplyr)

ar_all_predictions <- bind_rows(
  lapply(seq_len(nrow(best_ar_table)), function(i) {
    
    h <- best_ar_table$Horizon[i]
    p <- best_ar_table$AR_Order[i]
    
    # Pre-COVID predictions
    pre <- ar_results[[
      paste0("h", h, "_AR", p)
    ]]$forecasts
    
    pre$Period <- "Pre-COVID"
    
    # Post-COVID predictions
    post <- ar_postcovid_results[[
      paste0("h", h)
    ]]$forecasts
    
    post$Period <- "Post-COVID"
    
    # Combine
    df <- bind_rows(pre, post)
    
    df$Model <- paste0("AR(", p, ")")
    df$Horizon <- h
    df$Predictor_Set <- "CAN"
    
    df
  })
)

dir.create("results", showWarnings = FALSE)

write.csv(
  ar_all_predictions,
  "results/AR_predictions.csv",
  row.names = FALSE
)

plot_AR(1, "post")
plot_AR(3, "post")
plot_AR(6, "post")
plot_AR(12, "post")


plot_AR(1, "all")
plot_AR(3, "all")
plot_AR(6, "all")
plot_AR(12, "all")
