library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

can <- read_excel('data/CAN_data_cleaned.xlsx')
can$Date <- as.Date(can$Date)
can <- can[order(can$Date), ]

horizons <- c(1, 3, 6, 12)
ar_orders <- 1:12
required_cols <- c('Date', 'Employ_Rate', 'Employ_Rate_diff_lag1',
                   paste0('Employ_Rate_diff_h', horizons))
stopifnot(all(required_cols %in% names(can)))
stopifnot(!anyNA(can$Date), !anyDuplicated(can$Date))
stopifnot(identical(can$Date, seq(min(can$Date), max(can$Date), by = 'month')))

# Historical outcome-month indicators (not future forecast information).
can$COVID_Shock <- as.integer(can$Date >= as.Date('2020-03-01') &
                              can$Date <= as.Date('2020-04-01'))
can$COVID_Recovery <- as.integer(can$Date >= as.Date('2020-05-01') &
                                 can$Date <= as.Date('2022-02-01'))

# Each target is Employ_Rate[t+h] - Employ_Rate[t]. For h=1, the
# historical dummy is the dummy for the outcome month t+1.
# For h>1, the provisional control is the fraction of months t+1,...,t+h
# falling in each phase. Revisit this choice before interpreting h>1.
for (h in horizons) {
  for (phase in c('COVID_Shock', 'COVID_Recovery')) {
    z <- rep(NA_real_, nrow(can))
    valid <- seq_len(nrow(can) - h)
    z[valid] <- vapply(valid, function(t) {
      mean(can[[phase]][(t + 1):(t + h)])
    }, numeric(1))
    can[[paste0(phase, '_h', h)]] <- z
  }
}

############ runAR and ar.rolling.window functions ##########
runAR <- function(Y, p, horizon = 1, use_dummy = FALSE) {
  # Y columns: forward target, backward one-month change,
  # and optionally two historical target-aligned COVID controls.
  n <- nrow(Y)
  if (p < 1 || horizon < 1 || p != as.integer(p) ||
      horizon != as.integer(horizon) || n < p + horizon + 3) {
    stop('Invalid AR order/horizon or insufficient estimation data.')
  }

  X.all <- embed(Y[, 2], p)
  y.all <- Y[p:n, 1]
  X.out <- matrix(tail(X.all, 1), nrow = 1)
  n.train <- nrow(X.all) - horizon
  if (n.train <= p + 1) stop('Insufficient observations for estimation.')
  X <- X.all[seq_len(n.train), , drop = FALSE]
  y <- y.all[seq_len(n.train)]
  if (anyNA(X) || anyNA(y) || anyNA(X.out)) {
    stop('Missing values in AR estimation sample or forecast predictors.')
  }

  if (use_dummy) {
    if (ncol(Y) != 4) stop('Dummy specification requires four Y columns.')
    d <- Y[p:(n - horizon), 3:4, drop = FALSE]
    if (anyNA(d)) stop('Missing historical COVID dummy values.')
    # Do not estimate dummy coefficients before any corresponding
    # historical observations exist (all-zero columns are unidentified).
    active <- which(colSums(abs(d)) > 0)
    design <- data.frame(y = y, X)
    names(design) <- c('y', paste0('lag', seq_len(p)))
    if (length(active)) {
      for (k in active) design[[c('COVID_Shock', 'COVID_Recovery')[k]]] <- d[, k]
    }
    model <- lm(y ~ ., data = design)
    if (anyNA(coef(model))) stop('Rank-deficient AR/dummy regression.')
    # Future dummy values are ZERO: historical outlier adjustment only.
    newdata <- as.data.frame(X.out)
    names(newdata) <- paste0('lag', seq_len(p))
    if (length(active)) {
      for (k in active) newdata[[c('COVID_Shock', 'COVID_Recovery')[k]]] <- 0
    }
    pred <- as.numeric(predict(model, newdata = newdata))
    ar_coef <- unname(coef(model)[c('(Intercept)', paste0('lag', seq_len(p)))])
    dummy_coef <- setNames(c(0, 0), c('COVID_Shock', 'COVID_Recovery'))
    if (length(active)) {
      nm <- c('COVID_Shock', 'COVID_Recovery')[active]
      dummy_coef[nm] <- coef(model)[nm]
    }
  } else {
    model <- lm(y ~ X)
    ar_coef <- unname(coef(model))
    if (anyNA(ar_coef)) stop('Rank-deficient AR regression.')
    pred <- as.numeric(c(1, X.out) %*% ar_coef)
    dummy_coef <- setNames(c(0, 0), c('COVID_Shock', 'COVID_Recovery'))
  }

  return(list(model = model, pred = pred, coef = ar_coef,
              dummy_coef = dummy_coef))
}

ar.rolling.window <- function(data, nprev = 100, p = 1, horizon = 1,
                              test_end = as.Date('2019-12-01'),
                              window_size = NULL, use_dummy = FALSE) {
  target_col <- paste0('Employ_Rate_diff_h', horizon)
  test_end_idx <- which(data$Date == as.Date(test_end))
  if (length(test_end_idx) != 1) stop('test_end must match exactly one date.')
  if (nprev < 1 || nprev != as.integer(nprev)) stop('Invalid nprev.')
  outcome_idx <- seq.int(test_end_idx - nprev + 1L, test_end_idx)
  origin_idx <- outcome_idx - horizon
  if (min(origin_idx) < 1) stop('Forecast origins precede available data.')
  if (is.null(window_size)) window_size <- min(origin_idx)
  if (window_size < p + horizon + 3 || window_size != as.integer(window_size) ||
      min(origin_idx) - window_size + 1 < 1) {
    stop('Invalid window size or insufficient historical observations.')
  }

  save.coef <- matrix(NA_real_, nprev, p + 1)
  save.dummy.coef <- matrix(NA_real_, nprev, 2,
                            dimnames = list(NULL, c('COVID_Shock', 'COVID_Recovery')))
  save.pred <- matrix(NA_real_, nprev, 1)
  cols <- c(target_col, 'Employ_Rate_diff_lag1')
  if (use_dummy) cols <- c(cols, paste0(c('COVID_Shock', 'COVID_Recovery'), '_h', horizon))

  # Backwards rolling loop, following the professor's structure.
  for (i in nprev:1) {
    j <- 1 + nprev - i
    origin <- origin_idx[j]
    Y.window <- as.matrix(data[(origin - window_size + 1):origin, cols])
    fact <- runAR(Y.window, p = p, horizon = horizon, use_dummy = use_dummy)
    save.coef[j, ] <- fact$coef
    save.dummy.coef[j, ] <- fact$dummy_coef
    save.pred[j, ] <- fact$pred
  }

  actual <- data[[target_col]][origin_idx]
  if (anyNA(actual)) stop('Missing realized targets in evaluation period.')
  forecast_errors <- actual - as.numeric(save.pred)
  rmse <- sqrt(mean(forecast_errors^2))
  return(list(pred = save.pred, coef = save.coef,
              dummy_coef = save.dummy.coef,
              errors = c(rmse = rmse),
              forecasts = data.frame(
                Forecast_Origin = data$Date[origin_idx],
                Outcome_Date = data$Date[outcome_idx],
                Actual = actual, Predicted = as.numeric(save.pred),
                Forecast_Error = forecast_errors)))
}

################## Run AR(1)-AR(12), all horizons ####################
# Keep the SAME raw fixed window length for every period and specification.
pre_end <- as.Date('2019-12-01')
post_end <- as.Date('2025-09-01')
n_pre <- 100
n_post <- 69
pre_end_idx <- match(pre_end, can$Date)
if (is.na(pre_end_idx) || is.na(match(post_end, can$Date))) stop('Evaluation end date missing.')
window_by_h <- setNames(vapply(horizons, function(h) {
  pre_end_idx - n_pre + 1 - h
}, numeric(1)), as.character(horizons))

ar_results <- list()
rmse_results <- data.frame()
all_forecasts <- list()

for (h in horizons) {
  for (p in ar_orders) {
    for (spec in c('No_Dummy', 'With_Dummy')) {
      for (period in c('Pre_COVID', 'Post_COVID')) {
        # Pre-COVID dummy and no-dummy fits are identical; avoid duplication.
        if (period == 'Pre_COVID' && spec == 'With_Dummy') next
        use_dummy <- spec == 'With_Dummy'
        nprev <- if (period == 'Pre_COVID') n_pre else n_post
        test_end <- if (period == 'Pre_COVID') pre_end else post_end
        cat('Running h=', h, ' AR(', p, ') ', spec, ' ', period, '\n', sep = '')
        result <- ar.rolling.window(
          data = can, nprev = nprev, p = p, horizon = h,
          test_end = test_end, window_size = window_by_h[as.character(h)],
          use_dummy = use_dummy)
        key <- paste(h, p, spec, period, sep = '_')
        ar_results[[key]] <- result
        df <- result$forecasts
        df$Horizon <- h
        df$AR_Order <- p
        df$Model <- paste0('AR(', p, ')')
        df$Specification <- spec
        df$Period <- period
        df$Predictor_Set <- 'CAN'
        all_forecasts[[key]] <- df
      }
    }
  }
}

ar_all_predictions <- bind_rows(all_forecasts)
# Pre-COVID predictions are identical in both specifications. Duplicate them
# in the reporting dataset only, not in estimation.
pre_dummy <- ar_all_predictions %>%
  filter(Period == 'Pre_COVID', Specification == 'No_Dummy') %>%
  mutate(Specification = 'With_Dummy')
ar_all_predictions <- bind_rows(ar_all_predictions, pre_dummy)

rmse_results <- ar_all_predictions %>%
  group_by(Horizon, AR_Order, Model, Specification, Period) %>%
  summarise(RMSE = sqrt(mean(Forecast_Error^2)), .groups = 'drop')

all_rmse <- ar_all_predictions %>%
  group_by(Horizon, AR_Order, Model, Specification) %>%
  summarise(Period = 'All', RMSE = sqrt(mean(Forecast_Error^2)), .groups = 'drop')
rmse_results <- bind_rows(rmse_results, all_rmse)

# Four tables: one per horizon, five RMSE columns.
rmse_tables <- lapply(horizons, function(h) {
  rmse_results %>%
    filter(Horizon == h) %>%
    mutate(Column = case_when(
      Period == 'Pre_COVID' & Specification == 'No_Dummy' ~ 'Pre_COVID',
      Period == 'Post_COVID' & Specification == 'No_Dummy' ~ 'Post_No_Dummy',
      Period == 'Post_COVID' & Specification == 'With_Dummy' ~ 'Post_With_Dummy',
      Period == 'All' & Specification == 'No_Dummy' ~ 'All_No_Dummy',
      Period == 'All' & Specification == 'With_Dummy' ~ 'All_With_Dummy',
      TRUE ~ NA_character_)) %>%
    filter(!is.na(Column)) %>%
    select(AR_Order, Column, RMSE) %>%
    pivot_wider(names_from = Column, values_from = RMSE) %>%
    arrange(AR_Order) %>%
    select(AR_Order, Pre_COVID, Post_No_Dummy, Post_With_Dummy,
           All_No_Dummy, All_With_Dummy)
})
names(rmse_tables) <- paste0('h', horizons)
for (h in horizons) {
  cat('\nRMSE table: horizon ', h, '\n', sep = '')
  print(rmse_tables[[paste0('h', h)]], n = 12)
}

############ Plot selected AR order (defaults to pre-COVID RMSE winner) ##########
best_ar_table <- rmse_results %>%
  filter(Period == 'Pre_COVID', Specification == 'No_Dummy') %>%
  group_by(Horizon) %>%
  slice_min(RMSE, n = 1, with_ties = FALSE) %>%
  ungroup()

plot_AR <- function(h, period = "all", p = NULL) {
  
  stopifnot(h %in% horizons)
  period <- match.arg(period, c("all", "pre", "post"))
  
  # Default: use the best pre-COVID AR order
  if (is.null(p)) {
    p <- best_ar_table$AR_Order[
      best_ar_table$Horizon == h
    ][1]
  }
  
  # Retrieve both specifications
  df <- ar_all_predictions %>%
    filter(Horizon == h, AR_Order == p)
  
  # Filter evaluation period
  if (period == "pre") {
    df <- filter(df, Period == "Pre_COVID")
  } else if (period == "post") {
    df <- filter(df, Period == "Post_COVID")
  }
  
  # Reshape predictions for comparison
  plot_df <- df %>%
    select(Outcome_Date, Actual, Predicted, Specification) %>%
    pivot_wider(
      names_from = Specification,
      values_from = Predicted
    ) %>%
    arrange(Outcome_Date)
  
  # Calculate RMSE for both specifications
  rmse_no <- sqrt(mean(
    (plot_df$Actual - plot_df$No_Dummy)^2
  ))
  
  rmse_dummy <- sqrt(mean(
    (plot_df$Actual - plot_df$With_Dummy)^2
  ))
  
  # Plot actual vs both predictions
  plot <- ggplot(plot_df, aes(x = Outcome_Date)) +
    
    # With Dummy: plotted first (underneath)
    geom_line(
      aes(y = With_Dummy, color = "With Dummy"),
      linewidth = 0.8
    ) +
    
    # Without Dummy: plotted second (on top)
    geom_line(
      aes(y = No_Dummy, color = "Without Dummy"),
      linewidth = 0.8
    ) +
    
    # Actual: plotted last (topmost)
    geom_line(
      aes(y = Actual, color = "Actual"),
      linewidth = 0.8
    ) +
    
    scale_color_manual(
      values = c(
        "Actual" = "black",
        "Without Dummy" = "#2563EB",
        "With Dummy" = "#E69F00"
      )
    ) +
    
    labs(
      title = paste0(
        h, "-Month Forecast: AR(", p, ")"
      ),
      subtitle = paste0(
        period,
        " | RMSE (No Dummy) = ", round(rmse_no, 4),
        " | RMSE (With Dummy) = ", round(rmse_dummy, 4)
      ),
      x = "Outcome Date",
      y = "Employment Rate Change (percentage points)",
      color = NULL
    ) +
    
    theme_minimal()
  
  print(plot)
}

############ Export results ##########
dir.create('results', showWarnings = FALSE)
write.csv(ar_all_predictions, 'results/AR_predictions.csv', row.names = FALSE)
write.csv(rmse_results, 'results/AR_RMSE_long.csv', row.names = FALSE)
for (h in horizons) {
  write.csv(rmse_tables[[paste0('h', h)]],
            paste0('results/AR_RMSE_h', h, '.csv'), row.names = FALSE)
}

# Example plots, after all results exist:
# Full test period
plot_AR(1, "all")
plot_AR(3, "all")
plot_AR(6, "all")
plot_AR(12, "all")

# Post-COVID only
plot_AR(1, "post")

# Pre-COVID only
plot_AR(1, "pre")

# Compare a specific AR order
plot_AR(1, "post", p = 4)

# Select the AR(p) with the lowest RMSE
# for each horizon and evaluation specification
library(dplyr)
library(tidyr)

best_ar_summary <- rmse_results %>%
  
  # Remove redundant pre-COVID dummy results
  filter(
    !(Period == "Pre_COVID" & Specification == "With_Dummy")
  ) %>%
  
  # Select lowest RMSE for each horizon and specification
  group_by(Horizon, Period, Specification) %>%
  slice_min(RMSE, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  
  mutate(
    Model_RMSE = paste0(
      "AR(", AR_Order, ")",
      " | ", sprintf("%.4f", RMSE)
    ),
    
    Category = case_when(
      Period == "Pre_COVID" ~ "Pre-COVID",
      
      Period == "Post_COVID" & Specification == "No_Dummy" ~
        "Post-COVID (No Dummy)",
      
      Period == "Post_COVID" & Specification == "With_Dummy" ~
        "Post-COVID (With Dummy)",
      
      Period == "All" & Specification == "No_Dummy" ~
        "All (No Dummy)",
      
      Period == "All" & Specification == "With_Dummy" ~
        "All (With Dummy)"
    )
  ) %>%
  
  select(Horizon, Category, Model_RMSE) %>%
  
  pivot_wider(
    names_from = Category,
    values_from = Model_RMSE
  ) %>%
  
  arrange(Horizon)



# Best AR(p) selected using pre-COVID RMSE only
best_pre_ar <- rmse_results %>%
  filter(
    Period == "Pre_COVID",
    Specification == "No_Dummy"
  ) %>%
  group_by(Horizon) %>%
  slice_min(RMSE, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(Horizon, AR_Order)

# Retrieve RMSE for the selected AR(p) across all periods
best_pre_ar_rmse <- rmse_results %>%

  inner_join(
    best_pre_ar,
    by = c("Horizon", "AR_Order")
  ) %>%

  # Remove redundant pre-COVID dummy results
  filter(
    !(Period == "Pre_COVID" & Specification == "With_Dummy")
  ) %>%

  mutate(
    Model = paste0("AR(", AR_Order, ")"),

    Category = case_when(
      Period == "Pre_COVID" ~ "Pre-COVID",

      Period == "Post_COVID" & Specification == "No_Dummy" ~
        "Post-COVID (No Dummy)",

      Period == "Post_COVID" & Specification == "With_Dummy" ~
        "Post-COVID (With Dummy)",

      Period == "All" & Specification == "No_Dummy" ~
        "All (No Dummy)",

      Period == "All" & Specification == "With_Dummy" ~
        "All (With Dummy)"
    )
  ) %>%

  select(Horizon, Model, Category, RMSE) %>%

  pivot_wider(
    names_from = Category,
    values_from = RMSE
  ) %>%

  arrange(Horizon)

# Best AR(p) selected using pre-COVID RMSE only
best_pre_ar <- rmse_results %>%
  filter(
    Period == "Pre_COVID",
    Specification == "No_Dummy"
  ) %>%
  group_by(Horizon) %>%
  slice_min(RMSE, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(Horizon, AR_Order)

# Retrieve RMSE for the selected AR(p) across all periods
best_pre_ar_rmse <- rmse_results %>%
  
  inner_join(
    best_pre_ar,
    by = c("Horizon", "AR_Order")
  ) %>%
  
  # Remove redundant pre-COVID dummy results
  filter(
    !(Period == "Pre_COVID" & Specification == "With_Dummy")
  ) %>%
  
  mutate(
    Model = paste0("AR(", AR_Order, ")"),
    
    Category = case_when(
      Period == "Pre_COVID" ~ "Pre-COVID",
      
      Period == "Post_COVID" & Specification == "No_Dummy" ~
        "Post-COVID (No Dummy)",
      
      Period == "Post_COVID" & Specification == "With_Dummy" ~
        "Post-COVID (With Dummy)",
      
      Period == "All" & Specification == "No_Dummy" ~
        "All (No Dummy)",
      
      Period == "All" & Specification == "With_Dummy" ~
        "All (With Dummy)"
    )
  ) %>%
  
  select(Horizon, Model, Category, RMSE) %>%
  
  pivot_wider(
    names_from = Category,
    values_from = RMSE
  ) %>%
  
  arrange(Horizon)

print(best_pre_ar_rmse, width = Inf)
print(best_ar_summary, width = Inf)

