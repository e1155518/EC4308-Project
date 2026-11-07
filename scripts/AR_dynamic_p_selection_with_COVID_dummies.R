# Dynamic AR(p): BIC order selection at each forecast origin.
# Run from EC4308-Project.Rproj root. Requires readxl, dplyr, tidyr, ggplot2.
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

can <- read_excel('data/CAN_data_cleaned.xlsx')
can$Date <- as.Date(can$Date)
can <- can[order(can$Date), ]

horizons <- c(1L, 3L, 6L, 12L)
ar_orders <- 1:12
required_cols <- c('Date', 'Employ_Rate', 'Employ_Rate_diff_lag1',
                   paste0('Employ_Rate_diff_h', horizons))
stopifnot(all(required_cols %in% names(can)), !anyNA(can$Date),
          !anyDuplicated(can$Date),
          identical(can$Date, seq(min(can$Date), max(can$Date), by = 'month')))

# Historical outcome-month controls, matching the fixed AR implementation.
can$COVID_Shock <- as.integer(can$Date >= as.Date('2020-03-01') &
                              can$Date <= as.Date('2020-04-01'))
can$COVID_Recovery <- as.integer(can$Date >= as.Date('2020-05-01') &
                                 can$Date <= as.Date('2022-02-01'))
for (h in horizons) {
  for (phase in c('COVID_Shock', 'COVID_Recovery')) {
    z <- rep(NA_real_, nrow(can))
    valid <- seq_len(nrow(can) - h)
    z[valid] <- vapply(valid, function(t) mean(can[[phase]][(t + 1):(t + h)]), numeric(1))
    can[[paste0(phase, '_h', h)]] <- z
  }
}

# Estimate all candidate orders on IDENTICAL observations (max-lag alignment).
# The last h target rows are excluded: their outcomes are not yet observable.
runAR_dynamic <- function(Y, horizon = 1L, max_lag = 12L, use_dummy = FALSE) {
  n <- nrow(Y)
  if (max_lag < 1 || horizon < 1 || max_lag != as.integer(max_lag) ||
      horizon != as.integer(horizon) || n < max_lag + horizon + 4) {
    stop('Invalid maximum lag/horizon or insufficient window data.')
  }
  if (use_dummy && ncol(Y) != 4L) stop('Dummy specification needs four columns.')
  if (!use_dummy && ncol(Y) != 2L) stop('No-dummy specification needs two columns.')

  # At row t, L1 is the contemporaneously observed monthly change,
  # L2 is the preceding change, etc. This matches fixed AR's embed() convention.
  target_t <- seq.int(max_lag, n - horizon)
  design <- data.frame(y = Y[target_t, 1])
  for (lag in seq_len(max_lag)) {
    design[[paste0('lag', lag)]] <- Y[target_t - lag + 1L, 2]
  }
  forecast_x <- as.data.frame(as.list(setNames(
    as.numeric(Y[n - seq_len(max_lag) + 1L, 2]), paste0('lag', seq_len(max_lag)))))

  active <- character(0)
  if (use_dummy) {
    dummy_train <- Y[target_t, 3:4, drop = FALSE]
    if (anyNA(dummy_train)) stop('Missing historical COVID controls.')
    active <- c('COVID_Shock', 'COVID_Recovery')[colSums(abs(dummy_train)) > 0]
    for (nm in active) {
      k <- match(nm, c('COVID_Shock', 'COVID_Recovery'))
      design[[nm]] <- dummy_train[, k]
      # Dummies are historical outlier adjustments, never forecast-period inputs.
      forecast_x[[nm]] <- 0
    }
  }
  if (anyNA(design) || anyNA(forecast_x)) stop('Missing target or lag values.')

  best_bic <- Inf
  best_fit <- NULL
  best_p <- NA_integer_
  for (p in seq_len(max_lag)) {
    vars <- c(paste0('lag', seq_len(p)), active)
    fit <- lm(reformulate(vars, response = 'y'), data = design)
    if (anyNA(coef(fit))) stop('Rank-deficient candidate AR/dummy regression.')
    score <- BIC(fit)
    if (score < best_bic) {
      best_bic <- score
      best_fit <- fit
      best_p <- p
    }
  }
  pred <- as.numeric(predict(best_fit, newdata = forecast_x))
  dummy_coef <- setNames(c(0, 0), c('COVID_Shock', 'COVID_Recovery'))
  if (length(active)) dummy_coef[active] <- coef(best_fit)[active]
  list(pred = pred, best_p = best_p, BIC = best_bic,
       model = best_fit, coef = coef(best_fit), dummy_coef = dummy_coef)
}

ar.rolling.window.dynamic <- function(data, nprev = 100L, horizon = 1L,
                                      test_end = as.Date('2019-12-01'),
                                      window_size = NULL, max_lag = 12L,
                                      use_dummy = FALSE) {
  target_col <- paste0('Employ_Rate_diff_h', horizon)
  end_idx <- match(as.Date(test_end), data$Date)
  if (is.na(end_idx)) stop('test_end must match an observed date.')
  if (nprev < 1 || nprev != as.integer(nprev)) stop('Invalid nprev.')
  outcome_idx <- seq.int(end_idx - nprev + 1L, end_idx)
  origin_idx <- outcome_idx - horizon
  if (min(origin_idx) < 1) stop('Forecast origins precede available data.')
  if (is.null(window_size)) window_size <- min(origin_idx)
  if (window_size < max_lag + horizon + 4 ||
      min(origin_idx) - window_size + 1L < 1L) stop('Invalid window size.')

  cols <- c(target_col, 'Employ_Rate_diff_lag1')
  if (use_dummy) cols <- c(cols, paste0(c('COVID_Shock', 'COVID_Recovery'), '_h', horizon))
  output <- vector('list', nprev)
  for (j in seq_len(nprev)) {
    origin <- origin_idx[j]
    window <- as.matrix(data[(origin - window_size + 1L):origin, cols])
    fit <- runAR_dynamic(window, horizon, max_lag, use_dummy)
    actual <- data[[target_col]][origin]
    if (is.na(actual)) stop('Missing realized evaluation target at ', as.character(data$Date[origin]))
    output[[j]] <- data.frame(
      Forecast_Origin = data$Date[origin],
      Outcome_Date = data$Date[outcome_idx[j]],
      Actual = actual, Predicted = fit$pred,
      Forecast_Error = actual - fit$pred,
      AR_Order = fit$best_p, BIC = fit$BIC,
      COVID_Shock_Coef = unname(fit$dummy_coef['COVID_Shock']),
      COVID_Recovery_Coef = unname(fit$dummy_coef['COVID_Recovery']))
  }
  forecasts <- bind_rows(output)
  list(forecasts = forecasts, RMSE = sqrt(mean(forecasts$Forecast_Error^2)))
}

# Identical pre/post dates, number of forecasts and fixed raw window lengths
# to AR_fixed_AR(p)_comparison_with_COVID_dummies.R.
pre_end <- as.Date('2019-12-01')
post_end <- as.Date('2025-09-01')
n_pre <- 100L
n_post <- 69L
pre_end_idx <- match(pre_end, can$Date)
stopifnot(!is.na(pre_end_idx), !is.na(match(post_end, can$Date)))
window_by_h <- setNames(vapply(horizons, function(h) {
  pre_end_idx - n_pre + 1L - h
}, numeric(1)), as.character(horizons))

ar_dynamic_results <- list()
all_forecasts <- list()
for (h in horizons) {
  for (period in c('Pre_COVID', 'Post_COVID')) {
    for (spec in c('No_Dummy', 'With_Dummy')) {
      # Pre-COVID dummy/no-dummy models coincide: estimate only once.
      if (period == 'Pre_COVID' && spec == 'With_Dummy') next
      cat('Dynamic BIC: h=', h, ' ', period, ' ', spec, '\n', sep = '')
      fit <- ar.rolling.window.dynamic(
        can, nprev = if (period == 'Pre_COVID') n_pre else n_post,
        horizon = h,
        test_end = if (period == 'Pre_COVID') pre_end else post_end,
        window_size = window_by_h[as.character(h)],
        max_lag = max(ar_orders), use_dummy = spec == 'With_Dummy')
      key <- paste(h, spec, period, sep = '_')
      ar_dynamic_results[[key]] <- fit
      df <- fit$forecasts
      df$Horizon <- h
      df$Model <- 'Dynamic_AR_BIC'
      df$Specification <- spec
      df$Period <- period
      df$Predictor_Set <- 'CAN'
      all_forecasts[[key]] <- df
    }
  }
}

ar_dynamic_predictions <- bind_rows(all_forecasts)
pre_dummy <- ar_dynamic_predictions %>%
  filter(Period == 'Pre_COVID', Specification == 'No_Dummy') %>%
  mutate(Specification = 'With_Dummy')
ar_dynamic_predictions <- bind_rows(ar_dynamic_predictions, pre_dummy) %>%
  arrange(Horizon, Specification, Outcome_Date)

rmse_dynamic <- ar_dynamic_predictions %>%
  group_by(Horizon, Model, Specification, Period) %>%
  summarise(RMSE = sqrt(mean(Forecast_Error^2)), .groups = 'drop')
rmse_all <- ar_dynamic_predictions %>%
  group_by(Horizon, Model, Specification) %>%
  summarise(Period = 'All', RMSE = sqrt(mean(Forecast_Error^2)), .groups = 'drop')
rmse_dynamic <- bind_rows(rmse_dynamic, rmse_all)

rmse_dynamic_tables <- lapply(horizons, function(h) {
  rmse_dynamic %>%
    filter(Horizon == h) %>%
    mutate(Column = case_when(
      Period == 'Pre_COVID' & Specification == 'No_Dummy' ~ 'Pre_COVID',
      Period == 'Post_COVID' & Specification == 'No_Dummy' ~ 'Post_No_Dummy',
      Period == 'Post_COVID' & Specification == 'With_Dummy' ~ 'Post_With_Dummy',
      Period == 'All' & Specification == 'No_Dummy' ~ 'All_No_Dummy',
      Period == 'All' & Specification == 'With_Dummy' ~ 'All_With_Dummy',
      TRUE ~ NA_character_)) %>%
    filter(!is.na(Column)) %>%
    select(Column, RMSE) %>%
    pivot_wider(names_from = Column, values_from = RMSE) %>%
    select(Pre_COVID, Post_No_Dummy, Post_With_Dummy, All_No_Dummy, All_With_Dummy)
})
names(rmse_dynamic_tables) <- paste0('h', horizons)
for (h in horizons) {
  cat('\nDynamic BIC RMSE: h=', h, '\n', sep = '')
  print(rmse_dynamic_tables[[paste0('h', h)]])
}

# Selected-order frequencies (do not double-count duplicate pre-COVID dummy forecasts).
ar_dynamic_order_frequency <- ar_dynamic_predictions %>%
  filter(!(Period == 'Pre_COVID' & Specification == 'With_Dummy')) %>%
  count(Horizon, Period, Specification, AR_Order, name = 'Count') %>%
  arrange(Horizon, Period, Specification, AR_Order)

plot_AR_dynamic <- function(h, period = 'all') {
  stopifnot(h %in% horizons)
  period <- match.arg(period, c('all', 'pre', 'post'))
  df <- ar_dynamic_predictions %>% filter(Horizon == h)
  if (period == 'pre') df <- filter(df, Period == 'Pre_COVID')
  if (period == 'post') df <- filter(df, Period == 'Post_COVID')
  plot_df <- df %>%
    select(Outcome_Date, Actual, Predicted, Specification) %>%
    pivot_wider(names_from = Specification, values_from = Predicted) %>%
    arrange(Outcome_Date)
  rmse_no <- sqrt(mean((plot_df$Actual - plot_df$No_Dummy)^2))
  rmse_dummy <- sqrt(mean((plot_df$Actual - plot_df$With_Dummy)^2))
  plot <- ggplot(plot_df, aes(x = Outcome_Date)) +
    geom_line(aes(y = With_Dummy, color = 'With Dummy'), linewidth = 0.8) +
    geom_line(aes(y = No_Dummy, color = 'Without Dummy'), linewidth = 0.8) +
    geom_line(aes(y = Actual, color = 'Actual'), linewidth = 0.8) +
    scale_color_manual(values = c('Actual' = 'black', 'Without Dummy' = '#2563EB',
                                  'With Dummy' = '#E69F00')) +
    labs(title = paste0(h, '-Month Forecast: Dynamic AR(1)-AR(12), BIC'),
         subtitle = paste0(period, ' | RMSE (No Dummy) = ', round(rmse_no, 4),
                           ' | RMSE (With Dummy) = ', round(rmse_dummy, 4)),
         x = 'Outcome Date', y = 'Employment Rate Change (percentage points)',
         color = NULL) + theme_minimal()
  print(plot)
  invisible(plot)
}


plot_AR_dynamic_orders <- function(h, period = "all") {
  stopifnot(h %in% horizons)
  period <- match.arg(period, c("all", "pre", "post"))
  
  df <- ar_dynamic_predictions %>%
    filter(Horizon == h)
  
  if (period == "pre") {
    df <- filter(df, Period == "Pre_COVID")
  } else if (period == "post") {
    df <- filter(df, Period == "Post_COVID")
  }
  
  df <- df %>%
    mutate(Specification = recode(
      Specification,
      No_Dummy = "Without Dummy",
      With_Dummy = "With Dummy"
    ))
  
  plot <- ggplot(
    df,
    aes(x = Outcome_Date, y = AR_Order,
        color = Specification, group = Specification)
  ) +
    geom_step(linewidth = 0.85) +
    geom_point(size = 1, alpha = 0.75) +
    scale_color_manual(values = c(
      "Without Dummy" = "#2563EB",
      "With Dummy" = "#E69F00"
    )) +
    scale_y_continuous(
      breaks = 1:12,
      limits = c(1, 12)
    ) +
    labs(
      title = paste0(
        "BIC-Selected AR Order: ", h, "-Month Horizon"
      ),
      subtitle = period,
      x = "Outcome Date",
      y = "Selected AR Order",
      color = NULL
    ) +
    theme_minimal()
  
  print(plot)
  invisible(plot)
}


# Use distinct filenames to avoid overwriting the fixed-AR model's exports.
dir.create('results', showWarnings = FALSE, recursive = TRUE)
write.csv(ar_dynamic_predictions, 'results/AR/AR_dynamic_p_predictions.csv', row.names = FALSE)
write.csv(rmse_dynamic, 'results/AR/AR_dynamic_p_RMSE_long.csv', row.names = FALSE)
write.csv(ar_dynamic_order_frequency, 'results/AR/AR_dynamic_p_order_frequency.csv', row.names = FALSE)
rmse_all_horizons <- dplyr::bind_rows(rmse_dynamic_tables, .id = "Horizon")
write.csv(rmse_all_horizons, "results/AR/AR_dynamic_p_RMSE.csv", row.names = FALSE)

AR_order_frequency <- function(period = "all") {
  
  period <- match.arg(period, c("all", "pre", "post"))
  
  df <- ar_dynamic_predictions
  
  if (period == "pre") {
    df <- df %>% filter(Period == "Pre_COVID")
  } else if (period == "post") {
    df <- df %>% filter(Period == "Post_COVID")
  }
  
  df %>%
    count(Horizon, Specification, AR_Order, name = "Frequency") %>%
    complete(
      Horizon = horizons,
      Specification = c("No_Dummy", "With_Dummy"),
      AR_Order = 1:12,
      fill = list(Frequency = 0)
    ) %>%
    mutate(Column = paste0("h", Horizon, "_", Specification)) %>%
    select(AR_Order, Column, Frequency) %>%
    pivot_wider(
      names_from = Column,
      values_from = Frequency
    ) %>%
    arrange(AR_Order)
}


# Example calls (run only after estimation finishes):
plot_AR_dynamic(12, 'all')
# plot_AR_dynamic(3, 'post')
# plot_AR_dynamic(12, 'pre')
plot_AR_dynamic_orders(12, 'all')
# plot_AR_dynamic_orders(6, 'post')

AR_order_frequency("all")
# AR_order_frequency("pre")
# AR_order_frequency("post")


plot_AR_order_heatmap <- function(period = "all") {
  
  period <- match.arg(period, c("all", "pre", "post"))
  
  # Retrieve frequency table
  df <- AR_order_frequency(period) %>%
    pivot_longer(
      cols = -AR_Order,
      names_to = "Column",
      values_to = "Frequency"
    ) %>%
    separate(
      Column,
      into = c("Horizon", "Specification"),
      sep = "_",
      extra = "merge"
    ) %>%
    mutate(
      Horizon = factor(
        Horizon,
        levels = paste0("h", c(1, 3, 6, 12)),
        labels = c("1 Month", "3 Months",
                   "6 Months", "12 Months")
      ),
      Specification = recode(
        Specification,
        No_Dummy = "Without Dummy",
        With_Dummy = "With Dummy"
      ),
      AR_Order = factor(
        AR_Order,
        levels = 12:1,
        labels = paste0("AR(", 12:1, ")")
      )
    )
  
  ggplot(df, aes(
    x = Horizon,
    y = AR_Order,
    fill = Frequency
  )) +
    geom_tile(color = "white", linewidth = 0.6) +
    geom_text(
      aes(label = Frequency,
          color = Frequency > max(Frequency) / 2),
      size = 4,
      fontface = "bold"
    ) +
    scale_color_manual(
      values = c("FALSE" = "black", "TRUE" = "white"),
      guide = "none"
    ) +
    scale_fill_gradient(
      low = "#EFF6FF",
      high = "#1E3A8A"
    ) +
    facet_wrap(~ Specification, nrow = 1) +
    labs(
      title = "Frequency of BIC-Selected AR Orders",
      subtitle = paste("Evaluation Period:", period),
      x = "Forecast Horizon",
      y = "Selected AR Order",
      fill = "Frequency"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(face = "bold"),
      strip.text = element_text(face = "bold", size = 12),
      panel.grid = element_blank()
    )
}

plot_AR_order_heatmap("all")
# plot_AR_order_heatmap("pre")
# plot_AR_order_heatmap("post")
