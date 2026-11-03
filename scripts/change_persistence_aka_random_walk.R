# Persistence benchmark: predict the next h-month employment-rate change
# using the immediately preceding h-month change.
# Run from EC4308-Project.Rproj root.
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

can <- read_excel('data/CAN_data_cleaned.xlsx')
can$Date <- as.Date(can$Date)
can <- can[order(can$Date), ]
horizons <- c(1L, 3L, 6L, 12L)
required_cols <- c('Date', 'Employ_Rate', paste0('Employ_Rate_diff_h', horizons))
stopifnot(all(required_cols %in% names(can)), !anyNA(can$Date),
          !anyDuplicated(can$Date),
          identical(can$Date, seq(min(can$Date), max(can$Date), by = 'month')))

pre_end <- as.Date('2019-12-01')
post_end <- as.Date('2025-09-01')
n_pre <- 100L
n_post <- 69L
pre_end_idx <- match(pre_end, can$Date)
stopifnot(!is.na(pre_end_idx), !is.na(match(post_end, can$Date)))
# Match the raw rolling-window lengths used by dynamic AR, for each horizon.
window_by_h <- setNames(vapply(horizons, function(h) {
  pre_end_idx - n_pre + 1L - h
}, numeric(1)), as.character(horizons))

run_benchmark <- function(data, nprev = 100L, horizon = 1L,
                          test_end = as.Date('2019-12-01'),
                          window_size = NULL) {
  target_col <- paste0('Employ_Rate_diff_h', horizon)
  if (!target_col %in% names(data)) stop('Missing target column: ', target_col)
  end_idx <- match(as.Date(test_end), data$Date)
  if (is.na(end_idx)) stop('test_end must match an observed date.')
  if (nprev < 1 || nprev != as.integer(nprev)) stop('Invalid nprev.')
  outcome_idx <- seq.int(end_idx - nprev + 1L, end_idx)
  origin_idx <- outcome_idx - horizon
  if (min(origin_idx) < 1L) stop('Forecast origins precede available data.')
  if (is.null(window_size)) window_size <- min(origin_idx)
  if (window_size < horizon + 1L ||
      min(origin_idx) - window_size + 1L < 1L) stop('Invalid window size.')

  out <- vector('list', nprev)
  for (j in seq_len(nprev)) {
    origin <- origin_idx[j]
    actual <- data[[target_col]][origin]
    if (is.na(actual)) stop('Missing actual at origin ', as.character(data$Date[origin]))
    # Persistence in changes: forecast E(t+h)-E(t) using E(t)-E(t-h).
    # Both employment-rate levels are observed at the forecast origin.
    if (origin - horizon < 1L) stop('Insufficient history for lagged change.')
    current_level <- data$Employ_Rate[origin]
    previous_level <- data$Employ_Rate[origin - horizon]
    if (is.na(current_level) || is.na(previous_level)) {
      stop('Missing employment rate needed at origin ', as.character(data$Date[origin]))
    }
    pred <- current_level - previous_level
    out[[j]] <- data.frame(
      Forecast_Origin = data$Date[origin],
      Outcome_Date = data$Date[outcome_idx[j]],
      Actual = actual,
      Predicted = pred,
      Forecast_Error = actual - pred)
  }
  forecasts <- bind_rows(out)
  list(forecasts = forecasts,
       RMSE = sqrt(mean(forecasts$Forecast_Error^2)))
}

benchmark_results <- list()
all_forecasts <- list()
for (h in horizons) {
  for (period in c('Pre_COVID', 'Post_COVID')) {
    cat('Change Persistence: h=', h, ' ', period, '\n', sep = '')
    fit <- run_benchmark(
      can,
      nprev = if (period == 'Pre_COVID') n_pre else n_post,
      horizon = h,
      test_end = if (period == 'Pre_COVID') pre_end else post_end,
      window_size = window_by_h[as.character(h)])
    key <- paste(h, period, sep = '_')
    benchmark_results[[key]] <- fit
    df <- fit$forecasts
    df$Horizon <- h
    df$Model <- 'Change_Persistence'
    df$Specification <- 'No_Dummy'
    df$Period <- period
    df$Predictor_Set <- 'CAN'
    all_forecasts[[key]] <- df
  }
}

benchmark_predictions <- bind_rows(all_forecasts) %>%
  arrange(Horizon, Outcome_Date)
benchmark_rmse <- benchmark_predictions %>%
  group_by(Horizon, Model, Specification, Period) %>%
  summarise(RMSE = sqrt(mean(Forecast_Error^2)), .groups = 'drop')
benchmark_rmse_all <- benchmark_predictions %>%
  group_by(Horizon, Model, Specification) %>%
  summarise(Period = 'All', RMSE = sqrt(mean(Forecast_Error^2)), .groups = 'drop')
benchmark_rmse <- bind_rows(benchmark_rmse, benchmark_rmse_all)

benchmark_rmse_tables <- lapply(horizons, function(h) {
  benchmark_rmse %>%
    filter(Horizon == h) %>%
    select(Period, RMSE) %>%
    mutate(Period = recode(Period,
                           Pre_COVID = 'Pre_COVID',
                           Post_COVID = 'Post_COVID',
                           All = 'All')) %>%
    pivot_wider(names_from = Period, values_from = RMSE) %>%
    select(Pre_COVID, Post_COVID, All)
})
names(benchmark_rmse_tables) <- paste0('h', horizons)
for (h in horizons) {
  cat('\nChange Persistence RMSE: h=', h, '\n', sep = '')
  print(benchmark_rmse_tables[[paste0('h', h)]])
}

plot_benchmark <- function(h, period = 'all') {
  stopifnot(h %in% horizons)
  period <- match.arg(period, c('all', 'pre', 'post'))
  df <- benchmark_predictions %>% filter(Horizon == h)
  if (period == 'pre') df <- filter(df, Period == 'Pre_COVID')
  if (period == 'post') df <- filter(df, Period == 'Post_COVID')
  rmse <- sqrt(mean(df$Forecast_Error^2))
  plot <- ggplot(df, aes(x = Outcome_Date)) +
    geom_line(aes(y = Predicted, color = 'Predicted'), linewidth = 0.8) +
    geom_line(aes(y = Actual, color = 'Actual'), linewidth = 0.8) +
    scale_color_manual(values = c('Actual' = 'black', 'Predicted' = '#2563EB')) +
    labs(title = paste0(h, '-Month Forecast: Change Persistence'),
         subtitle = paste0(period, ' | RMSE = ', round(rmse, 4)),
         x = 'Outcome Date',
         y = 'Employment Rate Change (percentage points)', color = NULL) +
    theme_minimal()
  print(plot)
  invisible(plot)
}

# Export same forecast/error columns as AR for future DM/CW tests and ensembles.
dir.create('results', showWarnings = FALSE, recursive = TRUE)
write.csv(benchmark_predictions, 'results/Change_Persistence_predictions.csv', row.names = FALSE)
write.csv(benchmark_rmse, 'results/Change_Persistence_RMSE_long.csv', row.names = FALSE)
rmse_all_horizons <- bind_rows(benchmark_rmse_tables, .id = 'Horizon')
write.csv(rmse_all_horizons, 'results/Change_Persistence_RMSE.csv', row.names = FALSE)

# Example plots:
plot_benchmark(1, 'all')
# plot_benchmark(3, 'pre')
# plot_benchmark(6, 'post')
