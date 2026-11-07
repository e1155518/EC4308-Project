# Random walk without drift: Canadian employment-rate LEVEL forecasts.
# Run from EC4308-Project.Rproj root.
# At forecast origin t, forecast Employ_Rate[t+h] = Employ_Rate[t].
# No estimation, future information, or COVID controls are required.

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

horizons <- c(1L, 3L, 6L, 12L)
pre_end <- as.Date('2019-12-01')
post_end <- as.Date('2025-09-01')
n_pre <- 100L
n_post <- 69L

can <- as.data.frame(read_excel('data/CAN_data_cleaned.xlsx'))
if ('Employ_rate' %in% names(can) && !'Employ_Rate' %in% names(can)) {
  names(can)[names(can) == 'Employ_rate'] <- 'Employ_Rate'
}
stopifnot(all(c('Date', 'Employ_Rate') %in% names(can)))
can$Date <- as.Date(can$Date)
can <- can[order(can$Date), , drop = FALSE]
stopifnot(!anyNA(can$Date), !anyDuplicated(can$Date),
          identical(can$Date, seq(min(can$Date), max(can$Date), by = 'month')),
          is.numeric(can$Employ_Rate))

run_random_walk <- function(data, horizon = 1L, nprev = 100L,
                            test_end = as.Date('2019-12-01')) {
  stopifnot(horizon >= 1L, horizon == as.integer(horizon),
            nprev >= 1L, nprev == as.integer(nprev))
  end_idx <- match(as.Date(test_end), data$Date)
  if (is.na(end_idx)) stop('Evaluation end date not found: ', test_end)
  outcome_idx <- seq.int(end_idx - nprev + 1L, end_idx)
  origin_idx <- outcome_idx - horizon
  if (min(origin_idx) < 1L) stop('Forecast origin precedes first observation.')
  actual <- data$Employ_Rate[outcome_idx]
  predicted <- data$Employ_Rate[origin_idx]
  if (anyNA(actual) || anyNA(predicted) ||
      any(!is.finite(actual)) || any(!is.finite(predicted))) {
    stop('Missing or nonfinite employment rate in test observations.')
  }
  forecasts <- data.frame(
    Forecast_Origin = data$Date[origin_idx],
    Outcome_Date = data$Date[outcome_idx],
    Actual = actual,
    Predicted = predicted,
    Forecast_Error = actual - predicted
  )
  list(forecasts = forecasts,
       RMSE = sqrt(mean(forecasts$Forecast_Error^2)))
}

rw_results <- list()
all_forecasts <- list()
for (h in horizons) {
  for (period in c('Pre_COVID', 'Post_COVID')) {
    fit <- run_random_walk(
      can, horizon = h,
      nprev = if (period == 'Pre_COVID') n_pre else n_post,
      test_end = if (period == 'Pre_COVID') pre_end else post_end
    )
    key <- paste(h, period, sep = '_')
    rw_results[[key]] <- fit
    df <- fit$forecasts
    df$Horizon <- h
    df$Model <- 'Random_Walk'
    df$Specification <- 'No_Dummy'
    df$Period <- period
    df$Predictor_Set <- 'CAN'
    all_forecasts[[key]] <- df
  }
}

rw_predictions <- bind_rows(all_forecasts) %>%
  arrange(Horizon, Outcome_Date)

# Ensure exactly the same outcome dates for every horizon within each period.
for (period in c('Pre_COVID', 'Post_COVID')) {
  dates_ref <- rw_predictions %>%
    filter(Period == period, Horizon == horizons[1]) %>% pull(Outcome_Date)
  for (h in horizons[-1]) {
    dates_h <- rw_predictions %>%
      filter(Period == period, Horizon == h) %>% pull(Outcome_Date)
    stopifnot(identical(dates_ref, dates_h))
  }
}

rw_rmse <- rw_predictions %>%
  group_by(Horizon, Model, Specification, Period) %>%
  summarise(RMSE = sqrt(mean(Forecast_Error^2)), .groups = 'drop')
rw_rmse_all <- rw_predictions %>%
  group_by(Horizon, Model, Specification) %>%
  summarise(Period = 'All', RMSE = sqrt(mean(Forecast_Error^2)), .groups = 'drop')
rw_rmse <- bind_rows(rw_rmse, rw_rmse_all)

rw_rmse_table <- rw_rmse %>%
  select(Horizon, Period, RMSE) %>%
  pivot_wider(names_from = Period, values_from = RMSE) %>%
  arrange(match(Horizon, horizons)) %>%
  select(Horizon, Pre_COVID, Post_COVID, All) %>%
  mutate(Horizon = paste0('h', Horizon))
print(rw_rmse_table, n = Inf)

plot_random_walk <- function(h, period = 'all') {
  stopifnot(h %in% horizons)
  period <- match.arg(period, c('all', 'pre', 'post'))
  df <- rw_predictions %>% filter(Horizon == h)
  if (period == 'pre') df <- filter(df, Period == 'Pre_COVID')
  if (period == 'post') df <- filter(df, Period == 'Post_COVID')
  rmse <- sqrt(mean(df$Forecast_Error^2))
  ggplot(df, aes(x = Outcome_Date)) +
    geom_line(aes(y = Predicted, color = 'Predicted'), linewidth = 0.8) +
    geom_line(aes(y = Actual, color = 'Actual'), linewidth = 0.8) +
    scale_color_manual(values = c('Actual' = 'black', 'Predicted' = '#2563EB')) +
    labs(title = paste0(h, '-Month Forecast: Random Walk (No Drift)'),
         subtitle = paste0(period, ' | RMSE = ', round(rmse, 4)),
         x = 'Outcome Date', y = 'Canadian Employment Rate (percentage points)',
         color = NULL) +
    theme_minimal(base_size = 12)
}

# Save CSVs and all 12 plots without overwriting other model outputs.
results_dir <- 'results/benchmarks'
plots_dir <- file.path(results_dir, 'level_Random_Walk_plots')
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
write.csv(rw_predictions,
          file.path(results_dir, 'level_random_walk_predictions.csv'), row.names = FALSE)
write.csv(rw_rmse,
          file.path(results_dir, 'level_random_walk_RMSE_long.csv'), row.names = FALSE)
write.csv(rw_rmse_table,
          file.path(results_dir, 'level_random_walk_RMSE.csv'), row.names = FALSE)

for (h in horizons) {
  for (period in c('all', 'pre', 'post')) {
    p <- plot_random_walk(h, period)
    filename <- paste0('level_random_walk_h', h, '_', period, '.png')
    ggsave(file.path(plots_dir, filename), plot = p,
           width = 10, height = 5.5, dpi = 300)
  }
}
cat('Saved CSVs to:', results_dir, '\n')
cat('Saved 12 plots to:', plots_dir, '\n')
