# Zero-change benchmark: predicted h-month employment-rate change = 0.
# Run from the EC4308-Project.Rproj root.
library(readxl)
library(dplyr)
library(tidyr)

can <- read_excel('data/CAN_data_cleaned.xlsx')
can$Date <- as.Date(can$Date)
can <- can[order(can$Date), ]

horizons <- c(1L, 3L, 6L, 12L)
periods <- data.frame(
  Period = c('Pre_COVID', 'Post_COVID'),
  End_Date = as.Date(c('2019-12-01', '2025-09-01')),
  N = c(100L, 69L)
)

results <- list()
for (h in horizons) {
  target <- paste0('Employ_Rate_diff_h', h)
  if (!target %in% names(can)) stop('Missing column: ', target)
  for (i in seq_len(nrow(periods))) {
    end_idx <- match(periods$End_Date[i], can$Date)
    if (is.na(end_idx)) stop('Missing end date: ', periods$End_Date[i])
    outcome_idx <- seq.int(end_idx - periods$N[i] + 1L, end_idx)
    origin_idx <- outcome_idx - h
    if (min(origin_idx) < 1L) stop('Not enough data for horizon ', h)
    actual <- can[[target]][origin_idx]
    if (anyNA(actual)) stop('Missing actual values for horizon ', h)
    results[[length(results) + 1L]] <- data.frame(
      Forecast_Origin = can$Date[origin_idx],
      Outcome_Date = can$Date[outcome_idx],
      Actual = actual,
      Predicted = 0,
      Forecast_Error = actual,
      Horizon = h,
      Model = 'Zero_Change',
      Specification = 'No_Dummy',
      Period = periods$Period[i],
      Predictor_Set = 'CAN'
    )
  }
}

zero_change_predictions <- bind_rows(results) %>%
  arrange(Horizon, Outcome_Date)

zero_change_RMSE_long <- bind_rows(
  zero_change_predictions %>%
    group_by(Horizon, Model, Specification, Period) %>%
    summarise(RMSE = sqrt(mean(Forecast_Error^2)), .groups = 'drop'),
  zero_change_predictions %>%
    group_by(Horizon, Model, Specification) %>%
    summarise(RMSE = sqrt(mean(Forecast_Error^2)), .groups = 'drop') %>%
    mutate(Period = 'All')
) %>% arrange(Horizon, Period)

zero_change_RMSE <- zero_change_RMSE_long %>%
  mutate(Horizon = paste0('h', Horizon)) %>%
  select(Horizon, Period, RMSE) %>%
  pivot_wider(names_from = Period, values_from = RMSE) %>%
  select(Horizon, Pre_COVID, Post_COVID, All)

print(zero_change_RMSE, n = Inf)

dir.create('results', showWarnings = FALSE, recursive = TRUE)
write.csv(zero_change_predictions, 'results/zero_change_predictions.csv', row.names = FALSE)
write.csv(zero_change_RMSE_long, 'results/zero_change_RMSE_long.csv', row.names = FALSE)
write.csv(zero_change_RMSE, 'results/zero_change_RMSE.csv', row.names = FALSE)
