# Run from your EC4308 project root (where the data/ folder exists).
# install.packages(c('readxl','dplyr','tidyr','ggplot2','randomForest'))
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(randomForest)
# Forecasting functions are defined below.
# Direct multi-horizon random forest, following professor's runrf / rf.rolling.window.
# No PCA. Four lags (current through t-3) for each selected predictor.

runrf <- function(Y, horizon = 1L, n_lags = 4L, use_dummy = FALSE,
                  seed = 4308L) {
  # Y columns: target, predictors, and optionally Shock/Recovery target controls.
  if (nrow(Y) <= horizon + n_lags + 5L) stop('Too few rows for RF estimation.')
  dcols <- if (use_dummy) c('COVID_Shock_Control', 'COVID_Recovery_Control') else character()
  pcols <- setdiff(colnames(Y), c('Target', dcols))
  if (!length(pcols)) stop('No RF predictors supplied.')
  n <- nrow(Y)
  # Each training row indexed s uses predictors known at s, and target E[s+h]-E[s].
  # Training targets observable at forecast origin n require s <= n-h.
  s <- seq.int(n_lags, n - horizon)
  make_x <- function(origins) {
    ans <- do.call(cbind, lapply(seq_len(n_lags) - 1L, function(k) {
      v <- as.matrix(Y[origins - k, pcols, drop = FALSE])
      colnames(v) <- paste0(pcols, '_lag', k)
      v
    }))
    as.data.frame(ans, check.names = FALSE)
  }
  X <- make_x(s)
  y <- as.numeric(Y[s, 'Target'])
  X.out <- make_x(n)
  if (use_dummy) {
    controls <- Y[s, dcols, drop = FALSE]
    # All-zero controls are not useful and cannot be split upon.
    active <- names(controls)[vapply(controls, function(v) any(v != 0), logical(1))]
    if (length(active)) {
      X <- cbind(X, controls[, active, drop = FALSE])
      X.out <- cbind(X.out, as.data.frame(matrix(0, 1, length(active),
                          dimnames = list(NULL, active))))
    }
  }
  if (anyNA(X) || anyNA(y) || anyNA(X.out) ||
      any(!is.finite(as.matrix(X))) || any(!is.finite(y)) ||
      any(!is.finite(as.matrix(X.out)))) {
    stop('Missing/nonfinite training data or forecast predictors. Inspect cleaned data.')
  }
  set.seed(seed)
  # Professor's defaults: ntree=500, mtry=max(floor(P/3),1), nodesize=5.
  model <- randomForest::randomForest(x = X, y = y, importance = TRUE)
  pred <- as.numeric(predict(model, newdata = X.out))
  list(model = model, pred = pred, importance = randomForest::importance(model))
}

rf.rolling.window <- function(data, predictor_cols, horizon = 1L,
                              nprev = 100L, test_end = as.Date('2019-12-01'),
                              window_size = NULL, use_dummy = FALSE,
                              n_lags = 4L, seed = 4308L, verbose = TRUE) {
  target_col <- paste0('Employ_Rate_diff_h', horizon)
  if (!target_col %in% names(data)) stop('Missing target: ', target_col)
  idx <- match(as.Date(test_end), data$Date)
  if (is.na(idx)) stop('Evaluation end date not found: ', test_end)
  outcome_idx <- seq.int(idx - nprev + 1L, idx)
  origin_idx <- outcome_idx - horizon
  if (min(origin_idx) < 1L) stop('Forecast origins before first data row.')
  if (is.null(window_size)) window_size <- min(origin_idx)
  if (min(origin_idx) - window_size + 1L < 1L ||
      window_size <= horizon + n_lags + 5L) stop('Invalid rolling window size.')
  controls <- if (use_dummy) paste0(c('COVID_Shock', 'COVID_Recovery'), '_h', horizon) else character()
  needed <- c(target_col, predictor_cols, controls)
  if (!all(needed %in% names(data))) stop('Missing columns: ', paste(setdiff(needed, names(data)), collapse=', '))
  pred <- numeric(nprev)
  imp <- vector('list', nprev)
  for (i in nprev:1) {
    j <- 1L + nprev - i
    origin <- origin_idx[j]
    w <- data[(origin - window_size + 1L):origin, needed, drop = FALSE]
    names(w) <- c('Target', predictor_cols,
                  if (use_dummy) c('COVID_Shock_Control', 'COVID_Recovery_Control'))
    fit <- runrf(w, horizon, n_lags, use_dummy, seed = seed + j)
    pred[j] <- fit$pred
    imp[[j]] <- fit$importance
    if (verbose && (j == 1L || j %% 20L == 0L || j == nprev))
      cat('RF horizon', horizon, 'forecast', j, '/', nprev, '\n')
  }
  actual <- data[[target_col]][origin_idx]
  if (anyNA(actual)) stop('Missing actual outcomes in evaluation period.')
  errors <- actual - pred
  list(pred = pred, errors = c(rmse = sqrt(mean(errors^2)), mae = mean(abs(errors))),
       save.importance = imp,
       forecasts = data.frame(Forecast_Origin = data$Date[origin_idx],
         Outcome_Date = data$Date[outcome_idx], Actual = actual,
         Predicted = pred, Forecast_Error = errors))
}


horizons <- c(1L, 3L, 6L, 12L)
n_lags <- 4L
n_pre <- 100L
n_post <- 69L
pre_end <- as.Date('2019-12-01')
post_end <- as.Date('2025-09-01')

# CAN, US-only predictors, CAN+US. All three forecast CANADIAN employment.
files <- c(CAN = 'data/CAN_data_cleaned.xlsx',
           US = 'data/US_data_cleaned.xlsx',
           CAN_US = 'data/CAN_US_data_cleaned.xlsx')

prepare_data <- function(file, label) {
  d <- as.data.frame(read_excel(file))
  if (!'Date' %in% names(d)) stop(label, ': missing Date column')
  d$Date <- as.Date(d$Date)
  d <- d[order(d$Date), , drop = FALSE]
  if (anyNA(d$Date) || anyDuplicated(d$Date) ||
      !identical(d$Date, seq(min(d$Date), max(d$Date), by='month')))
    stop(label, ': dates must be consecutive, unique monthly observations')
  d
}
raw <- Map(prepare_data, files, names(files))
names(raw) <- names(files)

# All three datasets already contain the Canadian employment targets.
# Explicitly exclude all forward targets and the employment-rate level
# from the predictor set. The historical one-month change is allowed
# as an autoregressive predictor, consistent with the AR benchmark.
target_vars <- c('Employ_Rate', 'Employ_Rate_diff_lag1',
                 'Employ_Rate_diff_h1', 'Employ_Rate_diff_h3',
                 'Employ_Rate_diff_h6', 'Employ_Rate_diff_h12')
targets <- paste0('Employ_Rate_diff_h', horizons)

build_set <- function(label) {
  d <- raw[[label]]
  # Normalize only the known alternate spelling, if present.
  if ('Employ_rate' %in% names(d) && !'Employ_Rate' %in% names(d))
    names(d)[names(d) == 'Employ_rate'] <- 'Employ_Rate'
  required <- c(target_vars, 'Date')
  if (!all(required %in% names(d)))
    stop(label, ': missing required columns: ',
         paste(setdiff(required, names(d)), collapse = ', '))
  if (!all(vapply(d[setdiff(required, 'Date')], is.numeric, logical(1))))
    stop(label, ': employment columns must be numeric')
  # Retain the observed lag-one change as a historical predictor.
  # All other employment targets/levels are excluded from predictors.
  pred_cols <- names(d)[vapply(d, is.numeric, logical(1)) &
    !(names(d) %in% c(target_vars, 'COVID_Shock', 'COVID_Recovery')) &
    !grepl('^Employ_Rate', names(d), ignore.case = TRUE)]
  pred_cols <- unique(c('Employ_Rate_diff_lag1', pred_cols))
  # If your cleaned datasets contain other forward-looking variables,
  # remove them from pred_cols explicitly before fitting.
  shock <- as.integer(d$Date >= as.Date('2020-03-01') &
                      d$Date <= as.Date('2020-04-01'))
  recovery <- as.integer(d$Date >= as.Date('2020-05-01') &
                         d$Date <= as.Date('2022-02-01'))
  for (h in horizons) {
    valid <- seq_len(nrow(d) - h)
    z1 <- z2 <- rep(NA_real_, nrow(d))
    z1[valid] <- vapply(valid, function(t) mean(shock[(t+1L):(t+h)]), numeric(1))
    z2[valid] <- vapply(valid, function(t) mean(recovery[(t+1L):(t+h)]), numeric(1))
    d[[paste0('COVID_Shock_h', h)]] <- z1
    d[[paste0('COVID_Recovery_h', h)]] <- z2
  }
  list(data = d, predictors = pred_cols)
}
sets <- lapply(names(files), build_set)
names(sets) <- names(files)

# All sets must cover the same dates and Canadian outcomes.
reference <- sets$CAN$data
for (nm in names(sets)) {
  d <- sets[[nm]]$data
  if (!identical(d$Date, reference$Date))
    stop(nm, ': dates do not match CAN. Align datasets before comparison.')
  for (col in targets) {
    if (!isTRUE(all.equal(d[[col]], reference[[col]],
                          tolerance = 1e-8, check.attributes = FALSE)))
      stop(nm, ': Canadian target differs from CAN for ', col)
  }
}
can_target <- reference[, c('Date', targets), drop = FALSE]

# Preserve same fixed raw window length as your AR analysis, per horizon.
pre_idx <- match(pre_end, can_target$Date)
if (is.na(pre_idx) || is.na(match(post_end, can_target$Date)))
  stop('Pre/post evaluation dates not present in CAN data')
window_by_h <- setNames(pre_idx - n_pre + 1L - horizons, as.character(horizons))

rf_results <- list()
forecast_list <- list()
importance_list <- list()
for (set_name in names(sets)) {
  obj <- sets[[set_name]]
  for (h in horizons) {
    for (period in c('Pre_COVID', 'Post_COVID')) {
      for (spec in c('No_Dummy', 'With_Dummy')) {
        if (period == 'Pre_COVID' && spec == 'With_Dummy') next
        key <- paste(set_name, h, period, spec, sep='_')
        cat('Running RF:', key, '\n')
        result <- rf.rolling.window(
          data = obj$data, predictor_cols = obj$predictors,
          horizon = h, nprev = if (period == 'Pre_COVID') n_pre else n_post,
          test_end = if (period == 'Pre_COVID') pre_end else post_end,
          window_size = window_by_h[as.character(h)],
          use_dummy = spec == 'With_Dummy', n_lags = n_lags)
        rf_results[[key]] <- result
        f <- result$forecasts
        f$Horizon <- h
        f$Model <- 'RF'
        f$Predictor_Set <- set_name
        f$Period <- period
        f$Specification <- spec
        forecast_list[[key]] <- f
        # Importance is stored separately for every forecast origin.
        importance_list[[key]] <- result$save.importance
      }
    }
  }
}
rf_all_predictions <- bind_rows(forecast_list)
# Duplicate identical pre-COVID predictions for reporting consistency only.
rf_all_predictions <- bind_rows(
  rf_all_predictions,
  rf_all_predictions %>%
    filter(Period == 'Pre_COVID', Specification == 'No_Dummy') %>%
    mutate(Specification = 'With_Dummy'))

rf_rmse_results <- rf_all_predictions %>%
  group_by(Predictor_Set, Horizon, Period, Specification) %>%
  summarise(RMSE = sqrt(mean(Forecast_Error^2)), .groups='drop')
rf_rmse_results <- bind_rows(rf_rmse_results,
  rf_all_predictions %>%
    group_by(Predictor_Set, Horizon, Specification) %>%
    summarise(RMSE = sqrt(mean(Forecast_Error^2)), .groups='drop') %>%
    mutate(Period = 'All'))

rf_rmse_table <- rf_rmse_results %>%
  mutate(Category = case_when(
    Period == 'Pre_COVID' & Specification == 'No_Dummy' ~ 'Pre_COVID',
    Period == 'Post_COVID' & Specification == 'No_Dummy' ~ 'Post_No_Dummy',
    Period == 'Post_COVID' & Specification == 'With_Dummy' ~ 'Post_With_Dummy',
    Period == 'All' & Specification == 'No_Dummy' ~ 'All_No_Dummy',
    Period == 'All' & Specification == 'With_Dummy' ~ 'All_With_Dummy',
    TRUE ~ NA_character_)) %>%
  filter(!is.na(Category)) %>%
  select(Predictor_Set, Horizon, Category, RMSE) %>%
  pivot_wider(names_from=Category, values_from=RMSE) %>%
  arrange(Horizon, Predictor_Set)
print(rf_rmse_table, n=Inf, width=Inf)

# Compare actual vs no-dummy vs with-dummy, for any predictor set / horizon.
plot_RF <- function(h, predictor_set='CAN', period='all') {
  stopifnot(h %in% horizons, predictor_set %in% names(sets))
  period <- match.arg(period, c('all', 'pre', 'post'))
  df <- rf_all_predictions %>%
    filter(Horizon == h, Predictor_Set == predictor_set)
  if (period == 'pre') df <- filter(df, Period == 'Pre_COVID')
  if (period == 'post') df <- filter(df, Period == 'Post_COVID')
  wide <- df %>% select(Outcome_Date, Actual, Specification, Predicted) %>%
    pivot_wider(names_from=Specification, values_from=Predicted) %>%
    arrange(Outcome_Date)
  rmse_no <- sqrt(mean((wide$Actual-wide$No_Dummy)^2))
  rmse_yes <- sqrt(mean((wide$Actual-wide$With_Dummy)^2))
  ggplot(wide, aes(x=Outcome_Date)) +
    geom_line(aes(y=With_Dummy, color='With Dummy'), linewidth=.8) +
    geom_line(aes(y=No_Dummy, color='Without Dummy'), linewidth=.8) +
    geom_line(aes(y=Actual, color='Actual'), linewidth=.8) +
    scale_color_manual(values=c('Actual'='black',
                                'Without Dummy'='#2563EB',
                                'With Dummy'='#E69F00')) +
    labs(title=paste0('RF ', predictor_set, ' | h=', h),
         subtitle=paste0(period, ' | RMSE No Dummy=', round(rmse_no,4),
                         ' | With Dummy=', round(rmse_yes,4)),
         x='Outcome Date', y='Canadian employment-rate change (pp)', color=NULL) +
    theme_minimal()
}

dir.create('results', showWarnings=FALSE)
write.csv(rf_all_predictions, 'results/RF_predictions.csv', row.names=FALSE)
write.csv(rf_rmse_table, 'results/RF_RMSE_summary.csv', row.names=FALSE)
saveRDS(importance_list, 'results/RF_variable_importance.rds')
saveRDS(rf_results, 'results/RF_full_results.rds')

# Examples:
plot_RF(1, 'CAN', 'post')
# plot_RF(3, 'US', 'all')
# plot_RF(12, 'CAN_US', 'all')


###################################### just horizon 1
# Retrieve completed horizon-1 results
pre <- rf_results[["CAN_1_Pre_COVID_No_Dummy"]]
post_no <- rf_results[["CAN_1_Post_COVID_No_Dummy"]]
post_dummy <- rf_results[["CAN_1_Post_COVID_With_Dummy"]]

# Combine forecast data
no_dummy <- rbind(pre$forecasts, post_no$forecasts)
with_dummy <- rbind(pre$forecasts, post_dummy$forecasts)

# RMSE function
calc_rmse <- function(df) {
  sqrt(mean((df$Actual - df$Predicted)^2))
}

# Summary table
rf_h1_rmse <- data.frame(
  Predictor_Set = "CAN",
  Pre_COVID = calc_rmse(pre$forecasts),
  Post_No_Dummy = calc_rmse(post_no$forecasts),
  Post_With_Dummy = calc_rmse(post_dummy$forecasts),
  All_No_Dummy = calc_rmse(no_dummy),
  All_With_Dummy = calc_rmse(with_dummy)
)

print(rf_h1_rmse)

library(dplyr)
library(tidyr)
library(ggplot2)

plot_RF_h1 <- function(period = "all") {
  
  period <- match.arg(period, c("all", "pre", "post"))
  
  # Combine predictions
  df_no <- rbind(
    pre$forecasts,
    post_no$forecasts
  )
  
  df_dummy <- rbind(
    pre$forecasts,
    post_dummy$forecasts
  )
  
  # Merge both specifications by outcome date
  plot_df <- inner_join(
    df_no %>%
      select(Outcome_Date, Actual, No_Dummy = Predicted),
    df_dummy %>%
      select(Outcome_Date, With_Dummy = Predicted),
    by = "Outcome_Date"
  )
  
  # Filter period
  if (period == "pre") {
    plot_df <- filter(
      plot_df,
      Outcome_Date <= as.Date("2019-12-01")
    )
  } else if (period == "post") {
    plot_df <- filter(
      plot_df,
      Outcome_Date >= as.Date("2020-01-01")
    )
  }
  
  # RMSE
  rmse_no <- sqrt(mean(
    (plot_df$Actual - plot_df$No_Dummy)^2
  ))
  
  rmse_dummy <- sqrt(mean(
    (plot_df$Actual - plot_df$With_Dummy)^2
  ))
  
  # Plot
  ggplot(plot_df, aes(x = Outcome_Date)) +
    
    geom_line(
      aes(y = With_Dummy, color = "With Dummy"),
      linewidth = 0.8
    ) +
    
    geom_line(
      aes(y = No_Dummy, color = "Without Dummy"),
      linewidth = 0.8
    ) +
    
    geom_line(
      aes(y = Actual, color = "Actual"),
      linewidth = 0.8
    )
    
    scale_color_manual(
      values = c(
        "Actual" = "black",
        "Without Dummy" = "#2563EB",
        "With Dummy" = "#E69F00"
      )
    ) +
    
    labs(
      title = "1-Month Forecast: Random Forest (CAN)",
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
}

plot_RF_h1("pre")
plot_RF_h1("post")
plot_RF_h1("all")