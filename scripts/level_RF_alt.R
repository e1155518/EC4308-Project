# Run from your EC4308 project root (where the data/ folder exists).
# install.packages(c('readxl','dplyr','tidyr','ggplot2','randomForest'))
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(randomForest)
library(parallel)
n_workers <- 4L
# Forecasting functions are defined below.

runrf <- function(Y, horizon = 1L, n_lags = 4L, use_dummy = FALSE,
                  seed = 4308L, mtry = NULL) {
  # Y columns: target, predictors, and optionally Shock/Recovery target controls.
  if (nrow(Y) <= horizon + n_lags + 5L) stop('Too few rows for RF estimation.')
  dcols <- if (use_dummy) c('COVID_Shock_Control', 'COVID_Recovery_Control') else character()
  pcols <- setdiff(colnames(Y), c('Target', dcols))
  if (!length(pcols)) stop('No RF predictors supplied.')
  n <- nrow(Y)
  # Each training row indexed s uses predictors known at s, and target Employ_Rate[s+h].
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
  model <- randomForest::randomForest(x = X, y = y, mtry = if (is.null(mtry)) max(1L, floor(ncol(X)/3)) else mtry, importance = TRUE)
  pred <- as.numeric(predict(model, newdata = X.out))
  list(model = model, pred = pred, importance = randomForest::importance(model))
}


#### the first rolling window is executed sequentially, the second uses parallel processing and shld be faster since it uses 4 workers

# rf.rolling.window <- function(data, predictor_cols, horizon = 1L,
#                               nprev = 100L, test_end = as.Date('2019-12-01'),
#                               window_size = NULL, use_dummy = FALSE,
#                               n_lags = 4L, seed = 4308L, verbose = TRUE) {
#   target_col <- 'Employ_Rate'
#   if (!target_col %in% names(data)) stop('Missing target: ', target_col)
#   idx <- match(as.Date(test_end), data$Date)
#   if (is.na(idx)) stop('Evaluation end date not found: ', test_end)
#   outcome_idx <- seq.int(idx - nprev + 1L, idx)
#   origin_idx <- outcome_idx - horizon
#   if (min(origin_idx) < 1L) stop('Forecast origins before first data row.')
#   if (is.null(window_size)) window_size <- min(origin_idx)
#   if (min(origin_idx) - window_size + 1L < 1L ||
#       window_size <= horizon + n_lags + 5L) stop('Invalid rolling window size.')
#   controls <- if (use_dummy) paste0(c('COVID_Shock', 'COVID_Recovery'), '_h', horizon) else character()
#   needed <- c(target_col, setdiff(predictor_cols, target_col), controls)
#   if (!all(needed %in% names(data))) stop('Missing columns: ', paste(setdiff(needed, names(data)), collapse=', '))
#   pred <- numeric(nprev)
#   imp <- vector('list', nprev)
#   for (i in nprev:1) {
#     j <- 1L + nprev - i
#     origin <- origin_idx[j]
#     w <- data[(origin - window_size + 1L):origin, needed, drop = FALSE]
#     names(w) <- c('Target', predictor_cols,
#                   if (use_dummy) c('COVID_Shock_Control', 'COVID_Recovery_Control'))
#     fit <- runrf(w, horizon, n_lags, use_dummy, seed = seed + j)
#     pred[j] <- fit$pred
#     imp[[j]] <- fit$importance
#     if (verbose && (j == 1L || j %% 20L == 0L || j == nprev))
#       cat('RF horizon', horizon, 'forecast', j, '/', nprev, '\n')
#   }
#   actual <- data[[target_col]][outcome_idx]
#   if (anyNA(actual)) stop('Missing actual outcomes in evaluation period.')
#   errors <- actual - pred
#   list(pred = pred, errors = c(rmse = sqrt(mean(errors^2)), mae = mean(abs(errors))),
#        save.importance = imp,
#        forecasts = data.frame(Forecast_Origin = data$Date[origin_idx],
#          Outcome_Date = data$Date[outcome_idx], Actual = actual,
#          Predicted = pred, Forecast_Error = errors))
# }

rf.rolling.window <- function(data, predictor_cols, horizon = 1L,
                              nprev = 100L, test_end = as.Date("2019-12-01"),
                              window_size = NULL, use_dummy = FALSE,
                              n_lags = 4L, seed = 4308L, verbose = TRUE,
                              n_workers = 4L, mtry = NULL) {
  
  target_col <- "Employ_Rate"
  
  if (!target_col %in% names(data))
    stop("Missing target: ", target_col)
  
  idx <- match(as.Date(test_end), data$Date)
  
  if (is.na(idx))
    stop("Evaluation end date not found: ", test_end)
  
  outcome_idx <- seq.int(idx - nprev + 1L, idx)
  origin_idx <- outcome_idx - horizon
  
  if (min(origin_idx) < 1L)
    stop("Forecast origins before first data row.")
  
  if (is.null(window_size))
    window_size <- min(origin_idx)
  
  if (min(origin_idx) - window_size + 1L < 1L ||
      window_size <= horizon + n_lags + 5L)
    stop("Invalid rolling window size.")
  
  controls <- if (use_dummy) {
    paste0(c("COVID_Shock", "COVID_Recovery"), "_h", horizon)
  } else {
    character()
  }
  
  needed <- c(target_col, setdiff(predictor_cols, target_col), controls)
  
  if (!all(needed %in% names(data)))
    stop("Missing columns: ",
         paste(setdiff(needed, names(data)), collapse = ", "))
  
  # The training target at row s is the level observed at s+h.
  # This shift is constructed only inside the rolling window; training stops at origin-h.
  # The last h rows are NA targets, but their predictors remain available.
  data$RF_forward_level <- dplyr::lead(data[[target_col]], horizon)
  needed <- c('RF_forward_level', predictor_cols, controls)

  # Start parallel workers
  cl <- parallel::makeCluster(n_workers)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  
  parallel::clusterEvalQ(cl, library(randomForest))
  
  # Export function and required objects
  parallel::clusterExport(
    cl,
    varlist = c(
      "runrf", "data", "predictor_cols", "horizon",
      "n_lags", "use_dummy", "seed", "origin_idx",
      "window_size", "needed", "mtry"
    ),
    envir = environment()
  )
  
  # Run rolling forecasts in parallel
  results <- parallel::parLapplyLB(
    cl,
    seq_len(nprev),
    function(j) {
      
      origin <- origin_idx[j]
      
      w <- data[
        (origin - window_size + 1L):origin,
        needed,
        drop = FALSE
      ]
      
      names(w) <- c(
        "Target",
        predictor_cols,
        if (use_dummy)
          c("COVID_Shock_Control", "COVID_Recovery_Control")
      )
      
      fit <- runrf(
        w, horizon, n_lags, use_dummy,
        seed = seed + j, mtry = mtry
      )
      
      list(
        pred = fit$pred,
        importance = fit$importance
      )
    }
  )
  
  # Collect results in chronological order
  pred <- vapply(results, function(x) x$pred, numeric(1))
  imp <- lapply(results, function(x) x$importance)
  
  actual <- data[[target_col]][outcome_idx]
  
  if (anyNA(actual))
    stop("Missing actual outcomes in evaluation period.")
  
  errors <- actual - pred
  
  if (verbose)
    cat("Completed RF horizon", horizon,
        "|", nprev, "forecasts using",
        n_workers, "workers\n")
  
  return(list(
    pred = pred,
    errors = c(
      rmse = sqrt(mean(errors^2)),
      mae = mean(abs(errors))
    ),
    save.importance = imp,
    forecasts = data.frame(
      Forecast_Origin = data$Date[origin_idx],
      Outcome_Date = data$Date[outcome_idx],
      Actual = actual,
      Predicted = pred,
      Forecast_Error = errors
    )
  ))
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

# ============================================================
# Create historical COVID outlier controls
# ============================================================

for (set_name in names(raw)) {
  
  df <- raw[[set_name]]
  
  for (h in c(1, 3, 6, 12)) {
    
    # Historical target: Employ_Rate(t+h)
    # COVID controls refer to historical outcome month t+h.
    
    origin_dates <- as.Date(df$Date)
    
    shock <- numeric(nrow(df))
    recovery <- numeric(nrow(df))
    
    for (k in seq_len(h)) {
      
      outcome_month <- seq(
        min(origin_dates),
        by = "month",
        length.out = nrow(df) + h
      )[seq_len(nrow(df)) + k]
      
      shock <- shock + as.numeric(
        outcome_month >= as.Date("2020-03-01") &
          outcome_month <= as.Date("2020-04-01")
      )
      
      recovery <- recovery + as.numeric(
        outcome_month >= as.Date("2020-05-01") &
          outcome_month <= as.Date("2022-02-01")
      )
    }
    
    # Level targets use the outcome-month dummy, not an average over h months.
    outcome_dates <- seq(min(origin_dates), by = 'month', length.out = nrow(df) + h)[seq_len(nrow(df)) + h]
    df[[paste0('COVID_Shock_h', h)]] <- as.integer(outcome_dates >= as.Date('2020-03-01') & outcome_dates <= as.Date('2020-04-01'))
    df[[paste0('COVID_Recovery_h', h)]] <- as.integer(outcome_dates >= as.Date('2020-05-01') & outcome_dates <= as.Date('2022-02-01'))
  }
  
  raw[[set_name]] <- df
}

names(raw) <- names(files)

# All three datasets already contain the Canadian employment targets.
# Exclude forward targets; include contemporaneously observed employment rate level. 
# The historical one-month change is allowed as an autoregressive predictor, consistent with the AR benchmark.
target_vars <- c('Employ_Rate', 'Employ_Rate_diff_lag1',
                 'Employ_Rate_diff_h1', 'Employ_Rate_diff_h3',
                 'Employ_Rate_diff_h6', 'Employ_Rate_diff_h12')
targets <- 'Employ_Rate'

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
  # Retain observed employment-rate level and lag-one change as historical predictors.
  # All other employment targets/levels are excluded from predictors.
  pred_cols <- names(d)[vapply(d, is.numeric, logical(1)) &
    !(names(d) %in% c(target_vars, 'COVID_Shock', 'COVID_Recovery')) &
    !grepl('^Employ_Rate', names(d), ignore.case = TRUE)]
  pred_cols <- unique(c('Employ_Rate', 'Employ_Rate_diff_lag1', pred_cols))
  # If your cleaned datasets contain other forward-looking variables,
  # remove them from pred_cols explicitly before fitting.
  shock <- as.integer(d$Date >= as.Date('2020-03-01') &
                      d$Date <= as.Date('2020-04-01'))
  recovery <- as.integer(d$Date >= as.Date('2020-05-01') &
                         d$Date <= as.Date('2022-02-01'))
  for (h in horizons) {
    valid <- seq_len(nrow(d) - h)
    z1 <- z2 <- rep(NA_real_, nrow(d))
    z1[valid] <- vapply(valid, function(t) shock[t+h], numeric(1))
    z2[valid] <- vapply(valid, function(t) recovery[t+h], numeric(1))
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
common_window <- pre_idx - n_pre + 1L - max(horizons)
window_by_h <- setNames(rep(common_window, length(horizons)), as.character(horizons))

rf_results <- list()
forecast_list <- list()
importance_list <- list()

# ============================================================
# Run Random Forest in batches with automatic saving
# ============================================================

# Choose which models to run
predictor_sets <- c("CAN", "US", "CAN_US")
horizons_to_run <- c(1, 3, 6, 12)

# Folder for saved results
save_dir <- "results/random_forest_alt/level_RF_alt_batches"
dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)

# Initialize results without deleting existing results
if (!exists("rf_results")) {
  rf_results <- list()
}

# Helper: run one model or load its saved result
run_or_load_rf <- function(set_name, h, period, use_dummy,
                           data, predictor_cols, nprev,
                           test_end, window_size = NULL, mtry) {
  
  spec <- if (use_dummy) "With_Dummy" else "No_Dummy"
  
  key <- paste(set_name, h, period, spec, sep = "_")
  file <- file.path(save_dir, paste0(key, ".rds"))
  
  if (file.exists(file)) {
    cat("Loading saved:", key, "\n")
    return(readRDS(file))
  }
  
  cat("Running:", key, "\n")
  
  result <- rf.rolling.window(
    data = data,
    predictor_cols = predictor_cols,
    nprev = nprev,
    horizon = h,
    use_dummy = use_dummy,
    test_end = as.Date(test_end),
    window_size = window_size,
    mtry = mtry
  )
  
  saveRDS(result, file)
  cat("Saved:", key, "\n")
  
  return(result)
}

# Tune once on the FIRST pre-COVID training sample for each predictor set/horizon.
# Training targets at s are Employ_Rate[s+h], so the last h rows cannot train.
# Use exactly the same lagged feature construction as runrf(), without dummies.
tune_first_rf <- function(data, predictor_cols, h, window_size, seed = 4308L) {
  first_outcome <- match(pre_end, data$Date) - n_pre + 1L
  origin <- first_outcome - h
  w <- data[(origin - window_size + 1L):origin, , drop = FALSE]
  pcols <- unique(predictor_cols)
  s <- seq.int(n_lags, nrow(w) - h)
  X <- do.call(cbind, lapply(seq_len(n_lags) - 1L, function(k) {
    v <- as.matrix(w[s - k, pcols, drop = FALSE])
    colnames(v) <- paste0(pcols, '_lag', k)
    v
  }))
  X <- as.data.frame(X, check.names = FALSE)
  y <- as.numeric(w$Employ_Rate[s + h])
  if (anyNA(X) || anyNA(y) || any(!is.finite(as.matrix(X))) || any(!is.finite(y)))
    stop('Nonfinite data in initial RF tuning sample.')
  P <- ncol(X)
  set.seed(seed)
  trials <- randomForest::tuneRF(
    x = X, y = y, mtryStart = max(1L, floor(sqrt(P))),
    stepFactor = 1.5, improve = 0.04, ntreeTry = 500,
    doBest = FALSE, plot = FALSE, trace = FALSE
  )
  best <- which.min(trials[, 'OOBError'])
  list(mtry = as.integer(trials[best, 'mtry']),
       trials = as.data.frame(trials), n_predictors = P,
       first_origin = data$Date[origin])
}

# Save tuning results independently of batch forecasts, so interrupted runs resume.
tuning_dir <- 'results/random_forest_alt'
dir.create(tuning_dir, recursive = TRUE, showWarnings = FALSE)
tuning_file <- file.path(tuning_dir, 'level_RF_alt_tuning.rds')
tuning_results <- if (file.exists(tuning_file)) readRDS(tuning_file) else list()
tuning_summary <- list()
for (set_name in predictor_sets) {
  data <- raw[[set_name]]
  predictor_cols <- sets[[set_name]]$predictors
  for (h in horizons_to_run) {
    key <- paste(set_name, h, sep = '_')
    if (is.null(tuning_results[[key]])) {
      cat('Tuning mtry:', key, '\n')
      tuning_results[[key]] <- tune_first_rf(
        data, predictor_cols, h, window_by_h[as.character(h)])
      saveRDS(tuning_results, tuning_file)
    }
    z <- tuning_results[[key]]
    tuning_summary[[key]] <- data.frame(
      Predictor_Set = set_name, Horizon = h, mtry = z$mtry,
      Number_of_Predictors = z$n_predictors,
      First_Forecast_Origin = z$first_origin,
      Minimum_OOB_MSE = min(z$trials$OOBError))
  }
}
write.csv(dplyr::bind_rows(tuning_summary),
          file.path(tuning_dir, 'level_RF_alt_mtry_summary.csv'), row.names = FALSE)
write.csv(dplyr::bind_rows(lapply(names(tuning_results), function(key) {
  z <- tuning_results[[key]]
  data.frame(Tuning_Key = key, z$trials)
})), file.path(tuning_dir, 'level_RF_alt_tuning_trials.csv'), row.names = FALSE)

for (set_name in predictor_sets) {
  
  data <- raw[[set_name]]
  
  predictor_cols <- sets[[set_name]]$predictors

  for (h in horizons_to_run) {
    
    cat("\n====================================\n")
    cat("Predictor set:", set_name, "| Horizon:", h, "\n")
    cat("====================================\n")
    
    selected_mtry <- tuning_results[[paste(set_name, h, sep = "_")]]$mtry

    # 1. Pre-COVID without dummy
    pre <- run_or_load_rf(
      set_name = set_name,
      h = h,
      period = "Pre_COVID",
      use_dummy = FALSE,
      data = data,
      nprev = 100,
      predictor_cols = predictor_cols,
      test_end = "2019-12-01",
      window_size = window_by_h[as.character(h)],
      mtry = selected_mtry
    )
    
    rf_results[[paste(set_name, h, "Pre_COVID_No_Dummy", sep = "_")]] <- pre
    
    # Match post-COVID window size to pre-COVID
    window_size <- window_by_h[as.character(h)]
    
    # 2. Post-COVID without dummy
    post_no <- run_or_load_rf(
      set_name = set_name,
      h = h,
      period = "Post_COVID",
      use_dummy = FALSE,
      data = data,
      predictor_cols = predictor_cols,
      nprev = 69,
      test_end = "2025-09-01",
      window_size = window_size,
      mtry = selected_mtry
    )
    
    rf_results[[paste(set_name, h, "Post_COVID_No_Dummy", sep = "_")]] <- post_no
    
    # 3. Post-COVID with historical outlier dummies
    post_dummy <- run_or_load_rf(
      set_name = set_name,
      h = h,
      period = "Post_COVID",
      use_dummy = TRUE,
      data = data,
      predictor_cols = predictor_cols,
      nprev = 69,
      test_end = "2025-09-01",
      window_size = window_size,
      mtry = selected_mtry
    )
    
    rf_results[[paste(set_name, h, "Post_COVID_With_Dummy", sep = "_")]] <- post_dummy
    
    cat("Completed:", set_name, "horizon", h, "\n")
  }
}

cat("\nAll requested RF batches completed!\n")

# ============================================================
# 1. Reconstruct forecasts from completed RF results
# ============================================================

forecast_list <- list()

for (set_name in c("CAN", "US", "CAN_US")) {
  for (h in c(1, 3, 6, 12)) {
    
    for (period in c("Pre_COVID", "Post_COVID")) {
      
      specs <- if (period == "Pre_COVID") {
        "No_Dummy"
      } else {
        c("No_Dummy", "With_Dummy")
      }
      
      for (spec in specs) {
        
        key <- paste(set_name, h, period, spec, sep = "_")
        
        if (!key %in% names(rf_results)) next
        
        df <- rf_results[[key]]$forecasts
        
        df$Predictor_Set <- set_name
        df$Horizon <- h
        df$Period <- period
        df$Specification <- spec
        
        forecast_list[[key]] <- df
      }
    }
  }
}

rf_all_predictions <- bind_rows(forecast_list)
rf_all_predictions$Model <- "RF_alt"

# ============================================================
# 2. Duplicate pre-COVID forecasts for reporting only
# ============================================================

rf_all_predictions <- bind_rows(
  rf_all_predictions,
  rf_all_predictions %>%
    filter(
      Period == "Pre_COVID",
      Specification == "No_Dummy"
    ) %>%
    mutate(Specification = "With_Dummy")
)

# ============================================================
# 3. Calculate RMSE for each period and specification
# ============================================================

rf_rmse_results <- rf_all_predictions %>%
  group_by(Predictor_Set, Horizon, Period, Specification) %>%
  summarise(
    RMSE = sqrt(mean(Forecast_Error^2)),
    .groups = "drop"
  )

# ============================================================
# 4. Calculate full-period RMSE
# ============================================================

rf_rmse_results <- bind_rows(
  rf_rmse_results,
  
  rf_all_predictions %>%
    group_by(Predictor_Set, Horizon, Specification) %>%
    summarise(
      RMSE = sqrt(mean(Forecast_Error^2)),
      .groups = "drop"
    ) %>%
    mutate(Period = "All")
)

# ============================================================
# 5. Create final RMSE comparison table
# ============================================================

rf_rmse_table <- rf_rmse_results %>%
  mutate(
    Category = case_when(
      Period == "Pre_COVID" &
        Specification == "No_Dummy" ~ "Pre_COVID",
      
      Period == "Post_COVID" &
        Specification == "No_Dummy" ~ "Post_No_Dummy",
      
      Period == "Post_COVID" &
        Specification == "With_Dummy" ~ "Post_With_Dummy",
      
      Period == "All" &
        Specification == "No_Dummy" ~ "All_No_Dummy",
      
      Period == "All" &
        Specification == "With_Dummy" ~ "All_With_Dummy",
      
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Category)) %>%
  select(Predictor_Set, Horizon, Category, RMSE) %>%
  pivot_wider(
    names_from = Category,
    values_from = RMSE
  ) %>%
  arrange(Horizon, Predictor_Set)

print(rf_rmse_table, n = Inf, width = Inf)


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
         x='Outcome Date', y='Canadian employment rate (%)', color=NULL) +
    theme_minimal()
}


# ============================================================
# Export Random Forest results
# ============================================================

dir.create("results/random_forest_alt", recursive = TRUE, showWarnings = FALSE)

# 1. Export predictions for DM tests and ensemble methods
write.csv(rf_all_predictions,"results/random_forest_alt/level_RF_alt_predictions.csv", row.names = FALSE)
write.csv(rf_rmse_table,"results/random_forest_alt/level_RF_alt_RMSE_summary.csv",row.names = FALSE)

# 3. Extract variable importance from completed RF models
importance_list <- lapply(rf_results,  function(x) x$save.importance)

# 4. Save variable importance separately
saveRDS(importance_list,"results/random_forest_alt/level_RF_alt_variable_importance.rds")

# 5. Save all RF results (predictions + importance)
saveRDS(  rf_results,  "results/random_forest_alt/level_RF_alt_full_results.rds")
cat("All RF results exported successfully!\n")

# Examples:
plot_RF(12, 'CAN', 'all')
plot_RF(12, 'US', 'all')
plot_RF(12, 'CAN_US', 'all')




# Save all actual-versus-predicted figures without overwriting change-model plots.
plot_dir <- 'results/random_forest_alt/level_RF_alt_plots'
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
for (set_name in predictor_sets) {
  for (h in horizons_to_run) {
    for (period in c('all', 'pre', 'post')) {
      fig <- plot_RF(h, set_name, period)
      ggplot2::ggsave(
        filename = file.path(plot_dir, paste0('level_RF_alt_', set_name, '_h', h, '_', period, '.png')),
        plot = fig, width = 11, height = 6, dpi = 300
      )
    }
  }
}


# ================================================================
# APPEND TO EITHER level_RF_default_v1.R OR level_RF_alt.R
# Requires: readxl, dplyr, tidyr, ggplot2 (loaded above).
# Put master_metadata.xlsx in data/ (or adjust metadata_file).
# Uses SAVED forest importance; DOES NOT refit forests.
# ================================================================

# ============================================================
# Variable Importance: Tuned Random Forest (ALT)
# ============================================================

metadata_file <- "data/master_metadata.xlsx"

importance_rds <- "results/random_forest_alt/level_RF_alt_full_results.rds"

importance_out <- "results/random_forest_alt/variable_importance"

# Check input files
stopifnot(
  file.exists(metadata_file),
  file.exists(importance_rds)
)

# Create output directory
dir.create(
  importance_out,
  recursive = TRUE,
  showWarnings = FALSE
)

# Load tuned RF results
rf_saved <- readRDS(importance_rds)

cat("Analyzing:", importance_rds, "\n")
cat("Saving results to:", importance_out, "\n")
cat("Number of RF batches:", length(rf_saved), "\n")

md <- as.data.frame(readxl::read_excel(metadata_file, sheet = 'master'))
stopifnot(all(c('country', 'variable', 'group name') %in% names(md)))
md <- md %>%
  transmute(Country = toupper(trimws(as.character(country))),
            Variable = trimws(as.character(variable)),
            Group = trimws(as.character(`group name`))) %>%
  filter(!is.na(Variable), nzchar(Variable), !is.na(Group), nzchar(Group)) %>%
  distinct(Country, Variable, .keep_all = TRUE)
if (anyDuplicated(paste(md$Country, md$Variable))) stop('Metadata country/variable keys are duplicated.')

# Each entry in save.importance corresponds to one chronological rolling window.
extract_importance <- function(key, result) {
  parts <- strsplit(key, '_', fixed = TRUE)[[1]]
  # Keys look like CAN_US_12_Post_COVID_With_Dummy.
  set_name <- if (startsWith(key, 'CAN_US_')) 'CAN_US' else if (startsWith(key, 'CAN_')) 'CAN' else 'US'
  remainder <- substring(key, nchar(set_name) + 2L)
  h <- as.integer(sub('_.*$', '', remainder))
  period <- if (grepl('_Pre_COVID_', key, fixed = TRUE)) 'Pre_COVID' else 'Post_COVID'
  spec <- if (endsWith(key, '_With_Dummy')) 'With_Dummy' else 'No_Dummy'
  imp <- result$save.importance
  if (is.null(imp) || !length(imp)) stop('Missing importance for ', key)
  dates <- result$forecasts$Outcome_Date
  if (length(imp) != length(dates)) stop('Importance/date length mismatch: ', key)
  dplyr::bind_rows(lapply(seq_along(imp), function(i) {
    a <- imp[[i]]
    if (is.null(dim(a)) || is.null(rownames(a)) || !'IncNodePurity' %in% colnames(a))
      stop('IncNodePurity missing in ', key, ' window ', i)
    data.frame(Predictor_Set = set_name, Horizon = h, Period = period,
               Specification = spec, Window = i, Outcome_Date = as.Date(dates[i]),
               Feature = rownames(a), Importance = as.numeric(a[, 'IncNodePurity']))
  }))
}
raw_imp <- dplyr::bind_rows(Map(extract_importance, names(rf_saved), rf_saved))
if (!nrow(raw_imp)) stop('No importance rows found.')

# Strip lag suffix, then classify country. US_ only occurs in CAN_US.
# COVID controls have their own group; employment rate and its change are separate.
feature_map <- raw_imp %>% distinct(Predictor_Set, Feature) %>%
  mutate(Variable_With_Prefix = sub('_lag[0-9]+$', '', Feature),
         Is_US_Prefixed = Predictor_Set == 'CAN_US' & startsWith(Variable_With_Prefix, 'US_'),
         Country = if_else(Predictor_Set == 'US' | Is_US_Prefixed, 'USA', 'CAN'),
         Variable = if_else(Is_US_Prefixed, sub('^US_', '', Variable_With_Prefix), Variable_With_Prefix),
         Special_Group = case_when(
           Variable %in% c('Employ_Rate', 'Employ_Rate_diff_lag1') ~ 'Own Employment Rate',
           Variable %in% c('COVID_Shock_Control', 'COVID_Recovery_Control') ~ 'COVID Controls',
           TRUE ~ NA_character_)) %>%
  mutate(Variable = case_when(
    Variable == 'IPB51222S' ~ 'IPB51222s',
    Variable == 'S.P.500' ~ 'S&P 500',
    Variable == 'S.P.div.yield' ~ 'S&P div yield',
    Variable == 'S.P.PE.ratio' ~ 'S&P PE ratio',
    TRUE ~ Variable)) %>%
  left_join(md, by = c('Country', 'Variable')) %>%
  mutate(Group = coalesce(Special_Group, Group),
         Group = if_else(is.na(Group), 'UNMAPPED', Group),
         Country_Group = if_else(Special_Group == 'Own Employment Rate', 'CAN', Country, missing = Country))

unmapped <- feature_map %>% filter(Group == 'UNMAPPED', !grepl('^COVID_(Shock|Recovery)', Variable))
write.csv(unmapped, file.path(importance_out, 'unmapped_features.csv'), row.names = FALSE)
if (nrow(unmapped)) {
  warning(nrow(unmapped), ' unmatched feature names. See unmapped_features.csv; unmapped importance remains in totals.')
}
write.csv(feature_map, file.path(importance_out, 'feature_category_mapping.csv'), row.names = FALSE)

imp <- raw_imp %>%
  left_join(feature_map, by = c('Predictor_Set', 'Feature')) %>%
  filter(!grepl('^COVID_(Shock|Recovery)', Variable))
window_keys <- c('Predictor_Set', 'Horizon', 'Period', 'Specification', 'Window', 'Outcome_Date')
# First normalize within each forest, THEN average window percentages.
imp <- imp %>%
  group_by(across(all_of(window_keys))) %>%
  mutate(Relative_Importance = if (sum(Importance) > 0) {
    100 * Importance / sum(Importance)
  } else {
    rep(NA_real_, n())
  }) %>%
  ungroup()
if (anyNA(imp$Relative_Importance)) stop('One or more forests have zero total IncNodePurity.')

# Sum all lags and predictors within a group for each forest.
group_window <- imp %>% group_by(across(all_of(window_keys)), Country_Group, Group) %>%
  summarise(Relative_Importance = sum(Relative_Importance), .groups = 'drop')
# Missing groups in a window are zero; include them in period means.
all_windows <- imp %>% distinct(across(all_of(window_keys)))
all_groups <- group_window %>% distinct(Predictor_Set, Horizon, Period, Specification, Country_Group, Group)
group_window <- all_windows %>%
  left_join(
    all_groups,
    by = c("Predictor_Set", "Horizon", "Period", "Specification"),
    relationship = "many-to-many"
  ) %>%
  left_join(
    group_window,
    by = c(window_keys, "Country_Group", "Group"),
    relationship = "many-to-one"
  ) %>%
  mutate(
    Relative_Importance = replace_na(Relative_Importance, 0)
  )

group_summary <- group_window %>%
  group_by(Predictor_Set, Horizon, Period, Specification, Country_Group, Group) %>%
  summarise(Mean_Percent = mean(Relative_Importance), .groups = 'drop')

# All-period summary is weighted by the number of rolling windows.
group_all <- group_window %>% group_by(Predictor_Set, Horizon, Specification, Country_Group, Group) %>%
  summarise(Period = 'All', Mean_Percent = mean(Relative_Importance), .groups = 'drop')
group_summary <- bind_rows(group_summary, group_all)
write.csv(group_summary, file.path(importance_out, 'group_relative_importance.csv'), row.names = FALSE)
write.csv(group_window, file.path(importance_out, 'group_importance_by_window.csv'), row.names = FALSE)

# Top predictors: aggregate all four lags, with separate CAN/USA identities.
indicator_window <- imp %>% group_by(across(all_of(window_keys)), Country_Group, Variable) %>%
  summarise(Relative_Importance = sum(Relative_Importance), .groups = 'drop')
# Correct for indicators absent from some forests (e.g., inactive COVID controls).
window_counts <- all_windows %>% count(Predictor_Set, Horizon, Period, Specification, name = 'N_Windows')
indicator_summary <- indicator_window %>% group_by(Predictor_Set, Horizon, Period, Specification, Country_Group, Variable) %>%
  summarise(Total_Percent = sum(Relative_Importance), .groups = 'drop') %>%
  left_join(window_counts, by = c('Predictor_Set','Horizon','Period','Specification')) %>%
  mutate(Mean_Percent = Total_Percent / N_Windows) %>% select(-Total_Percent, -N_Windows)
indicator_all <- indicator_window %>% group_by(Predictor_Set, Horizon, Specification, Country_Group, Variable) %>%
  summarise(Total_Percent = sum(Relative_Importance), .groups = 'drop') %>%
  left_join(all_windows %>% count(Predictor_Set, Horizon, Specification, name = 'N_Windows'),
            by = c('Predictor_Set', 'Horizon', 'Specification')) %>%
  mutate(Period = 'All', Mean_Percent = Total_Percent / N_Windows) %>%
  select(-Total_Percent, -N_Windows)
indicator_summary <- bind_rows(indicator_summary, indicator_all)
write.csv(indicator_summary, file.path(importance_out, 'indicator_relative_importance.csv'), row.names = FALSE)

# Canadian vs US shares; keep Own Employment Rate as Canadian.
country_window <- imp %>% group_by(across(all_of(window_keys)), Country_Group) %>%
  summarise(Relative_Importance = sum(Relative_Importance), .groups = 'drop')
country_summary <- country_window %>% group_by(Predictor_Set, Horizon, Period, Specification, Country_Group) %>%
  summarise(Mean_Percent = mean(Relative_Importance), .groups = 'drop')
country_summary <- bind_rows(country_summary, country_window %>%
                               group_by(Predictor_Set, Horizon, Specification, Country_Group) %>%
                               summarise(Period = 'All', Mean_Percent = mean(Relative_Importance), .groups = 'drop'))
write.csv(country_summary, file.path(importance_out, 'country_importance_shares.csv'), row.names = FALSE)

plot_periods <- c('Pre_COVID', 'Post_COVID')

for (set_name in unique(group_summary$Predictor_Set)) {
  for (period in c("Pre_COVID", "Post_COVID")) {
    for (spec in c("No_Dummy", "With_Dummy")) {
      
      # Pre-COVID dummy specification is identical to no-dummy
      if (period == "Pre_COVID" && spec == "With_Dummy") next
      
      plot_data <- group_summary %>%
        filter(
          Predictor_Set == set_name,
          Period == period,
          Specification == spec
        ) %>%
        mutate(
          Horizon_Label = factor(
            paste0(Horizon, " Month"),
            levels = paste0(c(1, 3, 6, 12), " Month")
          ),
          Row = if (set_name == "CAN_US") {
            paste(Country_Group, Group, sep = " | ")
          } else {
            Group
          }
        )
      
      if (nrow(plot_data) == 0) next
      
      fig <- ggplot(
        plot_data,
        aes(x = Horizon_Label, y = Row, fill = Mean_Percent)
      ) +
        geom_tile(color = "white", linewidth = 0.6) +
        geom_text(
          aes(label = sprintf("%.1f%%", Mean_Percent)),
          size = 3
        ) +
        scale_fill_gradient(
          low = "#EFF6FF",
          high = "#1E3A8A"
        ) +
        labs(
          title = paste(
            "RF Relative Importance by Group:", set_name
          ),
          subtitle = paste(
            period, "|", spec,
            "| Mean share of IncNodePurity across rolling forests"
          ),
          x = "Forecast Horizon",
          y = NULL,
          fill = "Importance (%)"
        ) +
        theme_minimal(base_size = 11) +
        theme(
          panel.grid = element_blank(),
          axis.text.y = element_text(size = 10),
          axis.text.x = element_text(face = "bold"),
          plot.title = element_text(face = "bold")
        )
      
      ggsave(
        filename = file.path(
          importance_out,
          paste0(
            "heatmap_", set_name, "_",
            period, "_", spec, ".png"
          )
        ),
        plot = fig,
        width = 11,
        height = max(
          5,
          0.38 * n_distinct(plot_data$Row) + 2
        ),
        dpi = 300
      )
      
    }
  }
}

# Top-20 plots per predictor set, horizon and evaluation period.
for (set_name in c('CAN', 'US', 'CAN_US')) for (h in c(1,3,6,12)) for (period in plot_periods) {
  spec <- if (period == 'Pre_COVID') 'No_Dummy' else 'With_Dummy'
  dat <- indicator_summary %>% filter(Predictor_Set == set_name, Horizon == h, Period == period,
                                      Specification == spec) %>%
    mutate(Label = paste(Country_Group, Variable, sep = ' | ')) %>%
    slice_max(Mean_Percent, n = 20, with_ties = FALSE) %>%
    arrange(Mean_Percent) %>% mutate(Label = factor(Label, levels = Label))
  fig <- ggplot(dat, aes(Mean_Percent, Label)) + geom_col(fill = '#2563EB') +
    labs(title = paste('Top 20 RF indicators:', set_name, '| h=', h),
         subtitle = paste(period, '|', spec), x = 'Mean relative importance (%)', y = NULL) +
    theme_minimal(base_size = 11)
  ggsave(file.path(importance_out, paste0('top20_', set_name, '_h', h, '_', period, '.png')),
         fig, width = 10, height = 7, dpi = 300)
}

# Canadian vs US relative-importance shares, for CAN+US only.
country_plot <- country_summary %>% filter(Predictor_Set == 'CAN_US', Period %in% plot_periods,
                                           (Period == 'Pre_COVID' & Specification == 'No_Dummy') |
                                             (Period == 'Post_COVID' & Specification == 'With_Dummy')) %>%
  mutate(Horizon_Label = factor(paste0('h=', Horizon), levels = paste0('h=', c(1,3,6,12))))
fig <- ggplot(country_plot, aes(Horizon_Label, Mean_Percent, fill = Country_Group)) +
  geom_col(position = 'stack') + facet_wrap(~Period) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(title = 'CAN+US: country shares of RF importance', x = 'Forecast horizon',
       y = 'Share of IncNodePurity (%)', fill = 'Country') + theme_minimal(base_size = 12)
ggsave(file.path(importance_out, 'CAN_US_country_shares.png'), fig,
       width = 10, height = 5.5, dpi = 300)
cat('Variable importance exports saved to:', importance_out, '\n')

