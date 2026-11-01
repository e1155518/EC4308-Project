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


#### the first rolling window is executed sequentially, the second uses parallel processing and shld be faster since it uses 4 workers

# rf.rolling.window <- function(data, predictor_cols, horizon = 1L,
#                               nprev = 100L, test_end = as.Date('2019-12-01'),
#                               window_size = NULL, use_dummy = FALSE,
#                               n_lags = 4L, seed = 4308L, verbose = TRUE) {
#   target_col <- paste0('Employ_Rate_diff_h', horizon)
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
#   needed <- c(target_col, predictor_cols, controls)
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
#   actual <- data[[target_col]][origin_idx]
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
                              n_workers = 4L) {
  
  target_col <- paste0("Employ_Rate_diff_h", horizon)
  
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
  
  needed <- c(target_col, predictor_cols, controls)
  
  if (!all(needed %in% names(data)))
    stop("Missing columns: ",
         paste(setdiff(needed, names(data)), collapse = ", "))
  
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
      "window_size", "needed"
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
        seed = seed + j
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
  
  actual <- data[[target_col]][origin_idx]
  
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
    
    # Historical target: E(t+h) - E(t)
    # COVID controls refer to months within that target interval.
    
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
    
    df[[paste0("COVID_Shock_h", h)]] <- shock / h
    df[[paste0("COVID_Recovery_h", h)]] <- recovery / h
  }
  
  raw[[set_name]] <- df
}

names(raw) <- names(files)

# All three datasets already contain the Canadian employment targets.
# Explicitly exclude all forward targets and the employment-rate level from the predictor set. 
# The historical one-month change is allowed as an autoregressive predictor, consistent with the AR benchmark.
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

# ============================================================
# Run Random Forest in batches with automatic saving
# ============================================================

# Choose which models to run
predictor_sets <- c("CAN", "US", "CAN_US")
horizons_to_run <- c(1, 3, 6, 12)

# Folder for saved results
save_dir <- "results/RF_batches"
dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)

# Initialize results without deleting existing results
if (!exists("rf_results")) {
  rf_results <- list()
}

# Helper: run one model or load its saved result
run_or_load_rf <- function(set_name, h, period, use_dummy,
                           data, predictor_cols, nprev,
                           test_end, window_size = NULL) {
  
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
    window_size = window_size
  )
  
  saveRDS(result, file)
  cat("Saved:", key, "\n")
  
  return(result)
}

for (set_name in predictor_sets) {
  
  data <- raw[[set_name]]
  
  # Exclude dates, outcome variables and COVID controls
  predictor_cols <- setdiff(
    names(data),
    c(
      "Date",
      target_vars,
      grep("^COVID_", names(data), value = TRUE)
    )
  )
  
  # Include historical Canadian employment-rate changes
  predictor_cols <- unique(c(
    predictor_cols,
    "Employ_Rate_diff_lag1"
  ))
  
  for (h in horizons_to_run) {
    
    cat("\n====================================\n")
    cat("Predictor set:", set_name, "| Horizon:", h, "\n")
    cat("====================================\n")
    
    # 1. Pre-COVID without dummy
    pre <- run_or_load_rf(
      set_name = set_name,
      h = h,
      period = "Pre_COVID",
      use_dummy = FALSE,
      data = data,
      nprev = 100,
      predictor_cols = predictor_cols,
      test_end = "2019-12-01"
    )
    
    rf_results[[paste(set_name, h, "Pre_COVID_No_Dummy", sep = "_")]] <- pre
    
    # Match post-COVID window size to pre-COVID
    window_size <- match(
      pre$forecasts$Forecast_Origin[1],
      data$Date
    )
    
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
      window_size = window_size
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
      window_size = window_size
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
         x='Outcome Date', y='Canadian employment-rate change (pp)', color=NULL) +
    theme_minimal()
}


# ============================================================
# Export Random Forest results
# ============================================================

dir.create("results", showWarnings = FALSE)

# 1. Export predictions for DM tests and ensemble methods
write.csv(rf_all_predictions,"results/RF_predictions.csv", row.names = FALSE)
write.csv(rf_rmse_table,"results/RF_RMSE_summary.csv",row.names = FALSE)

# 3. Extract variable importance from completed RF models
importance_list <- lapply(rf_results,  function(x) x$save.importance)

# 4. Save variable importance separately
saveRDS(importance_list,"results/RF_variable_importance.rds")

# 5. Save all RF results (predictions + importance)
saveRDS(  rf_results,  "results/RF_full_results.rds")
cat("All RF results exported successfully!\n")

# Examples:
plot_RF(12, 'CAN', 'all')
plot_RF(12, 'US', 'all')
plot_RF(12, 'CAN_US', 'all')



