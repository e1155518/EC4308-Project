# Ridge regression of the employment-rate LEVEL on raw lagged predictors; lambda re-picked by information criterion in every window
library(glmnet)
library(HDeconometrics); library(parallel)
source("functions/data_prep.R")
panels <- load_panels()

out_dir <- "results/Ridge"

# ---- Fit one window, return the h-step forecast from its last row ----
# Targets Employ_Rate_lead_h{h} are already the level at t+h, so only predictors get lagged.
run_ridge_window <- function(y, X, dum, h, ic = "bic") {
  Xl <- embed(as.matrix(X), n_lags)                     # row = [X_t, X_t-1, X_t-2, X_t-3]
  colnames(Xl) <- paste0(rep(colnames(X), n_lags), "_lag", rep(0:(n_lags - 1), each = ncol(X)))
  
  t   <- n_lags:(nrow(X) - h)                           # origins whose outcome (t+h) is already observed
  X_tr <- Xl[t - n_lags + 1, ]
  y_tr <- y[t]
  X_out <- tail(Xl, 1)                                  # predictors at the forecast origin
  
  # COVID dummies: keep only those active in this window; set to 0 at forecast time
  d_tr <- as.matrix(dum[t, , drop = FALSE])
  active <- colSums(abs(d_tr)) > 0
  if (any(active)) {
    X_tr  <- cbind(X_tr, d_tr[, active, drop = FALSE])
    X_out <- cbind(X_out, matrix(0, 1, sum(active)))
  }
  
  # Choose lambda by information criterion along glmnet's full lambda path (alpha = 0 is ridge).
  # For alpha = 0, ic.glmnet uses effective df tr[Xs (Xs'Xs + lambda*I)^-1 Xs'] with glmnet's lambda as is.
  # NB: glmnet's lambda corresponds to n*lambda/sd(y) in that formula, so this df is close to p at every
  #  lambda and BIC tends to pick an end of the path (check the At_Path_End columns in level_Ridge_lambda_choice.csv).
  # ic.glmnet returns every lambda tied at the minimum, so take the first one.
  fit  <- ic.glmnet(X_tr, y_tr, crit = ic, alpha = 0)
  path <- fit$glmnet$lambda
  lam  <- path[which.min(fit$ic.range)]

  list(pred = as.numeric(predict(fit$glmnet, X_out, s = lam)), lambda = lam,
       at_min = lam == min(path), at_max = lam == max(path))
}

# ---- Rolling window: one fit per forecast origin, spread over the cluster ----
ridge.rolling.window <- function(data, pcols, h, nprev, test_end, window_size = NULL,
                                 use_dummy = FALSE, ic = "bic") {
  y_name <- paste0("Employ_Rate_lead_h", h)             # level h months ahead
  d_cols <- if (use_dummy) paste0(c("COVID_Shock_h", "COVID_Recovery_h"), h) else character(0)
  outcome_idx <- match(test_end, data$Date) - (nprev - 1):0
  origin_idx  <- outcome_idx - h
  if (is.null(window_size)) window_size <- min(origin_idx)  # fixed window length, rolled forward
  
  force(pcols); force(ic)                               # evaluate now so the parallel workers receive them

  fits <- parLapplyLB(cl, origin_idx, function(o) {
    w <- data[(o - window_size + 1):o, ]
    run_ridge_window(w[[y_name]], w[, pcols], w[, d_cols, drop = FALSE], h, ic)
  })
  
  pred   <- sapply(fits, `[[`, "pred")
  actual <- data[[y_name]][origin_idx]                  # level at the outcome date
  list(choice    = data.frame(Forecast_Origin = data$Date[origin_idx],
                              Lambda = sapply(fits, `[[`, "lambda"),
                              At_Path_End_Min = sapply(fits, `[[`, "at_min"),
                              At_Path_End_Max = sapply(fits, `[[`, "at_max")),
       forecasts = data.frame(Forecast_Origin = data$Date[origin_idx],
                              Outcome_Date    = data$Date[outcome_idx],
                              Actual = actual, Predicted = pred,
                              Forecast_Error  = actual - pred))
}

# ---- Run every panel x horizon x spec, caching each result to disk ----
cl <- makeCluster(max(1, detectCores() - 1))
invisible(clusterEvalQ(cl, { library(glmnet); library(HDeconometrics) }))
clusterExport(cl, c("run_ridge_window", "n_lags"))

dir.create(file.path(out_dir, "level_Ridge_batches"), recursive = TRUE, showWarnings = FALSE)
run_or_load <- function(key, ...) {
  f <- file.path(out_dir, "level_Ridge_batches", paste0(key, ".rds"))
  if (file.exists(f)) return(readRDS(f))
  cat("Running", key, "\n")
  res <- ridge.rolling.window(...)
  saveRDS(res, f)
  res
}

preds <- list(); choices <- list()
for (s in names(panels$raw)) {
  data <- panels$raw[[s]]
  pcols <- c("Employ_Rate", setdiff(panels$predictor_cols[[s]], "Employ_Rate_diff_lag1"))  # level replaces its own change
  for (h in horizons) {
    key  <- function(spec) paste(s, h, spec, sep = "_")
    pre  <- run_or_load(key("Pre_COVID_No_Dummy"), data, pcols, h, n_pre, pre_end)
    ws   <- match(pre$forecasts$Forecast_Origin[1], data$Date)        # same window length post-COVID
    post <- run_or_load(key("Post_COVID_No_Dummy"),   data, pcols, h, n_post, post_end, ws, FALSE)
    dumm <- run_or_load(key("Post_COVID_With_Dummy"), data, pcols, h, n_post, post_end, ws, TRUE)
    
    # Tag forecasts in the team's standard format. Pre-COVID has no dummy fit (the dummy is
    # all zero there), so the same forecasts are reported under both specifications.
    tag <- function(r, period, spec, part = "forecasts") cbind(r[[part]], Predictor_Set = s, Horizon = h,
                                                               Period = period, Specification = spec)
    preds[[key("all")]] <- rbind(tag(pre,  "Pre_COVID",  "No_Dummy"), tag(pre,  "Pre_COVID",  "With_Dummy"),
                                 tag(post, "Post_COVID", "No_Dummy"), tag(dumm, "Post_COVID", "With_Dummy"))
    choices[[key("all")]] <- rbind(tag(pre,  "Pre_COVID",  "No_Dummy",   "choice"), tag(pre,  "Pre_COVID",  "With_Dummy", "choice"),
                                   tag(post, "Post_COVID", "No_Dummy",   "choice"), tag(dumm, "Post_COVID", "With_Dummy", "choice"))
  }
}

stopCluster(cl)

# ---- Export ----
preds   <- do.call(rbind, preds)
choices <- do.call(rbind, choices)
rmse <- aggregate(cbind(RMSE = Forecast_Error) ~ Predictor_Set + Horizon + Period + Specification,
                  data = preds, FUN = function(e) sqrt(mean(e^2)))

write.csv(preds,   file.path(out_dir, "level_Ridge_predictions.csv"), row.names = FALSE)
write.csv(rmse,    file.path(out_dir, "level_Ridge_RMSE_summary.csv"), row.names = FALSE)
write.csv(choices, file.path(out_dir, "level_Ridge_lambda_choice.csv"), row.names = FALSE)  # chosen lambda per window
