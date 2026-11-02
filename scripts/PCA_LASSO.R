# PCA-LASSO: LASSO on lagged principal components of the predictors (pure LASSO)
# Run from the project root.
library(glmnet)
source("functions/data_prep.R")
panels <- load_panels()
n_pcs <- 10                                             # factors per window, same for every panel

# ---- Fit one window, return the h-step forecast from its last row ----
run_pcalasso_window <- function(y, X, dum, h, ic = "bic") {
  # Factors from this window only; employment's own lag stays outside the PCA as its own regressor
  Z  <- X[, colnames(X) != "Employ_Rate_diff_lag1"]
  Z  <- Z[, apply(Z, 2, sd) > 0]                        # constant columns cannot be standardized
  F  <- prcomp(Z, center = TRUE, scale. = TRUE)$x[, 1:n_pcs]
  X2 <- cbind(Employ_Rate_diff_lag1 = X$Employ_Rate_diff_lag1, F)

  Xl <- embed(X2, n_lags)                               # row = [X2_t, X2_t-1, X2_t-2, X2_t-3]
  colnames(Xl) <- paste0(rep(colnames(X2), n_lags), "_lag", rep(0:(n_lags - 1), each = ncol(X2)))

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

  # Choose lambda by information criterion along glmnet's full lambda path
  fit <- glmnet(X_tr, y_tr)
  n   <- length(y_tr)
  k   <- fit$df + 1                                     # nonzero coefficients + intercept
  rss <- colSums((y_tr - predict(fit, X_tr))^2)
  pen <- switch(ic, bic = log(n) * k, aic = 2 * k, aicc = 2 * k + 2 * k * (k + 1) / pmax(n - k - 1, 1))
  lam <- fit$lambda[which.min(n * log(rss / n) + pen)]

  b <- coef(fit, s = lam)[-1, 1]
  list(pred = as.numeric(predict(fit, X_out, s = lam)), selected = names(b)[b != 0])
}

# ---- Rolling window: one fit per forecast origin ----
pcalasso.rolling.window <- function(data, pcols, h, nprev, test_end, window_size = NULL,
                                    use_dummy = FALSE, ic = "bic") {
  y_name <- paste0("Employ_Rate_diff_h", h)
  d_cols <- if (use_dummy) paste0(c("COVID_Shock_h", "COVID_Recovery_h"), h) else character(0)
  outcome_idx <- match(test_end, data$Date) - (nprev - 1):0
  origin_idx  <- outcome_idx - h
  if (is.null(window_size)) window_size <- min(origin_idx)  # fixed window length, rolled forward

  fits <- lapply(origin_idx, function(o) {
    w <- data[(o - window_size + 1):o, ]
    run_pcalasso_window(w[[y_name]], w[, pcols], w[, d_cols, drop = FALSE], h, ic)
  })

  pred   <- sapply(fits, `[[`, "pred")
  actual <- data[[y_name]][origin_idx]                  # change from origin to outcome date
  list(selected  = lapply(fits, `[[`, "selected"),
       forecasts = data.frame(Forecast_Origin = data$Date[origin_idx],
                              Outcome_Date    = data$Date[outcome_idx],
                              Actual = actual, Predicted = pred,
                              Forecast_Error  = actual - pred))
}

# ---- Run every panel x horizon x spec, caching each result to disk ----
dir.create("results/PCA_LASSO_batches", recursive = TRUE, showWarnings = FALSE)
run_or_load <- function(key, ...) {
  f <- file.path("results/PCA_LASSO_batches", paste0(key, ".rds"))
  if (file.exists(f)) return(readRDS(f))
  cat("Running", key, "\n")
  res <- pcalasso.rolling.window(...)
  saveRDS(res, f)
  res
}

pcalasso_results <- list(); preds <- list()
for (s in names(panels$raw)) {
  data <- panels$raw[[s]]; pcols <- panels$predictor_cols[[s]]
  for (h in horizons) {
    key  <- function(spec) paste(s, h, spec, sep = "_")
    pre  <- run_or_load(key("Pre_COVID_No_Dummy"), data, pcols, h, n_pre, pre_end)
    ws   <- match(pre$forecasts$Forecast_Origin[1], data$Date)        # same window length post-COVID
    post <- run_or_load(key("Post_COVID_No_Dummy"),   data, pcols, h, n_post, post_end, ws, FALSE)
    dumm <- run_or_load(key("Post_COVID_With_Dummy"), data, pcols, h, n_post, post_end, ws, TRUE)
    pcalasso_results[c(key("Pre_COVID_No_Dummy"), key("Post_COVID_No_Dummy"), key("Post_COVID_With_Dummy"))] <- list(pre, post, dumm)

    # Tag forecasts in the team's standard format. Pre-COVID has no dummy fit (the dummy is
    # all zero there), so the same forecasts are reported under both specifications.
    tag <- function(r, period, spec) cbind(r$forecasts, Predictor_Set = s, Horizon = h,
                                           Period = period, Specification = spec)
    preds[[key("all")]] <- rbind(tag(pre,  "Pre_COVID",  "No_Dummy"), tag(pre,  "Pre_COVID",  "With_Dummy"),
                                 tag(post, "Post_COVID", "No_Dummy"), tag(dumm, "Post_COVID", "With_Dummy"))
  }
}

# ---- Export ----
preds <- do.call(rbind, preds)
rmse <- aggregate(cbind(RMSE = Forecast_Error) ~ Predictor_Set + Horizon + Period + Specification,
                  data = preds, FUN = function(e) sqrt(mean(e^2)))

write.csv(preds, "results/PCA_LASSO_predictions.csv", row.names = FALSE)
write.csv(rmse,  "results/PCA_LASSO_RMSE_summary.csv", row.names = FALSE)
