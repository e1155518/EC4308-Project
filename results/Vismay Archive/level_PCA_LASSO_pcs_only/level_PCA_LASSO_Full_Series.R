# PCA-LASSO on the employment-rate LEVEL with the full series: LASSO on the lagged raw predictors AND their top
# principal components together (the professor's func-lasso.R design), lambda chosen by BIC via HDeconometrics::ic.glmnet.
# Only change from level_PCA_LASSO.R: the raw series now enter alongside the PCs. Run from the project root.
library(HDeconometrics)    # install once: remotes::install_github("gabrielrvsc/HDeconometrics")
source("functions/data_prep.R")
panels  <- load_panels()
n_pcs   <- 10              # same as level_PCA_LASSO.R so the two runs differ only by the raw series (prof's template uses 4)
out_dir <- "results/level_PCA_LASSO_Full_Series"

# ---- Fit one window, return the h-step forecast from its last row ----
run_pcalasso_full_window <- function(y, X, dum, h) {
  X  <- X[, apply(X, 2, sd) > 0]                        # drop series that are constant in this window
  # Factors from this window only (data up to the forecast origin, so no look-ahead); the level stays out of the PCA
  Z  <- X[, colnames(X) != "Employ_Rate"]
  F  <- prcomp(Z, center = TRUE, scale. = TRUE)$x[, 1:n_pcs]
  X2 <- cbind(as.matrix(X), F)                          # raw series (incl. the level) + PCs, side by side

  Xl <- embed(X2, n_lags)                               # row = [X2_t, X2_t-1, X2_t-2, X2_t-3]
  colnames(Xl) <- paste0(rep(colnames(X2), n_lags), "_lag", rep(0:(n_lags - 1), each = ncol(X2)))
  t     <- n_lags:(nrow(X) - h)                         # origins whose outcome (t+h) is already observed
  X_tr  <- Xl[t - n_lags + 1, ]
  y_tr  <- y[t]
  X_out <- tail(Xl, 1)                                  # predictors at the forecast origin

  # COVID dummies: keep only those active in this window; set to 0 at forecast time
  d_tr <- as.matrix(dum[t, , drop = FALSE])
  active <- colSums(abs(d_tr)) > 0
  if (any(active)) {
    X_tr  <- cbind(X_tr, d_tr[, active, drop = FALSE])
    X_out <- cbind(X_out, matrix(0, 1, sum(active)))
  }

  b <- coef(ic.glmnet(X_tr, y_tr, crit = "bic"))        # LASSO (alpha = 1); intercept first
  list(pred = b[1] + sum(X_out * b[-1]), selected = colnames(X_tr)[b[-1] != 0])
}

# ---- Rolling window: one fit per forecast origin ----
pcalasso_full.rolling.window <- function(data, pcols, h, nprev, test_end, window_size = NULL, use_dummy = FALSE) {
  y_name <- paste0("Employ_Rate_lead_h", h)             # level h months ahead
  d_cols <- if (use_dummy) paste0(c("COVID_Shock_h", "COVID_Recovery_h"), h) else character(0)
  outcome_idx <- match(test_end, data$Date) - (nprev - 1):0
  origin_idx  <- outcome_idx - h
  if (is.null(window_size)) window_size <- min(origin_idx)  # fixed window length, rolled forward

  fits <- lapply(origin_idx, function(o) {
    w <- data[(o - window_size + 1):o, ]
    run_pcalasso_full_window(w[[y_name]], w[, pcols], w[, d_cols, drop = FALSE], h)
  })
  pred   <- sapply(fits, `[[`, "pred")
  actual <- data[[y_name]][origin_idx]                  # level at the outcome date
  list(selected  = lapply(fits, `[[`, "selected"),
       forecasts = data.frame(Forecast_Origin = data$Date[origin_idx], Outcome_Date = data$Date[outcome_idx],
                              Actual = actual, Predicted = pred, Forecast_Error = actual - pred))
}

# ---- Run every panel x horizon x spec, caching each result to disk (a stopped run resumes) ----
dir.create(file.path(out_dir, "level_PCA_LASSO_Full_Series_batches"), recursive = TRUE, showWarnings = FALSE)
run_or_load <- function(key, ...) {
  f <- file.path(out_dir, "level_PCA_LASSO_Full_Series_batches", paste0(key, ".rds"))
  if (file.exists(f)) return(readRDS(f))
  cat(format(Sys.time(), "%H:%M"), "Running", key, "\n")
  res <- pcalasso_full.rolling.window(...)
  saveRDS(res, f)
  res
}

preds <- list()
for (s in names(panels$raw)) {
  data  <- panels$raw[[s]]
  pcols <- c("Employ_Rate", setdiff(panels$predictor_cols[[s]], "Employ_Rate_diff_lag1"))  # level replaces its own change
  for (h in horizons) {
    key  <- function(spec) paste(s, h, spec, sep = "_")
    pre  <- run_or_load(key("Pre_COVID_No_Dummy"), data, pcols, h, n_pre, pre_end)
    ws   <- match(pre$forecasts$Forecast_Origin[1], data$Date)        # same window length post-COVID
    post <- run_or_load(key("Post_COVID_No_Dummy"),   data, pcols, h, n_post, post_end, ws, FALSE)
    dumm <- run_or_load(key("Post_COVID_With_Dummy"), data, pcols, h, n_post, post_end, ws, TRUE)

    # Team format. Pre-COVID has no dummy fit (dummy is all zero there), so it is reported under both specs.
    tag <- function(r, period, spec) cbind(r$forecasts, Predictor_Set = s, Horizon = h, Period = period, Specification = spec)
    preds[[key("all")]] <- rbind(tag(pre,  "Pre_COVID",  "No_Dummy"), tag(pre,  "Pre_COVID",  "With_Dummy"),
                                 tag(post, "Post_COVID", "No_Dummy"), tag(dumm, "Post_COVID", "With_Dummy"))
  }
}

# ---- Export ----
preds <- do.call(rbind, preds)
rmse  <- aggregate(cbind(RMSE = Forecast_Error) ~ Predictor_Set + Horizon + Period + Specification,
                   data = preds, FUN = function(e) sqrt(mean(e^2)))
write.csv(preds, file.path(out_dir, "level_PCA_LASSO_Full_Series_predictions.csv"), row.names = FALSE)
write.csv(rmse,  file.path(out_dir, "level_PCA_LASSO_Full_Series_RMSE_summary.csv"), row.names = FALSE)
