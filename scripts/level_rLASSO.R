# rLASSO (hdm::rlasso, data-driven penalty of Belloni, Chernozhukov & Hansen) on the
# employment-rate LEVEL, as in the professor's func-rlasso.R. Run from the project root.
# Slow (rlasso with ~870 columns): windows run in parallel and every batch is cached, so a stopped run resumes.
library(hdm); library(parallel)
source("functions/data_prep.R")
panels  <- load_panels()
out_dir <- "results/level_rLASSO"

# ---- Fit one window: rlasso (no post-LASSO refit) ----
run_rlasso_window <- function(y, X, dum, h) {
  Xl <- embed(as.matrix(X), n_lags)                     # row = [X_t, X_t-1, X_t-2, X_t-3]
  colnames(Xl) <- paste0(rep(colnames(X), n_lags), "_lag", rep(0:(n_lags - 1), each = ncol(X)))
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

  lapply(c(rLASSO = FALSE), function(pl) {
    b <- rlasso(X_tr, y_tr, post = pl)$coefficients     # intercept first
    list(pred = unname(b[1] + sum(X_out * b[-1])), selected = colnames(X_tr)[b[-1] != 0])
  })
}

# ---- Rolling window: one fit per forecast origin, spread over the cluster ----
rlasso.rolling.window <- function(data, pcols, h, nprev, test_end, window_size = NULL, use_dummy = FALSE) {
  y_name <- paste0("Employ_Rate_lead_h", h)             # level h months ahead
  d_cols <- if (use_dummy) paste0(c("COVID_Shock_h", "COVID_Recovery_h"), h) else character(0)
  outcome_idx <- match(test_end, data$Date) - (nprev - 1):0
  origin_idx  <- outcome_idx - h
  if (is.null(window_size)) window_size <- min(origin_idx)  # fixed window length, rolled forward
  force(pcols)                                          # evaluate now so the parallel workers receive it

  fits <- parLapplyLB(cl, origin_idx, function(o) {
    w <- data[(o - window_size + 1):o, ]
    run_rlasso_window(w[[y_name]], w[, pcols], w[, d_cols, drop = FALSE], h)
  })
  actual <- data[[y_name]][origin_idx]                  # level at the outcome date
  lapply(c(rLASSO = "rLASSO"), function(m) {
    pred <- sapply(fits, function(f) f[[m]]$pred)
    list(selected  = lapply(fits, function(f) f[[m]]$selected),
         forecasts = data.frame(Forecast_Origin = data$Date[origin_idx], Outcome_Date = data$Date[outcome_idx],
                                Actual = actual, Predicted = pred, Forecast_Error = actual - pred))
  })
}

# ---- Run every panel x horizon x spec, caching each result to disk ----
cl <- makeCluster(max(1, detectCores() - 1))
invisible(clusterEvalQ(cl, library(hdm)))
clusterExport(cl, c("run_rlasso_window", "n_lags"))

dir.create(file.path(out_dir, "level_rLASSO_batches"), recursive = TRUE, showWarnings = FALSE)
run_or_load <- function(key, ...) {
  f <- file.path(out_dir, "level_rLASSO_batches", paste0(key, ".rds"))
  if (file.exists(f)) return(readRDS(f))
  cat(format(Sys.time(), "%d %b %H:%M"), "Running", key, "\n")
  res <- rlasso.rolling.window(...)
  saveRDS(res, f)
  res
}

preds <- list(rLASSO = list())
for (s in names(panels$raw)) {
  data  <- panels$raw[[s]]
  pcols <- c("Employ_Rate", setdiff(panels$predictor_cols[[s]], "Employ_Rate_diff_lag1"))  # level replaces its own change
  for (h in horizons) {
    key  <- function(spec) paste(s, h, spec, sep = "_")
    pre  <- run_or_load(key("Pre_COVID_No_Dummy"), data, pcols, h, n_pre, pre_end)
    ws   <- match(pre$rLASSO$forecasts$Forecast_Origin[1], data$Date)   # same window length post-COVID
    post <- run_or_load(key("Post_COVID_No_Dummy"),   data, pcols, h, n_post, post_end, ws, FALSE)
    dumm <- run_or_load(key("Post_COVID_With_Dummy"), data, pcols, h, n_post, post_end, ws, TRUE)

    # Team format. Pre-COVID has no dummy fit (dummy is all zero there), so it is reported under both specs.
    for (m in names(preds)) {
      tag <- function(r, period, spec) cbind(r[[m]]$forecasts, Predictor_Set = s, Horizon = h, Period = period, Specification = spec)
      preds[[m]][[key("all")]] <- rbind(tag(pre,  "Pre_COVID",  "No_Dummy"), tag(pre,  "Pre_COVID",  "With_Dummy"),
                                        tag(post, "Post_COVID", "No_Dummy"), tag(dumm, "Post_COVID", "With_Dummy"))
    }
  }
}
stopCluster(cl)

# ---- Export: level_rLASSO_* ----
for (m in names(preds)) {
  p    <- do.call(rbind, preds[[m]])
  rmse <- aggregate(cbind(RMSE = Forecast_Error) ~ Predictor_Set + Horizon + Period + Specification,
                    data = p, FUN = function(e) sqrt(mean(e^2)))
  write.csv(p,    file.path(out_dir, paste0("level_", m, "_predictions.csv")), row.names = FALSE)
  write.csv(rmse, file.path(out_dir, paste0("level_", m, "_RMSE_summary.csv")), row.names = FALSE)
}
