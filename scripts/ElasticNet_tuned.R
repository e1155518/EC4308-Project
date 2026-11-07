# Elastic net: alpha is tuned ONCE per predictor set x horizon (by information criterion, jointly with lambda,
# on the FIRST pre-COVID training sample) and then held fixed through the pre- and post-COVID runs;
# lambda is re-picked by IC in EVERY window.

library(glmnet)
library(HDeconometrics)    
source("functions/data_prep.R")
panels <- load_panels()

# Candidate mixing parameters. glmnet has no default grid for alpha; 0, 0.1, ..., 1 (0 = ridge, 1 = LASSO)
# is the usual choice (it is also what glmnetUtils::cva.glmnet uses). Make it coarser/finer as you like.
alpha.grid <- seq(0, 1, by = 0.1)
out_dir <- "results/level/ElasticNet_tuned"

# ---- Fit one window, return the h-step forecast from its last row ----
# alpha = NULL: search alpha.grid (and each alpha's lambda path) by IC, i.e. tune the (alpha, lambda) pair.
# alpha given:  keep that alpha and pick lambda by IC along its path.
# Targets Employ_Rate_diff_h{h} are already the change from t to t+h, so only predictors get lagged.
run_enet_window <- function(y, X, dum, h, alpha = NULL, ic = "bic") {
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
  
  # Information criterion along the lambda path of every candidate alpha (ic.glmnet picks lambda per alpha).
  # ic.glmnet counts df as the number of nonzero coefficients for alpha > 0 and uses effective df for alpha = 0.
  alphas <- if (is.null(alpha)) alpha.grid else alpha
  cands  <- lapply(alphas, function(a) ic.glmnet(X_tr, y_tr, crit = ic, alpha = a))

  # Pick the (alpha, lambda) pair with the smallest criterion (a single alpha leaves just the lambda choice).
  # ic.glmnet returns every lambda tied at the minimum (e.g. all-zero fits), so take the first one.
  best <- which.min(sapply(cands, function(fit) min(fit$ic.range)))
  fit  <- cands[[best]]$glmnet
  lam  <- fit$lambda[which.min(cands[[best]]$ic.range)]

  b <- coef(fit, s = lam)[-1, 1]
  list(pred = as.numeric(predict(fit, X_out, s = lam)), selected = names(b)[b != 0],
       alpha = alphas[best], lambda = lam)
}

# ---- Rolling window: re-pick lambda in every window ----
# alpha = NULL: tune alpha on the first window and keep it. alpha given: use it in every window.
enet.rolling.window <- function(data, pcols, h, nprev, test_end, window_size = NULL,
                                use_dummy = FALSE, alpha = NULL, ic = "bic") {
  y_name <- paste0("Employ_Rate_diff_h", h)
  d_cols <- if (use_dummy) paste0(c("COVID_Shock_h", "COVID_Recovery_h"), h) else character(0)
  outcome_idx <- match(test_end, data$Date) - (nprev - 1):0
  origin_idx  <- outcome_idx - h
  if (is.null(window_size)) window_size <- min(origin_idx)  # fixed window length, rolled forward
  
  fit_window <- function(o, alpha) {
    w <- data[(o - window_size + 1):o, ]
    run_enet_window(w[[y_name]], w[, pcols], w[, d_cols, drop = FALSE], h, alpha, ic)
  }
  
  first <- fit_window(origin_idx[1], alpha)             # alpha search happens here only (if alpha is NULL)
  fits  <- c(list(first), lapply(origin_idx[-1], fit_window, alpha = first$alpha))
  
  pred   <- sapply(fits, `[[`, "pred")
  actual <- data[[y_name]][origin_idx]                  # change from origin to outcome date
  list(selected  = lapply(fits, `[[`, "selected"),
       choice    = data.frame(Forecast_Origin = data$Date[origin_idx],
                              Alpha  = sapply(fits, `[[`, "alpha"),
                              Lambda = sapply(fits, `[[`, "lambda"),
                              N_Selected = sapply(fits, function(f) length(f$selected))),
       forecasts = data.frame(Forecast_Origin = data$Date[origin_idx],
                              Outcome_Date    = data$Date[outcome_idx],
                              Actual = actual, Predicted = pred,
                              Forecast_Error  = actual - pred))
}

# ---- Run every panel x horizon x spec, caching each result to disk ----
dir.create(file.path(out_dir, "batches"), recursive = TRUE, showWarnings = FALSE)
run_or_load <- function(key, ...) {
  f <- file.path(out_dir, "batches", paste0(key, ".rds"))
  if (file.exists(f)) return(readRDS(f))
  cat("Running", key, "\n")
  res <- enet.rolling.window(...)
  saveRDS(res, f)
  res
}

enet_results <- list(); preds <- list(); choices <- list()
for (s in names(panels$raw)) {
  data <- panels$raw[[s]]; pcols <- panels$predictor_cols[[s]]
  for (h in horizons) {
    key  <- function(spec) paste(s, h, spec, sep = "_")
    pre  <- run_or_load(key("Pre_COVID_No_Dummy"), data, pcols, h, n_pre, pre_end)   # alpha tuned here
    ws   <- match(pre$forecasts$Forecast_Origin[1], data$Date)        # same window length post-COVID
    a    <- pre$choice$Alpha[1]                                       # reuse the cell's alpha post-COVID
    post <- run_or_load(key("Post_COVID_No_Dummy"),   data, pcols, h, n_post, post_end, ws, FALSE, alpha = a)
    dumm <- run_or_load(key("Post_COVID_With_Dummy"), data, pcols, h, n_post, post_end, ws, TRUE,  alpha = a)
    enet_results[c(key("Pre_COVID_No_Dummy"), key("Post_COVID_No_Dummy"), key("Post_COVID_With_Dummy"))] <- list(pre, post, dumm)
    
    # Tag forecasts. Pre-COVID has no dummy fit (the dummy is
    # all zero there), so the same forecasts are reported under both specifications.
    tag <- function(r, period, spec, part = "forecasts") cbind(r[[part]], Predictor_Set = s, Horizon = h,
                                                               Period = period, Specification = spec)
    preds[[key("all")]] <- rbind(tag(pre,  "Pre_COVID",  "No_Dummy"), tag(pre,  "Pre_COVID",  "With_Dummy"),
                                 tag(post, "Post_COVID", "No_Dummy"), tag(dumm, "Post_COVID", "With_Dummy"))
    choices[[key("all")]] <- rbind(tag(pre,  "Pre_COVID",  "No_Dummy",   "choice"), tag(pre,  "Pre_COVID",  "With_Dummy", "choice"),
                                   tag(post, "Post_COVID", "No_Dummy",   "choice"), tag(dumm, "Post_COVID", "With_Dummy", "choice"))
  }
}

# ---- Export ----
preds   <- do.call(rbind, preds)
choices <- do.call(rbind, choices)
rmse <- aggregate(cbind(RMSE = Forecast_Error) ~ Predictor_Set + Horizon + Period + Specification,
                  data = preds, FUN = function(e) sqrt(mean(e^2)))

write.csv(preds,   file.path(out_dir, "level_ElasticNet_tuned_predictions.csv"), row.names = FALSE)
write.csv(rmse,    file.path(out_dir, "level_ElasticNet_tuned_RMSE_summary.csv"), row.names = FALSE)
write.csv(choices, file.path(out_dir, "level_ElasticNet_tuned_alpha_lambda_choice.csv"), row.names = FALSE) 
