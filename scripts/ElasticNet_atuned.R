# Elastic net: alpha is tuned ONCE (by information criterion, jointly with lambda, on the FIRST training
# sample of each rolling run) and then held fixed; lambda is re-picked by IC in EVERY window.

library(glmnet)
source("functions/data_prep.R")
panels <- load_panels()

# Candidate mixing parameters. glmnet has no default grid for alpha; 0, 0.1, ..., 1 (0 = ridge, 1 = LASSO)
# is the usual choice (it is also what glmnetUtils::cva.glmnet uses). Make it coarser/finer as you like.
alpha.grid <- seq(0, 1, by = 0.1)
out_dir <- "results/ElasticNet_tuned"

# ---- Effective degrees of freedom along one glmnet path ----
# alpha = 1: number of nonzero coefficients.
# alpha < 1: with active set A and standardized, centred X (glmnet's 1/n scaling),
#   df = tr[ X_A (X_A'X_A + n*lambda*(1-alpha) I)^-1 X_A' ]
# alpha = 0: computed from the singular values of X (all variables active).
enet_df <- function(fit, a, Xs, G) {
  n <- nrow(Xs)
  if (a == 1) return(fit$df)
  if (a == 0) {
    d2 <- svd(Xs, nu = 0, nv = 0)$d^2
    return(sapply(n * fit$lambda, function(cc) sum(d2 / (d2 + cc))))
  }
  sapply(seq_along(fit$lambda), function(k) {
    A <- which(fit$beta[, k] != 0)
    if (!length(A)) return(0)
    GA <- G[A, A, drop = FALSE]
    sum(GA * chol2inv(chol(GA + n * fit$lambda[k] * (1 - a) * diag(length(A)))))
  })
}

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
  
  n <- length(y_tr)
  
  # Standardize the way glmnet does internally (1/n standard deviation) to compute df
  sdn <- apply(X_tr, 2, function(v) sqrt(mean((v - mean(v))^2)))
  sdn[sdn == 0] <- 1
  Xs <- scale(X_tr, center = TRUE, scale = sdn)
  G  <- crossprod(Xs)
  
  # Information criterion along the lambda path of every candidate alpha
  alphas <- if (is.null(alpha)) alpha.grid else alpha
  cands <- lapply(alphas, function(a) {
    fit <- glmnet(X_tr, y_tr, alpha = a)
    k   <- enet_df(fit, a, Xs, G) + 1                   # effective df + intercept
    rss <- colSums((y_tr - predict(fit, X_tr))^2)
    pen <- switch(ic, bic = log(n) * k, aic = 2 * k, aicc = 2 * k + 2 * k * (k + 1) / pmax(n - k - 1, 1))
    list(fit = fit, crit = n * log(rss / n) + pen)
  })
  
  # Pick the (alpha, lambda) pair with the smallest criterion (a single alpha leaves just the lambda choice)
  best <- which.min(sapply(cands, function(cd) min(cd$crit)))
  fit  <- cands[[best]]$fit
  lam  <- fit$lambda[which.min(cands[[best]]$crit)]
  
  b <- coef(fit, s = lam)[-1, 1]
  list(pred = as.numeric(predict(fit, X_out, s = lam)), selected = names(b)[b != 0],
       alpha = alphas[best], lambda = lam)
}

# ---- Rolling window: tune alpha on the first window, re-pick lambda in every window ----
enet.rolling.window <- function(data, pcols, h, nprev, test_end, window_size = NULL,
                                use_dummy = FALSE, ic = "bic") {
  y_name <- paste0("Employ_Rate_diff_h", h)
  d_cols <- if (use_dummy) paste0(c("COVID_Shock_h", "COVID_Recovery_h"), h) else character(0)
  outcome_idx <- match(test_end, data$Date) - (nprev - 1):0
  origin_idx  <- outcome_idx - h
  if (is.null(window_size)) window_size <- min(origin_idx)  # fixed window length, rolled forward
  
  fit_window <- function(o, alpha) {
    w <- data[(o - window_size + 1):o, ]
    run_enet_window(w[[y_name]], w[, pcols], w[, d_cols, drop = FALSE], h, alpha, ic)
  }
  
  first <- fit_window(origin_idx[1], NULL)              # alpha search happens here only
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
    pre  <- run_or_load(key("Pre_COVID_No_Dummy"), data, pcols, h, n_pre, pre_end)
    ws   <- match(pre$forecasts$Forecast_Origin[1], data$Date)        # same window length post-COVID
    post <- run_or_load(key("Post_COVID_No_Dummy"),   data, pcols, h, n_post, post_end, ws, FALSE)
    dumm <- run_or_load(key("Post_COVID_With_Dummy"), data, pcols, h, n_post, post_end, ws, TRUE)
    enet_results[c(key("Pre_COVID_No_Dummy"), key("Post_COVID_No_Dummy"), key("Post_COVID_With_Dummy"))] <- list(pre, post, dumm)
    
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

# ---- Export ----
preds   <- do.call(rbind, preds)
choices <- do.call(rbind, choices)
rmse <- aggregate(cbind(RMSE = Forecast_Error) ~ Predictor_Set + Horizon + Period + Specification,
                  data = preds, FUN = function(e) sqrt(mean(e^2)))

write.csv(preds,   file.path(out_dir, "ElasticNet_tuned_predictions.csv"), row.names = FALSE)
write.csv(rmse,    file.path(out_dir, "ElasticNet_tuned_RMSE_summary.csv"), row.names = FALSE)
write.csv(choices, file.path(out_dir, "ElasticNet_tuned_alpha_lambda_choice.csv"), row.names = FALSE)  # alpha (constant within a run), lambda and #selected per window
