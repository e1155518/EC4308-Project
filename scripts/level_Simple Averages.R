#install.packages("sandwich")
library(sandwich) #library to estimate variance for DM test regression using NeweyWest()


########################################################
###Loading of all the predictions - 

in_dir <- "results"

files <- c(
  RF_def_pred = "random_forest_default/level_RF_predictions.csv",
  RF_alt_pred = "random_forest_alt/level_RF_alt_predictions.csv",
  PCA_LASSO_pred = "level_PCA_LASSO/level_PCA_LASSO_predictions.csv",
  rLASSO_pred = "level_rLASSO/level_rLASSO_predictions.csv",
  Elnet_pred = "ElasticNet_a05/level_ElasticNet_a05_predictions.csv",
  Ridge_pred = "Ridge/level_Ridge_predictions.csv",
  RW_pred = "benchmarks/level_random_walk_predictions.csv",
  AR_pred = "AR/level_AR_fixed_p_predictions.csv"
)

predictions <- lapply(file.path(in_dir, files), read.csv)
names(predictions) <- names(files)
list2env(predictions, envir = .GlobalEnv)

## Calc Squared Errors Column
RF_def_pred["SqErr"] <- RF_def_pred$Forecast_Error^2
RF_alt_pred["SqErr"] <- RF_alt_pred$Forecast_Error^2
PCA_LASSO_pred["SqErr"] <- PCA_LASSO_pred$Forecast_Error^2
rLASSO_pred["SqErr"] <- rLASSO_pred$Forecast_Error^2
Elnet_pred["SqErr"] <- Elnet_pred$Forecast_Error^2
Ridge_pred["SqErr"] <- Ridge_pred$Forecast_Error^2

RW_pred["SqErr"] <- RW_pred$Forecast_Error^2
AR_pred["SqErr"] <- AR_pred$Forecast_Error^2


sets     <- c("CAN", "US", "CAN_US")
horizons <- c(1, 3, 6, 12)

period <- list(
  PreCOVID        = c("Pre_COVID",  "No_Dummy"),
  PostCOVID       = c("Post_COVID", "No_Dummy"),
  PostCOVID_dummy = c("Post_COVID", "With_Dummy")
)

###Slice Each of the Models' Predictions Up by horizon, data set(CAN,US or CAN_US) and period

# Pull out one slice: one model's forecasts for one set, horizon and period, sorted by date
get_slice <- function(df, h, period, set) {
  keep <- df$Horizon == h &
    df$Period == period[1] &
    df$Specification == period[2] &
    df$Predictor_Set == set
  
  slice <- df[keep, c("Outcome_Date", "Actual", "Predicted", "Forecast_Error","SqErr")]
  slice[order(slice$Outcome_Date), ]
}

#RF default
RFdefsplit_preds <- list()
for (s in sets){
  for (h in horizons){
    for (p in names(period)){
      h_name <- paste0("h",h)
      
      slice <- get_slice(
        df = RF_def_pred,
        h = h,
        period = period[[p]],
        set = s
      )
      
      RFdefsplit_preds[[s]][[h_name]][[p]] <- slice
      
    }
  }
}

#RFalt
RFaltsplit_preds <- list()
for (s in sets){
  for (h in horizons){
    for (p in names(period)){
      h_name <- paste0("h",h)
      
      slice <- get_slice(
        df = RF_alt_pred,
        h = h,
        period = period[[p]],
        set = s
      )
      
      RFaltsplit_preds[[s]][[h_name]][[p]] <- slice
      
    }
  }
}

#PCA LASSO
PCALASSOsplit_preds <- list()
for (s in sets){
  for (h in horizons){
    for (p in names(period)){
      h_name <- paste0("h",h)
      
      slice <- get_slice(
        df = PCA_LASSO_pred,
        h = h,
        period = period[[p]],
        set = s
      )
      
      PCALASSOsplit_preds[[s]][[h_name]][[p]] <- slice
      
    }
  }
}


#rLASSO
rLASSOsplit_preds <- list()
for (s in sets){
  for (h in horizons){
    for (p in names(period)){
      h_name <- paste0("h",h)
      
      slice <- get_slice(
        df = rLASSO_pred,
        h = h,
        period = period[[p]],
        set = s
      )
      
      rLASSOsplit_preds[[s]][[h_name]][[p]] <- slice
      
    }
  }
}


#Elnet
Elnetsplit_preds <- list()
for (s in sets){
  for (h in horizons){
    for (p in names(period)){
      h_name <- paste0("h",h)
      
      slice <- get_slice(
        df = Elnet_pred,
        h = h,
        period = period[[p]],
        set = s
      )
      
      Elnetsplit_preds[[s]][[h_name]][[p]] <- slice
      
    }
  }
}

#Ridge
Ridgesplit_preds <- list()
for (s in sets){
  for (h in horizons){
    for (p in names(period)){
      h_name <- paste0("h",h)
      
      slice <- get_slice(
        df = Ridge_pred,
        h = h,
        period = period[[p]],
        set = s
      )
      
      Ridgesplit_preds[[s]][[h_name]][[p]] <- slice
      
    }
  }
}


#RW
RWsplit_preds <- list()
for (s in sets){
  for (h in horizons){
    for (p in names(period)){
      h_name <- paste0("h",h)
      
      slice <- get_slice(
        df = RW_pred,
        h = h,
        period = period[[p]],
        set = s
      )
      
      RWsplit_preds[[s]][[h_name]][[p]] <- slice
      
    }
  }
}

#AR
AR_pred <- AR_pred[AR_pred$AR_Order == 1,]
ARsplit_preds <- list()
for (s in sets){
  for (h in horizons){
    for (p in names(period)){
      h_name <- paste0("h",h)
      
      slice <- get_slice(
        df = AR_pred,
        h = h,
        period = period[[p]],
        set = s
      )
      
      ARsplit_preds[[s]][[h_name]][[p]] <- slice
      
    }
  }
}

########################################################
###It's the final showdown: Diebold-Mariano (DM) tests
#########################################################


##Loop For all variations###

# The models to test, and the split lists that hold their slices
model_lists <- list(
  "Random Forest Default"         = RFdefsplit_preds,
  "Random Forest Alternate"         = RFaltsplit_preds,
  "PCA-LASSO"             = PCALASSOsplit_preds,
  "rLASSO"                = rLASSOsplit_preds,
  "Elastic Net (a = 0.5)" = Elnetsplit_preds,
  "Ridge"                 = Ridgesplit_preds
)

# The two benchmarks (their slices are stored under the placeholder set "CAN")
benchmark_lists <- list(
  "AR(1)"       = ARsplit_preds,
  "Random walk" = RWsplit_preds
)

# Test periods. The random walk has no dummy version, so for PostCOVID_dummy
# the random walk's PostCOVID slice is used (its forecasts don't change).
test_periods <- c("PreCOVID", "PostCOVID", "PostCOVID_dummy")

nw_lag <- 5            # 100^(1/3) ~ 5, post COVID is 69^(1/3) ~ 4

dm_results <- data.frame()

for (b in names(benchmark_lists)) {                  # loop over benchmarks
  for (m in names(model_lists)) {                    # loop over models
    for (s in sets) {                                # loop over predictor sets
      for (p in test_periods) {                      # loop over test periods
        
        # Benchmark slice name: the random walk has no PostCOVID_dummy slice
        p_bench <- if (b == "Random walk" && p == "PostCOVID_dummy") "PostCOVID" else p
        
        t_stats <- c()                               # one DM t-statistic per horizon
        
        for (h in horizons) {                        # loop over horizons
          h_name <- paste0("h", h)
          
          # Sq Err
          l_bench <- benchmark_lists[[b]][["CAN"]][[h_name]][[p_bench]]$SqErr
          l_model <- model_lists[[m]][[s]][[h_name]][[p]]$SqErr
          
          # Loss differential, benchmark minus model (darrf1pre)
          d <- l_bench - l_model
          
          # DM regression on a constant with Newey-West variance (dmarrf1pre)
          dm_reg <- lm(d ~ 1)
          t_stats[h_name] <- as.numeric(dm_reg$coefficients / sqrt(NeweyWest(dm_reg, lag = nw_lag)))
        }
        
        # One row per variation
        one_row <- data.frame(
          Model     = m,
          Benchmark = b,
          Set       = s,
          Period    = p,
          
          t_h1  = t_stats[["h1"]],
          t_h3  = t_stats[["h3"]],
          t_h6  = t_stats[["h6"]],
          t_h12 = t_stats[["h12"]],
          
          p_h1  = 2 * pnorm(-abs(t_stats[["h1"]])),     # two-sided p-values
          p_h3  = 2 * pnorm(-abs(t_stats[["h3"]])),
          p_h6  = 2 * pnorm(-abs(t_stats[["h6"]])),
          p_h12 = 2 * pnorm(-abs(t_stats[["h12"]]))
        )
        
        dm_results <- rbind(dm_results, one_row)
        
      }
    }
  }
}

# Positive t means model had lower loss than benchmark 
dm_results

# E.g pulling out parts of the table
dm_results[dm_results$Benchmark == "AR(1)" & dm_results$Period == "PreCOVID", ]
dm_results[dm_results$Model == "Random Forest" & dm_results$Set == "CAN", ]


###############
### Analysis###
###############

#at 5% significance level
outperformers_fivepercent = dm_results[dm_results$t_h1 > 1.96 | dm_results$t_h3 > 1.96 |
                           dm_results$t_h6 > 1.96 | dm_results$t_h12 > 1.96, ]
#at 10% significance level
outperformers_tenpercent = dm_results[dm_results$t_h1 > 1.64 | dm_results$t_h3 > 1.64 |
                             dm_results$t_h6 > 1.64 | dm_results$t_h12 > 1.64, ]

#at 12.5% significance level
outperformers_twelvepercent = dm_results[dm_results$t_h1 > 1.5341 | dm_results$t_h3 > 1.5341 |
                                        dm_results$t_h6 > 1.5341 | dm_results$t_h12 > 1.5341, ]

#at 20% significance level
outperformers_twentypercent = dm_results[dm_results$t_h1 > 1.28 | dm_results$t_h3 > 1.28 |
                                           dm_results$t_h6 > 1.28 | dm_results$t_h12 > 1.28, ]




################################
### Simple Average Forecasts####
################################


#Try all combinations of benchmark with ML to see if there's any outperformance against benchmark itself

avg_versions <- list(
  "Simple average (All)"    = c("Random Forest Default", "PCA-LASSO", "rLASSO", "Elastic Net (a = 0.5)", "Ridge"),
  "Simple average (exclude Ridge)" = c("Random Forest Default", "PCA-LASSO", "rLASSO", "Elastic Net (a = 0.5)")
)


### Averaged forecasts stored as lists similar to splits above:
###    avg_lists[[average]][[set]][[h_name]][[period]]
avg_lists <- list()

for (a in names(avg_versions)) {                     # loop over the two averages
  for (s in sets) {                                  # loop over predictor sets
    for (h in horizons) {                            # loop over horizons
      for (p in test_periods) {                      # loop over test periods
        h_name <- paste0("h", h)
        
        # Take the dates and actual values from the first model's slice
        first_model <- avg_versions[[a]][1]
        slice <- model_lists[[first_model]][[s]][[h_name]][[p]]
        
        # Put each model's forecasts side by side, one column per model
        preds <- c()
        for (m in avg_versions[[a]]) {
          preds <- cbind(preds, model_lists[[m]][[s]][[h_name]][[p]]$Predicted)
        }
        
        # Average across the models, then recompute the errors
        slice$Predicted      <- rowMeans(preds)
        slice$Forecast_Error <- slice$Actual - slice$Predicted
        slice$SqErr          <- slice$Forecast_Error^2
        
        avg_lists[[a]][[s]][[h_name]][[p]] <- slice
      }
    }
  }
}



### DM Test for each average vs AR(1) and Random Walk
avg_dm_results <- data.frame()

for (b in names(benchmark_lists)) {                  # loop over benchmarks
  for (a in names(avg_lists)) {                      # loop over the two averages
    for (s in sets) {                                # loop over predictor sets
      for (p in test_periods) {                      # loop over test periods
        
        # The random walk has no PostCOVID_dummy slice
        p_bench <- if (b == "Random walk" && p == "PostCOVID_dummy") "PostCOVID" else p
        
        t_stats <- c()                               # one DM t-statistic per horizon
        
        for (h in horizons) {                        # loop over horizons
          h_name <- paste0("h", h)
          
          l_bench <- benchmark_lists[[b]][["CAN"]][[h_name]][[p_bench]]$SqErr
          l_avg   <- avg_lists[[a]][[s]][[h_name]][[p]]$SqErr
          
          # Loss differential, benchmark minus average
          d <- l_bench - l_avg
          
          # DM regression on a constant with Newey-West variance
          dm_reg <- lm(d ~ 1)
          t_stats[h_name] <- as.numeric(dm_reg$coefficients / sqrt(NeweyWest(dm_reg, lag = nw_lag)))
        }
        
        one_row <- data.frame(
          Model     = a,
          Benchmark = b,
          Set       = s,
          Period    = p,
          
          t_h1  = t_stats[["h1"]],
          t_h3  = t_stats[["h3"]],
          t_h6  = t_stats[["h6"]],
          t_h12 = t_stats[["h12"]],
          
          p_h1  = 2 * pnorm(-abs(t_stats[["h1"]])),     # two-sided p-values
          p_h3  = 2 * pnorm(-abs(t_stats[["h3"]])),
          p_h6  = 2 * pnorm(-abs(t_stats[["h6"]])),
          p_h12 = 2 * pnorm(-abs(t_stats[["h12"]]))
        )
        
        avg_dm_results <- rbind(avg_dm_results, one_row)
      }
    }
  }
}

# Positive t -> average had lower loss/outperformed benchmark
avg_dm_results


avg_outperformers_fivepercent <- avg_dm_results[avg_dm_results$t_h1 > 1.96 | avg_dm_results$t_h3 > 1.96 |
                                                  avg_dm_results$t_h6 > 1.96 | avg_dm_results$t_h12 > 1.96, ]
avg_outperformers_tenpercent  <- avg_dm_results[avg_dm_results$t_h1 > 1.64 | avg_dm_results$t_h3 > 1.64 |
                                                  avg_dm_results$t_h6 > 1.64 | avg_dm_results$t_h12 > 1.64, ]
avg_outperformers_twelvepercent  <- avg_dm_results[avg_dm_results$t_h1 > 1.5341 | avg_dm_results$t_h3 > 1.5341 |
                                                  avg_dm_results$t_h6 > 1.5341 | avg_dm_results$t_h12 > 1.5341, ]
avg_outperformers_twentypercent  <- avg_dm_results[avg_dm_results$t_h1 > 1.28 | avg_dm_results$t_h3 > 1.28 |
                                                     avg_dm_results$t_h6 > 1.28 | avg_dm_results$t_h12 > 1.28, ]

##RMSEs for average
#RMSE relative to the random walk (< 1 = beats RW), for the averages and each model

rmse_forecasts <- c(unique(unlist(avg_versions)), names(avg_lists))
avg_rmse_results <- data.frame()

for (f in rmse_forecasts) {                          # loop over single models and averages
  for (s in sets) {                                  # loop over predictor sets
    for (p in test_periods) {                        # loop over test periods
      
      p_rw <- if (p == "PostCOVID_dummy") "PostCOVID" else p
      ratios <- c()                                  # one RMSE ratio per horizon
      
      for (h in horizons) {                          # loop over horizons
        h_name <- paste0("h", h)
        
        # Squared errors of this forecast: from avg_lists for an average, model_lists otherwise
        if (f %in% names(avg_lists)) {
          sq_err <- avg_lists[[f]][[s]][[h_name]][[p]]$SqErr
        } else {
          sq_err <- model_lists[[f]][[s]][[h_name]][[p]]$SqErr
        }
        sq_err_rw <- RWsplit_preds[["CAN"]][[h_name]][[p_rw]]$SqErr
        
        ratios[h_name] <- sqrt(mean(sq_err)) / sqrt(mean(sq_err_rw))
      }
      
      one_row <- data.frame(
        Forecast  = f,
        Set       = s,
        Period    = p,
        ratio_h1  = ratios[["h1"]],
        ratio_h3  = ratios[["h3"]],
        ratio_h6  = ratios[["h6"]],
        ratio_h12 = ratios[["h12"]]
      )
      
      avg_rmse_results <- rbind(avg_rmse_results, one_row)
    }
  }
}

avg_rmse_results

# E.g. one period and set at a time
avg_rmse_results[avg_rmse_results$Period == "PostCOVID" & avg_rmse_results$Set == "CAN", ]


#################################################################################################################
#Ensemble with all combinations benchmarks + ML models then DM test against benchmarks and best ML model (Ridge)#
#################################################################################################################

#all combinations of ML + benchmarks
pool <- c(names(model_lists), names(benchmark_lists))


# One forecast's slice. Benchmarks are stored under the set "CAN", and the
# random walk has no with-dummy version, so its no-dummy forecasts are used there.
get_forecast <- function(f, s, h_name, p) {
  if (f %in% names(benchmark_lists)) {
    p_b <- if (f == "Random walk" && p == "PostCOVID_dummy") "PostCOVID" else p
    benchmark_lists[[f]][["CAN"]][[h_name]][[p_b]]
  } else {
    model_lists[[f]][[s]][[h_name]][[p]]
  }
}

# DM t-stat,  t > 0 means the second forecast has lower loss
dm_t <- function(sq_err_bench, sq_err_model) {
  d <- sq_err_bench - sq_err_model
  dm_reg <- lm(d ~ 1)
  as.numeric(dm_reg$coefficients / sqrt(NeweyWest(dm_reg, lag = nw_lag)))
}

# All combinations of 2 to 8 forecasts, named by their members joined with " + "
combos <- list()
for (k in 2:length(pool)) {
  for (members in combn(pool, k, simplify = FALSE)) {
    combos[[paste(members, collapse = " + ")]] <- members
  }
}
length(combos)   # 247

# Pre-specified averages also exported as prediction files (short name = members)
named_averages <- list(
  "Simple average (All)"            = c("Random Forest Default", "PCA-LASSO", "rLASSO", "Elastic Net (a = 0.5)", "Ridge"),
  "Simple average (exclude Ridge)"  = c("Random Forest Default", "PCA-LASSO", "rLASSO", "Elastic Net (a = 0.5)"),
  "Simple average (all 8)"          = pool
)


### Loop over every combination in every set / period / horizon and get averaged predictions, then DM test
combo_rows <- list()      # one data frame per set / period / horizon, joined at the end
avg_preds  <- list()      # monthly forecasts of the named averages, for the prediction CSV

for (s in sets) {                                    # loop over predictor sets
  for (p in test_periods) {                          # loop over test periods
    for (h in horizons) {                            # loop over horizons
      h_name <- paste0("h", h)
      
      # The 8 forecasts side by side, one column each
      base   <- get_forecast(pool[1], s, h_name, p)
      F      <- matrix(NA, nrow(base), length(pool), dimnames = list(NULL, pool))
      for (f in pool) {
        slice_f <- get_forecast(f, s, h_name, p)
        stopifnot(identical(slice_f$Outcome_Date, base$Outcome_Date))   # same months in the same order
        F[, f] <- slice_f$Predicted
      }
      actual   <- base$Actual
      sq_rw    <- get_forecast("Random walk", s, h_name, p)$SqErr
      sq_ar    <- get_forecast("AR(1)",       s, h_name, p)$SqErr
      sq_ridge <- (actual - F[, "Ridge"])^2 # Ridge was best single ML (h=12)
      rmse_rw  <- sqrt(mean(sq_rw))
      rmse_ar  <- sqrt(mean(sq_ar))
      
      # Every combination: average its members, then compare with the benchmarks
      cell <- data.frame()
      for (cmb in names(combos)) {
        members <- combos[[cmb]]
        avg     <- rowMeans(F[, members])
        sq_err  <- (actual - avg)^2
        t_rw    <- dm_t(sq_rw, sq_err)
        t_ar    <- dm_t(sq_ar, sq_err)
        t_ridge <- dm_t(sq_ridge, sq_err)
        
        cell <- rbind(cell, data.frame(
          Combination        = cmb,
          N_forecasts        = length(members),
          Includes_benchmark = any(members %in% names(benchmark_lists)),
          Set = s, Period = p, Horizon = h,
          RMSE        = sqrt(mean(sq_err)),
          Ratio_vs_RW = sqrt(mean(sq_err)) / rmse_rw,
          t_vs_RW  = t_rw, p_vs_RW  = 2 * pnorm(-abs(t_rw)),
          Ratio_vs_AR = sqrt(mean(sq_err)) / rmse_ar,
          t_vs_AR1 = t_ar, p_vs_AR1 = 2 * pnorm(-abs(t_ar)),
          Ratio_vs_Ridge = sqrt(mean(sq_err)) / sqrt(mean(sq_ridge)),
          t_vs_ridge = t_ridge, p_vs_ridge = 2 * pnorm(-abs(t_ridge))
        ))
      }
      combo_rows[[paste(s, p, h_name)]] <- cell
      
      # Monthly forecasts of the named averages
      for (a in names(named_averages)) {
        avg <- rowMeans(F[, named_averages[[a]]])
        avg_preds[[paste(a, s, p, h_name)]] <- data.frame(
          Model = a, Outcome_Date = base$Outcome_Date, Actual = actual,
          Predicted = avg, Forecast_Error = actual - avg,
          Predictor_Set = s, Horizon = h,
          Period = period[[p]][1], Specification = period[[p]][2]
        )
      }
    }
  }
}

combo_results <- do.call(rbind, combo_rows)
rownames(combo_results) <- NULL

# Combinations significantly better than a benchmark (t > 0), at 5% and 10%
combo_outperform_fivepercent <- combo_results[combo_results$t_vs_RW > 1.96  | combo_results$t_vs_AR1 > 1.96 |combo_results$t_vs_ridge > 1.645, ]
combo_outperform_tenpercent  <- combo_results[combo_results$t_vs_RW > 1.645 | combo_results$t_vs_AR1 > 1.645 |combo_results$t_vs_ridge > 1.645, ]



###Export to CSV###

out_dir <- "results/Result_analysis"

# Ensemble Simple Average RMSE and DM t-stats
write.csv(combo_results,       file.path(out_dir, "Ensemble_Simple_Average Results.csv"),row.names = FALSE)

# Ensemble Results 5% Significance
write.csv(combo_outperform_fivepercent,       file.path(out_dir, "Ensemble Results_five_significance.csv"),row.names = FALSE)

# Ensemble Simple Average RMSE and DM t-stats
write.csv(combo_outperform_tenpercent,       file.path(out_dir, "Ensemble Results_ten_significance.csv"),row.names = FALSE)

############### yihong plots dont delete me please ###################

library(tidyverse)

out_dir <- "results/Result_analysis"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---- 1. Calculate US vs CAN_US (not in existing results) ----
dm_us_canus <- data.frame()

for (m in names(model_lists)) {
  for (p in test_periods) {
    t_stats <- sapply(horizons, function(h) {
      hn <- paste0("h", h)
      us <- model_lists[[m]][["US"]][[hn]][[p]]
      both <- model_lists[[m]][["CAN_US"]][[hn]][[p]]
      
      # Align forecasts by outcome date
      paired <- inner_join(
        us[, c("Outcome_Date", "SqErr")],
        both[, c("Outcome_Date", "SqErr")],
        by = "Outcome_Date", suffix = c("_US", "_CANUS")
      )
      if (!nrow(paired)) stop("No matching forecast dates: ", m, " ", p, " h=", h)
      d <- paired$SqErr_US - paired$SqErr_CANUS
      fit <- lm(d ~ 1)
      v <- as.numeric(sandwich::NeweyWest(fit, lag = nw_lag, prewhite = FALSE))
      
      if (!is.finite(v) || v <= 0) {
        if (all(abs(d - d[1]) < 1e-12)) {
          return(NA_real_)
        }
        stop("Invalid HAC variance: ", m, " | ", p, " | h=", h)
      }
      
      as.numeric(coef(fit)) / sqrt(v)
    })
    
    dm_us_canus <- bind_rows(dm_us_canus, data.frame(
      Model = m, Comparison = "US vs CAN_US",
      Base_Set = "US", Other_Set = "CAN_US", Period = p,
      t_h1 = t_stats[1], t_h3 = t_stats[2],
      t_h6 = t_stats[3], t_h12 = t_stats[4]
    ))
  }
}

# ---- 2. Prepare all three predictor-set comparisons ----
dm_sets_all <- bind_rows(dm_sets_results, dm_us_canus)

model_order <- c("Random Forest Default", "Random Forest Alternate",
                 "PCA-LASSO", "rLASSO", "Elastic Net (a = 0.5)",
                 "Ridge", "Simple average (All)",
                 "Simple average (exclude Ridge)")

period_order <- c("Pre-COVID", "Post-COVID, no dummy",
                  "Post-COVID, with dummy")

prepare_dm <- function(df, row_type = "Model") {
  df %>%
    pivot_longer(cols = matches("^t_h(1|3|6|12)$"),
                 names_to = "Horizon", names_prefix = "t_h",
                 values_to = "t") %>%
    mutate(
      Horizon = factor(paste0("h=", Horizon),
                       levels = paste0("h=", c(1, 3, 6, 12))),
      Panel = factor(recode(Period,
                            PreCOVID = "Pre-COVID",
                            PostCOVID = "Post-COVID, no dummy",
                            PostCOVID_dummy = "Post-COVID, with dummy"),
                     levels = period_order),
      Model = factor(Model, levels = model_order),
      Row = if (row_type == "Model_Set")
        paste(as.character(Model), Set, sep = " | ")
      else as.character(Model)
    ) %>%
    arrange(Model, if ("Set" %in% names(.)) Set) %>%
    mutate(Row = fct_rev(fct_inorder(Row)))
}

# ---- 3. Reusable t-statistic heatmap function ----
plot_dm_t <- function(dat, title, subtitle, filename, height = 5) {
  
  stopifnot(nrow(dat) > 0)
  stopifnot(!anyDuplicated(dat[c("Row", "Horizon", "Panel")]))
  
  t_limit <- max(abs(dat$t), na.rm = TRUE)
  if (!is.finite(t_limit) || t_limit == 0) t_limit <- 1
  
  g <- ggplot(dat, aes(Horizon, Row, fill = t)) +
    geom_tile(colour = "white", linewidth = 0.8) +
    geom_text(aes(
      label = sprintf("%.2f", t),
      colour = abs(t) > 0.6 * t_limit
    ), size = 3.2) +
    scale_colour_manual(
      values = c(`FALSE` = "#1f1f1d", `TRUE` = "white"),
      guide = "none"
    ) +
    scale_fill_gradient2(
      low = "#e34948", mid = "#f0efec", high = "#2a78d6",
      midpoint = 0, limits = c(-t_limit, t_limit),
      name = "DM t-statistic"
    ) +
    facet_wrap(~Panel, nrow = 1) +
    labs(title = title, subtitle = subtitle, x = NULL, y = NULL) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid = element_blank(),
      legend.position = "bottom",
      legend.key.width = unit(2, "cm"),
      strip.text = element_text(face = "bold"),
      axis.text.y = element_text(size = 9)
    )
  
  ggsave(file.path(out_dir, filename), g,
         width = 15, height = height, dpi = 300, bg = "white")
  return(g)
}

# ---- 4. Predictor-set comparisons: three heatmaps ----
for (comp in c("CAN vs CAN_US", "CAN vs US", "US vs CAN_US")) {
  
  dat <- dm_sets_all %>%
    filter(Comparison == comp) %>%
    prepare_dm()
  
  file_tag <- case_when(
    comp == "CAN vs CAN_US" ~ "CAN_vs_CAN_US",
    comp == "CAN vs US" ~ "CAN_vs_US",
    TRUE ~ "US_vs_CAN_US"
  )
  
  plot_dm_t(
    dat,
    title = paste("DM test:", comp),
    subtitle = paste0(
      "Positive t = ", sub(".* vs ", "", comp),
      " has lower squared forecast loss; negative t = ",
      sub(" vs.*", "", comp), " has lower loss"
    ),
    filename = paste0("DM_", file_tag, "_t_heatmap.png"),
    height = 5
  )
}

# ---- 5. Models and simple averages vs benchmarks ----
dm_bench_all <- bind_rows(dm_results, avg_dm_results)

for (bench in unique(dm_bench_all$Benchmark)) {
  
  dat <- dm_bench_all %>%
    filter(Benchmark == bench) %>%
    prepare_dm(row_type = "Model_Set")
  
  bench_tag <- if (bench == "AR(1)") "AR1" else "RW"
  
  plot_dm_t(
    dat,
    title = paste("DM test: Models vs", bench),
    subtitle = "Positive t = model outperforms benchmark; negative t = benchmark outperforms model",
    filename = paste0("DM_vs_", bench_tag, "_t_heatmap.png"),
    height = 12
  )
}


