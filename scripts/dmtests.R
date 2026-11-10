#install.packages("sandwich")
library(sandwich) #library to estimate variance for DM test regression using NeweyWest()

########################################################
###Loading of all the predictions - 


in_dir <- "results/Predictions"


RF_def_pred <- read.csv(file.path(in_dir,"level_RF_default_predictions.csv"), na = "NA")
RF_alt_pred <- read.csv(file.path(in_dir, "level_RF_alt_predictions.csv"), na = "NA")
PCA_LASSO_pred <- read.csv(file.path(in_dir,"level_PCA_LASSO_predictions.csv"), na = "NA")
rLASSO_pred <- read.csv(file.path(in_dir,"level_rLASSO_predictions.csv"), na = "NA")
Elnet_pred <- read.csv(file.path(in_dir,"level_ElasticNet_a05_predictions.csv"), na = "NA")
Ridge_pred <- read.csv(file.path(in_dir,"level_Ridge_predictions.csv"), na = "NA")

RW_pred <- read.csv(file.path(in_dir,"level_random_walk_predictions.csv"), na = "NA")
AR_pred <- read.csv(file.path(in_dir,"level_AR_fixed_p_predictions.csv"), na = "NA")

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

lrw1pre = (RWsplit_preds[["CAN"]][["h1"]][["PreCOVID"]]$SqErr)
lrw3pre = (RWsplit_preds[["CAN"]][["h3"]][["PreCOVID"]]$SqErr)
lrw6pre = (RWsplit_preds[["CAN"]][["h6"]][["PreCOVID"]]$SqErr)
lrw12pre = (RWsplit_preds[["CAN"]][["h12"]][["PreCOVID"]]$SqErr)


lar1pre = (ARsplit_preds[["CAN"]][["h1"]][["PreCOVID"]]$SqErr)
lar3pre = (ARsplit_preds[["CAN"]][["h3"]][["PreCOVID"]]$SqErr)
lar6pre = (ARsplit_preds[["CAN"]][["h6"]][["PreCOVID"]]$SqErr)
lar12pre = (ARsplit_preds[["CAN"]][["h12"]][["PreCOVID"]]$SqErr)

lrw1post = (RWsplit_preds[["CAN"]][["h1"]][["PostCOVID"]]$SqErr)
lrw3post = (RWsplit_preds[["CAN"]][["h3"]][["PostCOVID"]]$SqErr)
lrw6post = (RWsplit_preds[["CAN"]][["h6"]][["PostCOVID"]]$SqErr)
lrw12post = (RWsplit_preds[["CAN"]][["h12"]][["PostCOVID"]]$SqErr)


lar1post = (ARsplit_preds[["CAN"]][["h1"]][["PostCOVID"]]$SqErr)
lar3post = (ARsplit_preds[["CAN"]][["h3"]][["PostCOVID"]]$SqErr)
lar6post = (ARsplit_preds[["CAN"]][["h6"]][["PostCOVID"]]$SqErr)
lar12post = (ARsplit_preds[["CAN"]][["h12"]][["PostCOVID"]]$SqErr)


##################################################################
#Set up for the tests of Model vs. AR benchmark #### CAN ONLY ####
##################################################################


##AR-RF PRE##

#Compute squared loss for different horizons (RF)
lrf1pre=(RFdefsplit_preds[["CAN"]][["h1"]][["PreCOVID"]]$SqErr)
lrf3pre=(RFdefsplit_preds[["CAN"]][["h3"]][["PreCOVID"]]$SqErr)
lrf6pre=(RFdefsplit_preds[["CAN"]][["h6"]][["PreCOVID"]]$SqErr)
lrf12pre=(RFdefsplit_preds[["CAN"]][["h12"]][["PreCOVID"]]$SqErr)


#Compute loss differentials (d_t) for different horizons (AR-RF)
darrf1pre=lar1pre-lrf1pre
darrf3pre=lar3pre-lrf3pre
darrf6pre=lar6pre-lrf6pre
darrf12pre=lar12pre-lrf12pre

#Create ts object containing loss differentials
dtarrfpre.ts=ts(cbind(darrf1pre,darrf3pre,darrf6pre,darrf12pre), start=c(2011,9), end=c(2019,12), freq=12)
colnames(dtarrfpre.ts)=c("1-step dt","3-step dt","6-step dt","12-step dt")
#Plot them to examine stationarity:
windows()
plot.ts(dtarrfpre.ts, main="Loss differential AR-RF",cex.axis=1.8)

#Regress d_t (AR-RF) for 1-step forecasts on a constant - get estimate of mean(d_t)
dmarrf1pre=lm(darrf1pre~1) #regression
acf(dmarrf1pre$residuals) #check serial correlation of residuals - number of significant autocorrelations is a good guess for number lags included in the HAC variance estimator
t_dmarrf1pre  <- as.numeric(dmarrf1pre$coefficients/sqrt(NeweyWest(dmarrf1pre,lag=5))) #form the DM t-statistic
#Use Lag = 100 ^ (1/3) ~ 5

#3-step forecast test
dmarrf3pre=lm(darrf3pre~1)
acf(dmarrf3pre$residuals)
t_dmarrf3pre  <- as.numeric(dmarrf3pre$coefficients/sqrt(NeweyWest(dmarrf3pre,lag=5)))

#6-step forecast test
dmarrf6pre=lm(darrf6pre~1)
acf(dmarrf6pre$residuals)
t_dmarrf6pre  <- as.numeric(dmarrf6pre$coefficients/sqrt(NeweyWest(dmarrf6pre,lag=5)))

#12-step forecast test
dmarrf12pre=lm(darrf12pre~1)
acf(dmarrf12pre$residuals) #7 significant lags, but doesn't a big diff
t_dmarrf12pre  <- as.numeric(dmarrf12pre$coefficients/sqrt(NeweyWest(dmarrf12pre,lag=5))) 

dmarrfpre_results <- data.frame(
  Model     = "Random Forest",
  Benchmark = "AR(1)",
  Set       = "CAN",
  Period    = "PreCOVID",
  
  t_h1  = t_dmarrf1pre,
  t_h3  = t_dmarrf3pre,
  t_h6  = t_dmarrf6pre,
  t_h12 = t_dmarrf12pre,
  
  p_h1  = 2 * pnorm(-abs(t_dmarrf1pre)),     # two-sided p-values
  p_h3  = 2 * pnorm(-abs(t_dmarrf3pre)),
  p_h6  = 2 * pnorm(-abs(t_dmarrf6pre)),
  p_h12 = 2 * pnorm(-abs(t_dmarrf12pre))
)


##RW-RF PRE##

#Compute loss differentials (d_t) for different horizons (RW-RF)
drwrf1pre=lrw1pre-lrf1pre
drwrf3pre=lrw3pre-lrf3pre
drwrf6pre=lrw6pre-lrf6pre
drwrf12pre=lrw12pre-lrf12pre

#Create ts object containing loss differentials
dtrwrfpre.ts=ts(cbind(drwrf1pre,drwrf3pre,drwrf6pre,drwrf12pre), start=c(2011,9), end=c(2019,12), freq=12)
colnames(dtrwrfpre.ts)=c("1-step dt","3-step dt","6-step dt","12-step dt")
#Plot them to examine stationarity:
windows()
plot.ts(dtrwrfpre.ts, main="Loss differential RW-RF",cex.axis=1.8)

#Regress d_t (RW-RF) for 1-step forecasts on a constant - get estimate of mean(d_t)
dmrwrf1pre=lm(drwrf1pre~1) #regression
acf(dmrwrf1pre$residuals) #check serial correlation of residuals - number of significant autocorrelations is a good guess for number lags included in the HAC variance estimator
t_dmrwrf1pre  <- as.numeric(dmrwrf1pre$coefficients/sqrt(NeweyWest(dmrwrf1pre,lag=5))) #form the DM t-statistic
#Use Lag = 100 ^ (1/3) ~ 5

#3-step forecast test
dmrwrf3pre=lm(drwrf3pre~1)
acf(dmrwrf3pre$residuals)
t_dmrwrf3pre <- as.numeric(dmrwrf3pre$coefficients/sqrt(NeweyWest(dmrwrf3pre,lag=5)))

#6-step forecast test
dmrwrf6pre=lm(drwrf6pre~1)
acf(dmrwrf6pre$residuals)
t_dmrwrf6pre  <- as.numeric(dmrwrf6pre$coefficients/sqrt(NeweyWest(dmrwrf6pre,lag=5)))

#12-step forecast test
dmrwrf12pre=lm(drwrf12pre~1)
acf(dmrwrf12pre$residuals) #7 significant lags, but doesn't a big diff
t_dmrwrf12pre <- as.numeric(dmrwrf12pre$coefficients/sqrt(NeweyWest(dmrwrf12pre,lag=5))) 

dmrwrfpre_results <- data.frame(
  Model     = "Random Forest",
  Benchmark = "Random Walk",
  Set       = "CAN",
  Period    = "PreCOVID",
  
  t_h1  = t_dmrwrf1pre,
  t_h3  = t_dmrwrf3pre,
  t_h6  = t_dmrwrf6pre,
  t_h12 = t_dmrwrf12pre,
  
  p_h1  = 2 * pnorm(-abs(t_dmrwrf1pre)),     # two-sided p-values
  p_h3  = 2 * pnorm(-abs(t_dmrwrf3pre)),
  p_h6  = 2 * pnorm(-abs(t_dmrwrf6pre)),
  p_h12 = 2 * pnorm(-abs(t_dmrwrf12pre))
)


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



################################################################
### CAN VS CAN_US####
###CAN vs CAN_US  and  CAN vs US
### d = SqErr(CAN) - SqErr(other set): t > 0 means the other set (CAN_US or US) beats Canada-only
##################################################################

# CAN vs Other Set
set_pairs <- list(
  "CAN vs CAN_US" = c("CAN", "CAN_US"),
  "CAN vs US"     = c("CAN", "US")
)

dm_sets_results <- data.frame()

for (m in names(model_lists)) {                      # loop over models
  for (pr in names(set_pairs)) {                     # loop over set comparisons
    for (p in test_periods) {                        # loop over test periods
      
      base_set  <- set_pairs[[pr]][1]                # "CAN"
      other_set <- set_pairs[[pr]][2]                # "CAN_US" or "US"
      
      t_stats <- c()                                 # one DM t-statistic per horizon
      
      for (h in horizons) {                          # loop over horizons
        h_name <- paste0("h", h)
        
        # Squared losses of the same model with the two predictor sets
        l_base  <- model_lists[[m]][[base_set]][[h_name]][[p]]$SqErr
        l_other <- model_lists[[m]][[other_set]][[h_name]][[p]]$SqErr
        
        # Loss differential, CAN minus the other set
        d <- l_base - l_other
        
        # DM regression on a constant with Newey-West variance
        dm_reg <- lm(d ~ 1)
        t_stats[h_name] <- as.numeric(dm_reg$coefficients / sqrt(NeweyWest(dm_reg, lag = nw_lag)))
      }
      
      # One row per model / comparison / period
      one_row <- data.frame(
        Model      = m,
        Comparison = pr,
        Base_Set   = base_set,
        Other_Set  = other_set,
        Period     = p,
        
        t_h1  = t_stats[["h1"]],
        t_h3  = t_stats[["h3"]],
        t_h6  = t_stats[["h6"]],
        t_h12 = t_stats[["h12"]],
        
        p_h1  = 2 * pnorm(-abs(t_stats[["h1"]])),     # two-sided p-values
        p_h3  = 2 * pnorm(-abs(t_stats[["h3"]])),
        p_h6  = 2 * pnorm(-abs(t_stats[["h6"]])),
        p_h12 = 2 * pnorm(-abs(t_stats[["h12"]]))
      )
      
      dm_sets_results <- rbind(dm_sets_results, one_row)
      
    }
  }
}

# t > 0: using the other set (CAN_US or US) gives lower loss than Canada-only
dm_sets_results

# Significant at 5% in either direction
us_helps_fivepercent   <- dm_sets_results[dm_sets_results$t_h1 > 1.96 | dm_sets_results$t_h3 > 1.96 |
                                    dm_sets_results$t_h6 > 1.96 | dm_sets_results$t_h12 > 1.96, ]

us_helps_tenpercent   <- dm_sets_results[dm_sets_results$t_h1 > 1.64 | dm_sets_results$t_h3 > 1.64 |
                                    dm_sets_results$t_h6 > 1.64 | dm_sets_results$t_h12 > 1.64, ]


#########################################
library(tidyverse)

out_dir <- "results/Result_analysis"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Prepare DM results
dm_plot <- dm_sets_results %>%
  filter(Comparison == "CAN vs CAN_US") %>%
  pivot_longer(
    cols = matches("^[tp]_h"),
    names_to = c("Statistic", "Horizon"),
    names_pattern = "([tp])_h(\\d+)",
    values_to = "Value"
  ) %>%
  pivot_wider(names_from = Statistic, values_from = Value) %>%
  mutate(
    Horizon = factor(paste0("h=", Horizon),
                     levels = paste0("h=", c(1, 3, 6, 12))),
    Panel = factor(
      recode(Period,
             PreCOVID = "Pre-COVID",
             PostCOVID = "Post-COVID, no dummy",
             PostCOVID_dummy = "Post-COVID, with dummy"),
      levels = c("Pre-COVID", "Post-COVID, no dummy",
                 "Post-COVID, with dummy")
    ),
    Model = factor(Model, levels = rev(c(
      "Random Forest Default", "Random Forest Alternate",
      "Ridge", "Elastic Net (a = 0.5)", "PCA-LASSO", "rLASSO"
    )))
  )

# ---- 1. DM t-statistic heatmap ----

t_limit <- max(abs(dm_plot$t), na.rm = TRUE)

g_dm_t <- ggplot(dm_plot, aes(Horizon, Model, fill = t)) +
  geom_tile(colour = "white", linewidth = 0.8) +
  geom_text(aes(label = sprintf("%.2f", t),
                colour = abs(t) > 0.6 * t_limit), size = 3.3) +
  scale_colour_manual(values = c(`FALSE` = "#1f1f1d", `TRUE` = "white"),
                      guide = "none") +
  scale_fill_gradient2(low = "#e34948", mid = "#f0efec", high = "#2a78d6",
                       midpoint = 0, limits = c(-t_limit, t_limit),
                       name = "DM t-statistic") +
  facet_wrap(~Panel, nrow = 1) +
  labs(title = "DM test: CAN vs CAN_US",
       subtitle = "Positive t = CAN_US performs better; negative t = CAN performs better",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(), legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        strip.text = element_text(face = "bold"))

ggsave(file.path(out_dir, "DM_CAN_vs_CAN_US_t_heatmap.png"),
       g_dm_t, width = 13, height = 4.5, dpi = 300, bg = "white")










