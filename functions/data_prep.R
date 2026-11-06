# Shared data prep for LASSO.R and PCA_LASSO.R (same panels, dates and targets for both)
library(readxl)

horizons <- c(1, 3, 6, 12)
n_lags   <- 4
pre_end  <- as.Date("2019-12-01"); n_pre  <- 100   # pre-COVID evaluation: last 100 outcomes up to Dec 2019
post_end <- as.Date("2025-09-01"); n_post <- 69    # post-COVID evaluation: Jan 2020 to Sep 2025

files <- c(CAN    = "data/CAN_data_cleaned.xlsx",
           US     = "data/US_data_cleaned.xlsx",
           CAN_US = "data/CAN_US_data_cleaned.xlsx")

# Fractional COVID dummies: share of the (t, t+h] outcome window inside each COVID phase
add_covid_dummies <- function(d) {
  shock    <- d$Date >= as.Date("2020-03-01") & d$Date <= as.Date("2020-04-01")
  recovery <- d$Date >= as.Date("2020-05-01") & d$Date <= as.Date("2022-02-01")
  for (h in horizons) {
    t <- seq_len(nrow(d) - h)
    d[[paste0("COVID_Shock_h", h)]]    <- c(sapply(t, function(i) mean(shock[(i + 1):(i + h)])),    rep(NA, h))
    d[[paste0("COVID_Recovery_h", h)]] <- c(sapply(t, function(i) mean(recovery[(i + 1):(i + h)])), rep(NA, h))
  }
  d
}

load_panels <- function() {
  raw <- lapply(files, function(f) {
    d <- as.data.frame(read_excel(f))
    d$Date <- as.Date(d$Date)
    stopifnot(identical(d$Date, seq(min(d$Date), max(d$Date), by = "month")))  # clean monthly dates
    for (h in horizons) d[[paste0("Employ_Rate_lead_h", h)]] <- c(tail(d$Employ_Rate, -h), rep(NA, h))  # level h months ahead (level targets)
    add_covid_dummies(d)
  })
  # Predictors: everything except Date, employment targets/level and COVID columns,
  # plus last month's employment change (a legitimate autoregressive predictor)
  predictor_cols <- lapply(raw, function(d)
    c("Employ_Rate_diff_lag1", grep("^(Date|Employ_Rate|COVID_)", names(d), value = TRUE, invert = TRUE)))
  list(raw = raw, predictor_cols = predictor_cols)
}
