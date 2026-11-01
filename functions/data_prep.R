# ============================================================
# Shared data-prep for LASSO.R and PCA_LASSO.R
# Centralizing this guarantees both scripts agree on forecast-origin/outcome-date
# convention, test periods, target definitions and predictor information sets
# (per the team's Stage 2 standardization recommendations), since they source
# the same code rather than two hand-copied versions.
# ============================================================

library(readxl)

# ---- shared conventions ----
horizons <- c(1L, 3L, 6L, 12L)
n_lags   <- 4L      # matches the professor's template and the group's RF script
pre_end  <- as.Date("2019-12-01")
post_end <- as.Date("2025-09-01")
n_pre    <- 100L
n_post   <- 69L

files <- c(CAN = "data/CAN_data_cleaned.xlsx",
           US  = "data/US_data_cleaned.xlsx",
           CAN_US = "data/CAN_US_data_cleaned.xlsx")

# ---- 1. Load & sanity-check each panel (same convention as RF_v1.R) ----
prepare_data <- function(file, label) {
  d <- as.data.frame(read_excel(file))
  if (!"Date" %in% names(d)) stop(label, ": missing Date column")
  d$Date <- as.Date(d$Date)
  d <- d[order(d$Date), , drop = FALSE]
  if (anyNA(d$Date) || anyDuplicated(d$Date) ||
      !identical(d$Date, seq(min(d$Date), max(d$Date), by = "month")))
    stop(label, ": dates must be consecutive, unique monthly observations")
  d
}

# ---- 2. Build the fractional COVID shock/recovery columns, per horizon ----
# For an origin at time t forecasting h steps ahead, the dummy records what
# *fraction* of the (t, t+h] outcome window falls inside the shock / recovery
# period. h=1 gives an ordinary 0/1 dummy; h>1 blends partial overlap. This
# describes a TRAINING row's already-observed outcome -- it is NOT used at
# live-forecast time, where the dummy columns get set to 0 instead (handled
# in each model's forecasting function via the "active" check).
add_covid_fractions <- function(d) {
  shock    <- as.integer(d$Date >= as.Date("2020-03-01") & d$Date <= as.Date("2020-04-01"))
  recovery <- as.integer(d$Date >= as.Date("2020-05-01") & d$Date <= as.Date("2022-02-01"))
  for (h in horizons) {
    valid <- seq_len(nrow(d) - h)
    z1 <- z2 <- rep(NA_real_, nrow(d))
    z1[valid] <- vapply(valid, function(t) mean(shock[(t + 1L):(t + h)]), numeric(1))
    z2[valid] <- vapply(valid, function(t) mean(recovery[(t + 1L):(t + h)]), numeric(1))
    d[[paste0("COVID_Shock_h", h)]]    <- z1
    d[[paste0("COVID_Recovery_h", h)]] <- z2
  }
  d
}

# ---- 3. Decide the predictor set for a panel ----
# Exclude: Date, every Employ_Rate* target/level column (forward info or the level
# itself), and the COVID_* columns (supplied separately as "dum", not mixed into
# the ordinary predictor matrix). Re-include Employ_Rate_diff_lag1 on purpose: it's
# the one-month-*behind* change, a legitimate autoregressive predictor, consistent
# with the AR benchmark and RF script.
target_family <- c("Employ_Rate", "Employ_Rate_diff_lag1",
                    paste0("Employ_Rate_diff_h", horizons))

build_predictor_cols <- function(d) {
  cols <- setdiff(names(d), c("Date", target_family, grep("^COVID_", names(d), value = TRUE)))
  unique(c("Employ_Rate_diff_lag1", cols))
}

# ---- convenience: load all three panels in one call ----
load_panels <- function() {
  raw <- Map(prepare_data, files, names(files))
  raw <- lapply(raw, add_covid_fractions)
  predictor_cols <- lapply(raw, build_predictor_cols)
  list(raw = raw, predictor_cols = predictor_cols)
}
