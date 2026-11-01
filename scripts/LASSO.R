# ============================================================
# LASSO / Elastic Net on raw (lagged) predictors
# Canadian employment-rate change, CAN / US / CAN+US panels
# Vismay's component.
# Run from the EC4308 project root (where the data/ and functions/ folders exist).
# install.packages(c("readxl","glmnet"))
# ============================================================

library(glmnet)
source("functions/data_prep.R")

panels <- load_panels()
raw <- panels$raw
predictor_cols <- panels$predictor_cols

# Quick sanity check when sourcing interactively:
# sapply(predictor_cols, length)   # CAN / US / CAN_US predictor counts

# ---- next: runlasso() goes here (PCA-free, raw lagged predictors + active dummy) ----
