# ============================================================
# LASSO on principal components only (no raw predictors)
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

# ---- next: runpcalasso() goes here (PCA on the predictor block, LASSO on the PCs + active dummy) ----
