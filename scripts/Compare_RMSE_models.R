# EC4308: reproducible RMSE comparison across group members' results
# Run from the project root (where the results/ folder is located).
# Packages: install.packages(c('dplyr','tidyr','readr'))
library(dplyr)
library(tidyr)
library(readr)

results_dir <- 'results'
out_dir <- file.path(results_dir, 'comparison')
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Convert a wide table with columns Pre_COVID, Post_No_Dummy, etc. to long format.
wide_to_long <- function(path, model, predictor_set = 'AR_only') {
  x <- read_csv(path, show_col_types = FALSE)
  if (!'Predictor_Set' %in% names(x)) x$Predictor_Set <- predictor_set
  if (!'Horizon' %in% names(x)) stop('No Horizon column: ', path)
  if ('AR_Order' %in% names(x)) x$Model <- paste0('AR(', x$AR_Order, ')')
  else x$Model <- model
  if (!'Pre_COVID' %in% names(x)) stop('Expected wide RMSE columns in ', path)
  x %>%
    pivot_longer(cols = any_of(c('Pre_COVID','Post_No_Dummy','Post_With_Dummy',
                                 'All_No_Dummy','All_With_Dummy','Post_COVID','All')),
                 names_to = 'Category', values_to = 'RMSE') %>%
    mutate(Horizon = as.integer(gsub('[^0-9]', '', as.character(Horizon))),
           Period = case_when(Category == 'Pre_COVID' ~ 'Pre_COVID',
                              grepl('^Post', Category) ~ 'Post_COVID',
                              grepl('^All', Category) ~ 'All'),
           Specification = if_else(grepl('With_Dummy$', Category), 'With_Dummy', 'No_Dummy')) %>%
    select(Model, Predictor_Set, Horizon, Period, Specification, RMSE)
}

# Read a long table already containing Period, Specification and RMSE.
long_to_common <- function(path, model, predictor_set = 'AR_only') {
  x <- read_csv(path, show_col_types = FALSE)
  if (!'Model' %in% names(x)) x$Model <- model
  if (!'Predictor_Set' %in% names(x)) x$Predictor_Set <- predictor_set
  x %>% transmute(Model = as.character(Model), Predictor_Set = as.character(Predictor_Set),
                  Horizon = as.integer(gsub('[^0-9]', '', as.character(Horizon))),
                  Period = as.character(Period), Specification = as.character(Specification),
                  RMSE = as.numeric(RMSE))
}

# Use one authoritative RMSE file per model: avoid counting long/wide duplicates.
files <- list(
  AR_fixed = 'AR_fixed_p_RMSE_long.csv',
  AR_dynamic = 'AR_dynamic_p_RMSE_long.csv',
  RF_default = 'RF_default_RMSE_summary.csv',
  LASSO = 'LASSO/LASSO_RMSE_summary.csv',
  PCA_LASSO = 'PCA_LASSO/PCA_LASSO_RMSE_summary.csv',
  ElasticNet_a05 = 'ElasticNet_a05/ElasticNet_a05_RMSE_summary.csv',
  ElasticNet_tuned = 'ElasticNet_tuned/ElasticNet_tuned_RMSE_summary.csv',
  Change_Persistence = 'Change_Persistence_RMSE_long.csv',
  Zero_Change = "zero_change_RMSE_long.csv",
  Historical_Average_Rolling = 'Historical_Average_Rolling_RMSE_long.csv'
)

missing_files <- names(files)[!file.exists(file.path(results_dir, unlist(files)))]
if (length(missing_files)) stop('Missing required files: ', paste(missing_files, collapse = ', '))

long_models <- c("AR_fixed", "AR_dynamic", "LASSO", "PCA_LASSO", "ElasticNet_a05", "ElasticNet_tuned", "Change_Persistence", "Historical_Average_Rolling", "Zero_Change")

rmse_master <- bind_rows(lapply(names(files), function(m) {
  path <- file.path(results_dir, files[[m]])
  x <- if (m %in% long_models) long_to_common(path, m) else wide_to_long(path, m)
  # Standardize model labels without discarding AR(p) identities.
  if (m != 'AR_fixed') x$Model <- m
  x$Source_File <- files[[m]]
  x
})) %>%
  filter(!(Period == 'Pre_COVID' & Specification == 'With_Dummy')) %>%
  arrange(Horizon, Model, Predictor_Set, Period, Specification)

# Ensure no duplicate RMSE records after normalization.
key_cols <- c('Model','Predictor_Set','Horizon','Period','Specification')
duplicates <- rmse_master %>% count(across(all_of(key_cols))) %>% filter(n > 1)
if (nrow(duplicates)) {
  print(duplicates)
  stop('Duplicate model/period/specification keys found. Resolve before comparing.')
}
stopifnot(all(is.finite(rmse_master$RMSE)), all(rmse_master$RMSE >= 0))
write_csv(rmse_master, file.path(out_dir, 'RMSE_master.csv'))

# Choose one AR order per horizon using ONLY the pre-COVID no-dummy RMSE.
selected_ar <- rmse_master %>%
  filter(grepl('^AR\\([0-9]+\\)$', Model), Period == 'Pre_COVID',
         Specification == 'No_Dummy') %>%
  group_by(Horizon) %>% slice_min(RMSE, n = 1, with_ties = FALSE) %>% ungroup() %>%
  select(Horizon, Selected_AR = Model, Selection_RMSE = RMSE)
write_csv(selected_ar, file.path(out_dir, 'AR_preCOVID_selected_orders.csv'))

ar_selected <- rmse_master %>%
  inner_join(selected_ar, by = 'Horizon') %>%
  filter(Model == Selected_AR) %>%
  mutate(Model = paste0('AR_fixed_preselected_', Selected_AR)) %>%
  select(-Selected_AR, -Selection_RMSE)

# Primary comparison excludes the other 11 AR orders at each horizon.
rmse_comparison <- bind_rows(
  rmse_master %>% filter(!grepl('^AR\\([0-9]+\\)$', Model)), ar_selected
) %>% arrange(Horizon, Period, Specification, RMSE)
write_csv(rmse_comparison, file.path(out_dir, 'RMSE_comparison_preselected_AR.csv'))

# Ranking: compare like with like, keeping predictor sets distinct.
rankings <- rmse_comparison %>%
  group_by(Horizon, Period, Specification) %>%
  arrange(RMSE, .by_group = TRUE) %>% mutate(Rank = row_number()) %>% ungroup()
write_csv(rankings, file.path(out_dir, 'RMSE_rankings.csv'))

# Core research question: does CAN_US beat CAN within each ML method?
us_incremental <- rmse_comparison %>%
  filter(Predictor_Set %in% c('CAN','CAN_US')) %>%
  select(Model, Horizon, Period, Specification, Predictor_Set, RMSE) %>%
  pivot_wider(names_from = Predictor_Set, values_from = RMSE) %>%
  filter(!is.na(CAN), !is.na(CAN_US)) %>%
  mutate(RMSE_Reduction = CAN - CAN_US,
         Percent_Reduction = 100 * (CAN - CAN_US) / CAN,
         CAN_US_Better = CAN_US < CAN) %>%
  arrange(Model, Horizon, Period, Specification)
write_csv(us_incremental, file.path(out_dir, 'CAN_vs_CAN_US.csv'))

# Do dummy controls improve post-COVID / all-period RMSE?
dummy_comparison <- rmse_comparison %>%
  filter(Period != 'Pre_COVID') %>%
  select(Model, Predictor_Set, Horizon, Period, Specification, RMSE) %>%
  pivot_wider(names_from = Specification, values_from = RMSE) %>%
  filter(!is.na(No_Dummy), !is.na(With_Dummy)) %>%
  mutate(RMSE_Reduction = No_Dummy - With_Dummy,
         Percent_Reduction = 100 * RMSE_Reduction / No_Dummy,
         Dummy_Better = With_Dummy < No_Dummy)
write_csv(dummy_comparison, file.path(out_dir, 'COVID_dummy_comparison.csv'))

# Basic checks of whether each model has 4 horizons and expected periods.
coverage <- rmse_comparison %>%
  count(Model, Predictor_Set, Horizon, name = 'RMSE_Rows') %>%
  arrange(Model, Predictor_Set, Horizon)
write_csv(coverage, file.path(out_dir, 'RMSE_coverage.csv'))

cat('\nDone. Comparison files saved in:', out_dir, '\n')
cat('Models:', paste(unique(rmse_comparison$Model), collapse = ', '), '\n')
cat('Master rows:', nrow(rmse_master), '\n')
cat('Comparison rows:', nrow(rmse_comparison), '\n')
print(selected_ar)
print(rankings %>% filter(Rank == 1) %>% select(Horizon, Period, Specification, Model, Predictor_Set, RMSE), n = Inf)

# IMPORTANT: These are descriptive comparisons of supplied RMSE tables.
# Before formal DM tests, verify matching forecast origins/outcome dates and targets
# using the predictions CSVs. No zero-change RMSE file was found in this ZIP.
