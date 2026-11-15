# ============================================================
# Employment-rate level forecasts: model comparison plots
# Standalone: run from the project root, after prediction CSVs exist.
# No model fitting or DM testing is performed.
# ============================================================
library(dplyr)
library(tidyr)
library(ggplot2)

# ---------------- USER SETTINGS: EDIT THESE ----------------
model_files <- c(
  'Random Walk' = 'benchmarks/level_random_walk_predictions.csv',
  'AR(1)' = 'AR/level_AR_fixed_p_predictions.csv',
  'Random Forest (Default)' = 'random_forest_default/level_RF_predictions.csv',
  'Random Forest (Tuned)' = 'random_forest_alt/level_RF_alt_predictions.csv',
  'PCA-LASSO' = 'level_PCA_LASSO/level_PCA_LASSO_predictions.csv',
  'rLASSO' = 'level_rLASSO/level_rLASSO_predictions.csv',
  'Elastic Net (a = 0.5)' = 'ElasticNet_a05/level_ElasticNet_a05_predictions.csv',
  'Ridge' = 'Ridge/level_Ridge_predictions.csv'
)

# Remove names here to declutter BOTH types of plots.
selected_models <- c(
  'Random Walk', 'AR(1)',
  'Random Forest (Default)', 'Random Forest (Tuned)',
  'PCA-LASSO', 'rLASSO', 'Elastic Net (a = 0.5)', 'Ridge'
)
selected_horizons <- c(1, 3, 6, 12)
selected_sets <- c('CAN', 'US', 'CAN_US')
selected_periods <- c('Pre_COVID', 'Post_COVID')
# Pre-COVID only has No_Dummy; Post-COVID can have either/both.
selected_specs <- c('No_Dummy', 'With_Dummy')

# Set TRUE to produce each plot as a PNG. Set FALSE to skip that type.
save_prediction_plots <- TRUE
save_cumulative_plots <- TRUE
show_plots_in_RStudio <- FALSE

results_dir <- 'results'
output_dir <- file.path(results_dir, 'Forecast_comparison_plots')
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
# -----------------------------------------------------------

stopifnot(all(selected_models %in% names(model_files)),
          all(selected_horizons %in% c(1,3,6,12)),
          all(selected_sets %in% c('CAN','US','CAN_US')),
          all(selected_periods %in% c('Pre_COVID','Post_COVID')),
          all(selected_specs %in% c('No_Dummy','With_Dummy')))

# Read only selected files, keeping the source name as the model label.
read_predictions <- function(model_name) {
  path <- file.path(results_dir, unname(model_files[[model_name]]))
  if (!file.exists(path)) stop('Missing prediction CSV for ', model_name, ': ', path)
  x <- read.csv(path, stringsAsFactors = FALSE)
  needed <- c('Horizon','Period','Specification','Outcome_Date','Actual','Predicted')
  if (!all(needed %in% names(x)))
    stop(model_name, ': missing columns: ', paste(setdiff(needed, names(x)), collapse = ', '))
  if (!'Predictor_Set' %in% names(x)) {
    if (model_name %in% c('Random Walk', 'AR(1)')) x$Predictor_Set <- 'CAN'
    else stop(model_name, ': Predictor_Set column missing')
  }
  if (model_name == 'AR(1)') {
    if ('AR_Order' %in% names(x)) x <- x[x$AR_Order == 1, , drop = FALSE]
    else if ('Model' %in% names(x)) x <- x[x$Model == 'AR(1)', , drop = FALSE]
    else stop('AR file needs AR_Order or Model to select AR(1).')
  }
  x$Outcome_Date <- as.Date(x$Outcome_Date)
  if (anyNA(x$Outcome_Date)) stop(model_name, ': invalid Outcome_Date')
  x$Horizon <- as.integer(x$Horizon)
  x$Actual <- as.numeric(x$Actual)
  x$Predicted <- as.numeric(x$Predicted)
  x$Plot_Model <- model_name
  x[, c('Plot_Model', 'Predictor_Set', 'Horizon', 'Period',
        'Specification', 'Outcome_Date', 'Actual', 'Predicted')]
}

all_predictions <- dplyr::bind_rows(lapply(selected_models, read_predictions))
# The RW has no meaningful dummy specification: reuse it for both post-COVID panels.
# The same treatment applies to AR(1) only when a genuine dummy fit exists in its CSV.
benchmark_models <- c('Random Walk', 'AR(1)')
if ('Random Walk' %in% selected_models) {
  rw_extra <- all_predictions %>%
    filter(Plot_Model == 'Random Walk', Period == 'Post_COVID',
           Specification == 'No_Dummy') %>%
    mutate(Specification = 'With_Dummy')
  all_predictions <- bind_rows(all_predictions, rw_extra)
}

# Prevent duplicates, including redundant pre-COVID dummy rows.
all_predictions <- all_predictions %>%
  filter(!(Period == 'Pre_COVID' & Specification == 'With_Dummy')) %>%
  distinct(Plot_Model, Predictor_Set, Horizon, Period, Specification,
           Outcome_Date, .keep_all = TRUE)

# Benchmarks have no CAN/US/CAN_US macro predictor panels; use same forecast in each.
bench <- all_predictions %>% filter(Plot_Model %in% benchmark_models)
macro <- all_predictions %>% filter(!Plot_Model %in% benchmark_models)
bench <- bind_rows(lapply(selected_sets, function(s) {
  bench %>% mutate(Predictor_Set = s)
}))
plot_predictions <- bind_rows(macro, bench) %>%
  filter(Predictor_Set %in% selected_sets,
         Horizon %in% selected_horizons,
         Period %in% selected_periods,
         Specification %in% selected_specs)

if (!nrow(plot_predictions)) stop('No matching predictions. Check settings and CSV content.')

# Date alignment is essential for meaningful model comparisons.
# Require all included models to have the same outcome dates and actuals.
plot_predictions <- plot_predictions %>%
  mutate(Squared_Error = (Actual - Predicted)^2)

# Consistent colours for a model across all plots.
model_colors <- c(
  'Random Walk' = '#7F7F7F', 'AR(1)' = '#D55E00',
  'Random Forest (Default)' = '#0072B2',
  'Random Forest (Tuned)' = '#56B4E9',
  'PCA-LASSO' = '#CC79A7', 'rLASSO' = '#009E73',
  'Elastic Net (a = 0.5)' = '#E69F00', 'Ridge' = '#6A51A3'
)

plot_one <- function(h, period, set_name, spec) {
  df <- plot_predictions %>%
    filter(Horizon == h, Period == period,
           Predictor_Set == set_name, Specification == spec)
  if (!nrow(df)) return(invisible(NULL))
  keys <- c('Plot_Model','Outcome_Date')
  if (anyDuplicated(df[keys])) stop('Duplicate model/date rows: ', set_name, ' h=', h)
  # All models must refer to identical dates; never silently compare mismatched samples.
  counts <- df %>% count(Plot_Model)
  n_dates <- n_distinct(df$Outcome_Date)
  if (any(counts$n != n_dates))
    stop('Model outcome dates are not aligned for ', set_name, ', h=', h,
         ', ', period, ', ', spec)
  actual_check <- df %>% group_by(Outcome_Date) %>%
    summarise(spread = max(Actual) - min(Actual), .groups = 'drop')
  if (any(!is.finite(actual_check$spread)) || any(actual_check$spread > 1e-7))
    stop('Actual employment rates differ between models: ', set_name, ' h=', h)
  if (any(!is.finite(df$Predicted))) stop('Nonfinite predictions encountered')

  df <- df %>% arrange(Plot_Model, Outcome_Date) %>%
    group_by(Plot_Model) %>% mutate(Cumulative_Sq_Error = cumsum(Squared_Error)) %>%
    ungroup()
  actual <- df %>% distinct(Outcome_Date, Actual) %>% arrange(Outcome_Date)
  title_suffix <- paste0(set_name, ' | h=', h, ' | ', period, ' | ', spec)
  file_suffix <- paste(set_name, paste0('h',h), period, spec, sep = '_')
  cols <- model_colors[intersect(names(model_colors), unique(df$Plot_Model))]

  p1 <- ggplot() +
    geom_line(data = df, aes(Outcome_Date, Predicted, colour = Plot_Model), linewidth = 0.75) +
    geom_line(data = actual, aes(Outcome_Date, Actual), colour = 'black', linewidth = 1.25) +
    scale_colour_manual(values = cols, breaks = selected_models, drop = TRUE) +
    labs(title = paste('Actual vs predicted employment rate:', title_suffix),
         subtitle = 'Black = actual employment rate; coloured lines = model forecasts',
         x = 'Outcome date', y = 'Canadian employment rate (%)', colour = 'Model') +
    theme_minimal(base_size = 12) +
    theme(legend.position = 'bottom', panel.grid.minor = element_blank()) +
    guides(colour = guide_legend(nrow = 2, byrow = TRUE))

  p2 <- ggplot(df, aes(Outcome_Date, Cumulative_Sq_Error, colour = Plot_Model)) +
    geom_line(linewidth = 0.85) +
    scale_colour_manual(values = cols, breaks = selected_models, drop = TRUE) +
    labs(title = paste('Cumulative squared forecast error:', title_suffix),
         subtitle = 'Sum of squared errors from the start of this evaluation period; lower is better',
         x = 'Outcome date', y = 'Cumulative squared error (percentage points squared)',
         colour = 'Model') +
    theme_minimal(base_size = 12) +
    theme(legend.position = 'bottom', panel.grid.minor = element_blank()) +
    guides(colour = guide_legend(nrow = 2, byrow = TRUE))

  if (save_prediction_plots)
    ggsave(file.path(output_dir, paste0('actual_vs_predicted_', file_suffix, '.png')),
           p1, width = 12, height = 6.5, dpi = 300, bg = 'white')
  if (save_cumulative_plots)
    ggsave(file.path(output_dir, paste0('cumulative_squared_error_', file_suffix, '.png')),
           p2, width = 12, height = 6.5, dpi = 300, bg = 'white')
  if (show_plots_in_RStudio) { print(p1); print(p2) }
  invisible(list(actual_vs_predicted = p1, cumulative_squared_error = p2))
}

for (s in selected_sets) for (h in selected_horizons) for (period in selected_periods) {
  specs_here <- if (period == 'Pre_COVID') 'No_Dummy' else selected_specs
  for (spec in specs_here) {
    cat('Plotting:', s, 'h=', h, period, spec, '\n')
    plot_one(h, period, s, spec)
  }
}
cat('Plots saved in: ', normalizePath(output_dir), '\n', sep = '')

# Example for plotting just one combination interactively:
# show_plots_in_RStudio <- TRUE
# plot_one(12, 'Post_COVID', 'CAN_US', 'With_Dummy')
