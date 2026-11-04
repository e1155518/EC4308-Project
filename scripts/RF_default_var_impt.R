library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

importance_list <- readRDS("results/RF_variable_importance.rds")
metadata <- read_excel("data/master_metadata.xlsx", sheet = "master")

metadata <- metadata %>%
  transmute(
    country = toupper(trimws(country)),
    variable = trimws(variable),
    group = as.character(group),
    category = `group name`
  ) %>%
  distinct(country, variable, .keep_all = TRUE)

# Keep category names in metadata order
category_levels <- metadata %>%
  arrange(as.numeric(group)) %>%
  pull(category) %>%
  unique()

# Consistent colours across all charts
category_colors <- setNames(
  c("#4477AA", "#EE6677", "#228833", "#CCBB44",
    "#66CCEE", "#AA3377", "#BBBBBB", "#EE8866"),
  category_levels
)

stopifnot(length(category_levels) == 8)


importance_long <- bind_rows(
  lapply(names(importance_list), function(key) {
    extract_importance(key, importance_list[[key]])
  })
)

importance_matched <- importance_long %>%
  left_join(
    metadata,
    by = c("country", "variable")
  )

# Check unmatched predictors
unmatched <- importance_matched %>%
  filter(is.na(category)) %>%
  distinct(country, variable) %>%
  arrange(country, variable)

print(unmatched, n = Inf)

importance_matched <- importance_matched %>%
  filter(!is.na(category))
importance_matched
