# How many variables do rLASSO, PCA-LASSO and Elastic Net keep in each rolling window, and which ones?
# Reads the cached batches of level_rLASSO.R and level_PCA_LASSO.R plus the saved Elastic Net counts, so nothing is refitted.
# Elastic Net: only the count per window was saved, so it is in the plot and summary but not in the selected-terms file.
# Run from the project root.
# Outputs (results/Result_analysis): LASSO_variable_count_over_time.png, LASSO_variable_count_summary.csv,
# LASSO_selected_terms.csv (every term kept, with how often). COVID dummies are not counted.
library(tidyverse); library(readxl)
source("functions/data_prep.R")                       # horizons, n_lags, load_panels()
panels  <- load_panels()
n_pcs   <- 10L                                        # as in level_PCA_LASSO.R
out_dir <- "results/Result_analysis"
dir.create(out_dir, showWarnings = FALSE)
hlab <- function(h) factor(paste0("h=", h), paste0("h=", horizons))

models <- c("rLASSO" = "level_rLASSO", "PCA-LASSO" = "level_PCA_LASSO")
specs  <- c(Pre_COVID_No_Dummy = "Pre-COVID", Post_COVID_No_Dummy = "Post-COVID, no dummy",
            Post_COVID_With_Dummy = "Post-COVID, with dummy")
sets   <- c("CAN", "US", "CAN_US")

# ---- One row per rolling window: the terms kept (and how many) ----
windows <- expand_grid(Model = names(models), Predictor_Set = sets, Horizon = horizons, Spec = names(specs)) %>%
  pmap_dfr(function(Model, Predictor_Set, Horizon, Spec) {
    b <- readRDS(sprintf("results/%s/%s_batches/%s_%d_%s.rds", models[[Model]], models[[Model]], Predictor_Set, Horizon, Spec))
    if (Model == "rLASSO") b <- b$rLASSO                # rLASSO batches hold one entry per method
    kept <- map(b$selected, ~ .x[!grepl("^COVID_", .x)])
    tibble(Model, Predictor_Set, Horizon, Spec, Origin = b$forecasts$Forecast_Origin,
           Kept  = lengths(kept),
           PCs   = map_int(kept, ~ sum(grepl("^PC[0-9]+_lag", .x))),
           US    = map_int(kept, ~ sum(grepl("^US_", .x))),    # only meaningful in the CAN_US panel
           Terms = kept)
  }) %>%
  bind_rows(
    # Elastic Net (alpha = 0.5), teammate's run: count per window. With-dummy counts can include up to 2 COVID dummies.
    read_csv("results/ElasticNet_a05/level_ElasticNet_a05_lambda_choice.csv", show_col_types = FALSE) %>%
      filter(!(Period == "Pre_COVID" & Specification == "With_Dummy")) %>%       # pre-COVID has no dummy fit (duplicated rows)
      transmute(Model = "Elastic Net", Predictor_Set, Horizon, Spec = paste(Period, Specification, sep = "_"),
                Origin = Forecast_Origin, Kept = N_Selected, PCs = 0L, US = NA_integer_, Terms = list(NULL))) %>%
  mutate(Model = factor(Model, c(names(models), "Elastic Net")), Predictor_Set = factor(Predictor_Set, sets))

# ---- Candidate terms and training rows per fit, to put the counts in proportion ----
size <- expand_grid(Predictor_Set = sets, Horizon = horizons) %>%
  mutate(Series = map_int(Predictor_Set, ~ length(setdiff(panels$predictor_cols[[.x]], "Employ_Rate_diff_lag1")) + 1L),  # + the level
         Window = map2_int(Predictor_Set, Horizon, function(s, h)                                    # fixed window length
           match(min(windows$Origin[windows$Predictor_Set == s & windows$Horizon == h & windows$Spec == "Pre_COVID_No_Dummy"]),
                 panels$raw[[s]]$Date)),
         Train_rows = Window - Horizon - n_lags + 1L)

summary <- windows %>%
  group_by(Model, Predictor_Set, Horizon, Spec) %>%
  summarise(Windows = n(), Mean = mean(Kept), Median = median(Kept), Min = min(Kept), Max = max(Kept),
            Share_with_none = mean(Kept == 0), Mean_PCs = mean(PCs), US_share = sum(US) / sum(Kept),
            .groups = "drop") %>%
  left_join(size, by = c("Predictor_Set", "Horizon")) %>%
  mutate(Candidates = 4 * (Series + if_else(Model == "PCA-LASSO", n_pcs, 0L)),      # raw series (+ PCs) x 4 lags, approx.
         Share_of_candidates = Mean / Candidates, Share_of_train_rows = Mean / Train_rows,
         US_share = if_else(Predictor_Set == "CAN_US", US_share, NA_real_),
         Period = factor(specs[Spec], specs)) %>%
  select(Model, Predictor_Set, Period, Horizon, Windows, Mean, Median, Min, Max, Share_with_none, Mean_PCs, US_share,
         Candidates, Train_rows, Share_of_candidates, Share_of_train_rows) %>%
  arrange(Model, Predictor_Set, Period, Horizon)
write_csv(summary %>% mutate(across(where(is.double), ~ round(.x, 3))), file.path(out_dir, "LASSO_variable_count_summary.csv"))

# ---- Variables kept over time (no-dummy fits: one continuous timeline per model) ----
colours <- c("rLASSO" = "#E7298A", "PCA-LASSO" = "#7570B3", "Elastic Net" = "#1B9E77")
g1 <- windows %>%
  filter(Spec != "Post_COVID_With_Dummy") %>%
  mutate(Horizon = hlab(Horizon)) %>%
  ggplot(aes(Origin, Kept, colour = Model)) +
  geom_vline(xintercept = as.Date("2020-01-01"), linetype = "dotted", colour = "grey50") +
  geom_line(linewidth = 0.5) +
  scale_colour_manual(values = colours) +
  facet_grid(Predictor_Set ~ Horizon, scales = "free_y") +
  labs(title = "How many variables does each model keep?",
       subtitle = sprintf("Variables with a non-zero coefficient in each rolling window; each fit has %d-%d training rows. Dotted line = start of the post-COVID test.",
                          min(size$Train_rows), max(size$Train_rows)),
       x = "Forecast origin", y = "Variables kept (COVID dummies not counted)", colour = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold"))
ggsave(file.path(out_dir, "LASSO_variable_count_over_time.png"), g1, width = 13, height = 8, dpi = 300, bg = "white")

# ---- Exactly which terms were kept, and in what share of windows ----
meta <- read_excel("data/master_metadata.xlsx", sheet = "master")
norm <- function(x) tolower(gsub("[^A-Za-z0-9]", "", x))                  # "S.P.500" matches "S&P 500"
meta_key <- meta %>% transmute(key = paste(country, norm(variable)), group = `group name`) %>% distinct(key, .keep_all = TRUE)

picked <- windows %>%
  filter(!map_lgl(Terms, is.null)) %>%                                     # Elastic Net has no term names saved
  select(Model, Predictor_Set, Horizon, Spec, Terms) %>%
  group_by(Model, Predictor_Set, Horizon, Spec) %>% mutate(Windows = n()) %>% ungroup() %>%
  unnest_longer(Terms, values_to = "Term") %>%
  count(Model, Predictor_Set, Horizon, Spec, Windows, Term, name = "Windows_kept") %>%
  mutate(Variable = sub("_lag[0-9]+$", "", Term),                         # "US_CLAIMSx_lag2" -> series "US_CLAIMSx", lag 2
         Lag = as.integer(sub(".*_lag", "", Term)),
         Country = if_else(Predictor_Set == "US" | grepl("^US_", Variable), "USA", "CAN"),
         Group = meta_key$group[match(paste(Country, norm(sub("^US_", "", Variable))), meta_key$key)],
         Group = case_when(Variable == "Employ_Rate" ~ "Own level (employment rate)",
                           grepl("^PC[0-9]+$", Variable) ~ "Principal component",
                           is.na(Group) ~ "Unmatched in metadata", TRUE ~ Group),
         Period = factor(specs[Spec], specs), Share_of_windows = Windows_kept / Windows) %>%
  select(Model, Predictor_Set, Period, Horizon, Term, Variable, Lag, Group, Windows_kept, Windows, Share_of_windows) %>%
  arrange(Model, Predictor_Set, Period, Horizon, desc(Share_of_windows), Term)
write_csv(picked %>% mutate(Share_of_windows = round(Share_of_windows, 3)), file.path(out_dir, "LASSO_selected_terms.csv"))
