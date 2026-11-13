# Result analysis: (1) RMSE vs the random walk, (2) CAN vs CAN_US RMSE heatmap, (3) cumulative squared-error plot,
# (4) which indicator groups rLASSO keeps.
# Run from the project root after every model script has written its RMSE summary.
library(tidyverse); library(readxl)
source("functions/data_prep.R")                       # horizons
out_dir <- "results/Result_analysis"
dir.create(out_dir, showWarnings = FALSE)
hlab <- function(h) factor(paste0("h=", h), paste0("h=", c(1, 3, 6, 12)))

# Results panel of a forecast: pre-COVID, or post-COVID without / with the COVID dummy
panel_of <- function(period, spec)
  factor(case_when(period == "Pre_COVID" ~ "Pre-COVID", spec == "No_Dummy" ~ "Post-COVID, no dummy", TRUE ~ "Post-COVID, with dummy"),
         c("Pre-COVID", "Post-COVID, no dummy", "Post-COVID, with dummy"))

# ---- 1. RMSE relative to the random walk ----
# Every file is brought to: Model, Predictor_Set, Horizon, Period, Specification, RMSE
read_long <- function(path, model) {
  read_csv(path, show_col_types = FALSE) %>%
    mutate(Model = model) %>% select(Model, Predictor_Set, Horizon, Period, Specification, RMSE)
}

# AR with a fixed lag order: only AR(1) is used, no predictor panel
ar <- read_csv("results/AR/level_AR_fixed_p_RMSE_long.csv", show_col_types = FALSE) %>%
  filter(Model == "AR(1)") %>% mutate(Predictor_Set = "Own lags") %>% select(Model, Predictor_Set, Horizon, Period, Specification, RMSE)

# Random Forest summary is wide (and has empty trailing columns)
rf <- read_csv("results/random_forest_alt/level_RF_alt_RMSE_summary.csv", show_col_types = FALSE) %>%
  select(Predictor_Set, Horizon, Pre_COVID, Post_No_Dummy, Post_With_Dummy) %>%
  pivot_longer(-c(Predictor_Set, Horizon), names_to = "col", values_to = "RMSE") %>%
  mutate(Model = "Random Forest",
         Period = if_else(col == "Pre_COVID", "Pre_COVID", "Post_COVID"),
         Specification = if_else(col == "Post_With_Dummy", "With_Dummy", "No_Dummy")) %>%
  select(Model, Predictor_Set, Horizon, Period, Specification, RMSE)

predictor_models <- c("Ridge" = "results/Ridge/level_Ridge_RMSE_summary.csv",
                      "Elastic Net (a = 0.5)" = "results/ElasticNet_a05/level_ElasticNet_a05_RMSE_summary.csv",
                      "PCA-LASSO" = "results/level_PCA_LASSO/level_PCA_LASSO_RMSE_summary.csv",
                      "rLASSO" = "results/level_rLASSO/level_rLASSO_RMSE_summary.csv")
rmse <- bind_rows(ar, rf, imap_dfr(predictor_models, ~ read_long(.x, .y))) %>%
  filter(Period != "All", !(Period == "Pre_COVID" & Specification == "With_Dummy"))  # pre-COVID has no dummy fit
model_order <- c("AR(1)", "Random Forest", names(predictor_models))

rw <- read_csv("results/benchmarks/level_random_walk_RMSE_long.csv", show_col_types = FALSE) %>%
  filter(Period != "All") %>% select(Horizon, Period, RW_RMSE = RMSE)

ratio <- rmse %>%
  inner_join(rw, by = c("Horizon", "Period")) %>%
  mutate(Ratio = RMSE / RW_RMSE,
         Panel = panel_of(Period, Specification),
         Type = if_else(Predictor_Set == "Own lags", "AR", "With predictors"),
         Predictor_Set = factor(Predictor_Set, c("Own lags", "CAN", "US", "CAN_US")),
         Model = factor(Model, model_order),
         Horizon = hlab(Horizon)) %>%
  arrange(Model, Predictor_Set) %>%
  mutate(Row = fct_rev(fct_inorder(if_else(Predictor_Set == "Own lags", as.character(Model),
                                           paste(Model, Predictor_Set, sep = " | ")))))
stopifnot(nrow(count(ratio, Row, Horizon, Panel) %>% filter(n > 1)) == 0)  # one RMSE per cell

write_csv(ratio %>% select(Model, Predictor_Set, Panel, Horizon, RMSE, RW_RMSE, Ratio),
          file.path(out_dir, "RMSE_ratio_table.csv"))

g1 <- ggplot(ratio, aes(Horizon, Row, fill = Ratio)) +
  geom_tile(colour = "white", linewidth = 0.8) +
  geom_text(aes(label = sprintf("%.2f", Ratio), colour = abs(Ratio - 1) > 0.2), size = 3) +
  scale_colour_manual(values = c(`FALSE` = "#1f1f1d", `TRUE` = "white"), guide = "none") +
  # Diverging around 1: blue = beats the random walk, red = worse. Colours capped; the numbers are exact.
  scale_fill_gradient2(low = "#2a78d6", mid = "#f0efec", high = "#e34948", midpoint = 1,
                       limits = c(0.75, 1.25), oob = scales::squish, name = "RMSE / random-walk RMSE") +
  facet_grid(Type ~ Panel, scales = "free_y", space = "free_y") +
  labs(title = "Forecast accuracy relative to the random walk",
       subtitle = "Below 1 = lower RMSE than forecasting no change in the employment rate; above 1 = higher RMSE",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(), legend.position = "bottom", legend.key.width = unit(2, "cm"),
        strip.text = element_text(face = "bold"))
ggsave(file.path(out_dir, "RMSE_ratio_heatmap.png"), g1, width = 13, height = 9, dpi = 300, bg = "white")

# ---- 2. Does adding US data help? RMSE with CAN_US predictors / RMSE with CAN-only predictors ----
us_gain <- ratio %>%
  filter(Predictor_Set %in% c("CAN", "CAN_US")) %>%                       # AR has no predictor panel
  select(Model, Panel, Horizon, Predictor_Set, RMSE) %>%
  pivot_wider(names_from = Predictor_Set, values_from = RMSE) %>%
  mutate(Ratio = CAN_US / CAN, Model = fct_rev(fct_drop(Model)))
write_csv(us_gain, file.path(out_dir, "CAN_vs_CAN_US_table.csv"))

g3 <- ggplot(us_gain, aes(Horizon, Model, fill = Ratio)) +
  geom_tile(colour = "white", linewidth = 0.8) +
  geom_text(aes(label = sprintf("%.2f", Ratio), colour = abs(Ratio - 1) > 0.1), size = 3.4) +
  scale_colour_manual(values = c(`FALSE` = "#1f1f1d", `TRUE` = "white"), guide = "none") +
  # Blue = adding US data lowers the RMSE, red = it raises it. Colours capped; the numbers are exact.
  scale_fill_gradient2(low = "#2a78d6", mid = "#f0efec", high = "#e34948", midpoint = 1,
                       limits = c(0.85, 1.15), oob = scales::squish, name = "RMSE (CAN_US) / RMSE (CAN)") +
  facet_wrap(~Panel) +
  labs(title = "Does adding US indicators improve forecasts of Canadian employment?",
       subtitle = "Below 1 = the CAN_US predictor set forecasts better than the Canadian-only set; above 1 = worse",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(), legend.position = "bottom", legend.key.width = unit(2, "cm"),
        strip.text = element_text(face = "bold"))
ggsave(file.path(out_dir, "CAN_vs_CAN_US_heatmap.png"), g3, width = 13, height = 4.5, dpi = 300, bg = "white")

# ---- 3. Cumulative squared-error difference, CAN-only vs CAN_US ----
# Running sum of (CAN error^2 - CAN_US error^2): rising = US data helped so far, falling = it hurt. Restarts in each panel.
pred_paths <- c("Ridge" = "results/Ridge/level_Ridge_predictions.csv",
                "Elastic Net (a = 0.5)" = "results/ElasticNet_a05/level_ElasticNet_a05_predictions.csv",
                "PCA-LASSO" = "results/level_PCA_LASSO/level_PCA_LASSO_predictions.csv",
                "rLASSO" = "results/level_rLASSO/level_rLASSO_predictions.csv",
                "Random Forest" = "results/random_forest_default/level_RF_predictions.csv")
cum_diff <- imap_dfr(pred_paths, ~ read_csv(.x, show_col_types = FALSE) %>% mutate(Model = .y)) %>%
  filter(Predictor_Set %in% c("CAN", "CAN_US"), !(Period == "Pre_COVID" & Specification == "With_Dummy")) %>%
  mutate(Panel = panel_of(Period, Specification), Model = factor(Model, names(pred_paths)), Horizon = hlab(Horizon)) %>%
  select(Model, Panel, Horizon, Outcome_Date, Predictor_Set, Forecast_Error) %>%
  pivot_wider(names_from = Predictor_Set, values_from = Forecast_Error) %>%
  arrange(Outcome_Date) %>%
  group_by(Model, Panel, Horizon) %>%
  mutate(Cum_Diff = cumsum(CAN^2 - CAN_US^2)) %>% ungroup()

g4 <- ggplot(cum_diff, aes(Outcome_Date, Cum_Diff, colour = Model)) +
  geom_hline(yintercept = 0, colour = "grey50") +
  geom_line(linewidth = 0.7) +
  scale_colour_brewer(palette = "Dark2") +
  facet_wrap(vars(Horizon, Panel), ncol = 3, scales = "free") +                # own y-scale per panel: pre-COVID is small
  labs(title = "Cumulative squared-error difference: Canadian-only vs Canadian + US predictors",
       subtitle = "Above zero = adding US data has reduced squared forecast error so far; below zero = it has increased it",
       x = "Outcome date", y = "Cumulative squared-error gain from adding US data (pp squared)", colour = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold"))
ggsave(file.path(out_dir, "CAN_vs_CAN_US_cumulative_sq_error.png"), g4, width = 13, height = 11, dpi = 300, bg = "white")

# ---- 4. Which economic groups rLASSO keeps (CAN_US panel) ----
meta <- read_excel("data/master_metadata.xlsx", sheet = "master")
norm <- function(x) tolower(gsub("[^A-Za-z0-9]", "", x))                  # "S.P.500" matches "S&P 500"
meta_key <- meta %>% transmute(key = paste(country, norm(variable)), group = `group name`) %>% distinct(key, .keep_all = TRUE)

# Selected terms (e.g. "US_CLAIMSx_lag0") -> groups (e.g. "US Labor Market"); dummies excluded
group_of <- function(v) {
  v  <- unique(sub("_lag[0-9]+$", "", v[!grepl("^COVID_", v)]))
  us <- grepl("^US_", v)
  g  <- meta_key$group[match(paste(if_else(us, "USA", "CAN"), norm(sub("^US_", "", v))), meta_key$key)]
  g  <- paste(if_else(us, "US", "CAN"), g)
  g[v == "Employ_Rate"] <- "CAN Own level (employment rate)"
  stopifnot(!any(grepl(" NA$", g)))                                       # every series must find a group
  unique(g)
}

# Pre-COVID: the only fit. Post-COVID: the with-dummy fit, so selections are not driven by the 2020 outliers
specs <- c(Pre_COVID = "Pre_COVID_No_Dummy", Post_COVID = "Post_COVID_With_Dummy")
group_share <- function(method) {
  map_dfr(names(specs), function(period) map_dfr(horizons, function(h) {
    sel   <- readRDS(sprintf("results/level_rLASSO/level_rLASSO_batches/CAN_US_%d_%s.rds", h, specs[[period]]))[[method]]$selected
    share <- table(unlist(lapply(sel, group_of))) / length(sel)
    us    <- mean(sapply(sel, function(v) mean(grepl("^US_", v[!grepl("^COVID_", v)]))), na.rm = TRUE)
    tibble(Period = period, Horizon = h, Group = c(names(share), "Share of selected terms that are US"),
           Share = c(as.numeric(share), us))
  }))
}

group_plot <- function(method, label) {
  gs <- group_share(method)
  groups <- gs %>%
    filter(!str_starts(Group, "Share of")) %>%
    complete(Group, Period, Horizon, fill = list(Share = 0)) %>%             # groups never selected = 0
    mutate(Horizon = hlab(Horizon),
           Period  = factor(if_else(Period == "Pre_COVID", "Pre-COVID", "Post-COVID (with dummy)"),
                            c("Pre-COVID", "Post-COVID (with dummy)")),
           Country = if_else(str_starts(Group, "US"), "US groups", "Canadian groups"),
           Group   = str_remove(Group, "^(US|CAN) "),
           Group   = fct_reorder(Group, Share, mean))
  cap <- gs %>% filter(str_starts(Group, "Share of")) %>% mutate(Period = factor(Period, names(specs))) %>% group_by(Period) %>%
    summarise(txt = paste0(str_replace(first(Period), "_", "-"), ": ",
                           paste(sprintf("h=%d %.0f%%", Horizon, 100 * Share), collapse = ", ")), .groups = "drop") %>% pull(txt)
  g <- ggplot(groups, aes(Horizon, Group, fill = Share)) +
    geom_tile(colour = "white", linewidth = 0.8) +
    geom_text(aes(label = sprintf("%.0f%%", 100 * Share), colour = Share > 0.55), size = 3.2) +
    scale_colour_manual(values = c(`FALSE` = "#1f1f1d", `TRUE` = "white"), guide = "none") +
    scale_fill_gradientn(colours = c("#fcfcfb", "#9ec5f4", "#2a78d6", "#0d366b"), limits = c(0, 1),
                         labels = scales::percent, name = "Share of rolling windows") +
    facet_grid(Country ~ Period, scales = "free_y", space = "free_y") +
    labs(title = sprintf("Which groups of indicators %s keeps (CAN_US panel)", label),
         subtitle = "Share of rolling windows in which at least one indicator from the group was selected",
         caption = paste0("US terms as a share of all selected terms (average over windows)\n", paste(cap, collapse = "\n")),
         x = NULL, y = NULL) +
    theme_minimal(base_size = 12) +
    theme(panel.grid = element_blank(), legend.position = "bottom", legend.key.width = unit(2, "cm"),
          strip.text = element_text(face = "bold"), plot.caption = element_text(hjust = 0))
  ggsave(file.path(out_dir, sprintf("%s_group_selection_heatmap.png", method)), g, width = 12, height = 8, dpi = 300, bg = "white")
  write_csv(gs %>% pivot_wider(names_from = Horizon, names_prefix = "Share_h", values_from = Share, values_fill = 0),
            file.path(out_dir, sprintf("%s_group_selection.csv", method)))
}
group_plot("rLASSO", "rLASSO")
