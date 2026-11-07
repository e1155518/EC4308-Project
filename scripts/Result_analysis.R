# Result analysis: (1) RMSE-ratio heatmap across all models, (2) which indicator groups rLASSO keeps.
# Run from the project root after every model script has written its RMSE summary.
# A level forecast error (actual level - forecast level) equals the change forecast error when the forecast
# is "level at t + predicted change", so RMSEs of level and change models share one scale.
library(tidyverse); library(readxl)
source("functions/data_prep.R")                       # horizons
out_dir <- "results/Result_analysis"
dir.create(out_dir, showWarnings = FALSE)
hlab <- function(h) factor(paste0("h=", h), paste0("h=", c(1, 3, 6, 12)))

# ---- 1. RMSE relative to the random walk (= zero-change forecast) ----
# Every file is brought to: Model, Predictor_Set, Horizon, Period, Specification, RMSE
read_long <- function(path, model) {
  x <- read_csv(path, show_col_types = FALSE)
  if (!"Predictor_Set" %in% names(x)) x$Predictor_Set <- "Own lags"       # AR uses no predictor panel
  x %>% mutate(Model = model) %>% select(Model, Predictor_Set, Horizon, Period, Specification, RMSE)
}

rf <- read_csv("results/random_forest_default/RF_default_RMSE_summary.csv", show_col_types = FALSE) %>%  # RF summary is wide
  pivot_longer(c(Pre_COVID, Post_No_Dummy, Post_With_Dummy), names_to = "col", values_to = "RMSE") %>%
  mutate(Model = "Random Forest",
         Period = if_else(col == "Pre_COVID", "Pre_COVID", "Post_COVID"),
         Specification = if_else(col == "Post_With_Dummy", "With_Dummy", "No_Dummy")) %>%
  select(Model, Predictor_Set, Horizon, Period, Specification, RMSE)

# Target of each model: employment-rate change (earlier runs) or level (professor's version)
models <- tribble(
  ~Model,                         ~Target,          ~Path,
  "AR (BIC lag order)",           "Change target",  "results/AR/AR_dynamic_p_RMSE_long.csv",
  "Ridge",                        "Change target",  "results/Ridge/Ridge_RMSE_summary.csv",
  "Elastic Net (a = 0.5)",        "Change target",  "results/ElasticNet_a05/ElasticNet_a05_RMSE_summary.csv",
  "LASSO (BIC)",                  "Change target",  "results/Vismay Archive/LASSO_Archive/LASSO_RMSE_summary.csv",
  "PCA-LASSO (BIC)",              "Change target",  "results/Vismay Archive/PCA_LASSO_archive/PCA_LASSO_RMSE_summary.csv",
  "PCA-LASSO (level)",            "Level target",   "results/level_PCA_LASSO/level_PCA_LASSO_RMSE_summary.csv",
  "rLASSO (level)",               "Level target",   "results/level_rLASSO/level_rLASSO_RMSE_summary.csv")
rmse <- bind_rows(pmap(models, function(Model, Target, Path) read_long(Path, Model) %>% mutate(Target = Target)),
                  rf %>% mutate(Target = "Change target")) %>%
  filter(Period != "All", !(Period == "Pre_COVID" & Specification == "With_Dummy"))  # pre-COVID has no dummy fit
model_order <- c("AR (BIC lag order)", "Random Forest", "Ridge", "Elastic Net (a = 0.5)", "LASSO (BIC)", "PCA-LASSO (BIC)",
                 "PCA-LASSO (level)", "rLASSO (level)")

rw <- read_csv("results/benchmarks/zero_change_RMSE_long.csv", show_col_types = FALSE) %>%
  filter(Period != "All") %>% select(Horizon, Period, RW_RMSE = RMSE)

ratio <- rmse %>%
  inner_join(rw, by = c("Horizon", "Period")) %>%
  mutate(Ratio = RMSE / RW_RMSE,
         Panel = case_when(Period == "Pre_COVID" ~ "Pre-COVID",
                           Specification == "No_Dummy" ~ "Post-COVID, no dummy",
                           TRUE ~ "Post-COVID, with dummy"),
         Panel = factor(Panel, c("Pre-COVID", "Post-COVID, no dummy", "Post-COVID, with dummy")),
         Predictor_Set = factor(Predictor_Set, c("Own lags", "CAN", "US", "CAN_US")),
         Model = factor(Model, model_order),
         Horizon = hlab(Horizon)) %>%
  arrange(Model, Predictor_Set) %>%
  mutate(Row = fct_rev(fct_inorder(if_else(Predictor_Set == "Own lags", as.character(Model),
                                           paste(Model, Predictor_Set, sep = " | ")))))
stopifnot(nrow(count(ratio, Row, Horizon, Panel) %>% filter(n > 1)) == 0)  # one RMSE per cell

write_csv(ratio %>% select(Model, Target, Predictor_Set, Panel, Horizon, RMSE, RW_RMSE, Ratio),
          file.path(out_dir, "RMSE_ratio_table.csv"))

g1 <- ggplot(ratio, aes(Horizon, Row, fill = Ratio)) +
  geom_tile(colour = "white", linewidth = 0.8) +
  geom_text(aes(label = sprintf("%.2f", Ratio), colour = abs(Ratio - 1) > 0.2), size = 3) +
  scale_colour_manual(values = c(`FALSE` = "#1f1f1d", `TRUE` = "white"), guide = "none") +
  # Diverging around 1: blue = beats the random walk, red = worse. Colours capped; the numbers are exact.
  scale_fill_gradient2(low = "#2a78d6", mid = "#f0efec", high = "#e34948", midpoint = 1,
                       limits = c(0.75, 1.25), oob = scales::squish, name = "RMSE / random-walk RMSE") +
  facet_grid(Target ~ Panel, scales = "free_y", space = "free_y") +
  labs(title = "Forecast accuracy relative to the random walk",
       subtitle = "Below 1 = more accurate than forecasting no change in the employment rate; above 1 = less accurate",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(), legend.position = "bottom", legend.key.width = unit(2, "cm"),
        strip.text = element_text(face = "bold"))
ggsave(file.path(out_dir, "RMSE_ratio_heatmap.png"), g1, width = 13, height = 12, dpi = 300, bg = "white")

# ---- 2. Which economic groups rLASSO keeps (CAN_US panel) ----
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
