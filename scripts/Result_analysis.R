# Result analysis: (1) RMSE-ratio heatmap across all models, (2) LASSO group-selection heatmap.
# Run from the project root after every model script has written its RMSE summary.
library(tidyverse)
out_dir <- "results/Result_analysis"
dir.create(out_dir, showWarnings = FALSE)

# ---- 1. RMSE relative to the zero-change forecast ----
# Every file is brought to: Model, Predictor_Set, Horizon, Period, Specification, RMSE
read_long <- function(path, model) {
  x <- read_csv(path, show_col_types = FALSE)
  if (!"Predictor_Set" %in% names(x)) x$Predictor_Set <- "Own lags"       # AR uses no predictor panel
  x %>% mutate(Model = model) %>% select(Model, Predictor_Set, Horizon, Period, Specification, RMSE)
}

rf <- read_csv("results/RF_default_RMSE_summary.csv", show_col_types = FALSE) %>%   # RF summary is wide
  pivot_longer(c(Pre_COVID, Post_No_Dummy, Post_With_Dummy), names_to = "col", values_to = "RMSE") %>%
  mutate(Model = "Random Forest",
         Period = if_else(col == "Pre_COVID", "Pre_COVID", "Post_COVID"),
         Specification = if_else(col == "Post_With_Dummy", "With_Dummy", "No_Dummy")) %>%
  select(Model, Predictor_Set, Horizon, Period, Specification, RMSE)

models <- c("AR (BIC lag order)", "Random Forest", "Ridge", "Elastic Net (a = 0.5)", "LASSO", "PCA-LASSO")
rmse <- bind_rows(
  read_long("results/AR_dynamic_p_RMSE_long.csv", models[1]), rf,
  read_long("results/Ridge/Ridge_RMSE_summary.csv", "Ridge"),
  read_long("results/ElasticNet_a05/ElasticNet_a05_RMSE_summary.csv", models[4]),
  read_long("results/LASSO/LASSO_RMSE_summary.csv", "LASSO"),
  read_long("results/PCA_LASSO/PCA_LASSO_RMSE_summary.csv", "PCA-LASSO")) %>%
  filter(Period != "All", !(Period == "Pre_COVID" & Specification == "With_Dummy"))  # pre-COVID has no dummy fit

zero <- read_csv("results/zero_change_RMSE_long.csv", show_col_types = FALSE) %>%
  filter(Period != "All") %>% select(Horizon, Period, Zero_RMSE = RMSE)

ratio <- rmse %>%
  inner_join(zero, by = c("Horizon", "Period")) %>%
  mutate(Ratio = RMSE / Zero_RMSE,
         Panel = case_when(Period == "Pre_COVID" ~ "Pre-COVID",
                           Specification == "No_Dummy" ~ "Post-COVID, no dummy",
                           TRUE ~ "Post-COVID, with dummy"),
         Panel = factor(Panel, c("Pre-COVID", "Post-COVID, no dummy", "Post-COVID, with dummy")),
         Predictor_Set = factor(Predictor_Set, c("Own lags", "CAN", "US", "CAN_US")),
         Model = factor(Model, models),
         Horizon = factor(paste0("h=", Horizon), paste0("h=", c(1, 3, 6, 12)))) %>%
  arrange(Model, Predictor_Set) %>%
  mutate(Row = fct_rev(fct_inorder(if_else(Predictor_Set == "Own lags", as.character(Model),
                                           paste(Model, Predictor_Set, sep = " | ")))))
stopifnot(nrow(count(ratio, Row, Horizon, Panel) %>% filter(n > 1)) == 0)  # one RMSE per cell

write_csv(ratio %>% select(Model, Predictor_Set, Panel, Horizon, RMSE, Zero_RMSE, Ratio),
          file.path(out_dir, "RMSE_ratio_table.csv"))

g1 <- ggplot(ratio, aes(Horizon, Row, fill = Ratio)) +
  geom_tile(colour = "white", linewidth = 0.8) +
  geom_text(aes(label = sprintf("%.2f", Ratio), colour = abs(Ratio - 1) > 0.2), size = 3.2) +
  scale_colour_manual(values = c(`FALSE` = "#1f1f1d", `TRUE` = "white"), guide = "none") +
  # Diverging around 1: blue = beats "no change", red = worse. Colours capped; the numbers are exact.
  scale_fill_gradient2(low = "#2a78d6", mid = "#f0efec", high = "#e34948", midpoint = 1,
                       limits = c(0.75, 1.25), oob = scales::squish,
                       name = "RMSE / zero-change RMSE") +
  facet_wrap(~Panel) +
  labs(title = "Forecast accuracy relative to a zero-change forecast",
       subtitle = "Below 1 = more accurate than forecasting no change in the employment rate; above 1 = less accurate",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(), legend.position = "bottom", legend.key.width = unit(2, "cm"),
        strip.text = element_text(face = "bold"))
ggsave(file.path(out_dir, "RMSE_ratio_heatmap.png"), g1, width = 13, height = 7.5, dpi = 300, bg = "white")

# ---- 2. Which economic groups LASSO keeps (CAN_US panel) ----
gs <- read_csv("results/LASSO/LASSO_group_selection.csv", show_col_types = FALSE)
us_share <- gs %>% filter(str_starts(Group, "Share of"))                   # shown in the caption instead

groups <- gs %>%
  filter(!str_starts(Group, "Share of")) %>%
  pivot_longer(starts_with("Share_h"), names_to = "Horizon", names_prefix = "Share_h", values_to = "Share") %>%
  mutate(Horizon = factor(paste0("h=", Horizon), paste0("h=", c(1, 3, 6, 12))),
         Period  = factor(if_else(Period == "Pre_COVID", "Pre-COVID", "Post-COVID (with dummy)"),
                          c("Pre-COVID", "Post-COVID (with dummy)")),
         Country = if_else(str_starts(Group, "US"), "US groups", "Canadian groups"),
         Group   = str_replace(str_remove(Group, "^(US|CAN) "), "^own lag", "Own lag"),
         Group   = fct_reorder(Group, Share, mean))

caption <- us_share %>%
  mutate(txt = sprintf("%s: h=1 %.0f%%, h=3 %.0f%%, h=6 %.0f%%, h=12 %.0f%%", str_replace(Period, "_", "-"),
                       100 * Share_h1, 100 * Share_h3, 100 * Share_h6, 100 * Share_h12)) %>%
  pull(txt) %>% paste(collapse = "\n")

g2 <- ggplot(groups, aes(Horizon, Group, fill = Share)) +
  geom_tile(colour = "white", linewidth = 0.8) +
  geom_text(aes(label = sprintf("%.0f%%", 100 * Share), colour = Share > 0.55), size = 3.2) +
  scale_colour_manual(values = c(`FALSE` = "#1f1f1d", `TRUE` = "white"), guide = "none") +
  scale_fill_gradientn(colours = c("#fcfcfb", "#9ec5f4", "#2a78d6", "#0d366b"), limits = c(0, 1),
                       labels = scales::percent, name = "Share of rolling windows") +
  facet_grid(Country ~ Period, scales = "free_y", space = "free_y") +
  labs(title = "Which groups of indicators LASSO keeps (CAN_US panel, BIC)",
       subtitle = "Share of rolling windows in which at least one indicator from the group was selected",
       caption = paste0("US terms as a share of all selected terms (average over windows)\n", caption), x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(), legend.position = "bottom", legend.key.width = unit(2, "cm"),
        strip.text = element_text(face = "bold"), plot.caption = element_text(hjust = 0))
ggsave(file.path(out_dir, "LASSO_group_selection_heatmap.png"), g2, width = 12, height = 8, dpi = 300, bg = "white")
