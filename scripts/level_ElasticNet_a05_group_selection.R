# Which economic groups the level Elastic Net (alpha = 0.5) keeps (CAN_US panel).
# Same method and layout as section 4 of Result_analysis.R (the rLASSO heatmap), applied to the Elastic Net selections.
# Run from the project root after ElasticNet_a05.R has written its batches.
library(tidyverse); library(readxl)
source("functions/data_prep.R")                       # horizons
out_dir <- "results/Result_analysis"
dir.create(out_dir, showWarnings = FALSE)
hlab <- function(h) factor(paste0("h=", h), paste0("h=", c(1, 3, 6, 12)))

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
group_share <- function() {
  map_dfr(names(specs), function(period) map_dfr(horizons, function(h) {
    sel   <- readRDS(sprintf("results/ElasticNet_a05/level_ElasticNet_a05_batches/CAN_US_%d_%s.rds", h, specs[[period]]))$selected
    share <- table(unlist(lapply(sel, group_of))) / length(sel)
    us    <- mean(sapply(sel, function(v) mean(grepl("^US_", v[!grepl("^COVID_", v)]))), na.rm = TRUE)
    tibble(Period = period, Horizon = h, Group = c(names(share), "Share of selected terms that are US"),
           Share = c(as.numeric(share), us))
  }))
}

group_plot <- function(method, label) {
  gs <- group_share()
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
group_plot("ElasticNet_a05", "Elastic Net (alpha = 0.5)")
