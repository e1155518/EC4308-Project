library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

can <- read_excel("data/CAN_data_cleaned.xlsx") %>%
  mutate(Date = as.Date(Date)) %>%
  arrange(Date)

shock_start <- as.Date("2020-03-01")
shock_end <- as.Date("2020-04-01")

recovery_start <- as.Date("2020-05-01")
recovery_end <- as.Date("2022-02-01")

covid_zoom_start <- as.Date("2019-01-01")
covid_zoom_end <- as.Date("2022-12-01")

# COVID disruption period for summary statistics
covid_start <- shock_start
covid_end <- recovery_end

p_employ_full <- ggplot(can, aes(Date, Employ_Rate)) +
  geom_line(color = "#2563EB", linewidth = 0.65) +
  annotate("rect",
           xmin = shock_start, xmax = recovery_end,
           ymin = -Inf, ymax = Inf,
           fill = "#E69F00", alpha = 0.12) +
  labs(
    title = "Canadian Employment Rate (1981–2025)",
    subtitle = "Shaded region: COVID shock and recovery",
    x = NULL,
    y = "Employment rate (%)"
  ) +
  theme_minimal(base_size = 13)

print(p_employ_full)

p_employ_covid <- ggplot(
  filter(can, between(Date, covid_zoom_start, covid_zoom_end)),
  aes(Date, Employ_Rate)
) +
  annotate("rect",
           xmin = shock_start, xmax = shock_end,
           ymin = -Inf, ymax = Inf,
           fill = "#D55E00", alpha = 0.18) +
  annotate("rect",
           xmin = recovery_start, xmax = recovery_end,
           ymin = -Inf, ymax = Inf,
           fill = "#E69F00", alpha = 0.15) +
  geom_line(color = "#2563EB", linewidth = 1) +
  geom_point(color = "#2563EB", size = 1.2) +
  labs(
    title = "Canadian Employment Rate During COVID",
    subtitle = "Red: initial shock | Orange: recovery and disruptions",
    x = NULL,
    y = "Employment rate (%)"
  ) +
  theme_minimal(base_size = 13)

print(p_employ_covid)

forward_df <- can %>%
  select(Date, starts_with("Employ_Rate_diff_h")) %>%
  pivot_longer(
    cols = -Date,
    names_to = "Horizon",
    values_to = "Change"
  ) %>%
  mutate(
    Horizon = factor(
      Horizon,
      levels = paste0("Employ_Rate_diff_h", c(1, 3, 6, 12)),
      labels = paste0(c(1, 3, 6, 12), "-month")
    )
  )

p_diff_full <- ggplot(forward_df, aes(Date, Change)) +
  annotate("rect",
           xmin = shock_start, xmax = recovery_end,
           ymin = -Inf, ymax = Inf,
           fill = "#E69F00", alpha = 0.12) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_line(color = "#2563EB", linewidth = 0.5) +
  facet_wrap(~ Horizon, ncol = 1, scales = "free_y") +
  labs(
    title = "Forward Changes in Canada's Employment Rate",
    subtitle = "Forecast horizons: 1, 3, 6 and 12 months",
    x = NULL,
    y = "Change (percentage points)"
  ) +
  theme_minimal(base_size = 12)

print(p_diff_full)

p_diff_covid <- forward_df %>%
  filter(between(Date, covid_zoom_start, covid_zoom_end)) %>%
  ggplot(aes(Date, Change)) +
  annotate("rect",
           xmin = shock_start, xmax = shock_end,
           ymin = -Inf, ymax = Inf,
           fill = "#D55E00", alpha = 0.18) +
  annotate("rect",
           xmin = recovery_start, xmax = recovery_end,
           ymin = -Inf, ymax = Inf,
           fill = "#E69F00", alpha = 0.15) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_line(color = "#2563EB", linewidth = 0.85) +
  facet_wrap(~ Horizon, ncol = 1, scales = "free_y") +
  labs(
    title = "Forward Employment-Rate Changes Around COVID",
    subtitle = "Changes are indexed by forecast origin, not outcome date",
    x = NULL,
    y = "Change (percentage points)"
  ) +
  theme_minimal(base_size = 12)

print(p_diff_covid)

summary_employment <- function(df, label) {
  df %>%
    summarise(
      Period = label,
      Observations = sum(!is.na(Employ_Rate)),
      Mean = mean(Employ_Rate, na.rm = TRUE),
      Median = median(Employ_Rate, na.rm = TRUE),
      SD = sd(Employ_Rate, na.rm = TRUE),
      Minimum = min(Employ_Rate, na.rm = TRUE),
      Maximum = max(Employ_Rate, na.rm = TRUE),
      Range = Maximum - Minimum
    )
}

employment_summary <- bind_rows(
  summary_employment(can, "Full sample"),
  summary_employment(
    filter(can, between(Date, covid_start, covid_end)),
    "COVID period (Mar 2020–Feb 2022)"
  )
)

print(employment_summary, width = Inf)

monthly_change_summary <- can %>%
  mutate(
    Period = case_when(
      Date < shock_start ~ "Pre-COVID",
      between(Date, shock_start, shock_end) ~ "COVID shock",
      between(Date, recovery_start, recovery_end) ~ "COVID recovery",
      TRUE ~ "Post-recovery"
    )
  ) %>%
  group_by(Period) %>%
  summarise(
    N = sum(!is.na(Employ_Rate_diff_lag1)),
    Mean = mean(Employ_Rate_diff_lag1, na.rm = TRUE),
    SD = sd(Employ_Rate_diff_lag1, na.rm = TRUE),
    Min = min(Employ_Rate_diff_lag1, na.rm = TRUE),
    Max = max(Employ_Rate_diff_lag1, na.rm = TRUE),
    Mean_Absolute_Change = mean(abs(Employ_Rate_diff_lag1), na.rm = TRUE),
    .groups = "drop"
  )

print(monthly_change_summary, width = Inf)

dir.create("results/EDA", recursive = TRUE, showWarnings = FALSE)

plots <- list(
  Employment_Full = p_employ_full,
  Employment_COVID = p_employ_covid,
  Forward_Differences_Full = p_diff_full,
  Forward_Differences_COVID = p_diff_covid
)

for (nm in names(plots)) {
  ggsave(
    filename = paste0("results/EDA/", nm, ".png"),
    plot = plots[[nm]],
    width = 12,
    height = if (grepl("Differences", nm)) 10 else 5.5,
    dpi = 300
  )
}

# ====================================================================================
# Correlation matrix of the predictors (CAN_US panel = every CAN and US predictor)
# ====================================================================================
# Estimated on the pre-COVID sample so the 2020 outliers do not drive the correlations.
corr_end <- as.Date("2019-12-01")

panel <- read_excel("data/CAN_US_data_cleaned.xlsx") %>% mutate(Date = as.Date(Date))
X <- panel %>%
  filter(Date <= corr_end) %>%
  select(Employ_Rate_diff_lag1, !matches("^(Date|Employ_Rate|COVID_)"))   # same predictor set the models use

# Attach each predictor's economic group (master_metadata), ordered CAN first, then US
meta <- read_excel("data/master_metadata.xlsx", sheet = "master")
norm <- function(x) tolower(gsub("[^A-Za-z0-9]", "", x))                  # "S.P.500" matches "S&P 500"
meta_key <- meta %>%
  transmute(key = paste(country, norm(variable)), Group = `group name`, Code = group) %>%
  distinct(key, .keep_all = TRUE)

vars <- tibble(Variable = names(X)) %>%
  mutate(Country = if_else(grepl("^US_", Variable), "US", "CAN"),
         key = paste(if_else(Country == "US", "USA", "CAN"), norm(sub("^US_", "", Variable)))) %>%
  left_join(meta_key, by = "key") %>%
  mutate(Group = if_else(Variable == "Employ_Rate_diff_lag1", "Own lag (employment change)", Group),
         Code  = if_else(Variable == "Employ_Rate_diff_lag1", 0, Code),
         Block = paste(Country, Group)) %>%
  arrange(desc(Country == "CAN"), Code, match(Variable, names(X)))
stopifnot(!anyNA(vars$Group), nrow(vars) == ncol(X))                       # every predictor found its group

R <- cor(X[vars$Variable])
blocks <- unique(vars$Block)
size   <- table(factor(vars$Block, blocks))
edge   <- cumsum(size) + 0.5                                               # group boundaries on the plot grid
mid    <- vars$Variable[round(cumsum(size) - size / 2 + 0.5)]              # one tick per group, at its centre
n      <- nrow(vars)
can_edge <- edge[sum(startsWith(blocks, "CAN"))]                            # last Canadian group ends here

# ---- Figure 1: full matrix, variables grouped by economic category ----
cor_long <- as.data.frame(as.table(R), responseName = "r") %>%
  mutate(Var1 = factor(Var1, vars$Variable), Var2 = factor(Var2, rev(vars$Variable)))

p_corr <- ggplot(cor_long, aes(Var1, Var2, fill = r)) +
  geom_raster() +
  geom_vline(xintercept = edge[-length(edge)], colour = "grey30", linewidth = 0.2) +
  geom_hline(yintercept = n - edge[-length(edge)] + 1, colour = "grey30", linewidth = 0.2) +
  geom_vline(xintercept = can_edge, colour = "black", linewidth = 0.7) +      # thick line: CAN | US
  geom_hline(yintercept = n - can_edge + 1, colour = "black", linewidth = 0.7) +
  scale_fill_gradient2(low = "#2a78d6", mid = "#f0efec", high = "#e34948", midpoint = 0,
                       limits = c(-1, 1), name = "Correlation") +
  scale_x_discrete(breaks = mid, labels = blocks, position = "top") +
  scale_y_discrete(breaks = rev(mid), labels = rev(blocks)) +
  coord_fixed() +
  labs(title = "Correlation matrix of the predictors (CAN_US panel)",
       subtitle = sprintf("%d predictors, monthly, Feb 1981 - Dec 2019; black lines separate Canadian from US variables", n),
       x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5),
        legend.position = "right")
print(p_corr)

# ---- Figure 2: the same matrix summarised by group (average absolute correlation) ----
M    <- sapply(blocks, function(b) as.numeric(vars$Block == b))            # variables x groups membership
absR <- abs(R); diag(absR) <- 0
pairs_n <- t(M) %*% (matrix(1, n, n) - diag(n)) %*% M                      # number of distinct variable pairs
grp <- (t(M) %*% absR %*% M) / pairs_n                                     # NaN for a group of one (own lag) vs itself

grp_long <- as.data.frame(as.table(grp), responseName = "Avg_abs_r") %>%
  mutate(Var1 = factor(Var1, blocks), Var2 = factor(Var2, rev(blocks)))

p_corr_grp <- ggplot(grp_long, aes(Var1, Var2, fill = Avg_abs_r)) +
  geom_tile(colour = "white", linewidth = 0.6) +
  geom_text(aes(label = ifelse(is.finite(Avg_abs_r), sprintf("%.2f", Avg_abs_r), ""), colour = Avg_abs_r > 0.35), size = 2.6, na.rm = TRUE) +
  scale_colour_manual(values = c(`FALSE` = "#1f1f1d", `TRUE` = "white"), guide = "none") +
  scale_fill_gradientn(colours = c("#fcfcfb", "#9ec5f4", "#2a78d6", "#0d366b"), limits = c(0, NA),
                       na.value = "grey90", name = "Average |correlation|") +
  scale_x_discrete(position = "top") +
  coord_fixed() +
  labs(title = "Average absolute correlation between groups of predictors",
       subtitle = "Diagonal = within-group average (excluding each variable with itself)", x = NULL, y = NULL) +
  theme_minimal(base_size = 10) +
  theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5))
print(p_corr_grp)

# ---- Summary: how collinear are the predictors? ----
up <- which(upper.tri(R), arr.ind = TRUE)
pair_tbl <- tibble(Var_1 = rownames(R)[up[, 1]], Var_2 = colnames(R)[up[, 2]], r = R[up]) %>%
  mutate(Type = paste(pmin(vars$Country[match(Var_1, vars$Variable)], vars$Country[match(Var_2, vars$Variable)]),
                      pmax(vars$Country[match(Var_1, vars$Variable)], vars$Country[match(Var_2, vars$Variable)]),
                      sep = "-"))

corr_summary <- pair_tbl %>%
  group_by(Type) %>%
  summarise(Pairs = n(), Mean_abs_r = mean(abs(r)),
            Share_abs_r_above_0.5 = mean(abs(r) > 0.5), Share_abs_r_above_0.8 = mean(abs(r) > 0.8),
            .groups = "drop")
print(corr_summary, width = Inf)
print(pair_tbl %>% arrange(desc(abs(r))) %>% slice_head(n = 10), width = Inf)

dir.create("results/EDA", recursive = TRUE, showWarnings = FALSE)
ggsave("results/EDA/Correlation_Predictors.png", p_corr,     width = 14, height = 13, dpi = 300, bg = "white")
ggsave("results/EDA/Correlation_By_Group.png",   p_corr_grp, width = 14, height = 13, dpi = 300, bg = "white")
