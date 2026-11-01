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