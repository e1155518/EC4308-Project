# Forecast plots for the level Ridge: actual employment rate vs forecasts (with / without COVID dummy),
# one plot per predictor set and horizon, with a small RMSE table against the random walk and the rolling average.
# Red ticks along the top mark windows where BIC picked the largest lambda on the path (coefficients ~ 0).
# Run after Ridge.R. Run from the project root.
library(ggplot2); library(grid); library(readxl)
rmse <- function(e) sqrt(mean(e^2))

model   <- "level_Ridge"
out_dir <- sprintf("results/Ridge/%s_plots", model)
dir.create(out_dir, showWarnings = FALSE)

p   <- read.csv(sprintf("results/Ridge/%s_predictions.csv", model))
lam <- read.csv(sprintf("results/Ridge/%s_lambda_choice.csv", model))
for (c in c("Outcome_Date", "Forecast_Origin")) p[[c]] <- as.Date(p[[c]])
lam$Forecast_Origin <- as.Date(lam$Forecast_Origin)

# Random walk in levels: the forecast for t+h is the level at the forecast origin t
can <- read_excel("data/CAN_data_cleaned.xlsx"); can$Date <- as.Date(can$Date)
p$RW <- can$Employ_Rate[match(p$Forecast_Origin, can$Date)]

# Rolling historical average of the level (what a fully shrunk Ridge collapses to)
ha <- read.csv("results/benchmarks/level_Historical_Average_Rolling_predictions.csv")
p$HA <- ha$Predicted[match(paste(p$Horizon, p$Outcome_Date), paste(ha$Horizon, ha$Outcome_Date))]

# Draw a text table (first row = header, first column = row labels) in the bottom-left corner (empty before the COVID dip)
rmse_table <- function(tab, best) {
  nr <- nrow(tab); nc <- ncol(tab)
  cx <- c(0.04, 0.68, 0.97); ry <- 1 - (seq_len(nr) - 0.5) / nr
  cells <- lapply(seq_len(nr), function(i) lapply(seq_len(nc), function(j)
    textGrob(tab[i, j], x = cx[j], y = ry[i], hjust = if (j == 1) 0 else 1,
             gp = gpar(fontsize = 10, fontface = if (i == 1 || best[i, j]) "bold" else "plain"))))
  box <- grobTree(rectGrob(gp = gpar(fill = "white", col = "grey70")), do.call(gList, unlist(cells, FALSE)),
                  vp = viewport(x = 0.01, y = 0.02, width = unit(4.2, "in"), height = unit(1.45, "in"), just = c("left", "bottom")))
  grobTree(box)   # outer wrapper: annotation_custom replaces the outer viewport, so the box keeps its own size/position
}

summary_rows <- list()
for (s in unique(p$Predictor_Set)) for (h in sort(unique(p$Horizon))) {
  # One row per outcome date, with both specifications side by side
  nd <- subset(p, Predictor_Set == s & Horizon == h & Specification == "No_Dummy")
  wd <- subset(p, Predictor_Set == s & Horizon == h & Specification == "With_Dummy")
  w  <- data.frame(nd[c("Outcome_Date", "Period", "Actual", "RW", "HA")], No_Dummy = nd$Predicted,
                   With_Dummy = wd$Predicted[match(nd$Outcome_Date, wd$Outcome_Date)])
  pre <- w$Period == "Pre_COVID"; post <- !pre

  # Windows where BIC chose the largest lambda (No-dummy fit)
  l  <- subset(lam, Predictor_Set == s & Horizon == h & Specification == "No_Dummy")
  mx <- data.frame(Outcome_Date = nd$Outcome_Date[l$At_Path_End_Max[match(nd$Forecast_Origin, l$Forecast_Origin)]])

  # RMSE (and ratio to the random walk). No dummy is fitted pre-COVID (the dummy is all zero there), hence the dash.
  rw   <- c(Pre = rmse(w$Actual[pre] - w$RW[pre]), Post = rmse(w$Actual[post] - w$RW[post]))
  num  <- cbind(c(rmse(w$Actual[pre] - w$No_Dummy[pre]), NA, rmse(w$Actual[pre] - w$HA[pre]), rw[["Pre"]]),
                c(rmse(w$Actual[post] - w$No_Dummy[post]), rmse(w$Actual[post] - w$With_Dummy[post]),
                  rmse(w$Actual[post] - w$HA[post]), rw[["Post"]]))
  ratio <- sweep(num, 2, rw, "/")
  cell  <- ifelse(is.na(num), "-", sprintf("%.3f (%.2f)", num, ratio))
  tab   <- rbind(c("RMSE (ratio to RW)", "Pre-COVID", "Post-COVID"),
                 cbind(c("No dummy", "With dummy", "Rolling average", "Random walk"), cell))
  best  <- rbind(FALSE, cbind(FALSE, apply(num, 2, function(x) !is.na(x) & x == min(x, na.rm = TRUE))))

  summary_rows[[length(summary_rows) + 1]] <- data.frame(
    Predictor_Set = s, Horizon = h,
    Period = rep(c("Pre_COVID", "Post_COVID"), each = 4),
    Specification = rep(c("No_Dummy", "With_Dummy", "Rolling_Average", "Random_Walk"), 2),
    RMSE = c(num[, 1], num[, 2]), Ratio_to_RW = c(ratio[, 1], ratio[, 2]))

  g <- ggplot(w, aes(x = Outcome_Date)) +
    geom_vline(xintercept = as.Date("2020-01-01"), linetype = "dotted", colour = "grey50") +  # post-COVID test starts
    geom_line(aes(y = RW,         colour = "Random walk"), linewidth = 0.6, linetype = "dashed") +
    geom_line(aes(y = HA,         colour = "Rolling average"), linewidth = 0.6, linetype = "dotted") +
    geom_line(aes(y = With_Dummy, colour = "With Dummy"), linewidth = 0.8) +
    geom_line(aes(y = No_Dummy,   colour = "Without Dummy"), linewidth = 0.8) +
    geom_line(aes(y = Actual,     colour = "Actual"), linewidth = 0.8) +
    geom_rug(data = mx, aes(x = Outcome_Date), sides = "t", colour = "#C0392B", linewidth = 0.6, inherit.aes = FALSE) +
    annotation_custom(rmse_table(tab, best), -Inf, Inf, -Inf, Inf) +
    scale_colour_manual(values = c("Actual" = "black", "Without Dummy" = "#2563EB", "With Dummy" = "#E69F00",
                                   "Rolling average" = "#7B3FA0", "Random walk" = "grey55"),
                        breaks = c("Actual", "Without Dummy", "With Dummy", "Rolling average", "Random walk")) +
    labs(title = sprintf("Level Ridge %s | h=%d", s, h),
         subtitle = sprintf("BIC picked the largest lambda in %.0f%% of windows (red ticks, top)", 100 * nrow(mx) / nrow(nd)),
         x = "Outcome Date", y = "Canadian employment rate (%)", colour = NULL) +
    theme_minimal()

  ggsave(file.path(out_dir, sprintf("%s_%s_h%d.png", model, s, h)), g, bg = "white", width = 12, height = 6, dpi = 300)
}

# Full comparison with the random walk (ratio < 1 = beats the random walk)
cmp <- do.call(rbind, summary_rows)
write.csv(cmp, sprintf("results/Ridge/%s_RMSE_vs_RW.csv", model), row.names = FALSE)
m <- subset(cmp, Specification != "Random_Walk" & !is.na(RMSE))
m$Column <- paste(m$Period, m$Specification, sep = "_")
wide <- reshape(m[c("Predictor_Set", "Horizon", "Column", "Ratio_to_RW")], idvar = c("Predictor_Set", "Horizon"),
                timevar = "Column", direction = "wide")
names(wide) <- sub("Ratio_to_RW.", "", names(wide), fixed = TRUE)
print(wide, digits = 3, row.names = FALSE)
