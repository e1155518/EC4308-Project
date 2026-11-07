# Forecast plots for LASSO and PCA-LASSO: actual vs forecasts with and without the COVID dummy,
# one plot per predictor set and horizon, with a small RMSE table (incl. zero-change benchmark).
# Run after LASSO.R and PCA_LASSO.R. Run from the project root.
library(ggplot2); library(grid)
rmse <- function(e) sqrt(mean(e^2))

# Draw a text table (first row = header, first column = row labels) in the top-left corner of the panel
rmse_table <- function(tab, best) {
  nr <- nrow(tab); nc <- ncol(tab)
  cx <- c(0.04, 0.66, 0.96); ry <- 1 - (seq_len(nr) - 0.5) / nr
  cells <- lapply(seq_len(nr), function(i) lapply(seq_len(nc), function(j)
    textGrob(tab[i, j], x = cx[j], y = ry[i], hjust = if (j == 1) 0 else 1,
             gp = gpar(fontsize = 10, fontface = if (i == 1 || best[i, j]) "bold" else "plain"))))
  box <- grobTree(rectGrob(gp = gpar(fill = "white", col = "grey70")), do.call(gList, unlist(cells, FALSE)),
                  vp = viewport(x = 0.01, y = 0.98, width = unit(3.3, "in"), height = unit(1.2, "in"), just = c("left", "top")))
  grobTree(box)   # outer wrapper: annotation_custom replaces the outer viewport, so the box keeps its own size/position
}

plot_model <- function(model, label) {
  p <- read.csv(sprintf("results/%s/%s_predictions.csv", model, model))
  p$Outcome_Date <- as.Date(p$Outcome_Date)
  out_dir <- sprintf("results/%s/%s_plots", model, model)
  dir.create(out_dir, showWarnings = FALSE)

  for (s in unique(p$Predictor_Set)) for (h in sort(unique(p$Horizon))) {
    # One row per outcome date, with both specifications side by side
    nd <- subset(p, Predictor_Set == s & Horizon == h & Specification == "No_Dummy")
    wd <- subset(p, Predictor_Set == s & Horizon == h & Specification == "With_Dummy")
    w  <- data.frame(nd[c("Outcome_Date", "Period", "Actual")], No_Dummy = nd$Predicted,
                     With_Dummy = wd$Predicted[match(nd$Outcome_Date, wd$Outcome_Date)])
    pre <- w$Period == "Pre_COVID"; post <- !pre

    # RMSE table. Zero change = forecasting "no change", so its error is the actual change itself.
    # No dummy is fitted pre-COVID (the dummy is all zero there), hence the dash.
    num <- cbind(c(rmse(w$Actual[pre] - w$No_Dummy[pre]), NA, rmse(w$Actual[pre])),     # Pre-COVID column
                 c(rmse(w$Actual[post] - w$No_Dummy[post]), rmse(w$Actual[post] - w$With_Dummy[post]),
                   rmse(w$Actual[post])))                                                # Post-COVID column
    tab <- rbind(c("RMSE", "Pre-COVID", "Post-COVID"),
                 cbind(c("No dummy", "With dummy", "Zero change"), ifelse(is.na(num), "-", sprintf("%.3f", num))))
    best <- rbind(FALSE, cbind(FALSE, apply(num, 2, function(x) !is.na(x) & x == min(x, na.rm = TRUE))))

    g <- ggplot(w, aes(x = Outcome_Date)) +
      geom_hline(aes(yintercept = 0, colour = "Zero-change forecast"), linetype = "dashed") +
      geom_vline(xintercept = as.Date("2020-01-01"), linetype = "dotted", colour = "grey50") +  # post-COVID test starts
      geom_line(aes(y = With_Dummy, colour = "With Dummy"), linewidth = 0.8) +
      geom_line(aes(y = No_Dummy,   colour = "Without Dummy"), linewidth = 0.8) +
      geom_line(aes(y = Actual,     colour = "Actual"), linewidth = 0.8) +
      annotation_custom(rmse_table(tab, best), -Inf, Inf, -Inf, Inf) +
      scale_colour_manual(values = c("Actual" = "black", "Without Dummy" = "#2563EB",
                                     "With Dummy" = "#E69F00", "Zero-change forecast" = "grey55"),
                          breaks = c("Actual", "Without Dummy", "With Dummy", "Zero-change forecast")) +
      labs(title = sprintf("%s %s | h=%d", label, s, h),
           x = "Outcome Date", y = "Canadian employment-rate change (pp)", colour = NULL) +
      theme_minimal()

    ggsave(file.path(out_dir, sprintf("%s_%s_h%d.png", model, s, h)), g, bg ="white", width = 12, height = 6, dpi = 300)
  }
}

plot_model("LASSO", "LASSO")
plot_model("PCA_LASSO", "PCA-LASSO")
