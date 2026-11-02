# Group-level selection frequency for LASSO on the CAN_US panel:
# share of rolling windows in which at least one indicator from each economic group was kept.
# Run after LASSO.R (reads its saved results). Run from the project root.
library(readxl)
source("functions/data_prep.R")

meta <- as.data.frame(read_excel("data/master_metadata.xlsx", sheet = "master"))
norm <- function(x) tolower(gsub("[^A-Za-z0-9]", "", x))   # so "S.P.500" matches "S&P 500"
meta$key <- paste(meta$country, norm(meta$variable))

# Map selected terms (e.g. "US_CLAIMSx_lag0") to groups (e.g. "US Labor Market")
group_of <- function(v) {
  v <- unique(sub("_lag[0-9]+$", "", v[!grepl("^COVID_", v)]))   # one entry per series; dummies excluded
  us <- grepl("^US_", v)
  g  <- meta$`group name`[match(paste(ifelse(us, "USA", "CAN"), norm(sub("^US_", "", v))), meta$key)]
  g  <- paste(ifelse(us, "US", "CAN"), g)
  g[v == "Employ_Rate_diff_lag1"] <- "CAN own lag (employment change)"
  stopifnot(!grepl(" NA$", g))                                     # every series must find a group
  unique(g)
}

# Pre-COVID: the only fit. Post-COVID: the with-dummy fit, so selections are not driven by the 2020 outliers
specs <- c(Pre_COVID = "Pre_COVID_No_Dummy", Post_COVID = "Post_COVID_With_Dummy")
out <- list()
for (period in names(specs)) for (h in horizons) {
  sel <- readRDS(sprintf("results/LASSO_batches/CAN_US_%d_%s.rds", h, specs[period]))$selected
  share <- table(unlist(lapply(sel, group_of))) / length(sel)
  us_terms <- mean(sapply(sel, function(v) mean(grepl("^US_", v[!grepl("^COVID_", v)]))), na.rm = TRUE)
  out[[paste(period, h)]] <- data.frame(Period = period, Horizon = h,
                                        Group = c(names(share), "Share of selected terms that are US"),
                                        Share = round(c(as.numeric(share), us_terms), 2))
}

# One row per group and period, one column per horizon; groups never selected get 0
wide <- reshape(do.call(rbind, out), idvar = c("Group", "Period"), timevar = "Horizon",
                direction = "wide", sep = "_h")
wide[is.na(wide)] <- 0
wide <- wide[order(wide$Period != "Pre_COVID", grepl("^Share of", wide$Group), -wide$Share_h1), ]
write.csv(wide, "results/LASSO_group_selection.csv", row.names = FALSE)
print(wide, row.names = FALSE)
