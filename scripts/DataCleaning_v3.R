# EC4308 data cleaning. Run from the EC4308-Project.Rproj root.
# Outputs: CAN_data_cleaned.xlsx, US_data_cleaned.xlsx, CAN_US_data_cleaned.xlsx
library(readxl)
library(writexl)

start_date <- as.Date("1981-02-01")
end_date <- as.Date("2025-09-01")
horizons <- c(1, 3, 6, 12)
target_vars <- c("Employ_Rate", "Employ_Rate_diff_lag1", paste0("Employ_Rate_diff_h", horizons))
in_sample <- function(d) d >= start_date & d <= end_date

# 1. Canadian predictors: retain columns with no interior missing values.
can_data <- read.csv("data/raw data/CAN_data_transformed.csv", na.strings = "NA")
can_data$Date <- as.Date(paste0(substr(can_data$Date, 1, 4), "-", substr(can_data$Date, 6, 7), "-01"))
can_data <- can_data[can_data$Date >= as.Date("1981-01-01"), ]
keep_columns <- vapply(can_data, function(x) {
  if (inherits(x, "Date")) return(TRUE)
  all(which(is.na(x)) %in% c(1L, nrow(can_data)))
}, logical(1))
can_data_cleaned <- can_data[in_sample(can_data$Date), keep_columns, drop = FALSE]

# 2. US FRED-MD: read transformation codes and convert predictors.
us_input_path <- "data/raw data/USA_Fred_MD_raw.csv"
if (!file.exists(us_input_path)) stop("FRED-MD CSV not found: ", us_input_path)
us_raw <- read.csv(us_input_path, na.strings = "NA")
transform_codes <- as.numeric(unlist(us_raw[1, -1]))
names(transform_codes) <- names(us_raw)[-1]
us_data <- us_raw[-1, ]
names(us_data)[names(us_data) == "sasdate"] <- "Date"
us_data$Date <- seq(as.Date("1959-01-01"), by = "month", length.out = nrow(us_data))
for (variable in names(transform_codes)) us_data[[variable]] <- as.numeric(us_data[[variable]])

# FRED-MD transformations 
transform_fred <- function(x, code) {
  n <- length(x)
  if (code == 1) return(x)
  if (code == 2) return(c(NA, diff(x)))
  if (code == 3) return(c(NA, NA, diff(x, differences = 2)))
  if (code == 4) return(log(x))
  if (code == 5) return(c(NA, diff(log(x))))
  if (code == 6) return(c(NA, NA, diff(log(x), differences = 2)))
  if (code == 7) return(c(NA, diff(c(NA, x[-1] / x[-n] - 1))))
  stop("Unknown transformation code: ", code)
}

us_data_cleaned <- us_data
for (variable in names(transform_codes)) {
  us_data_cleaned[[variable]] <- transform_fred(us_data[[variable]], transform_codes[[variable]])
}
us_data_cleaned <- us_data_cleaned[in_sample(us_data_cleaned$Date),
                                   !names(us_data_cleaned) %in% c("ACOGNO", "CP3Mx", "COMPAPFFx"), drop = FALSE]
stopifnot(identical(can_data_cleaned$Date, us_data_cleaned$Date))

# 3. Canadian employment rate and forecasting targets.
EMrate_data <- read.csv("data/raw data/CAN_employ_rate_raw.csv", na.strings = "NA")
EMrate_data <- EMrate_data[!is.na(EMrate_data$Labour.force.characteristics) &
                           EMrate_data$Labour.force.characteristics == "Employment rate", c("REF_DATE", "VALUE")]
names(EMrate_data) <- c("Date", "Employ_Rate")
EMrate_data$Date <- as.Date(paste0(EMrate_data$Date, "-01"))
EMrate_data$Employ_Rate <- as.numeric(EMrate_data$Employ_Rate)
EMrate_data <- EMrate_data[order(EMrate_data$Date), ]
rownames(EMrate_data) <- NULL
if (anyDuplicated(EMrate_data$Date)) stop("Employment rate dataset contains duplicate dates.")
for (h in horizons) {
  EMrate_data[[paste0("Employ_Rate_diff_h", h)]] <- c(
    EMrate_data$Employ_Rate[(h + 1):nrow(EMrate_data)] -
      EMrate_data$Employ_Rate[1:(nrow(EMrate_data) - h)], rep(NA_real_, h))
}
EMrate_data$Employ_Rate_diff_lag1 <- c(NA_real_, diff(EMrate_data$Employ_Rate))
EMrate_data <- EMrate_data[in_sample(EMrate_data$Date), ]

# 4. Attach the same Canadian employment targets to CAN and US.
attach_employment <- function(predictors) {
  result <- merge(predictors, EMrate_data, by = "Date", all.x = TRUE, sort = FALSE)
  result <- result[order(result$Date), ]
  rownames(result) <- NULL
  result
}
can_data_cleaned_emrate <- attach_employment(can_data_cleaned)
us_data_cleaned_emrate <- attach_employment(us_data_cleaned)
stopifnot(identical(can_data_cleaned_emrate$Date, us_data_cleaned_emrate$Date),
          isTRUE(all.equal(can_data_cleaned_emrate$Employ_Rate, us_data_cleaned_emrate$Employ_Rate)),
          !anyNA(can_data_cleaned_emrate$Employ_Rate), !anyNA(us_data_cleaned_emrate$Employ_Rate))

# 5. Retain Canadian predictors tagged CAN or unspecified in metadata.
can_md <- read_excel("data/Table_can_md.xlsx")
can_vars_to_keep <- can_md$Variable[can_md$Region == "CAN" | is.na(can_md$Region)]
vars_to_keep <- unique(c("Date", intersect(can_vars_to_keep, names(can_data_cleaned_emrate)), target_vars))
can_data_cleaned_emrate <- can_data_cleaned_emrate[, vars_to_keep, drop = FALSE]
stopifnot(all(target_vars %in% names(can_data_cleaned_emrate)),
          all(target_vars %in% names(us_data_cleaned_emrate)))

# 6. Combine CAN predictors and prefixed US predictors, without duplicate targets.
us_for_merge <- us_data_cleaned_emrate[, !names(us_data_cleaned_emrate) %in% target_vars, drop = FALSE]
names(us_for_merge)[names(us_for_merge) != "Date"] <- paste0("US_", names(us_for_merge)[names(us_for_merge) != "Date"])
can_us_data_cleaned <- merge(can_data_cleaned_emrate, us_for_merge, by = "Date", all = FALSE, sort = TRUE)
stopifnot(!anyDuplicated(can_us_data_cleaned$Date), !anyDuplicated(names(can_us_data_cleaned)),
          all(target_vars %in% names(can_us_data_cleaned)),
          nrow(can_us_data_cleaned) == nrow(can_data_cleaned_emrate))

# 7. Export all three datasets (Date is standardized in each).
write_xlsx(can_data_cleaned_emrate, "data/CAN_data_cleaned.xlsx")
write_xlsx(us_data_cleaned_emrate, "data/US_data_cleaned.xlsx")
write_xlsx(can_us_data_cleaned, "data/CAN_US_data_cleaned.xlsx")
cat("Data cleaning completed. All three datasets use Date and include Canadian employment targets.\n")
