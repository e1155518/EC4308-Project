# Open the EC4308-Project.Rproj file before running.
# All file paths are relative to the project root.
# all 3 datasets include Employ_Rate (Canadian employment rate).

library(readxl)
library(writexl)

# 1. Load and clean Canadian data

can_data <- read.csv(
  "data/raw data/CAN_data_transformed.csv",
  na = "NA"
)

# Convert Date column to proper R dates
# Example input: 1981M01

can_data$Date <- as.Date(
  paste0(
    substr(can_data$Date, 1, 4), "-",
    substr(can_data$Date, 6, 7),
    "-01"
  )
)


class(can_data$Date)

# Keep observations from 1981 onward

can_data <- can_data[
  can_data$Date >= as.Date("1981-01-01"),
]

# Remove predictors with interior missing observations
# Original filtering methodology preserved

keep_columns <- sapply(can_data, function(x) {
  
  # Always keep Date
  if (inherits(x, "Date")) {
    return(TRUE)
  }
  
  # Find missing rows
  missing_rows <- which(is.na(x))
  
  # Keep if no missing values
  if (length(missing_rows) == 0) {
    return(TRUE)
  }
  
  # Allow missingness only at first and/or last row
  allowed_rows <- c(1, nrow(can_data))
  
  all(missing_rows %in% allowed_rows)
})

# Apply column filter

can_data_cleaned <- can_data[, keep_columns]

# Restrict final sample to Feb 1981 - Sep 2025

can_data_cleaned <- can_data_cleaned[
  can_data_cleaned$Date >= as.Date("1981-02-01") &
    can_data_cleaned$Date <= as.Date("2025-09-01"),
]

# Sanity checks

dim(can_data_cleaned)
range(can_data_cleaned$Date)
sum(is.na(can_data_cleaned))

head(can_data_cleaned$Date)
tail(can_data_cleaned$Date)





# ============================================================
# 2. Load and clean US FRED-MD data
# ============================================================

# Confirm this file exists in data/raw data/

us_input_path <- "data/raw data/USA_Fred_MD_raw.csv"

if (!file.exists(us_input_path)) {
  stop(
    "FRED-MD.xlsx not found at: ",
    us_input_path,
    "\nPlease check the filename and location."
  )
}

us_raw <- read.csv(
  us_input_path,
  na = "NA"
)

# Extract transformation codes
# First row contains FRED-MD transformation codes

transform_codes <- as.numeric(
  unlist(us_raw[1, -1])
)

names(transform_codes) <- names(us_raw)[-1]

# Remove transformation-code row

us_data <- us_raw[-1, ]

# Create correct monthly date sequence

names(us_data)[names(us_data) == "sasdate"] <- "Date"

us_data$Date <- seq(
  from = as.Date("1959-01-01"),
  by = "month",
  length.out = nrow(us_data)
)

head(us_data$Date, 15)

# Convert all predictors to numeric

for (variable in setdiff(names(us_data), c("sasdate", "Date"))) {
  
  us_data[[variable]] <- as.numeric(us_data[[variable]])
  
}


# ============================================================
# 3. Define FRED-MD transformation function
# ============================================================

transform_fred <- function(x, code) {
  
  n <- length(x)
  
  if (code == 1) {
    
    # Level
    return(x)
    
  } else if (code == 2) {
    
    # First difference
    return(c(NA, diff(x)))
    
  } else if (code == 3) {
    
    # Second difference
    return(c(NA, NA, diff(x, differences = 2)))
    
  } else if (code == 4) {
    
    # Natural logarithm
    return(log(x))
    
  } else if (code == 5) {
    
    # First difference of natural logarithm
    return(c(NA, diff(log(x))))
    
  } else if (code == 6) {
    
    # Second difference of natural logarithm
    return(c(NA, NA, diff(log(x), differences = 2)))
    
  } else if (code == 7) {
    
    # Percentage change
    pct_change <- c(
      NA,
      x[-1] / x[-n] - 1
    )
    
    # Difference of percentage change
    return(c(
      NA,
      diff(pct_change)
    ))
    
  } else {
    
    stop(paste("Unknown transformation code:", code))
    
  }
}


# ============================================================
# 4. Apply FRED-MD transformations
# ============================================================

us_data_cleaned <- us_data

for (variable in names(transform_codes)) {
  
  code <- transform_codes[variable]
  
  us_data_cleaned[[variable]] <- transform_fred(
    us_data[[variable]],
    code
  )
  
}

# Restrict US data to Feb 1981 - Sep 2025

us_data_cleaned <- us_data_cleaned[
  us_data_cleaned$Date >= as.Date("1981-02-01") &
    us_data_cleaned$Date <= as.Date("2025-09-01"),
]

# Drop problematic US predictors

us_data_cleaned <- us_data_cleaned[
  , !names(us_data_cleaned) %in% c(
    "ACOGNO",
    "CP3Mx",
    "COMPAPFFx"
  )
]

# Sanity checks

dim(us_data_cleaned)
head(us_data_cleaned$Date, 15)
tail(us_data_cleaned$Date, 15)

all(diff(us_data_cleaned$Date) > 0)

colSums(is.na(us_data_cleaned))
str(us_data_cleaned)


# ============================================================
# 5. Check Canadian and US date alignment
# ============================================================

# Check date ranges

range(can_data_cleaned$Date)
range(us_data_cleaned$Date)

# Check dimensions

dim(can_data_cleaned)
dim(us_data_cleaned)

# Check remaining missing values

sum(is.na(can_data_cleaned))
sum(is.na(us_data_cleaned))

# Check monthly observations line up

nrow(can_data_cleaned)
nrow(us_data_cleaned)

# Check that both datasets contain exactly the same dates

stopifnot(
  identical(
    can_data_cleaned$Date,
    us_data_cleaned$Date
  )
)



# ============================================================
# 6. Load and clean Canadian employment rate
# ============================================================

# Load raw employment rate data

EMrate_data <- read.csv(
  "data/raw data/CAN_employ_rate_raw.csv",
  na.strings = "NA"
)

# Keep employment rate observations

EMrate_data <- EMrate_data[
  !is.na(EMrate_data$Labour.force.characteristics) &
    EMrate_data$Labour.force.characteristics == "Employment rate",
  c("REF_DATE", "VALUE")
]

# Rename columns

names(EMrate_data) <- c(
  "Date",
  "Employ_Rate"
)

# Convert Date to proper R Date format

EMrate_data$Date <- as.Date(
  paste0(EMrate_data$Date, "-01")
)

# Convert employment rate to numeric

EMrate_data$Employ_Rate <- as.numeric(
  EMrate_data$Employ_Rate
)

# Sort chronologically

EMrate_data <- EMrate_data[
  order(EMrate_data$Date),
]

rownames(EMrate_data) <- NULL

# Check for duplicate monthly dates

if (anyDuplicated(EMrate_data$Date) > 0) {
  stop("Employment rate dataset contains duplicate dates.")
}

# ============================================================
# Create forward differences for forecasting horizons
# ============================================================

# Forecast horizons: 1, 3, 6 and 12 months

horizons <- c(1, 3, 6, 12)

for (h in horizons) {
  
  # Forward difference: y(t+h) - y(t)
  
  EMrate_data[[paste0("Employ_Rate_diff_h", h)]] <- c(
    EMrate_data$Employ_Rate[(h + 1):nrow(EMrate_data)] -
      EMrate_data$Employ_Rate[1:(nrow(EMrate_data) - h)],
    rep(NA_real_, h)
  )
  
}


# ============================================================
# Create backward first difference of employment rate
# ============================================================

# Backward difference: y(t) - y(t-1)

EMrate_data$Employ_Rate_diff_lag1 <- c(
  NA_real_,
  diff(EMrate_data$Employ_Rate)
)


# Restrict to forecasting sample

EMrate_data <- EMrate_data[
  EMrate_data$Date >= as.Date("1981-02-01") &
    EMrate_data$Date <= as.Date("2025-09-01"),
]

# ============================================================
# Sanity checks
# ============================================================

# Check column names

names(EMrate_data)

# Check first and last observations

head(EMrate_data)
tail(EMrate_data, 15)

# Check date range

range(EMrate_data$Date)

# Check missing values for each horizon

colSums(is.na(EMrate_data))



# ============================================================
# 7. Merge employment rate into Canadian dataset
# ============================================================

can_data_cleaned_emrate <- merge(
  can_data_cleaned,
  EMrate_data,
  by = "Date",
  all.x = TRUE,
  sort = FALSE
)

# Restore chronological order

can_data_cleaned_emrate <- can_data_cleaned_emrate[
  order(can_data_cleaned_emrate$Date),
]

rownames(can_data_cleaned_emrate) <- NULL

# Check results

dim(can_data_cleaned_emrate)
range(can_data_cleaned_emrate$Date)

sum(is.na(can_data_cleaned_emrate$Employ_Rate))


# ============================================================
# 8. Merge Canadian employment rate into US dataset
# ============================================================

# Employment data already uses Date, matching the US dataset

EMrate_data_us <- EMrate_data

# Merge using monthly dates

us_data_cleaned_emrate <- merge(
  us_data_cleaned,
  EMrate_data_us,
  by = "Date",
  all.x = TRUE,
  sort = FALSE
)

# Restore chronological order

us_data_cleaned_emrate <- us_data_cleaned_emrate[
  order(us_data_cleaned_emrate$Date),
]

rownames(us_data_cleaned_emrate) <- NULL

# Check results

dim(us_data_cleaned_emrate)
range(us_data_cleaned_emrate$Date)

sum(is.na(us_data_cleaned_emrate$Employ_Rate))

# ============================================================
# 9. Final sanity checks
# ============================================================

# Check that both datasets have matching dates

stopifnot(
  identical(
    can_data_cleaned_emrate$Date,
    us_data_cleaned_emrate$Date
  )
)

# Check that employment rates are identical

stopifnot(
  isTRUE(all.equal(
    can_data_cleaned_emrate$Employ_Rate,
    us_data_cleaned_emrate$Employ_Rate
  ))
)

# Check number of observations

cat(
  "Canadian observations:",
  nrow(can_data_cleaned_emrate), "\n"
)

cat(
  "US observations:",
  nrow(us_data_cleaned_emrate), "\n"
)

# Check missing values

cat(
  "Missing Canadian employment rates:",
  sum(is.na(can_data_cleaned_emrate$Employ_Rate)),
  "\n"
)

cat(
  "Missing US employment rates:",
  sum(is.na(us_data_cleaned_emrate$Employ_Rate)),
  "\n"
)

# Ensure no missing employment rates before export

stopifnot(
  !anyNA(can_data_cleaned_emrate$Employ_Rate),
  !anyNA(us_data_cleaned_emrate$Employ_Rate)
)


# ============================================================
# Filter Canadian predictors: Keep CAN and NA regions
# ============================================================

# Load Canadian predictor metadata

can_md <- readxl::read_excel(
  "data/Table_can_md.xlsx"
)

# Identify predictors with Region == CAN or NA

can_vars_to_keep <- can_md$Variable[
  can_md$Region == "CAN" | is.na(can_md$Region)
]

# Employment rate variables to preserve

target_vars <- c(
  "Employ_Rate",
  "Employ_Rate_diff_lag1",
  "Employ_Rate_diff_h1",
  "Employ_Rate_diff_h3",
  "Employ_Rate_diff_h6",
  "Employ_Rate_diff_h12"
)

# Keep national predictors, Date, and all employment variables

vars_to_keep <- unique(c(
  "Date",
  intersect(can_vars_to_keep, names(can_data_cleaned_emrate)),
  target_vars
))

# Filter Canadian dataset

can_data_cleaned_emrate <- can_data_cleaned_emrate[
  , vars_to_keep
]

# Check results

names(can_data_cleaned_emrate)

# Confirm all employment variables are present

stopifnot(
  all(target_vars %in% names(can_data_cleaned_emrate))
)


# ============================================================
# Merge Canadian and US datasets
# ============================================================

# 1. Define employment rate variables

employment_vars <- c(
  "Employ_Rate",
  "Employ_Rate_diff_lag1",
  "Employ_Rate_diff_h1",
  "Employ_Rate_diff_h3",
  "Employ_Rate_diff_h6",
  "Employ_Rate_diff_h12"
)

# 2. Check employment variables exist in both datasets

stopifnot(
  all(employment_vars %in% names(can_data_cleaned_emrate)),
  all(employment_vars %in% names(us_data_cleaned_emrate))
)

# 3. US and Canadian date columns are already named Date

us_for_merge <- us_data_cleaned_emrate

# 4. Remove duplicated employment variables from US dataset
# Keep only the Canadian dataset's employment variables

us_for_merge <- us_for_merge[
  , !names(us_for_merge) %in% employment_vars
]

# 5. Prefix US predictors to prevent duplicate column names

names(us_for_merge)[
  names(us_for_merge) != "Date"
] <- paste0(
  "US_",
  names(us_for_merge)[names(us_for_merge) != "Date"]
)

# 6. Merge by monthly date

can_us_data_cleaned <- merge(
  can_data_cleaned_emrate,
  us_for_merge,
  by = "Date",
  all = FALSE,
  sort = TRUE
)

# 7. Sanity checks

cat(
  "Combined dataset dimensions:",
  dim(can_us_data_cleaned),
  "\n"
)

cat(
  "Combined dataset date range:",
  as.character(min(can_us_data_cleaned$Date)),
  "to",
  as.character(max(can_us_data_cleaned$Date)),
  "\n"
)

# Check for duplicate dates

stopifnot(
  !anyDuplicated(can_us_data_cleaned$Date)
)

# Check for duplicate column names

stopifnot(
  !anyDuplicated(names(can_us_data_cleaned))
)

# Check employment variables are preserved

stopifnot(
  all(employment_vars %in% names(can_us_data_cleaned))
)

# Check that no dates were lost in the merge

stopifnot(
  nrow(can_us_data_cleaned) ==
    nrow(can_data_cleaned_emrate)
)

# 8. Export combined dataset

write_xlsx(
  can_us_data_cleaned,
  "data/CAN_US_data_cleaned.xlsx"
)



# ============================================================
# 10. Export final cleaned datasets
# ============================================================

write_xlsx(
  can_data_cleaned_emrate,
  "data/CAN_data_cleaned.xlsx"
)

write_xlsx(
  us_data_cleaned_emrate,
  "data/US_data_cleaned.xlsx"
)

cat(
  "\nData cleaning completed successfully!\n",
  "All three datasets use Date and include Canadian Employ_Rate.\n"
)

