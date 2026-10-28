# ============================================================
# Clean Canadian macroeconomic data
# ============================================================

library(readxl)
library(writexl)

curr_direc <- "C:\\Users\\tokee\\Downloads\\EC4308 Project"
setwd(curr_direc)

# 1. Load data ------------------------------------------------

can_data = read_excel(
  "Data/CAN_data.xlsx",
  na = "NA"
)
# 2. Convert Date column to proper R dates --------------------

# Example input: 1981M01
can_data$Date = as.Date(
  paste0(
    substr(can_data$Date, 1, 4), "-",   # year
    substr(can_data$Date, 6, 7),        # month
    "-01"
  )
)


# Check conversion
head(can_data$Date)
class(can_data$Date)


# 3. Keep observations from 1981 onward -----------------------

can_data = can_data[
  can_data$Date >= as.Date("1981-01-01"),
]


# 4. Remove predictors with INTERIOR missing observations ------

keep_columns = sapply(can_data, function(x) {
  
  # Always keep Date
  if (inherits(x, "Date")) {
    return(TRUE)
  }
  
  # Find missing rows
  missing_rows = which(is.na(x))
  
  # Keep if no missing values
  if (length(missing_rows) == 0) {
    return(TRUE)
  }
  
  # Allow missingness only at first and/or last row
  allowed_rows = c(1, nrow(can_data))
  
  all(missing_rows %in% allowed_rows)
})


# Apply column filter
can_data_cleaned = can_data[, keep_columns]


# 5. Restrict final sample to Feb 1981 - Sep 2025 -------------

can_data_cleaned = can_data_cleaned[
  can_data_cleaned$Date >= as.Date("1981-02-01") &
    can_data_cleaned$Date <= as.Date("2025-09-01"),
]


# 6. Check results --------------------------------------------

dim(can_data_cleaned)

range(can_data_cleaned$Date)

sum(is.na(can_data_cleaned))

head(can_data_cleaned$Date)
tail(can_data_cleaned$Date)


# # 7. Export ----------------------------------------------------
# 
# write_xlsx(
#   can_data_cleaned,
#   "Data/CAN_data_cleaned.xlsx"
# )




# ============================================================
# Clean and transform US FRED-MD data
# ============================================================

library(readxl)
library(writexl)


# 1. Load FRED-MD data ----------------------------------------

us_raw = read_excel(
  "Data/FRED-MD.xlsx",
  na = "NA"
)


# 2. Extract transformation codes -----------------------------

# First row of imported data contains the FRED-MD
# transformation codes.
transform_codes = as.numeric(
  unlist(us_raw[1, -1])
)

names(transform_codes) = names(us_raw)[-1]


# 3. Remove transformation-code row ---------------------------

us_data = us_raw[-1, ]

# 4. Create correct monthly date sequence ---------------------

us_data$sasdate = seq(
  from = as.Date("1959-01-01"),
  by = "month",
  length.out = nrow(us_data)
)
head(us_data$sasdate, 15)

# 5. Convert all predictors to numeric ------------------------

for (variable in names(us_data)[-1]) {
  
  us_data[[variable]] = as.numeric(us_data[[variable]])
  
}


# 6. Define FRED-MD transformation function ------------------

transform_fred = function(x, code) {
  
  n = length(x)
  
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
    pct_change = c(
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


# 7. Apply transformations ------------------------------------

us_data_cleaned = us_data

for (variable in names(transform_codes)) {
  
  code = transform_codes[variable]
  
  us_data_cleaned[[variable]] = transform_fred(
    us_data[[variable]],
    code
  )
  
}

# 8. Restrict US data to Feb 1981 - Sep 2025 ------------------

us_data_cleaned = us_data_cleaned[
  us_data_cleaned$sasdate >= as.Date("1981-02-01") &
    us_data_cleaned$sasdate <= as.Date("2025-09-01"),
]

# 9. Drop problematic US predictors ---------------------------

us_data_cleaned = us_data_cleaned[
  , !names(us_data_cleaned) %in% c(
    "ACOGNO",
    "CP3Mx",
    "COMPAPFFx"
  )
]


# 10. Sanity checks ---------------------------------------------

# Check dimensions
dim(us_data_cleaned)

# Check first 15 dates
head(us_data_cleaned$sasdate, 15)

# Check final dates
tail(us_data_cleaned$sasdate, 15)

# Check that dates are strictly increasing
all(diff(us_data_cleaned$sasdate) > 0)

# Check number of missing values in each predictor
colSums(is.na(us_data_cleaned))

# Inspect structure
str(us_data_cleaned)


# 11. Export ----------------------------------------------------

write_xlsx(
  us_data_cleaned,
  "Data/US_data_cleaned.xlsx"
)






# 2. Restrict Canadian data to Feb 1981 - Sep 2025 ------------

can_data_cleaned = can_data_cleaned[
  can_data_cleaned$Date >= as.Date("1981-02-01") &
    can_data_cleaned$Date <= as.Date("2025-09-01"),
]


# 3. Restrict US data to Feb 1981 - Sep 2025 ------------------

us_data_cleaned = us_data_cleaned[
  us_data_cleaned$sasdate >= as.Date("1981-02-01") &
    us_data_cleaned$sasdate <= as.Date("2025-09-01"),
]


# 4. Sanity checks ---------------------------------------------

# Check date ranges
range(can_data_cleaned$Date)
range(us_data_cleaned$sasdate)

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
all(can_data_cleaned$Date == us_data_cleaned$sasdate)




# ----- Add Employment Rate from other Dataset ----

# 1. Load and Clean Employment Rate
EMrate_data = read.csv(
  "Data/Employment Rate.csv",
  na = "NA"
)

names(EMrate_data)

EMrate_data <- EMrate_data[EMrate_data$Labour.force.characteristics == "Employment rate", c("REF_DATE","VALUE")]
#sum(is.na(EMrate_data$REF_DATE)) Check for NAs
#sum(is.na(EMrate_data$VALUE))

names(EMrate_data)[names(EMrate_data) %in% c("REF_DATE","VALUE")] <- c("Date", "Employ_Rate")

EMrate_data$Date <- as.Date(paste0(EMrate_data$Date, "-01")) #Convert dates to proper dates with days

names(EMrate_data)
head(EMrate_data)


# 2. Merge Employment Rate Data with CAN_Data_Cleaned -----------------------------

can_data_cleaned_emrate <- merge(can_data_cleaned, EMrate_data, by = "Date")

#names(can_data_cleaned_emrate)
sum(is.na(can_data_cleaned_emrate$Employ_Rate)) #NA Checks after merge

head(can_data_cleaned_emrate)

# 3. Export final cleaned datasets -----------------------------

write_xlsx(
  can_data_cleaned_emrate,
  "Data/CAN_data_cleaned_emrate.xlsx"
)





