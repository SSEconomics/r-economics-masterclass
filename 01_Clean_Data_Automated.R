# ============================================================
# 01_clean_data_automated.R
# R Masterclass: Video 01
# Topic: Importing, Frequency Conversion, and Merging
# -----------------------------	
# Author: Stephen Snudden, PhD
# YouTube: https://youtube.com/@ssnudden
# GitHub:  https://github.com/SSEconomics/stata-economics-masterclass
# -----------------------------
  
# Datasets
#   - CDataM (monthly data)
#   - CDataQ (quarterly data)
# ============================================================

# -----------------------------
# 1) Packages (minimal, but convenient)
# -----------------------------

library(readr)
library(dplyr)
library(zoo)
library(ggplot2)
library(janitor)

# -----------------------------
# 2) Load data + monthly time index
# -----------------------------

# Import the raw Comma Separated Values (CSV) file
mdata <- read_csv(
  "CDataM.csv",
  na = c("", "NA", "."),
  show_col_types = FALSE
) |>
  # Convert names to lowercase (janitor:: says its using the janitor package)
  janitor::clean_names() |>
  # Set time series variable
  mutate(
    m = as.integer(m),
    year = as.integer(year),
    time = as.yearmon(sprintf("%d %02d", year, m), "%Y %m")
  ) |>
  arrange(time)

# Test with y-o-y inflation
mdata <- mdata |>
  mutate(gp12    = 100 * (cpi  / lag(cpi,  12) - 1))

# Now is a good time to check your data upload 
#dplyr::glimpse(mdata)
summary(select(mdata, time, cpi, emp, unemp))


#3 checks for time validity, duplicates, sorting
stopifnot(all(mdata$m %in% 1:12))
stopifnot(!anyDuplicated(mdata$time))
stopifnot(is.unsorted(mdata$time) == FALSE)


# ***************************
# 3. Frequency Conversion (Monthly -> Quarterly)
# ***************************
  
# Build a clean character vector of column names that we want to convert
num_vars <- mdata |>
  select(where(is.numeric)) |>
  names()

# Drop dates from variables to convert
num_vars <- setdiff(num_vars, c("year", "m"))

# 1) Quarterly average
q_avg <- mdata |>
  mutate(qtime = zoo::as.yearqtr(time)) |>
  group_by(qtime) |>
  summarise(across(all_of(num_vars), ~ mean(.x, na.rm = TRUE)), .groups = "drop") |>
  arrange(qtime)

# 2) End-of-quarter (last month of quarter)
q_last <- mdata |>
  mutate(qtime = zoo::as.yearqtr(time)) |>
  group_by(qtime) |>
  summarise(across(all_of(num_vars), dplyr::last), .groups = "drop") |>
  arrange(qtime)

# 3) Quarterly sum
q_sum <- mdata |>
  mutate(qtime = zoo::as.yearqtr(time)) |>
  group_by(qtime) |>
  summarise(across(all_of(num_vars), ~ sum(.x, na.rm = TRUE)), .groups = "drop") |>
  arrange(qtime)

# Test with y-o-y inflation
q_avg <- q_avg |>
  mutate(gp4    = 100 * (cpi  / lag(cpi,  4) - 1))

# ============================================================
# 2) Quarterly -> Add time index (DataQ) and SAVE
# ============================================================

# Import the raw Comma Separated Values (CSV) file
qdata <- read_csv(
  "CDataQ.csv",
  na = c("", "NA", "."),
  show_col_types = FALSE
) |>
  clean_names() |>
  mutate(
    q = as.integer(q),
    year = as.integer(year),
    qtime = as.yearqtr(paste(year, q), format = "%Y %q")
  ) |>
  arrange(qtime)

#3 checks for time validity, duplicates, sorting
stopifnot(all(qdata$q %in% 1:4))
stopifnot(!anyDuplicated(qdata$qtime))
stopifnot(is.unsorted(qdata$qtime) == FALSE)

# ============================================================
# 3) Merge + sample restriction (in-memory), plus a check print
# ============================================================

qmerged <- qdata |>
  full_join(q_avg,  by = "qtime") |>
  arrange(qtime)

# ============================================================
# 4) Transformations & Cleanup
# ============================================================

qmerged <- qmerged |>
  # Generate textbook real consumption
  mutate(c = c_hh + c_np) |>
  # Sample restriction
  filter(qtime >= zoo::as.yearqtr("1971 Q1")) |>
  # Drop unnecessary variables
  select(-c_hh, -c_np)

# Save
saveRDS(mdata, "mdata_clean.rds")
saveRDS(qmerged, "qdata_merged_clean.rds")
