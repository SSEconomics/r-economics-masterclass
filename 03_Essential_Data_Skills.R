# ============================================================
# 03_Essential_Skills.R
# R Masterclass: Video 03
# Topic: Graphing, Stylized Facts, Modelling
# -----------------------------	
# Author: Stephen Snudden, PhD
# YouTube: https://youtube.com/@ssnudden
# GitHub:  https://github.com/SSEconomics/stata-economics-masterclass
# -----------------------------

# Datasets
#   - CDataM (monthly data)
#   - CDataQ (quarterly data)

# ============================================================

# Packages 
library(readr)
library(dplyr)
library(zoo)
library(ggplot2)
library(janitor)
library(vars)
library(lmtest)

# ============================================================
# 1. DATA IMPORT & MERGE (Reproducible Automation)
# ============================================================

# 1A) Monthly data: import + clean + monthly time index
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

# -----------------------------
# 1B) Convert monthly -> quarterly by averaging numeric series

# Build a character vector of variables we want to convert
num_vars <- mdata |>
  dplyr::select(where(is.numeric)) |>
  names()

# Drop dates from variables to convert
num_vars <- setdiff(num_vars, c("year", "m"))

# Average to quarterly 
q_ave <- mdata |>
  mutate(qtime = zoo::as.yearqtr(time)) |>
  group_by(qtime) |>
  summarise(across(all_of(num_vars), ~ mean(.x, na.rm = TRUE)), .groups = "drop") |>
  arrange(qtime)

# -----------------------------
# 1C) Quarterly data: import + quarterly time index

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

# -----------------------------
# 1D) Merge into ONE master quarterly dataset
qmerged <- qdata |>
  full_join(q_ave,  by = "qtime") |>
  arrange(qtime)

# ============================================================
# 2. VARIABLE CREATION (The Time Series Engineer)
# ============================================================

qdata_features <- qmerged |>
  mutate(
    c      = c_hh + c_np,                     # real consumption
    c_nom  = c_hh_n + c_np_n,                 # nominal consumption
    gy4    = 100 * (y   / lag(y,   4) - 1),   # YoY real GDP growth
    gp4    = 100 * (cpi / lag(cpi, 4) - 1),   # YoY CPI inflation
    c_share = 100 * (c_nom / y_n)             # consumption share of GDP (%)
  )

# Keep one “analysis sample” object for plotting/stats
qdata_p2014 <- qdata_features |> 
  filter(qtime >= as.yearqtr("1994 Q1"))

# ============================================================
# 3. FIGURES (The Data Artist)
# ============================================================

# [THE STAND-ALONE PRINCIPLE]: 
# If I drop your graph on the floor, can a stranger understand it 
# without reading your paper? If no, not good enough.

# Figure 1: Basic Time Series (The GDP Share)
# [CHECKLIST]:
# 1. Title describes the figure generally? Yes.
# 2. Axes are for UNITS (Percent/Quarters), not variable names? Yes.
# 3. Notes describe Source, Date Range, and Transformations? Yes.
# 4. No blue background? Yes.

# -----------------------------
# Singe-axis  
p_single <- qdata_p2014 |>
  ggplot(aes(x = qtime, y = c_share)) +
  geom_line() +
  labs(
    title = "Consumption Share of GDP",
    x = "Quarter",
    y = "Percent",
    caption = "Notes: Authors' calculations using Statistics Canada Table 36-10-0104-01."
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0),
    plot.caption.position = "plot"
  )

ggsave("fig_ratios.png", p_single, width = 10, height = 5, dpi = 400)

# -----------------------------
# Dual-axis  
scale_fac <- with(qdata_p2014, max(abs(gy4), na.rm = TRUE) / max(abs(gp4), na.rm = TRUE))

p_dual <- ggplot(qdata_p2014, aes(x = qtime)) +
  geom_line(aes(y = gy4, color = "Real GDP"), na.rm = TRUE) +
  geom_line(aes(y = gp4 * scale_fac, color = "CPI Inflation (right axis)"), na.rm = TRUE) +
  scale_y_continuous(
    name = "Percent Change",
    sec.axis = sec_axis(~ . / scale_fac, name = "Percent Change (Inflation)")
  ) +
  scale_color_discrete(breaks = c("CPI Inflation (right axis)", "Real GDP")) +
  labs(x = "Quarter", color = NULL) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 12)
  )

ggsave("yoy_dualaxis.png", p_dual, width = 10, height = 5, dpi = 400)

# -----------------------------
# Scatter Plot

qdata_scatter <- qdata_p2014 |>
  filter(!is.na(gp4), !is.na(gy4)) |>
  mutate(period = if_else(qtime >= as.yearqtr("2010 Q1"), "2010+", "1990–2009"))

p_scatter <- ggplot(qdata_scatter, aes(x = gy4, y = gp4, color = period)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.8) +
  labs(
    title = "GDP Growth and Inflation",
    x = "GDP Growth (percent)",
    y = "CPI Inflation (percent)",
    color = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom",
    legend.text = element_text(size = 12)
  )

ggsave("scatter.png", p_scatter, width = 10, height = 5, dpi = 400)

# ============================================================
# 4. SUMMARY STATS (The Analyst)
# ============================================================

# 1. Only summarize STATIONARY data (Growth rates = Yes, Nominal Levels = No).
# 2. Do not copy output. Use max 3 decimal place max (e.g., 0.752). Precision implies false confidence.

qdata_p2014 |>
  summarise(
    mean_gy4 = mean(gy4, na.rm = TRUE),
    sd_gy4   = sd(gy4, na.rm = TRUE),
    n_gy4    = sum(!is.na(gy4)),
    mean_gp4 = mean(gp4, na.rm = TRUE),
    sd_gp4   = sd(gp4, na.rm = TRUE),
    n_gp4    = sum(!is.na(gp4))
  )

# Build features once (meaningful name)
qdata_p2014 <- qdata_p2014 |>
  mutate(
    L1_gp4 = lag(gp4, 1),
    F1_gp4 = lead(gp4, 1),
    L1_gy4 = lag(gy4, 1),
    F1_gy4 = lead(gy4, 1)
  )

vars1 <- c("L1_gy4", "gy4", "F1_gy4")
print(cor(dplyr::select(qdata_p2014, all_of(vars1)), use = "pairwise.complete.obs"))

vars2 <- c("L1_gp4", "gp4", "F1_gp4", "gy4")
print(cor(dplyr::select(qdata_p2014, all_of(vars2)), use = "pairwise.complete.obs"))

# ============================================================
# 5. The Modeller 
# ============================================================

# 5A) Simple ARDL(1,1): gp4 on lag(gp4) and lag(gy4)
qdata_est <- qdata_p2014 |>
  filter(qtime >= as.yearqtr("1994 Q1"),
         qtime <= as.yearqtr("2009 Q4")) |>
  arrange(qtime) |>
  filter(!is.na(gp4), !is.na(L1_gp4), !is.na(L1_gy4))

fit1 <- lm(gp4 ~ L1_gp4 + L1_gy4, data = qdata_est)

summary(fit1)

# BG test (choose order, e.g., 4 quarters)
lmtest::bgtest(fit1, order = 4)

# Residual autocorrelation 
e1 <- resid(fit1)
acf(e1, lag.max = 12, main = "ACF of ARDL residuals")

# VAR lag selection 
var_data <- qdata_est |>
  dplyr::select(gp4, gy4) |>
  na.omit()
print(VARselect(var_data, lag.max = 12, type = "const")$selection)

# -----------------------------

# 5B) Longer ARDL:
qdata_for <- qdata_p2014 |>
  arrange(qtime) |>
  mutate(
    L1_gy4 = lag(gy4, 1),
    L1_gp4 = lag(gp4, 1), L2_gp4 = lag(gp4, 2), L3_gp4 = lag(gp4, 3),
    L4_gp4 = lag(gp4, 4), L5_gp4 = lag(gp4, 5), L6_gp4 = lag(gp4, 6),
    L7_gp4 = lag(gp4, 7), L8_gp4 = lag(gp4, 8), L9_gp4 = lag(gp4, 9)
  ) 

qdata_est <- qdata_for |>
  filter(qtime <= as.yearqtr("2009 Q4")) |>
  arrange(qtime) |>
  filter(if_all(c(gp4, L1_gy4, L1_gp4, L2_gp4, L3_gp4, L4_gp4, L5_gp4, L6_gp4, L7_gp4, L8_gp4, L9_gp4),
                ~ !is.na(.x)))

fit2 <- lm(gp4 ~ L1_gy4 + L1_gp4 + L2_gp4 + L3_gp4 + L4_gp4 + L5_gp4 + L6_gp4 + L7_gp4 + L8_gp4 + L9_gp4,
           data = qdata_est)

summary(fit2)

lmtest::bgtest(fit2, order = 4)

# H0: beta on L1_gy4 = 0
lmtest::waldtest(fit2, . ~ . - L1_gy4)

# H0: coefficients on L8_gp4 and L9_gp4 are both zero
lmtest::waldtest(fit2, . ~ . - L8_gp4 - L9_gp4)

# -----------------------------
# 5C) Forecast plot: 

qdata_for <- qdata_for |> mutate(f_gp4 = predict(fit2, newdata = qdata_for))

p_forecast <- ggplot(qdata_for |> filter(qtime >= as.yearqtr("2005 Q1")),
                     aes(x = qtime)) +
  geom_line(aes(y = gp4), na.rm = TRUE, color = "blue") +
  geom_line(
    data = qdata_for |> filter(qtime >= as.yearqtr("2010 Q1")),
    aes(y = f_gp4), color = "red",na.rm = TRUE
  ) +
  labs(
    title = "One-Quarter-Ahead CPI Inflation Forecasts",
    x = "Quarter",
    y = "Percent"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")

ggsave("CPIforecast.png", p_forecast, width = 10, height = 5, dpi = 400)
