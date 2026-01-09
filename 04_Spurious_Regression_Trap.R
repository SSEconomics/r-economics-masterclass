# ============================================================
# 04_spurious_regression_trap.R
# R Masterclass: Video 04
# Topic: Monte Carlo simulation
# -----------------------------	
# Author: Stephen Snudden, PhD
# YouTube: https://youtube.com/@ssnudden
# GitHub:  https://github.com/SSEconomics/stata-economics-masterclass
# -----------------------------
#
# ============================================================

library(ggplot2)
library(dplyr)
library(tibble)
library(patchwork)  # for combining plots

# ----------------------------
# 1) Small sample (N=100) for visualization + "spurious" regression
# ----------------------------
set.seed(369)

N_vis <- 100
time <- 1:N_vis

# Two independent shock series -> two independent random walks
e1 <- rnorm(N_vis, mean = 0, sd = 1)
e2 <- rnorm(N_vis, mean = 0, sd = 1)
y1 <- cumsum(e1)
y2 <- cumsum(e2)

# ---- Visual inspection plot  ----

df_vis <- tibble(
  time = time,
  y1 = y1,
  y2 = y2
)

p_visual <- ggplot(df_vis, aes(x = time)) +
  geom_line(aes(y = y1), linewidth = 0.9) +
  geom_line(aes(y = y2), linewidth = 0.9, linetype = "dashed") +
  labs(
    title = "The Visual Trap: Spurious Correlation",
    x = NULL,
    y = NULL,
    caption = "These variables are completely unrelated."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave("visual_check.png", p_visual, width = 10, height = 6.665, dpi = 200)

# ---- The “lie”: regression in levels ----
fit_levels <- lm(y1 ~ y2)
print(summary(fit_levels))

# Automated extraction 
# p-value and t-stat for the slope coefficient on y2
coef_tab <- summary(fit_levels)$coefficients
t_stat  <- unname(coef_tab["y2", "t value"])
p_val   <- unname(coef_tab["y2", "Pr(>|t|)"])

cat("\nAutomated extraction (levels regression):\n")
cat("  t-stat (y2) =", formatC(t_stat, format = "f", digits = 6), "\n")
cat("  p-value     =", formatC(p_val,  format = "e", digits = 3), "\n\n")

# ----------------------------
# 2) Simulation loop ("proof")
# ----------------------------
set.seed(369)  # same seed for the simulation block in R

var     <- 1
sims    <- 500
periods <- 100
burn    <- 500
numobs  <- periods + burn
sdv     <- sqrt(var)

# Pre-allocate vectors for speed + clarity
t_stat_lvl  <- numeric(sims)
reject_lvl  <- integer(sims)
t_stat_diff <- numeric(sims)
reject_diff <- integer(sims)

for (s in seq_len(sims)) {
  
  # Generate random walks with burn-in
  eps1 <- rnorm(numobs, mean = 0, sd = sdv)
  eps2 <- rnorm(numobs, mean = 0, sd = sdv)
  rw1  <- cumsum(eps1)
  rw2  <- cumsum(eps2)
  
  # Drop burn-in: keep only the last `periods` observations
  y1s <- rw1[(burn + 1):(burn + periods)]
  y2s <- rw2[(burn + 1):(burn + periods)]
  
  # A) Levels regression (spurious)
  fitA <- lm(y1s ~ y2s)
  tabA <- summary(fitA)$coefficients
  tA   <- unname(tabA["y2s", "t value"])
  pA   <- unname(tabA["y2s", "Pr(>|t|)"])
  
  t_stat_lvl[s] <- tA
  reject_lvl[s] <- as.integer(pA < 0.05)
  
  # B) Differences regression (fix)
  dy1 <- diff(y1s)
  dy2 <- diff(y2s)
  
  fitB <- lm(dy1 ~ dy2)
  tabB <- summary(fitB)$coefficients
  tB   <- unname(tabB["dy2", "t value"])
  pB   <- unname(tabB["dy2", "Pr(>|t|)"])
  
  t_stat_diff[s] <- tB
  reject_diff[s] <- as.integer(pB < 0.05)
}

# Rejection rates (percent)
error_rate_lvl <- round(mean(reject_lvl) * 100, 1)
error_rate_dif <- round(mean(reject_diff) * 100, 1)

cat("Simulation rejection rates:\n")
cat("  Levels      :", error_rate_lvl, "%\n")
cat("  Differences :", error_rate_dif, "%\n\n")

# ----------------------------
# 3) Histograms + combined figure
# ----------------------------

# Helper to build "percent" histograms like Stata's percent option
hist_percent_plot <- function(x, title, subtitle, fill_color = "red") {
  df <- tibble(x = x)
  
  ggplot(df, aes(x = x)) +
    geom_histogram(
      aes(y = after_stat(count / sum(count) * 100)),
      bins = 22,
      fill = fill_color,
      alpha = 0.30,
      color = NA
    ) +
    geom_vline(xintercept = c(-1.96, 1.96), linetype = "dotted", linewidth = 0.8) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "t-statistic",
      y = "Percent"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
}

p_g1 <- hist_percent_plot(
  t_stat_lvl,
  title = "Levels (The Trap)",
  subtitle = paste0("Rejection Rate: ", error_rate_lvl, "% (Target: 5%)"),
  fill_color = "red"
)

p_g2 <- hist_percent_plot(
  t_stat_diff,
  title = "Differences (The Fix)",
  subtitle = paste0("Rejection Rate: ", error_rate_dif, "% (Target: 5%)"),
  fill_color = "green"
)

ggsave("g1.png", p_g1, width = 10, height = 6.665, dpi = 200)
ggsave("g2.png", p_g2, width = 10, height = 6.665, dpi = 200)

p_combined <- (p_g1 | p_g2) +
  plot_annotation(
    title = "Why You Must Test for Unit Roots",
    caption = "Left: Spurious Regression. Right: Stationary Regression."
  )

ggsave("Graph.png", p_combined, width = 12, height = 5.5, dpi = 200)
