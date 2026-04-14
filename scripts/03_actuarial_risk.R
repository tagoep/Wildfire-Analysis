# ============================================================
# U.S. Wildfire Risk Analysis (1992–2015)
# Actuarial Risk Analysis
# ============================================================

library(readr)
library(dplyr)
library(ggplot2)
library(fitdistrplus)
library(MASS)
library(patchwork)
library(scales)


# ── Load clean data ──────────────────────────────────────────
fires <- read_csv("data/fires_clean.csv", show_col_types = FALSE)
cat("Records loaded:", format(nrow(fires), big.mark = ","), "\n")

# ── Annual summaries ─────────────────────────────────────────
annual <- fires %>%
  group_by(FIRE_YEAR) %>%
  summarise(
    num_fires  = n(),
    total_acres = sum(FIRE_SIZE),
    mean_size  = mean(FIRE_SIZE),
    max_size   = max(FIRE_SIZE),
    .groups = "drop"
  )

# ============================================================
# U.S. Wildfire Risk Analysis (1992–2015)
# Loss Severity Analysis
# ============================================================

library(readr)
library(dplyr)
library(ggplot2)
library(fitdistrplus)
library(scales)

fires <- read_csv("data/fires_clean.csv", show_col_types = FALSE)

# ── Large fires only ─────────────────────────────────────────
large_fires <- fires %>%
  filter(FIRE_SIZE >= 10) %>%
  pull(FIRE_SIZE)

cat("Large fires (>=10 acres):", format(length(large_fires), 
                                        big.mark = ","), "\n")

# ── Severity summary ─────────────────────────────────────────
cat("\n--- Severity Summary Statistics ---\n")
cat("Mean:           ", round(mean(large_fires), 2), "acres\n")
cat("Median:         ", round(median(large_fires), 2), "acres\n")
cat("Std deviation:  ", round(sd(large_fires), 2), "acres\n")
cat("95th percentile:", round(quantile(large_fires, 0.95), 2), "acres\n")
cat("99th percentile:", round(quantile(large_fires, 0.99), 2), "acres\n")
cat("Maximum:        ", round(max(large_fires), 2), "acres\n")

# ── Fit distributions ────────────────────────────────────────
# Lognormal — standard for insurance loss severity
# Gamma     — with method of moments starting values to help optimizer
# Weibull   — common in reliability and catastrophe modeling

cat("\n--- Fitting Distributions ---\n")

fit_lnorm <- fitdist(large_fires, "lnorm")

# Gamma with manual starting values from method of moments
mom_shape <- mean(large_fires)^2 / var(large_fires)
mom_rate  <- mean(large_fires)   / var(large_fires)
fit_gamma <- tryCatch(
  fitdist(large_fires, "gamma",
          start = list(shape = mom_shape, rate = mom_rate),
          lower = c(0, 0)),
  error = function(e) {
    cat("Gamma fit failed — using Weibull instead\n")
    NULL
  }
)

fit_weibull <- fitdist(large_fires, "weibull")

cat("\nLognormal — AIC:", round(fit_lnorm$aic, 2),
    "| BIC:", round(fit_lnorm$bic, 2), "\n")

if (!is.null(fit_gamma)) {
  cat("Gamma     — AIC:", round(fit_gamma$aic, 2),
      "| BIC:", round(fit_gamma$bic, 2), "\n")
}

cat("Weibull   — AIC:", round(fit_weibull$aic, 2),
    "| BIC:", round(fit_weibull$bic, 2), "\n")

cat("\nLognormal parameters:\n")
print(fit_lnorm$estimate)
cat("\nWeibull parameters:\n")
print(fit_weibull$estimate)

# ── Determine best fit ────────────────────────────────────────
aic_lnorm   <- fit_lnorm$aic
aic_weibull <- fit_weibull$aic
aic_gamma   <- if (!is.null(fit_gamma)) fit_gamma$aic else Inf

best_model <- c("Lognormal", "Weibull", "Gamma")[
  which.min(c(aic_lnorm, aic_weibull, aic_gamma))]
cat("\nBest fitting distribution by AIC:", best_model, "\n")

# ── Plot 10: Severity distribution ───────────────────────────
plot_data <- data.frame(x = large_fires[large_fires <= 10000])

p10 <- ggplot(plot_data, aes(x = x)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 80, fill = "#2166ac",
                 alpha = 0.6, color = "white") +
  stat_function(fun = dlnorm,
                args = list(meanlog = fit_lnorm$estimate["meanlog"],
                            sdlog   = fit_lnorm$estimate["sdlog"]),
                aes(color = "Lognormal"), linewidth = 1.1) +
  stat_function(fun = dweibull,
                args = list(shape = fit_weibull$estimate["shape"],
                            scale = fit_weibull$estimate["scale"]),
                aes(color = "Weibull"), linewidth = 1.1,
                linetype = "dashed") +
  scale_color_manual(values = c("Lognormal" = "#d6604d",
                                "Weibull"   = "#1b7837")) +
  scale_x_continuous(labels = comma) +
  labs(title = "Loss Severity Distribution — Wildfire Size (>=10 acres)",
       subtitle = paste("Best fit:", best_model, 
                        "| Fires <= 10,000 acres shown for clarity"),
       x = "Fire Size (acres)", y = "Density",
       color = "Distribution",
       caption = "Source: FPA FOD Database 1992–2015") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom")

ggsave("outputs/10_severity_distribution.png",
       p10, width = 11, height = 5, dpi = 150)
cat("\nSaved: 10_severity_distribution.png\n")

# ── Save best model name for use in summary table ────────────
saveRDS(list(best_model = best_model,
             fit_lnorm  = fit_lnorm,
             fit_weibull = fit_weibull,
             fit_gamma  = fit_gamma),
        "data/severity_fits.rds")

cat("Severity fits saved to data/severity_fits.rds\n")
cat("=== Section 1 Complete ===\n")

# ============================================================
# SECTION 2: LOSS FREQUENCY ANALYSIS
# Model annual fire counts as a count distribution
# Poisson vs Negative Binomial
# ============================================================

cat("\n=== Section 2: Loss Frequency Analysis ===\n")

cat("Annual fire counts summary:\n")
print(summary(annual$num_fires))
cat("Variance:", round(var(annual$num_fires), 2), "\n")
cat("Mean:", round(mean(annual$num_fires), 2), "\n")
cat("Variance/Mean ratio:", round(var(annual$num_fires) /
                                    mean(annual$num_fires), 2),
    "(>1 suggests overdispersion)\n")

# Fit Poisson and Negative Binomial
fit_pois <- fitdist(annual$num_fires, "pois")
fit_nb   <- fitdist(annual$num_fires, "nbinom")

cat("\nPoisson         — AIC:", round(fit_pois$aic, 2),
    "| BIC:", round(fit_pois$bic, 2), "\n")
cat("Negative Binom  — AIC:", round(fit_nb$aic, 2),
    "| BIC:", round(fit_nb$bic, 2), "\n")

# ── Plot 11: Annual frequency with distribution fit ───────────
p11 <- ggplot(annual, aes(x = FIRE_YEAR, y = num_fires)) +
  geom_col(fill = "#d6604d", alpha = 0.7) +
  geom_hline(yintercept = mean(annual$num_fires),
             color = "black", linewidth = 0.8,
             linetype = "dashed") +
  geom_hline(yintercept = mean(annual$num_fires) +
               sd(annual$num_fires),
             color = "#762a83", linewidth = 0.7,
             linetype = "dotted") +
  geom_hline(yintercept = mean(annual$num_fires) -
               sd(annual$num_fires),
             color = "#762a83", linewidth = 0.7,
             linetype = "dotted") +
  annotate("text", x = 1993,
           y = mean(annual$num_fires) + 1500,
           label = paste("Mean =",
                         format(round(mean(annual$num_fires)),
                                big.mark = ",")),
           size = 3, color = "black") +
  scale_x_continuous(breaks = 1992:2015) +
  scale_y_continuous(labels = comma) +
  labs(title = "Annual Wildfire Frequency — United States (1992–2015)",
       subtitle = "Dashed = mean | Dotted = ±1 standard deviation",
       x = "Year", y = "Number of Fires",
       caption = "Source: FPA FOD Database") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("outputs/11_annual_frequency.png",
       p11, width = 12, height = 5, dpi = 150)
cat("Saved: 11_annual_frequency.png\n")

# ============================================================
# SECTION 3: PURE RISK PREMIUM BY STATE
# Pure Risk Premium = Expected Frequency x Expected Severity
# This is the actuarial foundation of wildfire insurance pricing
# ============================================================

cat("\n=== Section 3: Pure Risk Premium by State ===\n")

risk_premium <- fires %>%
  group_by(STATE) %>%
  summarise(
    total_fires   = n(),
    total_acres   = sum(FIRE_SIZE),
    avg_frequency = n() / 24,
    avg_severity  = mean(FIRE_SIZE),
    pure_premium  = (n() / 24) * mean(FIRE_SIZE),
    .groups = "drop"
  ) %>%
  arrange(desc(pure_premium))

cat("\n--- Top 15 States by Pure Risk Premium ---\n")
risk_premium %>%
  head(15) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
  print()

# ── Plot 12: Pure risk premium by state ──────────────────────
p12 <- risk_premium %>%
  head(15) %>%
  ggplot(aes(x = fct_reorder(STATE, pure_premium),
             y = pure_premium)) +
  geom_col(fill = "#d6604d", alpha = 0.85) +
  geom_text(aes(label = comma(round(pure_premium))),
            hjust = -0.1, size = 3) +
  coord_flip() +
  scale_y_continuous(labels = comma,
                     limits = c(0,
                                max(risk_premium$pure_premium[1:15])
                                * 1.18)) +
  labs(title = "Wildfire Pure Risk Premium by State (1992–2015)",
       subtitle = "Pure Risk Premium = Annual Avg. Fires × Avg. Fire Size (acres)\nHigher values indicate greater expected annual wildfire exposure",
       x = "State", y = "Pure Risk Premium (acres/year)",
       caption = "Source: FPA FOD Database | Actuarial method") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave("outputs/12_pure_risk_premium.png",
       p12, width = 11, height = 6, dpi = 150)
cat("Saved: 12_pure_risk_premium.png\n")

# ============================================================
# SECTION 4: RETURN PERIOD AND VALUE AT RISK
# Return Period = how often do we expect a year this bad?
# VaR = what is the loss at a given confidence level?
# ============================================================

cat("\n=== Section 4: Return Period and Value at Risk ===\n")

# Sort annual acres burned in ascending order
annual_sorted <- sort(annual$total_acres)
n <- length(annual_sorted)

# Return period = 1 / (1 - empirical CDF)
return_periods <- data.frame(
  total_acres   = annual_sorted,
  rank          = 1:n,
  exceedance    = 1 - (1:n) / (n + 1),
  return_period = (n + 1) / (n + 1 - 1:n)
)

cat("\n--- Annual Acres Burned Summary ---\n")
cat("Mean annual acres:", format(round(mean(annual$total_acres)),
                                 big.mark = ","), "\n")
cat("Median annual acres:", format(round(median(annual$total_acres)),
                                   big.mark = ","), "\n")

# Value at Risk
var_90 <- quantile(annual$total_acres, 0.90)
var_95 <- quantile(annual$total_acres, 0.95)
var_99 <- quantile(annual$total_acres, 0.99)

cat("\n--- Value at Risk (Annual Acres Burned) ---\n")
cat("VaR 90%:", format(round(var_90), big.mark = ","), "acres\n")
cat("VaR 95%:", format(round(var_95), big.mark = ","), "acres\n")
cat("VaR 99%:", format(round(var_99), big.mark = ","), "acres\n")

cat("\n--- Return Period Interpretation ---\n")
cat("Expect a year with >",
    format(round(var_90), big.mark = ","),
    "acres burned once every ~10 years\n")
cat("Expect a year with >",
    format(round(var_95), big.mark = ","),
    "acres burned once every ~20 years\n")

# ── Plot 13: Return period curve ─────────────────────────────
p13 <- ggplot(return_periods,
              aes(x = return_period, y = total_acres)) +
  geom_line(color = "#2166ac", linewidth = 1.2) +
  geom_point(color = "#2166ac", size = 2.5) +
  geom_hline(yintercept = var_90, linetype = "dashed",
             color = "#d6604d", linewidth = 0.8) +
  geom_hline(yintercept = var_95, linetype = "dashed",
             color = "#762a83", linewidth = 0.8) +
  annotate("text", x = max(return_periods$return_period) * 0.6,
           y = var_90 * 1.05,
           label = paste("90th pct —",
                         format(round(var_90), big.mark = ",")),
           color = "#d6604d", size = 3.5) +
  annotate("text", x = max(return_periods$return_period) * 0.6,
           y = var_95 * 1.05,
           label = paste("95th pct —",
                         format(round(var_95), big.mark = ",")),
           color = "#762a83", size = 3.5) +
  scale_x_continuous(breaks = 1:25) +
  scale_y_continuous(labels = comma) +
  labs(title = "Return Period Curve — Annual U.S. Wildfire Acres Burned",
       subtitle = "How often should we expect a year this severe?",
       x = "Return Period (years)",
       y = "Annual Acres Burned",
       caption = "Source: FPA FOD Database 1992–2015") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave("outputs/13_return_period.png",
       p13, width = 11, height = 5, dpi = 150)
cat("Saved: 13_return_period.png\n")

# ── Plot 14: Value at Risk bar chart ─────────────────────────
var_df <- data.frame(
  percentile = c("Mean", "Median", "VaR 90%",
                 "VaR 95%", "VaR 99%"),
  acres = c(mean(annual$total_acres),
            median(annual$total_acres),
            var_90, var_95, var_99)
) %>%
  mutate(percentile = factor(percentile,
                             levels = c("Median", "Mean",
                                        "VaR 90%", "VaR 95%",
                                        "VaR 99%")))

p14 <- ggplot(var_df, aes(x = percentile, y = acres,
                          fill = percentile)) +
  geom_col(alpha = 0.85) +
  geom_text(aes(label = comma(round(acres))),
            vjust = -0.4, size = 3.5) +
  scale_fill_manual(values = c(
    "Median"  = "#2166ac",
    "Mean"    = "#4393c3",
    "VaR 90%" = "#f4a582",
    "VaR 95%" = "#d6604d",
    "VaR 99%" = "#762a83"
  )) +
  scale_y_continuous(labels = comma,
                     limits = c(0, max(var_df$acres) * 1.15)) +
  labs(title = "Value at Risk — Annual U.S. Wildfire Acres Burned",
       subtitle = "Tail risk metrics showing expected loss at different confidence levels",
       x = "Risk Metric", y = "Annual Acres Burned",
       caption = "Source: FPA FOD Database 1992–2015") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "none")

ggsave("outputs/14_value_at_risk.png",
       p14, width = 10, height = 5, dpi = 150)
cat("Saved: 14_value_at_risk.png\n")

# ============================================================
# SECTION 5: RISK METRICS SUMMARY TABLE
# ============================================================

cat("\n=== Section 5: Saving Risk Metrics Summary ===\n")

risk_summary <- data.frame(
  Metric = c(
    "Total fires (1992-2015)",
    "Total acres burned",
    "Mean annual fires",
    "Mean annual acres burned",
    "Median annual acres burned",
    "VaR 90% annual acres",
    "VaR 95% annual acres",
    "VaR 99% annual acres",
    "Mean fire severity (acres)",
    "Median fire severity (acres)",
    "95th pct fire severity",
    "99th pct fire severity",
    "Best severity distribution",
    "Pct human-caused fires",
    "Pct natural-caused fires"
  ),
  Value = c(
    format(nrow(fires), big.mark = ","),
    format(round(sum(fires$FIRE_SIZE)), big.mark = ","),
    format(round(mean(annual$num_fires)), big.mark = ","),
    format(round(mean(annual$total_acres)), big.mark = ","),
    format(round(median(annual$total_acres)), big.mark = ","),
    format(round(var_90), big.mark = ","),
    format(round(var_95), big.mark = ","),
    format(round(var_99), big.mark = ","),
    format(round(mean(fires$FIRE_SIZE), 2)),
    format(round(median(fires$FIRE_SIZE), 2)),
    format(round(quantile(fires$FIRE_SIZE, 0.95), 2)),
    format(round(quantile(fires$FIRE_SIZE, 0.99), 2)),
    ifelse(fit_lnorm$aic < fit_gamma$aic, "Lognormal", "Gamma"),
    paste0(round(mean(fires$CAUSE_TYPE == "Human") * 100, 1), "%"),
    paste0(round(mean(fires$CAUSE_TYPE == "Natural") * 100, 1), "%")
  )
)

write.csv(risk_summary, "outputs/risk_metrics_summary.csv",
          row.names = FALSE)
cat("Saved: risk_metrics_summary.csv\n")
print(risk_summary)
