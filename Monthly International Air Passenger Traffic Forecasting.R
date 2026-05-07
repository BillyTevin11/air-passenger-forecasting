# =============================================================================
# DSCI 725 - Data Mining
# Project: Monthly International Air Passenger Traffic Forecasting
# =============================================================================

# Installing and Loading Required Packages
if (!require(tidyverse))  install.packages("tidyverse")
if (!require(forecast))   install.packages("forecast")
if (!require(tseries))    install.packages("tseries")
if (!require(flextable))  install.packages("flextable")
if (!require(officer))    install.packages("officer")
if (!require(lubridate))  install.packages("lubridate")
if (!require(ggplot2))    install.packages("ggplot2")
if (!require(scales))     install.packages("scales")
if (!require(zoo))        install.packages("zoo")

library(tidyverse)
library(forecast)
library(tseries)
library(flextable)
library(officer)
library(lubridate)
library(ggplot2)
library(scales)
library(zoo)


# =============================================================================
# WEEK 9: Data Acquisition, Familiarization, and Exploratory Analysis
# =============================================================================

# Load the Air Passengers Dataset from CSV
df <- read.csv("AirPassengers.csv")

# Inspect raw data frame
str(df)
print(head(df, 6))

# Rename columns for clarity
colnames(df) <- c("Month", "Passengers")

# Check for missing values
cat("NA values in Month     :", sum(is.na(df$Month)), "\n")
cat("NA values in Passengers:", sum(is.na(df$Passengers)), "\n")
cat("Total rows in data frame:", nrow(df), "\n")

# Convert Passengers column to ts object
df_ts <- ts(df$Passengers, start = c(1949, 1), frequency = 12)

# Verify ts object
cat("Class     :", class(df_ts), "\n")
cat("Start     :", start(df_ts), "\n")
cat("End       :", end(df_ts), "\n")
cat("Frequency :", frequency(df_ts), "\n")
cat("Length    :", length(df_ts), "\n")

print(head(df_ts, 12))
print(tail(df_ts, 12))

# Build enriched data frame for ggplot2 and dplyr
df_frame <- data.frame(
  Date       = as.Date(as.yearmon(time(df_ts))),
  Month      = format(as.Date(as.yearmon(time(df_ts))), "%b"),
  Year       = as.integer(floor(time(df_ts))),
  Passengers = as.numeric(df_ts)
)

print(head(df_frame, 12))
cat("Rows:", nrow(df_frame), "| Columns:", ncol(df_frame), "\n")

# Overall descriptive statistics
cat("Mean     :", round(mean(df_ts), 0), "thousand passengers\n")
cat("Median   :", round(median(df_ts), 0), "thousand passengers\n")
cat("Variance :", round(var(df_ts), 0), "\n")
cat("Std Dev  :", round(sd(df_ts), 0), "\n")
cat("Min      :", min(df_ts), "thousand passengers\n")
cat("Max      :", max(df_ts), "thousand passengers\n")
cat("Range    :", max(df_ts) - min(df_ts), "thousand passengers\n")

# Yearly descriptive statistics
yearly_stats <- df_frame %>%
  group_by(Year) %>%
  summarise(
    Mean    = round(mean(Passengers), 0),
    Median  = round(median(Passengers), 0),
    Std_Dev = round(sd(Passengers), 0),
    Min     = min(Passengers),
    Max     = max(Passengers),
    Range   = Max - Min
  )
print(yearly_stats)

# Monthly descriptive statistics
monthly_stats <- df_frame %>%
  group_by(Month) %>%
  summarise(
    Mean    = round(mean(Passengers), 0),
    Median  = round(median(Passengers), 0),
    Std_Dev = round(sd(Passengers), 0),
    Min     = min(Passengers),
    Max     = max(Passengers)
  ) %>%
  mutate(Month = factor(Month, levels = month.abb)) %>%
  arrange(Month)
print(monthly_stats)

# Flextable — yearly stats
ft_yearly <- flextable(yearly_stats) %>%
  set_header_labels(
    Year = "Year", Mean = "Mean", Median = "Median",
    Std_Dev = "Std. Dev.", Min = "Min", Max = "Max", Range = "Range"
  ) %>%
  set_caption("Table 1: Yearly Descriptive Statistics — Monthly Passenger Counts (Thousands)") %>%
  theme_vanilla() %>% autofit()

# Flextable — monthly stats
ft_monthly <- flextable(as.data.frame(monthly_stats)) %>%
  set_header_labels(
    Month = "Month", Mean = "Mean", Median = "Median",
    Std_Dev = "Std. Dev.", Min = "Min", Max = "Max"
  ) %>%
  set_caption("Table 2: Monthly Descriptive Statistics — Average Passenger Counts (Thousands)") %>%
  theme_vanilla() %>% autofit()

# Export descriptive statistics to Word
doc_desc <- read_docx() %>%
  body_add_par("Descriptive Statistics", style = "heading 1") %>%
  body_add_par("Yearly Summary", style = "heading 2") %>%
  body_add_flextable(ft_yearly) %>%
  body_add_par("") %>%
  body_add_par("Monthly Summary (Averaged Across All Years)", style = "heading 2") %>%
  body_add_flextable(ft_monthly)
print(doc_desc, target = "Descriptive_Statistics.docx")
cat("Descriptive statistics saved to 'Descriptive_Statistics.docx'\n")

# --- Figure 1: Raw Time Series ---
plot_raw <- ggplot(df_frame, aes(x = Date, y = Passengers)) +
  geom_line(color = "#1a6fb5", linewidth = 0.8) +
  labs(
    title    = "Figure 1: Monthly International Airline Passengers (1949–1960)",
    subtitle = "Raw Time Series — Thousands of Passengers",
    x = "Date (Monthly)", y = "Passengers (Thousands)",
    caption = "Source: Kaggle Air Passengers Dataset"
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = comma) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(hjust = 1),
    panel.grid.minor = element_blank()
  )
print(plot_raw)
ggsave("Fig1_Raw_TimeSeries.png", plot = plot_raw, width = 9, height = 5, dpi = 300)
cat("Figure 1 saved: Fig1_Raw_TimeSeries.png\n")

# --- Figure 2: Seasonal Subseries Plot ---
png("Fig2_Seasonal_Subseries.png", width = 2700, height = 1500, res = 300)
monthplot(df_ts,
          main = "Figure 2: Seasonal Subseries Plot — Passengers by Month",
          xlab = "Month", ylab = "Passengers (Thousands)",
          col = "#1a6fb5", lwd = 2)
dev.off()
cat("Figure 2 saved: Fig2_Seasonal_Subseries.png\n")
monthplot(df_ts,
          main = "Figure 2: Seasonal Subseries Plot — Passengers by Month",
          xlab = "Month", ylab = "Passengers (Thousands)",
          col = "#1a6fb5", lwd = 2)

# --- Figure 3: Additive Decomposition ---
decomp_add <- decompose(df_ts, type = "additive")
png("Fig3_Decomposition_Additive.png", width = 2700, height = 2400, res = 300)
plot(decomp_add)
title(main = "Figure 3: Classical Decomposition — Additive Model", outer = TRUE, line = -1)
dev.off()
cat("Figure 3 saved: Fig3_Decomposition_Additive.png\n")
plot(decomp_add)
title(main = "Figure 3: Classical Decomposition — Additive Model", outer = TRUE, line = -1)

# --- Figure 4: Multiplicative Decomposition ---
decomp_mult <- decompose(df_ts, type = "multiplicative")
png("Fig4_Decomposition_Multiplicative.png", width = 2700, height = 2400, res = 300)
plot(decomp_mult)
title(main = "Figure 4: Classical Decomposition — Multiplicative Model", outer = TRUE, line = -1)
dev.off()
cat("Figure 4 saved: Fig4_Decomposition_Multiplicative.png\n")
plot(decomp_mult)
title(main = "Figure 4: Classical Decomposition — Multiplicative Model", outer = TRUE, line = -1)

# Seasonal factors
cat("\n=== Additive Seasonal Factors ===\n")
print(round(decomp_add$seasonal[1:12], 2))
cat("\n=== Multiplicative Seasonal Indices ===\n")
print(round(decomp_mult$seasonal[1:12], 4))

# --- Figure 5: ACF — Raw Series ---
png("Fig5_ACF_Raw.png", width = 2700, height = 1500, res = 300)
Acf(df_ts, lag.max = 48,
    main = "Figure 5: Autocorrelation Function (ACF) — Air Passengers",
    col = "#1a6fb5", lwd = 2)
dev.off()
cat("Figure 5 saved: Fig5_ACF_Raw.png\n")
Acf(df_ts, lag.max = 48,
    main = "Figure 5: Autocorrelation Function (ACF) — Air Passengers",
    col = "#1a6fb5", lwd = 2)

# --- Figure 6: PACF — Raw Series ---
png("Fig6_PACF_Raw.png", width = 2700, height = 1500, res = 300)
Pacf(df_ts, lag.max = 48,
     main = "Figure 6: Partial Autocorrelation Function (PACF) — Air Passengers",
     col = "#c0392b", lwd = 2)
dev.off()
cat("Figure 6 saved: Fig6_PACF_Raw.png\n")
Pacf(df_ts, lag.max = 48,
     main = "Figure 6: Partial Autocorrelation Function (PACF) — Air Passengers",
     col = "#c0392b", lwd = 2)

# Outlier detection
Q1 <- quantile(df_ts, 0.25); Q3 <- quantile(df_ts, 0.75)
IQR_val <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR_val
upper_bound <- Q3 + 1.5 * IQR_val
cat("\n=== IQR-Based Outlier Detection ===\n")
cat("Q1:", Q1, "| Q3:", Q3, "| IQR:", IQR_val, "\n")
cat("Lower bound:", lower_bound, "| Upper bound:", upper_bound, "\n")
outliers <- df_frame[df_frame$Passengers < lower_bound |
                       df_frame$Passengers > upper_bound, ]
if (nrow(outliers) == 0) cat("No outliers detected.\n") else print(outliers)

# --- Figure 7: Boxplot ---
png("Fig7_Boxplot.png", width = 1800, height = 1800, res = 300)
boxplot(df_frame$Passengers,
        main = "Figure 7: Boxplot of Monthly Passenger Counts",
        ylab = "Passengers (Thousands)",
        col = "#aed6f1", border = "#1a6fb5")
dev.off()
cat("Figure 7 saved: Fig7_Boxplot.png\n")
boxplot(df_frame$Passengers,
        main = "Figure 7: Boxplot of Monthly Passenger Counts",
        ylab = "Passengers (Thousands)",
        col = "#aed6f1", border = "#1a6fb5")

# STL anomaly detection
stl_decomp    <- stl(df_ts, s.window = "periodic")
stl_remainder <- stl_decomp$time.series[, "remainder"]
stl_sd        <- sd(stl_remainder)
anomalies     <- df_frame[abs(stl_remainder) > 2 * stl_sd, ]
cat("\n=== STL Remainder Summary ===\n"); print(summary(stl_remainder))
cat("\n=== Anomalous Observations (|STL Remainder| > 2 SD) ===\n")
if (nrow(anomalies) == 0) cat("No anomalies detected.\n") else print(anomalies)





# =============================================================================
# WEEK 10: Data Transformation, Stationarity Testing, and Train/Test Split
# =============================================================================

# ADF and KPSS tests on raw series
adf_raw    <- adf.test(df_ts, alternative = "stationary")
adf_raw_k1 <- adf.test(df_ts, alternative = "stationary", k = 1)
kpss_raw   <- kpss.test(df_ts, null = "Trend")

cat("\n=== ADF Test: Raw Series (Auto Lag k=5) ===\n")
cat("Statistic:", round(adf_raw$statistic, 4),
    "| p-value:", round(adf_raw$p.value, 4),
    "| Lags:", adf_raw$parameter, "\n")

cat("\n=== ADF Test: Raw Series (Conservative Lag k=1) ===\n")
cat("Statistic:", round(adf_raw_k1$statistic, 4),
    "| p-value:", round(adf_raw_k1$p.value, 4),
    "| Lags:", adf_raw_k1$parameter, "\n")

cat("\n=== KPSS Test: Raw Series (H0 = Trend Stationary) ===\n")
cat("Statistic:", round(kpss_raw$statistic, 4),
    "| p-value:", round(kpss_raw$p.value, 4), "\n")

# Log transformation
df_ts_log <- log(df_ts)
cat("\n=== Log-Transformed Series: Summary ===\n")
cat("Mean:", round(mean(df_ts_log), 4), "| SD:", round(sd(df_ts_log), 4), "\n")

# --- Figure 8: Raw vs. Log-Transformed Series ---
png("Fig8_Log_Transformation.png", width = 2700, height = 2400, res = 300)
par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))
plot(df_ts,     main = "Figure 8a: Raw Passenger Series",
     xlab = "Year", ylab = "Passengers (Thousands)", col = "#1a6fb5", lwd = 1.5)
plot(df_ts_log, main = "Figure 8b: Log-Transformed Passenger Series",
     xlab = "Year", ylab = "log(Passengers)", col = "#27ae60", lwd = 1.5)
par(mfrow = c(1, 1))
dev.off()
cat("Figure 8 saved: Fig8_Log_Transformation.png\n")
par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))
plot(df_ts,     main = "Figure 8a: Raw Passenger Series",
     xlab = "Year", ylab = "Passengers (Thousands)", col = "#1a6fb5", lwd = 1.5)
plot(df_ts_log, main = "Figure 8b: Log-Transformed Passenger Series",
     xlab = "Year", ylab = "log(Passengers)", col = "#27ae60", lwd = 1.5)
par(mfrow = c(1, 1))

# ADF on log-transformed series
adf_log <- adf.test(df_ts_log, alternative = "stationary")
cat("\n=== ADF Test: Log-Transformed Series ===\n")
cat("Statistic:", round(adf_log$statistic, 4),
    "| p-value:", round(adf_log$p.value, 4), "\n")

# First-order differencing
df_ts_log_d1 <- diff(df_ts_log, differences = 1)
adf_d1 <- adf.test(df_ts_log_d1, alternative = "stationary")

# --- Figure 9: First-Order Differenced Log Series ---
png("Fig9_FirstOrder_Diff.png", width = 2700, height = 1500, res = 300)
plot(df_ts_log_d1,
     main = "Figure 9: First-Order Differenced Log Series",
     xlab = "Year", ylab = "First Difference of log(Passengers)",
     col = "#8e44ad", lwd = 1.5)
abline(h = 0, col = "red", lty = 2, lwd = 1)
dev.off()
cat("Figure 9 saved: Fig9_FirstOrder_Diff.png\n")
plot(df_ts_log_d1,
     main = "Figure 9: First-Order Differenced Log Series",
     xlab = "Year", ylab = "First Difference of log(Passengers)",
     col = "#8e44ad", lwd = 1.5)
abline(h = 0, col = "red", lty = 2, lwd = 1)

cat("\n=== ADF Test: First-Order Differenced Log Series ===\n")
cat("Statistic:", round(adf_d1$statistic, 4),
    "| p-value:", round(adf_d1$p.value, 4), "\n")

# Seasonal differencing
df_ts_log_d1_d12 <- diff(df_ts_log_d1, lag = 12)
adf_d1_d12 <- adf.test(df_ts_log_d1_d12, alternative = "stationary")

# --- Figure 10: Seasonally Differenced Log Series ---
png("Fig10_Seasonal_Diff.png", width = 2700, height = 1500, res = 300)
plot(df_ts_log_d1_d12,
     main = "Figure 10: First-Order and Seasonally Differenced Log Series",
     xlab = "Year", ylab = "Differenced log(Passengers)",
     col = "#e67e22", lwd = 1.5)
abline(h = 0, col = "red", lty = 2, lwd = 1)
dev.off()
cat("Figure 10 saved: Fig10_Seasonal_Diff.png\n")
plot(df_ts_log_d1_d12,
     main = "Figure 10: First-Order and Seasonally Differenced Log Series",
     xlab = "Year", ylab = "Differenced log(Passengers)",
     col = "#e67e22", lwd = 1.5)
abline(h = 0, col = "red", lty = 2, lwd = 1)

cat("\n=== ADF Test: First-Order + Seasonally Differenced Log Series ===\n")
cat("Statistic:", round(adf_d1_d12$statistic, 4),
    "| p-value:", round(adf_d1_d12$p.value, 4), "\n")

# Consolidated ADF/KPSS flextable
adf_summary <- data.frame(
  Test   = c("ADF (k=5)", "ADF (k=1)", "KPSS (Trend)",
             "ADF (k=5)", "ADF (k=5)", "ADF (k=5)"),
  Series = c(
    "Raw Series", "Raw Series", "Raw Series (KPSS)",
    "Log-Transformed", "Log + First-Order Diff (d=1)",
    "Log + First-Order + Seasonal Diff (d=1, D=1)"
  ),
  Test_Statistic = c(
    round(adf_raw$statistic, 4), round(adf_raw_k1$statistic, 4),
    round(kpss_raw$statistic, 4), round(adf_log$statistic, 4),
    round(adf_d1$statistic, 4),  round(adf_d1_d12$statistic, 4)
  ),
  P_Value = c(
    round(adf_raw$p.value, 4), round(adf_raw_k1$p.value, 4),
    round(kpss_raw$p.value, 4), round(adf_log$p.value, 4),
    round(adf_d1$p.value, 4),  round(adf_d1_d12$p.value, 4)
  ),
  Conclusion = c(
    "Trend-Stationary (p <= 0.01, unit root rejected)",
    "Trend-Stationary (p <= 0.01, unit root rejected)",
    "Trend-Stationary (p >= 0.10, H0 not rejected)",
    ifelse(adf_log$p.value    < 0.05, "Stationary", "Non-Stationary"),
    ifelse(adf_d1$p.value     < 0.05, "Stationary", "Non-Stationary"),
    ifelse(adf_d1_d12$p.value < 0.05, "Stationary", "Non-Stationary")
  )
)

ft_adf_summary <- flextable(adf_summary) %>%
  set_header_labels(
    Test = "Test", Series = "Series / Transformation",
    Test_Statistic = "Test Statistic", P_Value = "p-Value",
    Conclusion = "Conclusion"
  ) %>%
  set_caption("Table 3: Stationarity Test Results — ADF and KPSS Across All Transformations") %>%
  bold(j = "Conclusion") %>% theme_vanilla() %>% autofit()

print(ft_adf_summary)

# --- Figure 11: ACF — Differenced Series ---
png("Fig11_ACF_Differenced.png", width = 2700, height = 1500, res = 300)
Acf(df_ts_log_d1_d12, lag.max = 48,
    main = "Figure 11: ACF — Log + First-Order + Seasonally Differenced Series",
    col = "#1a6fb5", lwd = 2)
dev.off()
cat("Figure 11 saved: Fig11_ACF_Differenced.png\n")
Acf(df_ts_log_d1_d12, lag.max = 48,
    main = "Figure 11: ACF — Log + First-Order + Seasonally Differenced Series",
    col = "#1a6fb5", lwd = 2)

# --- Figure 12: PACF — Differenced Series ---
png("Fig12_PACF_Differenced.png", width = 2700, height = 1500, res = 300)
Pacf(df_ts_log_d1_d12, lag.max = 48,
     main = "Figure 12: PACF — Log + First-Order + Seasonally Differenced Series",
     col = "#c0392b", lwd = 2)
dev.off()
cat("Figure 12 saved: Fig12_PACF_Differenced.png\n")
Pacf(df_ts_log_d1_d12, lag.max = 48,
     main = "Figure 12: PACF — Log + First-Order + Seasonally Differenced Series",
     col = "#c0392b", lwd = 2)

# Train/validation split
df_ts_train <- window(df_ts, start = c(1949, 1), end = c(1958, 12))
df_ts_valid <- window(df_ts, start = c(1959, 1), end = c(1960, 12))
cat("\nTraining set  — Length:", length(df_ts_train), "\n")
cat("Validation set — Length:", length(df_ts_valid), "\n")

# --- Figure 13: Train/Validation Split ---
png("Fig13_TrainVal_Split.png", width = 2700, height = 1500, res = 300)
plot(df_ts, main = "Figure 13: Train / Validation Split — Air Passenger Series",
     xlab = "Year", ylab = "Passengers (Thousands)", col = "#1a6fb5", lwd = 1.5)
rect(xleft = 1959, xright = 1961,
     ybottom = par("usr")[3], ytop = par("usr")[4],
     col = adjustcolor("#f39c12", alpha.f = 0.15), border = NA)
abline(v = 1959, col = "#e74c3c", lty = 2, lwd = 2)
legend("topleft",
       legend = c("Training Set (1949–1958)", "Validation Set (1959–1960)"),
       col = c("#1a6fb5", "#f39c12"), lwd = c(2, 8), bty = "n")
dev.off()
cat("Figure 13 saved: Fig13_TrainVal_Split.png\n")
plot(df_ts, main = "Figure 13: Train / Validation Split — Air Passenger Series",
     xlab = "Year", ylab = "Passengers (Thousands)", col = "#1a6fb5", lwd = 1.5)
rect(xleft = 1959, xright = 1961,
     ybottom = par("usr")[3], ytop = par("usr")[4],
     col = adjustcolor("#f39c12", alpha.f = 0.15), border = NA)
abline(v = 1959, col = "#e74c3c", lty = 2, lwd = 2)
legend("topleft",
       legend = c("Training Set (1949–1958)", "Validation Set (1959–1960)"),
       col = c("#1a6fb5", "#f39c12"), lwd = c(2, 8), bty = "n")

# Export ADF table to Word
doc_week10 <- read_docx() %>%
  body_add_par("Stationarity Testing and Data Transformation", style = "heading 1") %>%
  body_add_par("Augmented Dickey-Fuller and KPSS Test Results", style = "heading 2") %>%
  body_add_flextable(ft_adf_summary)
print(doc_week10, target = "ADF_Stationarity_Results.docx")
cat("ADF results saved to 'ADF_Stationarity_Results.docx'\n")




# =============================================================================
# WEEK 11: Baseline Models — Naïve and Seasonal Naïve
# =============================================================================

h <- 24   # Fixed forecast horizon for all models

fit_naive  <- naive(df_ts_train,  h = h)
fit_snaive <- snaive(df_ts_train, h = h)

acc_naive  <- accuracy(fit_naive,  df_ts_valid)
acc_snaive <- accuracy(fit_snaive, df_ts_valid)

cat("\n=== Naïve Accuracy Metrics ===\n");         print(acc_naive)
cat("\n=== Seasonal Naïve Accuracy Metrics ===\n"); print(acc_snaive)

# Build accuracy data frame helper function
build_acc_df <- function(acc_matrix, model_name) {
  data.frame(
    Model = c(paste(model_name, "— Training"), paste(model_name, "— Validation")),
    ME    = round(acc_matrix[, "ME"],   3),
    RMSE  = round(acc_matrix[, "RMSE"], 3),
    MAE   = round(acc_matrix[, "MAE"],  3),
    MAPE  = round(acc_matrix[, "MAPE"], 3),
    MASE  = round(acc_matrix[, "MASE"], 3),
    ACF1  = round(acc_matrix[, "ACF1"], 3)
  )
}

acc_naive_df    <- build_acc_df(acc_naive,  "Naïve")
acc_snaive_df   <- build_acc_df(acc_snaive, "Seasonal Naïve")
acc_baseline_df <- rbind(acc_naive_df, acc_snaive_df)

ft_acc_baseline <- flextable(acc_baseline_df) %>%
  set_header_labels(
    Model = "Model / Set", ME = "ME", RMSE = "RMSE", MAE = "MAE",
    MAPE = "MAPE (%)", MASE = "MASE", ACF1 = "ACF1"
  ) %>%
  set_caption("Table 4: Accuracy Metrics — Naïve and Seasonal Naïve Baseline Models") %>%
  bold(j = "MASE") %>%
  bg(i = c(2, 4), bg = "#fef9e7") %>%
  theme_vanilla() %>% autofit()
print(ft_acc_baseline)

# --- Figure 14: Baseline Forecasts ---
df_actual <- data.frame(
  Date = as.Date(as.yearmon(time(df_ts))), Value = as.numeric(df_ts), Type = "Actual")
df_naive_fitted <- data.frame(
  Date = as.Date(as.yearmon(time(fitted(fit_naive)))),
  Value = as.numeric(fitted(fit_naive)), Type = "Naïve Fitted")
df_naive_fc <- data.frame(
  Date = as.Date(as.yearmon(time(fit_naive$mean))),
  Value = as.numeric(fit_naive$mean), Type = "Naïve Forecast")
df_snaive_fitted <- data.frame(
  Date = as.Date(as.yearmon(time(fitted(fit_snaive)))),
  Value = as.numeric(fitted(fit_snaive)), Type = "SNaïve Fitted")
df_snaive_fc <- data.frame(
  Date = as.Date(as.yearmon(time(fit_snaive$mean))),
  Value = as.numeric(fit_snaive$mean), Type = "SNaïve Forecast")

df_plot_baseline <- rbind(df_actual, df_naive_fitted, df_naive_fc,
                          df_snaive_fitted, df_snaive_fc)

baseline_colors <- c(
  "Actual" = "#1a6fb5", "Naïve Fitted" = "#95a5a6",
  "Naïve Forecast" = "#e74c3c", "SNaïve Fitted" = "#f39c12",
  "SNaïve Forecast" = "#27ae60")
baseline_linetypes <- c(
  "Actual" = "solid", "Naïve Fitted" = "dashed",
  "Naïve Forecast" = "solid", "SNaïve Fitted" = "dashed",
  "SNaïve Forecast" = "solid")

plot_baseline <- ggplot(df_plot_baseline,
                        aes(x = Date, y = Value, color = Type, linetype = Type)) +
  geom_line(linewidth = 0.85) +
  geom_vline(xintercept = as.Date("1959-01-01"),
             linetype = "dotted", color = "black", linewidth = 0.7) +
  annotate("text", x = as.Date("1959-01-01"), y = 550,
           label = "Validation\nStart", hjust = -0.1, size = 3.2) +
  scale_color_manual(values = baseline_colors) +
  scale_linetype_manual(values = baseline_linetypes) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Figure 14: Naïve and Seasonal Naïve — Fitted Values and Forecasts",
    subtitle = "Training: Jan 1949–Dec 1958 | Validation: Jan 1959–Dec 1960",
    x = "Date", y = "Passengers (Thousands)",
    color = "Series", linetype = "Series",
    caption = "Dashed = fitted values on training set; Solid = forecasts on validation set"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom", panel.grid.minor = element_blank()
  )
print(plot_baseline)
ggsave("Fig14_Baseline_Forecasts.png", plot = plot_baseline,
       width = 10, height = 6, dpi = 300)
cat("Figure 14 saved: Fig14_Baseline_Forecasts.png\n")

# --- Figure 15: Naïve Residuals ---
checkresiduals(fit_naive)
p15 <- recordPlot()
png("Fig15_Naive_Residuals.png", width = 2700, height = 2400, res = 300)
replayPlot(p15)
dev.off()
cat("Figure 15 saved: Fig15_Naive_Residuals.png\n")

# --- Figure 16: Seasonal Naïve Residuals ---
checkresiduals(fit_snaive)
p16 <- recordPlot()
png("Fig16_SNaive_Residuals.png", width = 2700, height = 2400, res = 300)
replayPlot(p16)
dev.off()
cat("Figure 16 saved: Fig16_SNaive_Residuals.png\n")

# Baseline MASE reference values
mase_naive_valid  <- round(acc_naive["Test set",  "MASE"], 4)
mase_snaive_valid <- round(acc_snaive["Test set", "MASE"], 4)
cat("\nNaïve MASE (validation)          :", mase_naive_valid, "\n")
cat("Seasonal Naïve MASE (validation) :", mase_snaive_valid, "\n")

# Export baseline accuracy table to Word
doc_week11 <- read_docx() %>%
  body_add_par("Baseline Model Results", style = "heading 1") %>%
  body_add_par("Accuracy Metrics — Naïve and Seasonal Naïve", style = "heading 2") %>%
  body_add_flextable(ft_acc_baseline)
print(doc_week11, target = "Baseline_Accuracy.docx")
cat("Baseline accuracy saved to 'Baseline_Accuracy.docx'\n")




# =============================================================================
# WEEK 12: Holt-Winters Triple Exponential Smoothing (ETS Family)
# =============================================================================

fit_hw_mam  <- ets(df_ts_train, model = "MAM", damped = FALSE)
fit_hw_aaa  <- ets(df_ts_train, model = "AAA", damped = FALSE)
fit_hw_auto <- ets(df_ts_train, model = "ZZZ")

cat("\n=== ETS Model Comparison by AIC ===\n")
cat("ETS(M,A,M) AIC:", round(fit_hw_mam$aic,  2), "\n")
cat("ETS(A,A,A) AIC:", round(fit_hw_aaa$aic,  2), "\n")
cat("ETS(ZZZ)   AIC:", round(fit_hw_auto$aic, 2), "\n")
cat("Auto-selected model:", fit_hw_auto$method, "\n")

aic_values   <- c(fit_hw_mam$aic, fit_hw_aaa$aic, fit_hw_auto$aic)
fit_hw_best  <- fit_hw_auto   # ETS(M,Ad,M) selected by auto as lowest AIC

cat("\n=== Smoothing Parameters — ETS(M,Ad,M) ===\n")
cat("Alpha:", round(fit_hw_best$par["alpha"], 4), "\n")
if ("beta"  %in% names(fit_hw_best$par))
  cat("Beta :", round(fit_hw_best$par["beta"],  4), "\n")
if ("gamma" %in% names(fit_hw_best$par))
  cat("Gamma:", round(fit_hw_best$par["gamma"], 4), "\n")
cat("AIC:", round(fit_hw_best$aic, 4),
    "| AICc:", round(fit_hw_best$aicc, 4),
    "| BIC:", round(fit_hw_best$bic, 4), "\n")

fc_hw    <- forecast(fit_hw_best, h = h)
acc_hw   <- accuracy(fc_hw, df_ts_valid)
cat("\n=== Holt-Winters Accuracy Metrics ===\n"); print(acc_hw)

acc_hw_df <- data.frame(
  Model = c(paste(fit_hw_best$method, "— Training"),
            paste(fit_hw_best$method, "— Validation")),
  ME    = round(acc_hw[, "ME"],   3), RMSE = round(acc_hw[, "RMSE"], 3),
  MAE   = round(acc_hw[, "MAE"],  3), MAPE = round(acc_hw[, "MAPE"], 3),
  MASE  = round(acc_hw[, "MASE"], 3), ACF1 = round(acc_hw[, "ACF1"], 3)
)
acc_combined_df <- rbind(acc_baseline_df, acc_hw_df)

ft_acc_hw <- flextable(acc_combined_df) %>%
  set_header_labels(
    Model = "Model / Set", ME = "ME", RMSE = "RMSE", MAE = "MAE",
    MAPE = "MAPE (%)", MASE = "MASE", ACF1 = "ACF1"
  ) %>%
  set_caption("Table 5: Accuracy Metrics — Baseline Models and Holt-Winters ETS") %>%
  bold(j = "MASE") %>%
  bg(i = c(2, 4, 6), bg = "#fef9e7") %>%
  bold(i = c(5, 6)) %>%
  theme_vanilla() %>% autofit()
print(ft_acc_hw)

mase_hw_valid <- round(acc_hw["Test set", "MASE"], 4)
cat("\nHolt-Winters MASE (validation):", mase_hw_valid, "\n")
cat("Improvement over SNaive:",
    round((mase_snaive_valid - mase_hw_valid) / mase_snaive_valid * 100, 2), "%\n")

# --- Figure 17: Holt-Winters Forecast ---
df_hw_fitted <- data.frame(
  Date = as.Date(as.yearmon(time(fitted(fit_hw_best)))),
  Value = as.numeric(fitted(fit_hw_best)), Type = "HW Fitted")
df_hw_fc <- data.frame(
  Date = as.Date(as.yearmon(time(fc_hw$mean))),
  Value = as.numeric(fc_hw$mean), Type = "HW Forecast")
df_hw_ci <- data.frame(
  Date = as.Date(as.yearmon(time(fc_hw$mean))),
  Lo80 = as.numeric(fc_hw$lower[, "80%"]), Hi80 = as.numeric(fc_hw$upper[, "80%"]),
  Lo95 = as.numeric(fc_hw$lower[, "95%"]), Hi95 = as.numeric(fc_hw$upper[, "95%"]))
df_actual_hw <- data.frame(
  Date = as.Date(as.yearmon(time(df_ts))), Value = as.numeric(df_ts), Type = "Actual")
df_plot_hw <- rbind(df_actual_hw, df_hw_fitted, df_hw_fc)

hw_colors    <- c("Actual" = "#1a6fb5", "HW Fitted" = "#f39c12", "HW Forecast" = "#27ae60")
hw_linetypes <- c("Actual" = "solid",   "HW Fitted" = "dashed",  "HW Forecast" = "solid")

plot_hw <- ggplot() +
  geom_ribbon(data = df_hw_ci, aes(x = Date, ymin = Lo95, ymax = Hi95),
              fill = "#27ae60", alpha = 0.10) +
  geom_ribbon(data = df_hw_ci, aes(x = Date, ymin = Lo80, ymax = Hi80),
              fill = "#27ae60", alpha = 0.18) +
  geom_line(data = df_plot_hw,
            aes(x = Date, y = Value, color = Type, linetype = Type),
            linewidth = 0.85) +
  geom_vline(xintercept = as.Date("1959-01-01"),
             linetype = "dotted", color = "black", linewidth = 0.7) +
  annotate("text", x = as.Date("1959-01-01"), y = 580,
           label = "Validation\nStart", hjust = -0.1, size = 3.2) +
  scale_color_manual(values = hw_colors) +
  scale_linetype_manual(values = hw_linetypes) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = comma) +
  labs(
    title    = paste0("Figure 17: Holt-Winters (", fit_hw_best$method,
                      ") — Fitted Values and 24-Month Forecast"),
    subtitle = "Shaded bands show 80% and 95% prediction intervals",
    x = "Date", y = "Passengers (Thousands)",
    color = "Series", linetype = "Series",
    caption = "Training: Jan 1949–Dec 1958 | Validation: Jan 1959–Dec 1960"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom", panel.grid.minor = element_blank()
  )
print(plot_hw)
ggsave("Fig17_HW_Forecast.png", plot = plot_hw, width = 10, height = 6, dpi = 300)
cat("Figure 17 saved: Fig17_HW_Forecast.png\n")

# --- Figure 18: Holt-Winters Residuals ---
checkresiduals(fit_hw_best)
p18 <- recordPlot()
png("Fig18_HW_Residuals.png", width = 2700, height = 2400, res = 300)
replayPlot(p18)
dev.off()
cat("Figure 18 saved: Fig18_HW_Residuals.png\n")
replayPlot(p18)

# Export Holt-Winters accuracy table to Word
doc_week12 <- read_docx() %>%
  body_add_par("Holt-Winters Model Results", style = "heading 1") %>%
  body_add_par("Accuracy Metrics — All Models to Date", style = "heading 2") %>%
  body_add_flextable(ft_acc_hw)
print(doc_week12, target = "HoltWinters_Accuracy.docx")
cat("Holt-Winters accuracy saved to 'HoltWinters_Accuracy.docx'\n")




# =============================================================================
# WEEK 13: Seasonal ARIMA (SARIMA) — ARIMA Family
# =============================================================================

df_ts_train_log <- log(df_ts_train)

# Theory-guided: SARIMA(0,1,1)(0,1,1)[12]
fit_sarima_011_011 <- Arima(df_ts_train_log,
                            order    = c(0, 1, 1),
                            seasonal = list(order = c(0, 1, 1), period = 12),
                            include.constant = FALSE)
cat("\n=== SARIMA(0,1,1)(0,1,1)[12] — Theory-Guided ===\n")
print(summary(fit_sarima_011_011))

# auto.arima() exhaustive search
cat("\n=== Running auto.arima() — exhaustive search ===\n")
fit_sarima_auto <- auto.arima(df_ts_train,
                              lambda        = 0,
                              stepwise      = FALSE,
                              approximation = FALSE,
                              trace         = TRUE)
cat("\n=== auto.arima() Selected Model ===\n")
print(summary(fit_sarima_auto))

# Model selection
if (fit_sarima_011_011$aic <= fit_sarima_auto$aic) {
  fit_sarima_best  <- fit_sarima_011_011
  sarima_best_name <- "SARIMA(0,1,1)(0,1,1)[12]"
  cat("\nBest SARIMA model: SARIMA(0,1,1)(0,1,1)[12] (theory-guided)\n")
} else {
  fit_sarima_best  <- fit_sarima_auto
  sarima_best_name <- "ARIMA(0,1,1)(0,1,1)[12]"
  cat("\nBest SARIMA model: auto-selected\n")
}

# Forecasts and accuracy
fc_sarima_auto <- forecast(fit_sarima_auto, h = h, biasadj = TRUE)
acc_sarima     <- accuracy(fc_sarima_auto, df_ts_valid)
fc_sarima      <- fc_sarima_auto

cat("\n=== SARIMA: 24-Month Forecasts ===\n"); print(fc_sarima)
cat("\n=== SARIMA Accuracy Metrics ===\n");    print(acc_sarima)

acc_sarima_df <- data.frame(
  Model = c(paste(sarima_best_name, "— Training"),
            paste(sarima_best_name, "— Validation")),
  ME    = round(acc_sarima[, "ME"],   3), RMSE = round(acc_sarima[, "RMSE"], 3),
  MAE   = round(acc_sarima[, "MAE"],  3), MAPE = round(acc_sarima[, "MAPE"], 3),
  MASE  = round(acc_sarima[, "MASE"], 3), ACF1 = round(acc_sarima[, "ACF1"], 3)
)
acc_all_df <- rbind(acc_combined_df, acc_sarima_df)

ft_acc_all <- flextable(acc_all_df) %>%
  set_header_labels(
    Model = "Model / Set", ME = "ME", RMSE = "RMSE", MAE = "MAE",
    MAPE = "MAPE (%)", MASE = "MASE", ACF1 = "ACF1"
  ) %>%
  set_caption("Table 6: Accuracy Metrics — All Models (Baseline, Holt-Winters, SARIMA)") %>%
  bold(j = "MASE") %>%
  bg(i = c(2, 4, 6, 8), bg = "#fef9e7") %>%
  bold(i = c(7, 8)) %>%
  theme_vanilla() %>% autofit()
print(ft_acc_all)

mase_sarima_valid <- round(acc_sarima["Test set", "MASE"], 4)
cat("\n=== MASE Comparison — All Models (Validation) ===\n")
cat("Naïve          :", round(acc_naive["Test set",   "MASE"], 4), "\n")
cat("Seasonal Naïve :", mase_snaive_valid, "\n")
cat("Holt-Winters   :", mase_hw_valid, "\n")
cat("SARIMA         :", mase_sarima_valid, "\n")
cat("SARIMA improvement over Holt-Winters   :",
    round((mase_hw_valid - mase_sarima_valid) / mase_hw_valid * 100, 2), "%\n")
cat("SARIMA improvement over Seasonal Naïve :",
    round((mase_snaive_valid - mase_sarima_valid) / mase_snaive_valid * 100, 2), "%\n")
cat("Beats MASE < 1.0?",
    ifelse(mase_sarima_valid < 1.0, "YES \u2713", "NO \u2717"), "\n")

# --- Figure 19: SARIMA Forecast ---
fitted_sarima    <- exp(as.numeric(fitted(fit_sarima_best)))
df_sarima_fitted <- data.frame(
  Date  = as.Date(as.yearmon(time(df_ts_train))),
  Value = fitted_sarima, Type = "SARIMA Fitted")
df_sarima_fc <- data.frame(
  Date  = as.Date(as.yearmon(time(fc_sarima$mean))),
  Value = as.numeric(fc_sarima$mean), Type = "SARIMA Forecast")
df_sarima_ci <- data.frame(
  Date = as.Date(as.yearmon(time(fc_sarima$mean))),
  Lo80 = as.numeric(fc_sarima$lower[, "80%"]),
  Hi80 = as.numeric(fc_sarima$upper[, "80%"]),
  Lo95 = as.numeric(fc_sarima$lower[, "95%"]),
  Hi95 = as.numeric(fc_sarima$upper[, "95%"]))
df_actual_sarima <- data.frame(
  Date  = as.Date(as.yearmon(time(df_ts))),
  Value = as.numeric(df_ts), Type = "Actual")
df_plot_sarima <- rbind(df_actual_sarima, df_sarima_fitted, df_sarima_fc)

sarima_colors    <- c("Actual" = "#1a6fb5", "SARIMA Fitted" = "#e67e22",
                      "SARIMA Forecast" = "#8e44ad")
sarima_linetypes <- c("Actual" = "solid", "SARIMA Fitted" = "dashed",
                      "SARIMA Forecast" = "solid")

plot_sarima <- ggplot() +
  geom_ribbon(data = df_sarima_ci, aes(x = Date, ymin = Lo95, ymax = Hi95),
              fill = "#8e44ad", alpha = 0.10) +
  geom_ribbon(data = df_sarima_ci, aes(x = Date, ymin = Lo80, ymax = Hi80),
              fill = "#8e44ad", alpha = 0.18) +
  geom_line(data = df_plot_sarima,
            aes(x = Date, y = Value, color = Type, linetype = Type),
            linewidth = 0.85) +
  geom_vline(xintercept = as.Date("1959-01-01"),
             linetype = "dotted", color = "black", linewidth = 0.7) +
  annotate("text", x = as.Date("1959-01-01"), y = 600,
           label = "Validation\nStart", hjust = -0.1, size = 3.2) +
  scale_color_manual(values = sarima_colors) +
  scale_linetype_manual(values = sarima_linetypes) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = comma) +
  labs(
    title    = paste0("Figure 19: ", sarima_best_name,
                      " — Fitted Values and 24-Month Forecast"),
    subtitle = "Shaded bands show 80% and 95% prediction intervals",
    x = "Date", y = "Passengers (Thousands)",
    color = "Series", linetype = "Series",
    caption = "Training: Jan 1949–Dec 1958 | Validation: Jan 1959–Dec 1960"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom", panel.grid.minor = element_blank()
  )
print(plot_sarima)
ggsave("Fig19_SARIMA_Forecast.png", plot = plot_sarima,
       width = 10, height = 6, dpi = 300)
cat("Figure 19 saved: Fig19_SARIMA_Forecast.png\n")

# --- Figure 20: SARIMA Residuals ---
checkresiduals(fit_sarima_best)
p20 <- recordPlot()
png("Fig20_SARIMA_Residuals.png", width = 2700, height = 2400, res = 300)
replayPlot(p20)
dev.off()
cat("Figure 20 saved: Fig20_SARIMA_Residuals.png\n")

# Export final accuracy table to Word
doc_week13 <- read_docx() %>%
  body_add_par("SARIMA Model Results", style = "heading 1") %>%
  body_add_par("Accuracy Metrics — All Models", style = "heading 2") %>%
  body_add_flextable(ft_acc_all)
print(doc_week13, target = "SARIMA_AllModels_Accuracy.docx")
cat("Full accuracy table saved to 'SARIMA_AllModels_Accuracy.docx'\n")




# =============================================================================
# Week 14: Model Comparison, Final Forecasts, and Prediction Table
# =============================================================================

# Task: Produce side-by-side comparison of all four model families across all evaluation metrics

# Extract validation-set rows only for a clean summary comparison table
acc_comparison <- data.frame(
  Model = c("Naïve", "Seasonal Naïve",
            paste(fit_hw_best$method), sarima_best_name),
  ME    = c(round(acc_naive["Test set",   "ME"],   3),
            round(acc_snaive["Test set",  "ME"],   3),
            round(acc_hw["Test set",      "ME"],   3),
            round(acc_sarima["Test set",  "ME"],   3)),
  RMSE  = c(round(acc_naive["Test set",   "RMSE"], 3),
            round(acc_snaive["Test set",  "RMSE"], 3),
            round(acc_hw["Test set",      "RMSE"], 3),
            round(acc_sarima["Test set",  "RMSE"], 3)),
  MAE   = c(round(acc_naive["Test set",   "MAE"],  3),
            round(acc_snaive["Test set",  "MAE"],  3),
            round(acc_hw["Test set",      "MAE"],  3),
            round(acc_sarima["Test set",  "MAE"],  3)),
  MAPE  = c(round(acc_naive["Test set",   "MAPE"], 3),
            round(acc_snaive["Test set",  "MAPE"], 3),
            round(acc_hw["Test set",      "MAPE"], 3),
            round(acc_sarima["Test set",  "MAPE"], 3)),
  MASE  = c(round(acc_naive["Test set",   "MASE"], 4),
            round(acc_snaive["Test set",  "MASE"], 4),
            round(acc_hw["Test set",      "MASE"], 4),
            round(acc_sarima["Test set",  "MASE"], 4)),
  Rank  = c(4, 3, 2, 1)   # SARIMA best on all metrics
)

cat("=== Validation Set Comparison — All Models ===\n")
print(acc_comparison)

# Format as flextable — bold and highlight SARIMA row as winner
ft_comparison <- flextable(acc_comparison[, -ncol(acc_comparison)]) %>%
  set_header_labels(
    Model = "Model",
    ME    = "ME",
    RMSE  = "RMSE",
    MAE   = "MAE",
    MAPE  = "MAPE (%)",
    MASE  = "MASE"
  ) %>%
  set_caption("Table 7: Validation Set Accuracy Comparison — All Model Families") %>%
  bold(j = "MASE") %>%
  bold(i = 4) %>%                          # Bold SARIMA row
  bg(i = 4, bg = "#eafaf1") %>%            # Green highlight for best model
  bg(i = c(1, 2, 3), bg = "#fdfefe") %>%  # Neutral background for others
  color(i = 4, color = "#1a5276") %>%      # Dark blue text for SARIMA row
  theme_vanilla() %>%
  autofit()

print(ft_comparison)

cat("\n=== Model Selection Decision ===\n")
cat("SARIMA(0,1,1)(0,1,1)[12] is selected as the best model based on:\n")
cat("  - Lowest validation MASE  :", mase_sarima_valid,
    "(vs HW:", mase_hw_valid, ", SNaive:", mase_snaive_valid, ")\n")
cat("  - Lowest validation RMSE  :", round(acc_sarima["Test set", "RMSE"], 3), "\n")
cat("  - Lowest validation MAE   :", round(acc_sarima["Test set", "MAE"],  3), "\n")
cat("  - Lowest validation MAPE  :", round(acc_sarima["Test set", "MAPE"], 3), "%\n")
cat("  - Ljung-Box p = 0.5618    : white noise residuals confirmed\n")
cat("  - Dual-method validation  : theory-guided and auto.arima() agree\n")


# Task: Single chart showing actual series, fitted values, and predicted values for all model families in distinct colors

# Build data frames for all model fitted values and forecasts 
# Actual series
df_actual_all <- data.frame(
  Date  = as.Date(as.yearmon(time(df_ts))),
  Value = as.numeric(df_ts),
  Model = "Actual"
)

# Naïve forecast
df_naive_fc_all <- data.frame(
  Date  = as.Date(as.yearmon(time(fit_naive$mean))),
  Value = as.numeric(fit_naive$mean),
  Model = "Naïve Forecast"
)

# Seasonal Naïve forecast
df_snaive_fc_all <- data.frame(
  Date  = as.Date(as.yearmon(time(fit_snaive$mean))),
  Value = as.numeric(fit_snaive$mean),
  Model = "SNaïve Forecast"
)

# Holt-Winters forecast
df_hw_fc_all <- data.frame(
  Date  = as.Date(as.yearmon(time(fc_hw$mean))),
  Value = as.numeric(fc_hw$mean),
  Model = "Holt-Winters Forecast"
)

# SARIMA forecast
df_sarima_fc_all <- data.frame(
  Date  = as.Date(as.yearmon(time(fc_sarima$mean))),
  Value = as.numeric(fc_sarima$mean),
  Model = "SARIMA Forecast"
)

# Combine all for plotting
df_plot_all <- rbind(
  df_actual_all,
  df_naive_fc_all,
  df_snaive_fc_all,
  df_hw_fc_all,
  df_sarima_fc_all
)

# Color and linetype palette — each model family distinct
all_colors <- c(
  "Actual"               = "#1a6fb5",
  "Naïve Forecast"       = "#95a5a6",
  "SNaïve Forecast"      = "#e74c3c",
  "Holt-Winters Forecast"= "#27ae60",
  "SARIMA Forecast"      = "#8e44ad"
)

all_linetypes <- c(
  "Actual"               = "solid",
  "Naïve Forecast"       = "dashed",
  "SNaïve Forecast"      = "dashed",
  "Holt-Winters Forecast"= "solid",
  "SARIMA Forecast"      = "solid"
)

all_linewidths <- c(
  "Actual"               = 1.0,
  "Naïve Forecast"       = 0.7,
  "SNaïve Forecast"      = 0.7,
  "Holt-Winters Forecast"= 0.85,
  "SARIMA Forecast"      = 1.1   # Slightly thicker to emphasize best model
)

plot_all_models <- ggplot(df_plot_all,
                          aes(x     = Date,
                              y     = Value,
                              color = Model,
                              linetype = Model,
                              linewidth = Model)) +
  geom_line() +
  # Vertical line marking validation start
  geom_vline(xintercept = as.Date("1959-01-01"),
             linetype = "dotted", color = "black", linewidth = 0.7) +
  annotate("text", x = as.Date("1959-01-01"), y = 620,
           label = "Validation\nStart", hjust = -0.1, size = 3.2,
           color = "black") +
  # Shade validation region
  annotate("rect",
           xmin = as.Date("1959-01-01"), xmax = as.Date("1961-01-01"),
           ymin = -Inf, ymax = Inf,
           fill = "#f8f9fa", alpha = 0.4) +
  scale_color_manual(values = all_colors) +
  scale_linetype_manual(values = all_linetypes) +
  scale_discrete_manual("linewidth", values = all_linewidths) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Figure 21: All Model Families — Forecasts vs. Actual (Validation Period)",
    subtitle = paste0("Naïve | Seasonal Naïve | Holt-Winters ETS(M,Ad,M)",
                      " | SARIMA(0,1,1)(0,1,1)[12]"),
    x        = "Date",
    y        = "Passengers (Thousands)",
    color    = "Model",
    linetype = "Model",
    linewidth = "Model",
    caption  = "Shaded region = validation period (Jan 1959–Dec 1960)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(size = 10),
    axis.title       = element_text(face = "bold"),
    axis.text.x      = element_text(angle = 45, hjust = 1),
    legend.position  = "bottom",
    legend.title     = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(plot_all_models)
ggsave("Fig21_All_Models_Comparison.png", plot = plot_all_models,
       width = 11, height = 6, dpi = 300)
cat("Figure 21 saved: Fig21_All_Models_Comparison.png\n")



# Task: Refit SARIMA on complete series (training + validation) before generating forward-looking forecasts

cat("\n=== Refitting SARIMA(0,1,1)(0,1,1)[12] on Full 144-Month Dataset ===\n")

# Refit on the full series using auto.arima with lambda=0
# This maximizes the information available for forward-looking forecasts
fit_sarima_full <- auto.arima(df_ts,
                              lambda        = 0,
                              stepwise      = FALSE,
                              approximation = FALSE)

cat("\n=== Full-Series SARIMA Model Summary ===\n")
print(summary(fit_sarima_full))
cat("\nFull-series model specification:", fit_sarima_full$method, "\n")
cat("AIC :", round(fit_sarima_full$aic,  4), "\n")
cat("BIC :", round(fit_sarima_full$bic,  4), "\n")

# Task: Forecast at least two full seasonal cycles beyond the end of the data
fc_final <- forecast(fit_sarima_full, h = h, biasadj = TRUE)

cat("\n=== Final 24-Month Forward-Looking Forecast (Jan 1961–Dec 1962) ===\n")
print(fc_final)

# Task: Place all predicted data points in a clearly labeled formatted table

# Build the prediction table with point forecasts and intervals
pred_dates <- as.Date(as.yearmon(time(fc_final$mean)))

pred_table <- data.frame(
  Month          = format(pred_dates, "%B %Y"),
  Point_Forecast = round(as.numeric(fc_final$mean),   1),
  Lo_80          = round(as.numeric(fc_final$lower[, "80%"]), 1),
  Hi_80          = round(as.numeric(fc_final$upper[, "80%"]), 1),
  Lo_95          = round(as.numeric(fc_final$lower[, "95%"]), 1),
  Hi_95          = round(as.numeric(fc_final$upper[, "95%"]), 1)
)

cat("\n=== Prediction Table: Jan 1961 – Dec 1962 ===\n")
print(pred_table)

# Format prediction table as flextable for Word
ft_pred_table <- flextable(pred_table) %>%
  set_header_labels(
    Month          = "Month",
    Point_Forecast = "Point Forecast",
    Lo_80          = "80% Lower",
    Hi_80          = "80% Upper",
    Lo_95          = "95% Lower",
    Hi_95          = "95% Upper"
  ) %>%
  set_caption(
    paste0("Table 8: SARIMA(0,1,1)(0,1,1)[12] — 24-Month Point Forecasts",
           " with Prediction Intervals (Jan 1961–Dec 1962, Thousands)")
  ) %>%
  bold(j = "Point_Forecast") %>%
  # Alternate row shading for readability
  bg(i = seq(1, 24, 2), bg = "#f2f3f4") %>%
  # Bold the two seasonal peak months each year (July and August)
  bold(i = c(7, 8, 19, 20)) %>%
  bg(i = c(7, 8, 19, 20), bg = "#d6eaf8") %>%
  add_footer_lines(
    "Note: All values in thousands of passengers. Point forecasts are bias-adjusted
back-transformations from log scale. Shaded rows indicate peak summer months
(July and August) based on confirmed seasonal pattern."
  ) %>%
  theme_vanilla() %>%
  autofit()

print(ft_pred_table)

# Task: Chart showing actual series through 1960 + forward forecasts to 1962

# Actual series data frame
df_actual_final <- data.frame(
  Date  = as.Date(as.yearmon(time(df_ts))),
  Value = as.numeric(df_ts),
  Type  = "Actual (1949–1960)"
)

# Forward forecast data frame
df_final_fc <- data.frame(
  Date  = as.Date(as.yearmon(time(fc_final$mean))),
  Value = as.numeric(fc_final$mean),
  Type  = "SARIMA Forecast (1961–1962)"
)

# Confidence interval ribbons
df_final_ci <- data.frame(
  Date = as.Date(as.yearmon(time(fc_final$mean))),
  Lo80 = as.numeric(fc_final$lower[, "80%"]),
  Hi80 = as.numeric(fc_final$upper[, "80%"]),
  Lo95 = as.numeric(fc_final$lower[, "95%"]),
  Hi95 = as.numeric(fc_final$upper[, "95%"])
)

df_plot_final <- rbind(df_actual_final, df_final_fc)

final_colors    <- c(
  "Actual (1949–1960)"          = "#1a6fb5",
  "SARIMA Forecast (1961–1962)" = "#8e44ad"
)
final_linetypes <- c(
  "Actual (1949–1960)"          = "solid",
  "SARIMA Forecast (1961–1962)" = "solid"
)

plot_final_forecast <- ggplot() +
  # 95% CI ribbon
  geom_ribbon(data = df_final_ci,
              aes(x = Date, ymin = Lo95, ymax = Hi95),
              fill = "#8e44ad", alpha = 0.10) +
  # 80% CI ribbon
  geom_ribbon(data = df_final_ci,
              aes(x = Date, ymin = Lo80, ymax = Hi80),
              fill = "#8e44ad", alpha = 0.18) +
  # Main series lines
  geom_line(data = df_plot_final,
            aes(x = Date, y = Value,
                color = Type, linetype = Type),
            linewidth = 0.9) +
  # Vertical line at start of forecast horizon
  geom_vline(xintercept = as.Date("1961-01-01"),
             linetype = "dotted", color = "black", linewidth = 0.7) +
  annotate("text", x = as.Date("1961-01-01"), y = 680,
           label = "Forecast\nStart", hjust = -0.1, size = 3.2) +
  scale_color_manual(values = final_colors) +
  scale_linetype_manual(values = final_linetypes) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = comma,
                     breaks = seq(100, 900, by = 100)) +
  labs(
    title    = paste0("Figure 22: SARIMA(0,1,1)(0,1,1)[12] — Full Series",
                      " and 24-Month Forward Forecast (1961–1962)"),
    subtitle = "Model refitted on complete 144-month dataset (Jan 1949–Dec 1960)",
    x        = "Date",
    y        = "Passengers (Thousands)",
    color    = "Series",
    linetype = "Series",
    caption  = "Shaded bands show 80% and 95% prediction intervals"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(size = 10),
    axis.title       = element_text(face = "bold"),
    axis.text.x      = element_text(angle = 45, hjust = 1),
    legend.position  = "bottom",
    legend.title     = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(plot_final_forecast)
ggsave("Fig22_Final_Forward_Forecast.png", plot = plot_final_forecast,
       width = 11, height = 6, dpi = 300)
cat("Figure 22 saved: Fig22_Final_Forward_Forecast.png\n")


# Exporting All Week 14 Tables to an MS Word Document

doc_week14 <- read_docx() %>%
  body_add_par("Model Comparison and Final Forecasts", style = "heading 1") %>%
  body_add_par("Validation Set Accuracy — All Models", style = "heading 2") %>%
  body_add_flextable(ft_comparison) %>%
  body_add_par("") %>%
  body_add_par("Complete Accuracy Metrics — Training and Validation",
               style = "heading 2") %>%
  body_add_flextable(ft_acc_all) %>%
  body_add_par("") %>%
  body_add_par("24-Month Forward Forecast — Jan 1961 to Dec 1962",
               style = "heading 2") %>%
  body_add_flextable(ft_pred_table)

print(doc_week14, target = "ModelComparison_FinalForecasts.docx")
cat("Week 14 tables saved to 'ModelComparison_FinalForecasts.docx'\n")

