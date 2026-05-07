# Monthly International Air Passenger Traffic Forecasting

**Author:** Tevin Ochieng

---

## Project Overview

This project develops and evaluates a time series forecasting model
for monthly international airline passenger volumes using 144 months
of historical data (January 1949 – December 1960).

**Business Goal:** Minimize operational costs and protect revenue in
the aviation and travel industry by producing accurate 24-month
forward forecasts of passenger demand.

**Dataset:** Air Passengers Dataset (Kaggle)
[https://www.kaggle.com/datasets/rakannimer/air-passengers](https://www.kaggle.com/datasets/rakannimer/air-passengers)

---

## Models Evaluated

| Model | Family | Validation MASE | Validation MAPE |
|---|---|---|---|
| Naïve | Baseline | 4.033 | 23.58% |
| Seasonal Naïve | Baseline | 2.494 | 15.52% |
| ETS(M,Ad,M) | Holt-Winters | 2.212 | 13.30% |
| **SARIMA(0,1,1)(0,1,1)[12]** | **ARIMA** | **1.299** | **8.02%** |

**Recommended model:** SARIMA(0,1,1)(0,1,1)[12]
- Validation RMSE: 40.59
- Ljung-Box p-value: 0.5618 (white noise confirmed)
- 41.3% MASE improvement over Holt-Winters ETS

---

## Key Findings

- The series exhibits a sustained upward trend, strong annual
  seasonality (period = 12), and multiplicative seasonal amplitude
- SARIMA(0,1,1)(0,1,1)[12] was confirmed by both theory-guided
  ACF/PACF analysis and an exhaustive auto.arima() search of 98
  candidate specifications — both approaches arrived at identical
  coefficients (ma1 = -0.3424, sma1 = -0.5405)
- The model was refitted on the full 144-month dataset and used to
  generate a 24-month forward forecast (Jan 1961 – Dec 1962),
  projecting peak summer demand of approximately 664,000–672,000
  passengers by July–August 1962

---

## Repository Structure
├── report/       # Final written analysis report (.pdf)
├── code/         # Complete R script 
├── data/         # AirPassengers.csv
├── figures/      # All 22 project figures (Figs 1–22)
└── tables/       # Formatted output tables (.docx)

---

## Tools and Packages

**Language:** R

**Packages:** forecast, tseries, ggplot2, flextable, officer,
zoo, tidyverse, scales, lubridate

---

## References

- Box, G.E.P. and Jenkins, G.M. (1976). *Time Series Analysis:
  Forecasting and Control.* Holden-Day.
- Hyndman, R.J. and Koehler, A.B. (2006). Another look at measures
  of forecast accuracy. *International Journal of Forecasting,*
  22(4), 679–688.
- Makridakis, S., Spiliotis, E. and Assimakopoulos, V. (2018).
  The M4 Competition. *International Journal of Forecasting,*
  34(4), 802–808.
- Hyndman, R.J. and Athanasopoulos, G. (2021). *Forecasting:
  Principles and Practice* (3rd ed.). OTexts.
