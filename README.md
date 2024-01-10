# Power Consumption Forecasting

## Overview

This repository focuses on Power Consumption Forecasting using Time Series Models, specifically analyzing and predicting power consumption in various city zones. 

## Problem Statement

The primary objective is to determine which time series model predicts the power consumption data most accurately. The analysis involves evaluating different time series models to understand their performance and identify the model that excels in forecasting power consumption trends.

## Data

The Tétouan City Power Consumption dataset comprises 52416 rows and 9 columns, capturing power consumption recordings for three different zones. The data is recorded daily in 2017 at 10-minute intervals.

## Data Cleaning

To facilitate analysis, new data frames are created for each zone. These frames are grouped by average power consumption for each hour and day. The resulting data frame contains 364 rows (one for each day) and three columns: zone-wise power consumption, time, and weekday. This cleaned dataset is used for building and comparing time series models.

## Models Used

The following time series models are employed:

1. **Linear Model using Weekly Seasonality**
2. **SES (Simple Exponential Smoothing)**
3. **ETS (Error-Trend-Seasonality)**
4. **ARIMA (AutoRegressive Integrated Moving Average)**

## Results

| Zone | Model | Performance |
|------|-------|-------------|
| 1    | SES   | 2.79        |
| 1    | ETS   | 1.83        |
| 1    | ARIMA | 2.17        |
| 2    | SES   | 5.30        |
| 2    | ETS   | 2.65        |
| 2    | ARIMA | 3.05        |
| 3    | SES   | 2.34        |
| 3    | ETS   | 2.40        |
| 3    | ARIMA | 2.34        |

## Conclusion

- **Zone 1 and Zone 2:** ETS models perform better, capturing trends and seasonality explicitly modeled in these components.
- **Zone 3:** SES and ARIMA models outperform others. The absence of weekly seasonality in Zone 3, coupled with autocorrelation, makes ARIMA models more effective.

The analysis concludes that model performance depends on the specific characteristics of each zone, such as seasonality, trends, and autocorrelation. This information is crucial for selecting the most appropriate model for accurate power consumption forecasting.
