# case-studies
Two projects: 1. Forecasting Power Consumption. 2. Car and Currency change detection (Time Series).

This repository contains two data analysis projects focusing on time-series forecasting, clustering, and changepoint detection, with both **presentations** and **R code implementations**.

---

## 📊 Project 1: Forecasting Electrical Power Consumption  
**Files:**  
- Presentation: `project_1_presentation.pptx`  
- Code: `project_1_code.R`  

### Overview
- Dataset: 33 households with heat pumps (2019, variable: `HAUSHALT_TOT`, no PV systems).  
- Tasks:  
  - Data smoothing (hourly, weekday-based).  
  - MANOVA modeling & AIC-based model selection.  
  - Clustering using k-means and validation indices (Silhouette, Dunn, Davies–Bouldin).  
  - Load profile construction.  
  - Prediction via Kalman & Particle filters.  
  - Mean Squared Error (MSE) evaluation.  

### Code Highlights
- **Preprocessing:** Custom functions for averaging hourly consumption and smoothing (adjacent hours & same weekday).  
- **Modeling:** MANOVA models with/without interactions, evaluated using AIC.  
- **Clustering:** Coefficient-based k-means clustering with visualization (`factoextra`, `clusterSim`, `clValid`).  
- **Load Profiling:** Functions to generate cluster-based daily load profiles and error analysis.  
- **Outlier Detection:** Household 34 identified consistently as an outlier.  
- **Visualization:** Gradient bar plots, PCA projections, and cluster separation plots.  

### Key Insights
- Weekday-based smoothing (Method B) generally yields better predictions.  
- Particle filter outperforms Kalman filter when load profiles are used.  
- Clustering reveals strong weekday patterns, but more variability on weekends.  

---

## ⛽ Project 2: Detecting Change-Points and Trends in Petrol Consumption and Prices  
**Files:**  
- Presentation: `project_2_presentation.pdf`  
- Code: `project_2_code.R`  

### Overview
- Data: Two fuel logbooks (`grau` and `karriert`).  
- Variables: Date, odometer, price, currency, liters filled (+ consumption for `karriert`).  
- Tasks:  
  - Data cleaning: date corrections, typo fixing, currency conversions, missing value handling.  
  - Time-series construction for consumption & petrol prices.  
  - Changepoint detection: **PELT** & **Binary Segmentation**.  
  - Trend detection: **LOESS smoothing** and **Mann–Kendall (modified)** tests.  
  - Autocorrelation analysis.  

### Code Highlights
- **Preprocessing:** Extensive corrections of dates, currency conversion to EUR, typo fixes, and interpolation of missing entries.  
- **Changepoint Detection:** Implemented with `changepoint` package, results compared between PELT and BinSeg.  
- **Trend Analysis:** LOESS for smooth curves; Mann–Kendall test (with correction for autocorrelation) to confirm statistical trends.  
- **Visualization:** ggplot2-based time-series plots with annotated changepoints (e.g., car changes, Euro introduction).  
- **Outlier Handling:** Fixes for extreme or inconsistent values in prices and odometer readings.  

### Key Insights
- Car changes correspond to changepoints in consumption (`grau`).  
- Petrol prices exhibit multiple changepoints, but **not at Euro introduction**.  
- Significant increasing/decreasing trends detected in three out of four series.  

---

## 🚀 How to Use
- Open the presentation files for a conceptual walkthrough.  
- Run the R code files for full **implementation and reproducibility** of the analysis.  
  - `project_1_code.R` → Power consumption forecasting and clustering.  
  - `project_2_code.R` → Petrol price & consumption changepoint/trend detection.  

---

## 📝 Conclusion
These projects demonstrate practical applications of **statistical modeling, clustering, and time-series analysis** using R. They highlight methods for handling real-world datasets with inconsistencies, forecasting consumption, and detecting structural changes in currency (Euro) and fuel price dynamics.  
