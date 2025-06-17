# Population Migration and Housing Price Analysis in Taiwan (2015–2024)

This project analyzes the relationship between population migration patterns and housing price fluctuations across Taiwan from 2015 to 2024. Using publicly available government datasets, we explore whether price increases drive residents away, or whether in-migration drives prices higher, through regression analysis and interactive visualization.

## Motivation

Urban housing markets in Taiwan have seen significant changes over the past decade, with some regions experiencing sharp population outflows and others sharp price growth. This project investigates whether these two phenomena are correlated, and aims to provide insights into regional housing dynamics for urban planning, policy evaluation, and academic research.

## Data Sources

- **Population Data**:  
  [Department of Household Affairs – Population Dashboard](https://gis.ris.gov.tw/dashboard.html?key=E01)

- **Real Estate Transaction Data**:  
  [Ministry of the Interior – Actual Price Registration Open Data](https://plvr.land.moi.gov.tw/DownloadOpenData)

- **Time Range**: 2015 to 2024  
- **Spatial Granularity**: District level (e.g., Shilin, Datong, Beitou)

## Methods

- **Data Processing**:  
  Data was cleaned, merged by administrative region and time period, and transformed into time series format for analysis.

- **Statistical Analysis**:
  - Linear regression to evaluate correlation between net population migration and median housing prices per district.
  - Comparison across time to identify trend reversals or delayed effects.

- **Visualization Tools**:  
  Interactive dashboards and time series plots were built using R and the following libraries:
  - `dplyr` (data manipulation)
  - `ggplot2` (visualization)
  - `zoo`, `scales` (time series formatting)
  - `shiny` (dashboard construction)

## Key Findings

- **Positive Correlation Identified**:  
  Strong positive correlation observed between net in-migration and housing price increases in **Shilin**, **Datong**, and **Beitou** districts of Taipei.

- **Temporal Synchronization**:  
  In many districts, population inflow and housing price spikes occurred within the same year, suggesting a close relationship between mobility and affordability.

- **Example Visualization**:  
  *(Replace with actual chart once available)*  
  ![Price-Migration Correlation Chart](assets/correlation_plot_shilin.png)

## Project Role

This project was completed as a team coursework assignment. I was responsible for:

- Collecting and cleaning data from government open data portals
- Performing regression analysis and visualization
- Designing and implementing the interactive dashboard using Shiny

## Limitations

- The regression model is linear and does not account for external confounding factors (e.g., economic policy, transportation access).
- Real estate data is based on transaction records and may not fully reflect market price trends.

## Future Work

- Incorporate additional socioeconomic variables (e.g., income level, infrastructure projects)
- Apply geospatial mapping techniques (`sf`, `leaflet`) for enhanced visualization
- Explore machine learning models to capture nonlinear patterns

## Demo

*(If the Shiny app or report is hosted online, provide a link here)*

## Author

Liu Tzu-Yang (劉子揚)  
Department of Computer Science and Engineering  
Yuan Ze University
