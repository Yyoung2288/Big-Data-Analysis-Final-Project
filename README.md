# Population Migration and Housing Price Analysis in Taiwan (2015–2024)

This project investigates the interplay between population mobility and housing price dynamics across Taiwan’s 22 regions over a 10-year period. Leveraging open government datasets, we examine whether price spikes repel residents, or whether incoming migration drives price growth. The analysis combines data scraping, time-series aggregation, and interactive visualization via R Shiny.

## Motivation

In Taiwan’s rapidly urbanizing landscape, cities experience both soaring housing costs and demographic shifts. While high property prices are often blamed for population outflow, there remains limited empirical evidence quantifying this relationship across regions and time. Our research aims to fill this gap with large-scale, district-level data analytics.

## Data Sources

- Population migration: https://gis.ris.gov.tw/dashboard.html?key=E01
- Housing transactions: https://plvr.land.moi.gov.tw/DownloadOpenData
- Temporal scope: 2015–2024
- Spatial scope: Administrative district level (e.g., Shilin, Beitou)

## Methodology

### Data Workflow

- Data scraping automated via Python selenium
- Preprocessing includes:
  - Standardization of temporal granularity (monthly/quarterly)
  - Filtering out non-residential real estate
  - Merging by district codes

### Analytical Methods

- Pearson correlation between median housing price and net migration per district
- Time-lag analysis to detect delayed response between variables
- Linear regression for causal pattern inference
- Visualization via:
  - Dual-axis line plots (price vs. population)
  - Regression scatter plots
  - Interactive dashboards (Shiny)

## Key Results

- Strong positive correlation in districts like Shilin, Beitou, and Datong
- Temporal synchronization: Price and population often rise within the same calendar year
- Divergent patterns:
  - High price, declining population (e.g., Da’an, Xinyi)
  - Dual growth zones (e.g., Zhubei, Linkou)
  - Population growth without price support (e.g., Bali)
  - Dual decline (e.g., Shigang)

## Limitations

- Analysis limited to average migration and price per year
- Lacks confounding control variables (e.g., income, vacancy, job density)
- Quarterly and monthly data mismatch required manual alignment
- Linear models may oversimplify underlying nonlinear housing behaviors

## Future Work

- Integrate geospatial factors (e.g., transit, elevation, zoning)
- Explore machine learning (e.g., SVR, XGBoost) for nonlinear modeling
- Introduce causal inference frameworks (e.g., Granger causality)
- Expand dashboard usability and GIS visualization

## Demo

To launch the interactive dashboard:
```R
shiny::runApp("RApp/")
```

## Team Contribution

This project was conducted as a capstone research under the Department of Computer Science and Engineering at Yuan Ze University.

- Tz-Yang Liu: 
  - Python-based data scraping (2,640 files automated)
  - Data cleaning (standardization, NA handling, joining)
  - Summary statistics and early-stage visualizations
  - Literature review and contextual synthesis

- Wei-Lan Sze: 
  - Time-series modeling and regression
  - Interactive Shiny app development
  - Visualization design and narrative framing
  - Final report writing and presentation

Advisor: Prof. Ting-Ying Chien

## Related Academic Report

For full methodology, data interpretation, and regional policy implications, refer to our thesis:
“Analysis of the Relationship Between Housing Prices and Population Mobility”  
Yuan Ze University, Department of Computer Science and Engineering (2025)  
(PDF available upon request)

## License

For academic or non-commercial use. Please cite this repository if reused.
