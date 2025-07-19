# Impact of AC standardisation Laws on Electricity Demand in the EU countries

This repository contains the dataset, code, and output used in the analysis of the impact of air conditioner standardization laws on electricity demand in Greece, Spain, and Italy.

## Repository Structure

### Files Included

- **Electricity EU.xlsx**  
  Metadata and compiled dataset used for analysis. It combines monthly electricity price and consumption data from Ember Energy for 27 EU countries between January 2018 and March 2025.

- **Electricity Study R codes.R**  
  The R script used to clean the dataset, run the Two-Way Fixed Effects (TWFE) regressions, perform robustness checks, and generate plots.

- **Electricity Study - Results.txt**  
  A summary of key regression outputs, placebo tests, and interpretation of results.

- **parallel trend plot 27 countries.png**  
  Visualization of the parallel trends assumption check across all 27 countries.

- **Parallel trend plot neighbouring countries.png**  
  Parallel trends plot using only neighboring countries (France, Portugal, Bulgaria, Switzerland, Austria, Slovenia).

- **Event Study - 27 countries.png**  
  Event study graph for the full 27-country dataset.

- **Event Study - Neighboring countries.png**  
  Event study graph for the subset of neighboring countries.

## Study Summary

The analysis uses a Two-Way Fixed Effects (TWFE) model to estimate the causal effect of AC energy-efficiency policies on electricity consumption. The dataset spans from January 2018 to March 2025, and includes controls for electricity price. Event study plots and placebo tests are used to evaluate robustness.

## License

This project is licensed under the [MIT License](LICENSE).

## Author

**Abhyuday Joshi**  
For queries or collaboration: abhyudayjoshins@email.com
