# Australian Wines Time Series Analysis

Welcome to the **Australian-Wines** repository! This project contains a time series analysis of Australian wine data, featuring both reproducible research documentation and an interactive web application.

## 📁 Repository Contents

*   **`AustralianWines.csv`**: The primary dataset used for the time series analysis.
*   **`AustralianWines.qmd`**: The main notebook containing the time series analysis and modeling, formatted as a white paper to provide a comprehensive research overview.
*   **`AustralianWines.html`**: The compiled HTML output of the Quarto analysis, ready for standalone viewing.
*   **`app.R`**: An interactive Shiny web application for exploring the data and forecasting results.
*   **`Posit_Connect_Cloud.R`**: A deployment script configured for publishing the project to Posit Connect Cloud.
*   **`manifest.json`**: The environment and configuration manifest required for deploying the application and reports.

## 🚀 Getting Started

### Prerequisites

To run the analysis and application locally, ensure you have R installed along with the required packages. You can typically install the core requirements via:
```R
install.packages(c("shiny", "rmarkdown", "rsconnect"))
