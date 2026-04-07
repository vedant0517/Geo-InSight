# Geo InSight

Geo InSight is an interactive R Shiny dashboard for exploring India crime statistics and live city-level air quality in one interface. The app combines a state-wise crime map, analytics views, comparison tools, and a full dataset table.

> College group project using NCRB 2022 data and the Open-Meteo Air Quality API.

## What The App Includes

- Locations: the main crime map with year, trend, and sort filters, plus a search box that pans to a selected state or UT
- Analytics: overview cards, trend charts, and summary panels
- Compare: side-by-side comparison for any two states
- Reports: the current implementation routes this tab to the pollution or AQI view
- Dataset: a full searchable table of the NCRB dataset

## Highlights

- All 36 states and union territories are shown on the crime map
- Markers are sized and colored by crime intensity
- Live AQI is fetched for 9 Indian cities from Open-Meteo
- Clicking a marker opens a detail card with state-level metrics
- Compare and dataset views support CSV export
- AQI refreshes are timestamped so users can see when the live data was last updated

## Getting Started

1. Install R from [CRAN](https://cran.r-project.org/) if it is not already installed.
2. Open the project in RStudio or VS Code.
3. Run the app with `start_project.R`:

```r
source("start_project.R")
```

`start_project.R` checks for required packages and launches the app from the project root.

If you want to run the app directly, open `app.R` and start it with:

```r
shiny::runApp("app.R")
```

You can also run the workspace task in VS Code with `Geo InSight: Run App`, which now launches `start_project.R`.

## Project Structure

```
Geo InSight/
├── app.R
├── .vscode/tasks.json
├── start_project.R
├── PROJECT_EXPLANATION.md
├── Group-15_GeoInsight_Project_Report.md
└── README.md
```

## Data Sources

- Crime data: NCRB 2022 state and union-territory crime totals
- Air quality: Open-Meteo Air Quality API for live PM2.5, PM10, NO2, and ozone readings

## Cities Covered For AQI

Delhi, Mumbai, Bangalore, Kolkata, Chennai, Hyderabad, Pune, Ahmedabad, and Nagpur

## Notes

- The crime dataset is stored in memory inside `app.R`.
- Live AQI depends on the external Open-Meteo service and network availability.
- The app is intended for academic and educational use.

## Team

| Name | Roll | ID |
|------|------|----|
| Vedant Hemraj Kapgate | 180 | 24070215 |
| Vibhanshu Murlidhar Kapse | 181 | 24071767 |
| Yash Dnyaneshwar Nimje | 182 | 24070084 |
| Yash Vijay Bhasakhetre | 183 | 24070603 |
| Omkar Ade | 301 | 18030186 |

Institution: Yeshwantrao Chavan College of Engineering, Nagpur
Course: Data Analytics using R (Instructor: Sharmeen Ahmed)

## License

For academic and educational use only. Crime data copyright NCRB India. AQI data copyright Open-Meteo under CC BY 4.0.
