# 🗺️ Geo InSight

An interactive R Shiny web application for geographical data visualization of **India's Crime Rates** and **Live Air Quality (AQI)** across states and cities.

> **College Group Project** | Data: NCRB 2022 + Open-Meteo Live AQI API

---

## 📸 Features

| Feature | Description |
|---------|-------------|
| 🗺️ Interactive Crime Map | Circle markers for all 36 states/UTs sized and colored by crime rate |
| 🌿 Live AQI Map | Real-time air quality data for 9 major Indian cities via Open-Meteo API |
| ⚡ Instant Filters | Year (2020–2022), trend, and sort filters update the map immediately |
| 🔍 State Search | Search bar pans the map to any state in real-time |
| 📊 Analytics Panel | Overview stats, 12-month trend chart, top-10 charts, distribution bars |
| ⚖️ Compare Mode | Side-by-side regional divergence analysis for any two states |
| 📋 Dataset Tab | Full sortable/searchable NCRB data table with inline color bars |
| 🕐 Live Timestamp | AQI panel shows when data was last refreshed |
| 💾 Export | Download full dataset or comparison report as CSV |

---

## 🚀 Getting Started

### 1. Install R
Download from [https://cran.r-project.org](https://cran.r-project.org) if not already installed.
Tested on **R 4.5.2**.

### 2. Install Required Packages (run once)
```r
# In RStudio or terminal:
source("install_packages.R")
# OR run via terminal:
# Rscript install_packages.R
```

### 3. Run the App
```r
shiny::runApp("app.R")
```
Or open `app.R` in RStudio and click **▶ Run App**.

**From terminal (Windows):**
```powershell
& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" -e "shiny::runApp('app.R')"
```

The app will open at `http://127.0.0.1:<port>` in your browser.

---

## 📦 Tech Stack

| Layer | Technology |
|-------|-----------|
| Framework | R Shiny |
| Mapping | Leaflet for R |
| Charts | Plotly for R |
| Data Table | DT (DataTables) |
| Live Data | Open-Meteo Air Quality API (free, no key needed) |
| HTTP | httr + jsonlite |
| Data Wrangling | dplyr |
| Fonts | Google Fonts — Manrope, Inter |

---

## 📁 Project Structure

```
DAr/
├── app.R                 # Main Shiny application (UI + Server)
├── install_packages.R    # One-time package installer
└── README.md             # This file
```

---

## 📊 Data Sources

- **Crime Data:** [NCRB (National Crime Records Bureau) 2022](https://ncrb.gov.in) — Total IPC cognizable crimes for 36 states/UTs
- **Air Quality:** [Open-Meteo Air Quality API](https://open-meteo.com/en/docs/air-quality-api) — Live PM2.5, PM10, NO₂, Ozone for 9 cities

> ⚠️ Tamil Nadu shows a ~78% drop in reported crimes (2020→2022), likely due to a reporting methodology change, not an actual reduction.

---

## 🏙️ Cities Covered (AQI)

Delhi · Mumbai · Bangalore · Kolkata · Chennai · Hyderabad · Pune · Ahmedabad · Nagpur

---

## 🎛️ How to Use

1. **Crime Map** — Select year/trend/sort filters in the sidebar → map updates instantly. Click any state marker to see its detailed stats card. The map flies to the selected state.
2. **Pollution Map** — Click "Pollution (AQI)" in the sidebar to switch to AQI city markers. Click "Refresh Live Data" to fetch current readings.
3. **Analytics** — Click the "Analytics" tab in the top nav to see charts, top-10 rankings, and distribution breakdowns.
4. **Compare** — Click "Compare" to pick two states and see a side-by-side divergence analysis.
5. **Dataset** — Click "Dataset" to browse the full NCRB table with search and sorting.
6. **Search** — Type a state name in the navbar search box to pan the map to it.

---

## 👥 Team

| Name | Roll | ID |
|------|------|----|
| Vedant Hemraj Kapgate | 180 | 24070215 |
| Vibhanshu Murlidhar Kapse | 181 | 24071767 |
| Yash Dnyaneshwar Nimje | 182 | 24070084 |
| Yash Vijay Bhasakhetre | 183 | 24070603 |

> **Institution:** Yeshwantrao Chavan College of Engineering, Nagpur
> **Course:** Data Analytics using R (Instructor: Sharmeen Ahmed)

---

## 📄 License

For academic/educational use only. Crime data © NCRB India. AQI data © Open-Meteo (CC BY 4.0).
