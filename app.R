# ============================================================
# GeoInsight (Lucid Cartographer)
# R Shiny Application | College Group Project
# UI: Matches Stitch HTML design exactly
# Data: NCRB 2022 + Open-Meteo Live AQ
# ============================================================

library(shiny)
library(leaflet)
library(plotly)
library(DT)
library(httr)
library(jsonlite)
library(dplyr)

# ── DATA ─────────────────────────────────────────────────────
india_crime <- data.frame(
  id = 1:36,
  state = c("Andhra Pradesh","Arunachal Pradesh","Assam","Bihar","Chhattisgarh",
            "Goa","Gujarat","Haryana","Himachal Pradesh","Jharkhand",
            "Karnataka","Kerala","Madhya Pradesh","Maharashtra","Manipur",
            "Meghalaya","Mizoram","Nagaland","Odisha","Punjab",
            "Rajasthan","Sikkim","Tamil Nadu","Telangana","Tripura",
            "Uttar Pradesh","Uttarakhand","West Bengal","Andaman & Nicobar",
            "Chandigarh","Dadra & NH/DD","Delhi","Jammu & Kashmir","Ladakh",
            "Lakshadweep","Puducherry"),
  lat = c(15.9129,28.218,26.2006,25.0961,21.2787,15.2993,22.2587,29.0588,
          31.1048,23.6102,15.3173,10.8505,22.9734,19.7515,24.6637,25.467,
          23.1645,26.1584,20.9517,31.1471,27.0238,27.533,11.1271,18.1124,
          23.9408,26.8467,30.0668,22.9868,11.7401,30.7333,20.1809,28.7041,
          33.7782,34.1526,10.5667,11.9416),
  lng = c(79.74,94.7278,92.9376,85.3131,81.8661,74.124,71.1924,76.0856,
          77.1734,85.2799,75.7139,76.2711,78.6569,75.7139,93.9063,91.3662,
          92.9376,94.5624,85.0985,75.3412,74.2179,88.5122,78.6569,79.0193,
          91.9882,80.9462,79.0193,87.855,92.6586,76.7794,73.0169,77.1025,
          76.5762,77.577,72.6417,79.8083),
  crimes_2020 = c(188997,2244,111558,194698,65216,3393,381849,103276,14803,
                  51033,106350,149099,283881,394017,2349,2871,1787,1022,
                  108533,49870,193279,504,891700,135885,4010,355110,13812,
                  158060,482,2583,441,249192,25233,387,107,6725),
  crimes_2021 = c(179611,2626,119883,186006,70519,2099,273056,112720,13041,
                  47684,115728,142643,304066,367218,2484,2672,2467,1033,
                  124956,46454,214552,532,322852,146131,4133,357905,15704,
                  157498,386,2401,490,291904,27447,519,89,3851),
  crimes_2022 = c(158547,2308,59315,211079,73822,2711,134600,125435,13231,
                  48726,129461,235858,298578,374038,3029,2914,3587,1008,
                  143414,43738,236090,549,193913,151849,3653,401787,16967,
                  156503,460,2941,1184,300429,25915,439,64,3237),
  population_lakhs = c(530.3,15.5,354.9,1255.3,299.5,15.7,709.3,299.7,74.4,
                       391.4,674.1,356.8,858.9,1257.4,32.0,33.3,12.3,22.2,
                       460.8,306.0,804.4,6.8,767.1,379.5,41.2,2340.9,115.6,
                       987.6,4.0,12.2,12.0,211.0,135.4,3.0,0.7,16.2),
  crime_rate_2022 = c(299.0,148.8,167.1,168.1,246.5,172.8,189.8,418.6,177.8,
                      124.5,192.1,661.0,347.6,297.5,94.6,87.6,291.6,45.4,
                      311.2,142.9,293.5,80.3,252.8,400.1,88.7,171.6,146.8,
                      158.5,114.4,240.5,98.6,1424.1,191.4,146.8,92.8,199.7),
  chargesheeting_rate = c(86.5,47.2,32.6,75.4,80.4,75.9,89.8,43.3,82.9,58.2,
                          78.3,96.0,86.6,75.3,10.4,26.9,58.0,58.0,77.9,66.4,
                          49.8,55.5,70.7,79.1,73.1,76.1,71.5,90.6,83.9,54.4,
                          82.5,30.2,77.1,82.4,73.5,91.3),
  trend = c("decreasing","increasing","decreasing","increasing","increasing",
            "decreasing","decreasing","increasing","decreasing","decreasing",
            "increasing","increasing","increasing","decreasing","increasing",
            "increasing","increasing","decreasing","increasing","decreasing",
            "increasing","increasing","decreasing","increasing","decreasing",
            "increasing","increasing","decreasing","decreasing","increasing",
            "increasing","increasing","increasing","increasing","decreasing",
            "decreasing"),
  stringsAsFactors = FALSE
)

NATIONAL_AVG <- 258.1

city_coords <- list(
  Delhi     = c(28.6139, 77.2090),
  Mumbai    = c(19.0760, 72.8777),
  Bangalore = c(12.9716, 77.5946),
  Kolkata   = c(22.5726, 88.3639),
  Chennai   = c(13.0827, 80.2707),
  Hyderabad = c(17.3850, 78.4867),
  Pune      = c(18.5204, 73.8567),
  Ahmedabad = c(23.0225, 72.5714),
  Nagpur    = c(21.1458, 79.0882)
)

# ── HELPERS ──────────────────────────────────────────────────
get_crime_color <- function(rate) {
  ifelse(rate>600,"#800026",ifelse(rate>400,"#BD0026",ifelse(rate>300,"#E31A1C",
                                                             ifelse(rate>200,"#FC4E2A",ifelse(rate>150,"#FD8D3C","#FEB24C")))))
}
get_aqi_color <- function(aqi) {
  ifelse(is.na(aqi),"#adb5bd",ifelse(aqi>300,"#800026",ifelse(aqi>200,"#E31A1C",
                                                              ifelse(aqi>150,"#FC4E2A",ifelse(aqi>100,"#FEB24C","#006d41")))))
}
get_aqi_label <- function(aqi) {
  if(is.na(aqi)) return("N/A")
  if(aqi>300) "Hazardous" else if(aqi>200) "Very Poor" else if(aqi>150) "Poor" else if(aqi>100) "Moderate" else "Good"
}
compute_stats <- function(v) {
  v <- v[!is.na(v)]
  list(mean=round(mean(v),1), median=round(median(v),1), sd=round(sd(v),1), max=round(max(v),1))
}

fetch_aqi <- function(city_name) {
  coords <- city_coords[[city_name]]
  tryCatch({
    url <- paste0("https://air-quality-api.open-meteo.com/v1/air-quality",
                  "?latitude=",coords[1],"&longitude=",coords[2],
                  "&current=pm10,pm2_5,nitrogen_dioxide,ozone&timezone=Asia/Kolkata")
    res  <- GET(url, timeout(10))
    data <- fromJSON(content(res,"text",encoding="UTF-8"))
    pm25 <- data$current$pm2_5
    aqi  <- round(pm25*4.2)
    list(city=city_name, aqi=aqi, pm25=round(pm25,1),
         pm10=round(data$current$pm10,1), no2=round(data$current$nitrogen_dioxide,1),
         label=get_aqi_label(aqi), color=get_aqi_color(aqi))
  }, error=function(e) list(city=city_name,aqi=NA,pm25=NA,pm10=NA,no2=NA,label="Error",color="#adb5bd"))
}

# ══════════════════════════════════════════════════════════════
# CSS — Matches Stitch Lucid Cartographer exactly
# ══════════════════════════════════════════════════════════════
stitch_css <- "
@import url('https://fonts.googleapis.com/css2?family=Manrope:wght@400;600;700;800&family=Inter:wght@400;500;600&display=swap');

* { box-sizing: border-box; margin: 0; padding: 0; }
body { font-family: 'Inter', sans-serif; background: #f8f9fa; color: #2b3437; overflow: hidden; height: 100vh; }

/* ── TOP NAVBAR ── */
.lc-navbar {
  position: fixed; top: 0; left: 0; right: 0; height: 56px; z-index: 1000;
  background: rgba(255,255,255,0.85); backdrop-filter: blur(20px);
  -webkit-backdrop-filter: blur(20px);
  border-bottom: 1px solid rgba(171,179,183,0.3);
  display: flex; align-items: center; justify-content: space-between;
  padding: 0 24px; box-shadow: 0 1px 12px rgba(44,55,60,0.06);
}
.lc-logo { font-family: 'Manrope', sans-serif; font-size: 20px; font-weight: 800; color: #1e293b; letter-spacing: -0.5px; }
.lc-logo span { color: #0061a4; }
.lc-nav-tabs { display: flex; gap: 4px; }
.lc-tab {
  background: none; border: none; cursor: pointer; padding: 6px 16px;
  border-radius: 8px; font-family: 'Manrope', sans-serif; font-size: 13px; font-weight: 600;
  color: #586064; transition: all 0.2s;
}
.lc-tab:hover { background: #f1f4f6; color: #2b3437; }
.lc-tab.active { color: #0061a4; border-bottom: 2px solid #0061a4; border-radius: 0; }
.lc-badge {
  background: rgba(0,97,164,0.1); color: #0061a4; border: 1px solid rgba(0,97,164,0.2);
  padding: 4px 12px; border-radius: 20px; font-size: 11px; font-weight: 700;
  font-family: 'Manrope', sans-serif; letter-spacing: 0.3px;
}
.lc-search {
  background: #f1f4f6; border: none; border-radius: 10px;
  padding: 7px 14px 7px 36px; font-size: 13px; width: 200px;
  font-family: 'Inter', sans-serif; color: #2b3437; outline: none;
}
.lc-search-wrap { position: relative; }
.lc-search-icon { position: absolute; left: 10px; top: 50%; transform: translateY(-50%); color: #adb5bd; font-size: 16px; }
.lc-nav-icons { display: flex; gap: 4px; }
.lc-icon-btn {
  background: none; border: none; cursor: pointer; width: 34px; height: 34px;
  border-radius: 50%; display: flex; align-items: center; justify-content: center;
  color: #586064; transition: background 0.15s; font-size: 18px;
}
.lc-icon-btn:hover { background: #f1f4f6; }

/* ── MAIN LAYOUT ── */
.lc-main { display: flex; height: calc(100vh - 56px); margin-top: 56px; overflow: hidden; }

/* ── LEFT SIDEBAR ── */
.lc-sidebar {
  width: 256px; flex-shrink: 0; height: 100%;
  background: rgba(248,249,250,0.92); backdrop-filter: blur(16px);
  -webkit-backdrop-filter: blur(16px);
  border-right: 1px solid rgba(171,179,183,0.25);
  display: flex; flex-direction: column; padding: 20px 14px;
  gap: 6px; overflow-y: auto; box-shadow: 2px 0 16px rgba(44,55,60,0.04);
}
.lc-sidebar::-webkit-scrollbar { width: 4px; }
.lc-sidebar::-webkit-scrollbar-thumb { background: #dbe4e7; border-radius: 2px; }

.lc-sidebar-title { font-family: 'Manrope', sans-serif; font-size: 18px; font-weight: 700; color: #1e293b; }
.lc-precision-tag { font-size: 10px; font-weight: 700; letter-spacing: 1.5px; text-transform: uppercase; color: #0061a4; margin-top: 2px; }

.lc-filter-label { font-size: 10px; font-weight: 700; letter-spacing: 1.5px; text-transform: uppercase; color: #586064; margin: 10px 0 6px; }

.lc-nav-item {
  display: flex; align-items: center; gap: 10px; padding: 10px 12px;
  border-radius: 8px; cursor: pointer; transition: all 0.2s;
  font-family: 'Inter', sans-serif; font-size: 13px; font-weight: 500;
  color: #586064; border: none; background: none; width: 100%; text-align: left;
}
.lc-nav-item:hover { transform: translateX(3px); color: #2b3437; }
.lc-nav-item.active { background: white; color: #0061a4; font-weight: 600; box-shadow: 0 1px 6px rgba(44,55,60,0.08); }
.lc-nav-item .material-symbols-outlined { font-size: 20px; }

.lc-select {
  width: 100%; padding: 8px 12px; background: white; border: 1px solid #dbe4e7;
  border-radius: 8px; font-family: 'Inter', sans-serif; font-size: 13px;
  color: #2b3437; outline: none; cursor: pointer; margin-top: 2px;
  box-shadow: 0 1px 3px rgba(44,55,60,0.04);
}
.lc-select:focus { border-color: #0061a4; }

/* stats in sidebar */
.lc-stat-grid { display: grid; grid-template-columns: 1fr 1fr; gap: 8px; margin-top: 4px; }
.lc-stat-card {
  background: white; border: 1px solid #eaeff1; border-radius: 10px;
  padding: 10px 12px; box-shadow: 0 1px 4px rgba(44,55,60,0.04);
}
.lc-stat-val { font-family: 'Manrope', sans-serif; font-size: 22px; font-weight: 800; color: #2b3437; }
.lc-stat-lbl { font-size: 10px; color: #7d8590; font-weight: 600; margin-top: 2px; text-transform: uppercase; letter-spacing: 0.5px; }
.lc-stat-full { grid-column: 1/-1; }

/* insight box */
.lc-insight-box {
  background: rgba(0,97,164,0.06); border: 1px solid rgba(0,97,164,0.15);
  border-radius: 10px; padding: 12px; margin-top: 6px;
}
.lc-insight-title { font-size: 9px; font-weight: 700; letter-spacing: 1.5px; text-transform: uppercase; color: #0061a4; margin-bottom: 8px; display: flex; align-items: center; gap: 6px; }
.lc-insight-item { display: flex; gap: 8px; margin-bottom: 5px; }
.lc-insight-arrow { color: #0061a4; flex-shrink: 0; font-size: 12px; font-weight: 700; }
.lc-insight-text { font-size: 11px; color: #2b3437; line-height: 1.5; }

.lc-apply-btn {
  width: 100%; padding: 11px; background: #0061a4; color: white; border: none;
  border-radius: 10px; font-family: 'Manrope', sans-serif; font-size: 13px;
  font-weight: 700; cursor: pointer; margin-top: 8px;
  box-shadow: 0 4px 14px rgba(0,97,164,0.25); transition: all 0.2s;
}
.lc-apply-btn:hover { background: #005590; box-shadow: 0 6px 18px rgba(0,97,164,0.3); }
.lc-apply-btn:active { transform: scale(0.98); }

.lc-ncrb-note {
  background: white; border: 1px solid #eaeff1; border-radius: 8px;
  padding: 8px 10px; font-size: 10px; color: #586064; line-height: 1.5; margin-top: 4px;
}

/* ── MAP CENTER ── */
.lc-map-area { flex: 1; position: relative; overflow: hidden; }
#main_map { width: 100%; height: 100%; }

/* floating data card */
.lc-float-card {
  position: absolute; top: 20px; left: 20px; z-index: 900;
  background: rgba(255,255,255,0.75); backdrop-filter: blur(16px);
  -webkit-backdrop-filter: blur(16px); border: 1px solid rgba(255,255,255,0.5);
  border-radius: 16px; padding: 20px; width: 300px;
  box-shadow: 0 8px 32px rgba(44,55,60,0.12); display: none;
}
.lc-float-card.visible { display: block; }
.lc-float-header { display: flex; justify-content: space-between; align-items: flex-start; margin-bottom: 14px; }
.lc-float-name { font-family: 'Manrope', sans-serif; font-size: 18px; font-weight: 800; color: #1e293b; }
.lc-float-close {
  background: none; border: none; cursor: pointer; color: #586064;
  font-size: 20px; line-height: 1; padding: 0; font-family: 'Inter', sans-serif;
}
.lc-float-close:hover { color: #2b3437; }
.lc-alert-badge {
  padding: 3px 10px; border-radius: 20px; font-size: 10px; font-weight: 700;
  font-family: 'Manrope', sans-serif; letter-spacing: 0.5px; text-transform: uppercase;
}
.badge-high { background: #bb1b21; color: white; }
.badge-med  { background: #FC4E2A; color: white; }
.badge-low  { background: #006d41; color: white; }

.lc-metrics-grid { display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 8px; margin-bottom: 12px; }
.lc-metric { text-align: center; }
.lc-metric-val { font-family: 'Manrope', sans-serif; font-size: 20px; font-weight: 800; color: #0061a4; }
.lc-metric-lbl { font-size: 9px; color: #586064; font-weight: 600; margin-top: 1px; text-transform: uppercase; }

.lc-card-insight {
  background: rgba(255,255,255,0.5); border: 1px solid rgba(255,255,255,0.7);
  border-radius: 10px; padding: 10px 12px;
}
.lc-card-insight-label { display: flex; align-items: center; gap: 6px; font-size: 9px; font-weight: 700; letter-spacing: 1px; color: #0061a4; text-transform: uppercase; margin-bottom: 5px; }
.lc-card-insight p { font-size: 12px; line-height: 1.6; color: #2b3437; }

.lc-trend-row { display: flex; gap: 6px; margin-bottom: 12px; flex-wrap: wrap; }
.lc-trend-pill {
  padding: 3px 10px; border-radius: 20px; font-size: 11px; font-weight: 600;
}
.pill-inc { background: rgba(187,27,33,0.1); color: #bb1b21; }
.pill-dec { background: rgba(0,109,65,0.12); color: #006d41; }

/* map zoom controls */
.lc-map-controls {
  position: absolute; bottom: 24px; right: 24px; z-index: 900;
  background: white; border-radius: 14px; padding: 6px;
  box-shadow: 0 4px 20px rgba(44,55,60,0.12); display: flex; flex-direction: column; gap: 2px;
}
.lc-zoom-btn {
  width: 36px; height: 36px; background: none; border: none; cursor: pointer;
  border-radius: 10px; display: flex; align-items: center; justify-content: center;
  color: #0061a4; transition: background 0.15s; font-size: 20px; font-weight: 300;
}
.lc-zoom-btn:hover { background: #f1f4f6; }

/* legend */
.lc-legend {
  position: absolute; bottom: 24px; left: 24px; z-index: 900;
  background: rgba(255,255,255,0.88); backdrop-filter: blur(12px);
  border: 1px solid rgba(171,179,183,0.3); border-radius: 12px; padding: 12px 14px;
  box-shadow: 0 4px 16px rgba(44,55,60,0.08);
}
.lc-legend-title { font-size: 9px; font-weight: 700; letter-spacing: 1.5px; text-transform: uppercase; color: #586064; margin-bottom: 8px; }
.lc-legend-item { display: flex; align-items: center; gap: 8px; font-size: 11px; color: #2b3437; margin-bottom: 4px; }
.lc-legend-dot { width: 10px; height: 10px; border-radius: 50%; flex-shrink: 0; }

/* ── RIGHT ANALYTICS PANEL ── */
.lc-analytics {
  width: 340px; flex-shrink: 0; height: 100%; overflow-y: auto;
  background: white; border-left: 1px solid rgba(171,179,183,0.2);
  padding: 20px 18px; box-shadow: -2px 0 16px rgba(44,55,60,0.04);
}
.lc-analytics::-webkit-scrollbar { width: 4px; }
.lc-analytics::-webkit-scrollbar-thumb { background: #eaeff1; border-radius: 2px; }

.lc-panel-title { font-family: 'Manrope', sans-serif; font-size: 22px; font-weight: 800; color: #1e293b; letter-spacing: -0.5px; }
.lc-panel-sub { font-size: 13px; color: #586064; font-weight: 500; margin-top: 2px; margin-bottom: 16px; }

/* panel tab switcher */
.lc-panel-tabs { display: flex; gap: 2px; background: #f1f4f6; border-radius: 10px; padding: 3px; margin-bottom: 16px; }
.lc-ptab {
  flex: 1; padding: 7px 4px; background: none; border: none; cursor: pointer;
  border-radius: 8px; font-family: 'Inter', sans-serif; font-size: 12px;
  font-weight: 600; color: #586064; transition: all 0.2s;
}
.lc-ptab.active { background: white; color: #0061a4; box-shadow: 0 1px 4px rgba(44,55,60,0.08); }

/* stat cards 2x2 */
.lc-stats-2x2 { display: grid; grid-template-columns: 1fr 1fr; gap: 10px; margin-bottom: 16px; }
.lc-scard {
  background: #f1f4f6; border-radius: 12px; padding: 14px;
}
.lc-scard-val { font-family: 'Manrope', sans-serif; font-size: 28px; font-weight: 800; color: #2b3437; }
.lc-scard-lbl { font-size: 9px; font-weight: 700; letter-spacing: 1px; text-transform: uppercase; color: #7d8590; margin-top: 3px; }

/* chart sections */
.lc-chart-section { margin-bottom: 16px; }
.lc-chart-header { display: flex; justify-content: space-between; align-items: center; margin-bottom: 8px; }
.lc-chart-title { font-family: 'Manrope', sans-serif; font-size: 13px; font-weight: 700; color: #2b3437; }
.lc-chart-action { font-size: 10px; font-weight: 700; color: #0061a4; cursor: pointer; letter-spacing: 0.5px; text-transform: uppercase; text-decoration: none; }
.lc-chart-action:hover { text-decoration: underline; }

/* distribution bars */
.lc-dist-bar { margin-bottom: 10px; }
.lc-dist-bar-header { display: flex; justify-content: space-between; font-size: 10px; margin-bottom: 3px; }
.lc-dist-bar-label { font-weight: 700; color: #586064; }
.lc-dist-bar-val { font-weight: 700; color: #2b3437; }
.lc-dist-bar-track { width: 100%; background: #f1f4f6; height: 5px; border-radius: 3px; overflow: hidden; }
.lc-dist-bar-fill { height: 100%; border-radius: 3px; background: #0061a4; }

/* top states list */
.lc-top-state {
  display: flex; align-items: center; gap: 8px; padding: 8px 10px;
  background: #f8f9fa; border-radius: 8px; margin-bottom: 5px; cursor: pointer;
  border: 1px solid transparent; transition: all 0.15s;
}
.lc-top-state:hover { border-color: #0061a4; background: white; }
.lc-top-rank { font-size: 11px; color: #adb5bd; font-weight: 700; width: 18px; }
.lc-top-name { font-size: 13px; font-weight: 500; flex: 1; color: #2b3437; }
.lc-top-bar-wrap { width: 70px; height: 4px; background: #eaeff1; border-radius: 2px; }
.lc-top-bar-fill { height: 100%; border-radius: 2px; background: #0061a4; }
.lc-top-val { font-family: 'Manrope', sans-serif; font-size: 13px; font-weight: 800; color: #2b3437; width: 48px; text-align: right; }

/* system status box */
.lc-status-box {
  background: rgba(0,109,65,0.06); border: 1px solid rgba(0,109,65,0.15);
  border-radius: 12px; padding: 14px; margin-top: 8px;
}
.lc-status-header { display: flex; align-items: center; gap: 8px; margin-bottom: 6px; }
.lc-status-icon { color: #006d41; font-size: 20px; }
.lc-status-title { font-family: 'Manrope', sans-serif; font-size: 13px; font-weight: 700; color: #005e37; }
.lc-status-text { font-size: 12px; color: #005e37; line-height: 1.6; opacity: 0.85; }

/* compare panel */
.lc-compare-header { display: flex; justify-content: space-between; align-items: center; margin-bottom: 6px; }
.lc-compare-badge {
  background: #eaeff1; color: #586064; padding: 3px 10px;
  border-radius: 20px; font-size: 10px; font-weight: 700;
}
.lc-compare-title { font-family: 'Manrope', sans-serif; font-size: 26px; font-weight: 800; color: #1e293b; letter-spacing: -0.5px; }
.lc-compare-sub { font-size: 13px; color: #586064; margin-top: 4px; margin-bottom: 16px; }

.lc-compare-cols { display: grid; grid-template-columns: 1fr 1fr; gap: 8px; margin-bottom: 16px; }
.lc-compare-col-a .lc-compare-col-label { font-size: 9px; font-weight: 700; letter-spacing: 1.5px; text-transform: uppercase; color: #0061a4; }
.lc-compare-col-b .lc-compare-col-label { font-size: 9px; font-weight: 700; letter-spacing: 1.5px; text-transform: uppercase; color: #bb1b21; text-align: right; }
.lc-compare-col-name { font-family: 'Manrope', sans-serif; font-size: 16px; font-weight: 800; color: #1e293b; }
.lc-compare-col-b .lc-compare-col-name { text-align: right; }

.lc-comp-bar-section { margin-bottom: 16px; }
.lc-comp-bar-header { display: flex; justify-content: space-between; align-items: center; margin-bottom: 8px; }
.lc-comp-bar-title { font-size: 11px; font-weight: 700; letter-spacing: 1px; text-transform: uppercase; color: #586064; }
.lc-comp-delta { display: flex; align-items: center; gap: 4px; font-size: 12px; font-weight: 700; }
.delta-up { color: #bb1b21; }
.delta-dn { color: #006d41; }
.lc-comp-bar-track { height: 44px; background: #f1f4f6; border-radius: 10px; display: flex; overflow: hidden; position: relative; }
.lc-comp-bar-a { height: 100%; background: rgba(0,97,164,0.35); display: flex; align-items: center; padding: 0 10px; }
.lc-comp-bar-b { height: 100%; background: rgba(187,27,33,0.35); display: flex; align-items: center; justify-content: flex-end; padding: 0 10px; margin-left: auto; }
.lc-comp-bar-label { font-size: 12px; font-weight: 700; }
.lc-comp-bar-label-a { color: #004171; }
.lc-comp-bar-label-b { color: #3b0002; }

.lc-delta-grid { display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 8px; margin-bottom: 16px; }
.lc-delta-card { background: #f1f4f6; border-radius: 12px; padding: 12px; text-align: center; }
.lc-delta-val { font-family: 'Manrope', sans-serif; font-size: 18px; font-weight: 800; }
.lc-delta-lbl { font-size: 9px; color: #586064; font-weight: 700; text-transform: uppercase; letter-spacing: 0.5px; margin-top: 3px; opacity: 0.7; }

.lc-comp-insight {
  background: rgba(0,97,164,0.06); border: 1px solid rgba(0,97,164,0.15);
  border-radius: 16px; padding: 18px; position: relative; overflow: hidden; margin-bottom: 14px;
}
.lc-comp-insight-label { font-size: 10px; font-weight: 700; letter-spacing: 1px; text-transform: uppercase; color: #0061a4; margin-bottom: 8px; display: flex; align-items: center; gap: 6px; }
.lc-comp-insight p { font-size: 13px; line-height: 1.7; color: #2b3437; font-style: italic; font-weight: 500; }

.lc-export-btn {
  width: 100%; padding: 14px; background: #2b3437; color: white; border: none;
  border-radius: 14px; font-family: 'Manrope', sans-serif; font-size: 14px;
  font-weight: 700; cursor: pointer; display: flex; align-items: center;
  justify-content: center; gap: 8px; transition: opacity 0.2s;
}
.lc-export-btn:hover { opacity: 0.85; }

/* tab content */
.lc-panel-content { display: none; }
.lc-panel-content.active { display: block; }

/* Fix Shiny actionButton inside lc-panel-tabs */
.lc-panel-tabs .action-button {
  flex: 1; padding: 7px 4px; background: none; border: none; cursor: pointer;
  border-radius: 8px; font-family: 'Inter', sans-serif; font-size: 12px;
  font-weight: 600; color: #586064; transition: all 0.2s;
  box-shadow: none !important; outline: none;
}
.lc-panel-tabs .action-button:hover { color: #2b3437; }
.lc-panel-tabs .action-button.active {
  background: white; color: #0061a4;
  box-shadow: 0 1px 4px rgba(44,55,60,0.08) !important;
}

/* AQI cards */
.lc-aqi-grid { display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 8px; margin-bottom: 16px; }
.lc-aqi-card {
  background: #f8f9fa; border: 1px solid #eaeff1; border-radius: 12px;
  padding: 12px 8px; text-align: center;
}
.lc-aqi-val { font-family: 'Manrope', sans-serif; font-size: 26px; font-weight: 800; }
.lc-aqi-city { font-size: 12px; font-weight: 600; color: #2b3437; margin-top: 2px; }
.lc-aqi-label { font-size: 9px; font-weight: 700; text-transform: uppercase; letter-spacing: 0.5px; margin-top: 4px; padding: 2px 7px; border-radius: 10px; display: inline-block; }
.lc-aqi-pm { font-size: 10px; color: #7d8590; margin-top: 3px; }

/* anomaly warning */
.lc-anomaly {
  background: rgba(210,153,34,0.08); border: 1px solid rgba(210,153,34,0.25);
  border-radius: 10px; padding: 10px 12px; font-size: 11px; color: #856404;
  line-height: 1.5; margin-bottom: 12px;
}

/* material icons */
.material-symbols-outlined {
  font-variation-settings: 'FILL' 0, 'wght' 400, 'GRAD' 0, 'opsz' 24;
  vertical-align: middle; font-size: 20px;
}

/* ── DYNAMIC IMPROVEMENTS CSS ── */

/* Smooth transitions on stat numbers */
.lc-stat-val, .lc-scard-val, .lc-aqi-val, .lc-metric-val {
  transition: all 0.4s ease;
}

/* Search bar focus glow */
.lc-search:focus {
  background: white;
  box-shadow: 0 0 0 2px rgba(0,97,164,0.25);
  border-radius: 10px;
}
.lc-search.has-value {
  background: white;
  box-shadow: 0 0 0 2px rgba(0,97,164,0.15);
}

/* AQI live pulse dot */
@keyframes lc-pulse {
  0%   { opacity: 1;   transform: scale(1);   }
  50%  { opacity: 0.4; transform: scale(1.4); }
  100% { opacity: 1;   transform: scale(1);   }
}
.lc-live-dot {
  display: inline-block; width: 8px; height: 8px;
  background: #22c55e; border-radius: 50%;
  animation: lc-pulse 1.8s ease-in-out infinite;
  margin-right: 6px; vertical-align: middle;
}

/* AQI timestamp */
.lc-aqi-ts {
  font-size: 11px; color: #7d8590; font-style: italic; margin-top: 4px; margin-bottom: 10px;
}

/* Selected-state marker ring via JS class on leaflet */
.lc-search-result-hint {
  font-size: 11px; color: #0061a4; margin-top: 4px; font-weight: 600;
  min-height: 16px; transition: opacity 0.3s;
}

/* Apply button pulse when filters are dirty */
@keyframes lc-btn-glow {
  0%   { box-shadow: 0 4px 14px rgba(0,97,164,0.25); }
  50%  { box-shadow: 0 4px 24px rgba(0,97,164,0.55); }
  100% { box-shadow: 0 4px 14px rgba(0,97,164,0.25); }
}
.lc-apply-btn.dirty { animation: lc-btn-glow 1.2s ease-in-out infinite; }

/* hide Shiny default UI */
#shiny-disconnected-overlay, .shiny-notification { display: none; }
"

# ══════════════════════════════════════════════════════════════
# UI
# ══════════════════════════════════════════════════════════════
ui <- fluidPage(
  tags$head(
    tags$style(stitch_css),
    tags$link(rel="stylesheet",
              href="https://fonts.googleapis.com/css2?family=Material+Symbols+Outlined:wght,FILL@100..700,0..1&display=swap")
  ),
  
  # ── TOP NAVBAR ──────────────────────────────────────────────
  tags$div(class="lc-navbar",
           tags$div(class="lc-logo", "Geo ", tags$span("InSight")),
           tags$div(class="lc-nav-tabs",
                    tags$button("Locations",  id="tab_loc",  class="lc-tab action-button active"),
                    tags$button("Analytics",  id="tab_anal", class="lc-tab action-button"),
                    tags$button("Compare",    id="tab_comp", class="lc-tab action-button"),
                    tags$button("Reports",    id="tab_rep",  class="lc-tab action-button"),
                    tags$button("Dataset",    id="tab_data", class="lc-tab action-button")
           ),
           # ── LIVE SEARCH BAR (functional) ────────────────────────
           tags$div(class="lc-search-wrap",
                    tags$span(class="lc-search-icon material-symbols-outlined", "search"),
                    textInput("state_search", label=NULL, placeholder="Search state...",
                              width="200px") %>%
                      tagAppendAttributes(class="lc-search", id="state_search_input")
           )
  ),
  
  # ── MAIN ────────────────────────────────────────────────────
  tags$div(class="lc-main",
           
           # ── LEFT SIDEBAR ──────────────────────────────────────────
           tags$div(class="lc-sidebar",
                    tags$div(class="lc-sidebar-title", "Map Filters"),
                    tags$div(class="lc-precision-tag", "Precision Lens Active"),
                    
                    tags$div(class="lc-filter-label", "Locations"),
                    tags$button(id="nav_crime", class="lc-nav-item action-button active", tags$span(class="material-symbols-outlined","map"), "India Crime Map"),
                    tags$button(id="nav_poll", class="lc-nav-item action-button", tags$span(class="material-symbols-outlined","air"), "Pollution (AQI)"),
                    
                    tags$div(class="lc-filter-label", "Year"),
                    selectInput("year_sel", NULL, choices=c("2022","2021","2020"), selected="2022", width="100%"),
                    
                    tags$div(class="lc-filter-label", "Sort States By"),
                    selectInput("sort_sel", NULL,
                                choices=c("Crime Rate"="crime_rate_2022","Total Crimes"="crimes_2022","Chargesheeting %"="chargesheeting_rate"),
                                width="100%"),
                    
                    tags$div(class="lc-filter-label", "Trend Filter"),
                    selectInput("trend_sel", NULL, choices=c("All","increasing","decreasing"), width="100%"),
                    
                    tags$div(class="lc-filter-label", "Statistics"),
                    uiOutput("sidebar_stats_ui"),
                    
                    tags$div(class="lc-filter-label", "Key Insights"),
                    uiOutput("sidebar_insights_ui"),
                    
                    tags$div(class="lc-ncrb-note", "⚠️ Crime data: NCRB 2022. Total IPC cognizable crimes. Category breakdown unavailable."),
                    actionButton("apply_btn", "Apply Filters", class="lc-apply-btn"),
                    uiOutput("search_hint_ui")
           ),
           
           # ── MAP CENTER ────────────────────────────────────────────
           tags$div(class="lc-map-area",
                    leafletOutput("main_map", width="100%", height="100%"),
                    
                    # Floating state detail card
                    tags$div(class="lc-float-card", id="float_card",
                             tags$div(class="lc-float-header",
                                      tags$div(class="lc-float-name", uiOutput("float_name")),
                                      tags$div(
                                        uiOutput("float_badge"),
                                        tags$button(class="lc-float-close", onclick="document.getElementById('float_card').classList.remove('visible')", "×")
                                      )
                             ),
                             uiOutput("float_metrics"),
                             uiOutput("float_trend_row"),
                             tags$div(class="lc-card-insight",
                                      tags$div(class="lc-card-insight-label",
                                               tags$span(class="material-symbols-outlined","lightbulb"), "Insight"),
                                      uiOutput("float_insight_text")
                             )
                    ),
                    
                    # Legend
                    tags$div(class="lc-legend",
                             tags$div(class="lc-legend-title", "Crime Rate / Lakh"),
                             tags$div(class="lc-legend-item", tags$div(class="lc-legend-dot",style="background:#800026"), "> 600 Critical"),
                             tags$div(class="lc-legend-item", tags$div(class="lc-legend-dot",style="background:#E31A1C"), "400–600 High"),
                             tags$div(class="lc-legend-item", tags$div(class="lc-legend-dot",style="background:#FC4E2A"), "200–400 Medium"),
                             tags$div(class="lc-legend-item", tags$div(class="lc-legend-dot",style="background:#FEB24C"), "< 200 Low")
                    ),
                    
                    # Zoom Controls
                    tags$div(class="lc-map-controls",
                             tags$button(class="lc-zoom-btn", onclick="try{var m=HTMLWidgets.find('#main_map').getMap();m.setZoom(m.getZoom()+1);}catch(e){}", "+"),
                             tags$button(class="lc-zoom-btn", onclick="try{var m=HTMLWidgets.find('#main_map').getMap();m.setZoom(m.getZoom()-1);}catch(e){}", "\u2212")
                    )
           ),
           
           # ── RIGHT ANALYTICS PANEL ─────────────────────────────────
           tags$div(class="lc-analytics", style="display: none;",
                    
                    # Panel tabs — wired through Shiny server for reliability
                    tags$div(class="lc-panel-tabs",
                             actionButton("pt_over", "Overview",  class="lc-ptab active"),
                             actionButton("pt_trnd", "Trends",    class="lc-ptab"),
                             actionButton("pt_poll", "Pollution", class="lc-ptab"),
                             actionButton("pt_cmp",  "Compare",   class="lc-ptab")
                    ),
                    
                    # ── OVERVIEW ──────────────────────────────────────────
                    tags$div(class="lc-panel-content active", id="pc_over",
                             tags$div(class="lc-panel-title", "Region Analytics"),
                             tags$div(class="lc-panel-sub", "India — All States & UTs"),
                             
                             uiOutput("anomaly_warning"),
                             uiOutput("stats_2x2"),
                             
                             tags$div(class="lc-chart-section",
                                      tags$div(class="lc-chart-header",
                                               tags$div(class="lc-chart-title", "12-Month Trend"),
                                               downloadLink("dl_csv", "EXPORT CSV", class="lc-chart-action")
                                      ),
                                      plotlyOutput("trend_chart_main", height="160px")
                             ),
                             
                             tags$div(class="lc-chart-section",
                                      tags$div(class="lc-chart-title", style="margin-bottom:10px;", "Distribution Breakdown"),
                                      uiOutput("dist_bars")
                             ),
                             
                             uiOutput("status_box")
                    ),
                    
                    # ── TRENDS ────────────────────────────────────────────
                    tags$div(class="lc-panel-content", id="pc_trnd",
                             tags$div(class="lc-panel-title", "Trend Analysis"),
                             tags$div(class="lc-panel-sub", "Crime patterns 2020–2022"),
                             
                             tags$div(class="lc-chart-section",
                                      tags$div(class="lc-chart-title", style="margin-bottom:8px;", "Top 10 States by Selected Metric"),
                                      plotlyOutput("top_states_chart", height="260px")
                             ),
                             tags$div(class="lc-chart-section",
                                      tags$div(class="lc-chart-title", style="margin-bottom:8px;", "National Crime Total"),
                                      plotlyOutput("national_chart", height="160px")
                             ),
                             tags$div(class="lc-chart-section",
                                      tags$div(class="lc-chart-title", style="margin-bottom:8px;", "Top 10 States — Crime Rate"),
                                      uiOutput("top_states_list")
                             )
                    ),
                    
                    # ── POLLUTION ─────────────────────────────────────────
                    tags$div(class="lc-panel-content", id="pc_poll",
                             tags$div(class="lc-panel-title", "Air Quality"),
                             tags$div(class="lc-panel-sub", "Live AQI — Open-Meteo API"),
                             tags$div(style="margin-bottom:10px;",
                                      actionButton("refresh_aqi", "↻  Refresh Live Data",
                                                   style="background:#0061a4;color:white;border:none;border-radius:8px;padding:7px 16px;font-size:12px;font-weight:700;cursor:pointer;font-family:'Manrope',sans-serif;")
                             ),
                             uiOutput("aqi_cards_ui"),
                             tags$div(class="lc-chart-section",
                                      tags$div(class="lc-chart-title", style="margin-bottom:8px;", "AQI Comparison"),
                                      plotlyOutput("aqi_bar", height="200px")
                             )
                    ),
                    
                    # ── COMPARE ───────────────────────────────────────────
                    tags$div(class="lc-panel-content", id="pc_cmp",
                             tags$div(class="lc-compare-header",
                                      tags$span(class="lc-compare-badge", "Comparison Mode"),
                                      tags$span()
                             ),
                             tags$div(class="lc-compare-title", "Regional Divergence"),
                             tags$div(class="lc-compare-sub", "Comparative analysis of selected states"),
                             
                             tags$div(class="lc-filter-label", "Region A"),
                             selectInput("cmp_a", NULL, choices=india_crime$state, selected="Delhi", width="100%"),
                             tags$div(class="lc-filter-label", "Region B"),
                             selectInput("cmp_b", NULL, choices=india_crime$state, selected="Kerala", width="100%"),
                             
                             uiOutput("compare_bars_ui"),
                             uiOutput("compare_deltas_ui"),
                             uiOutput("compare_insight_ui"),
                             
                             tags$div(style="margin-top:4px;",
                                      downloadButton("dl_compare", "Export Comparison Report", class="lc-export-btn",
                                                     icon=tags$span(class="material-symbols-outlined","ios_share"))
                             )
                    )
           ),
           
           # ── FULL SCREEN DATASET AREA ──────────────────────────────
           tags$div(id="dataset_full_area",
                    style="display:none; width:100%; height:100%; padding:32px; background:white; overflow:auto;",
                    tags$div(class="lc-compare-title", style="margin-bottom:8px;", "India Crime Dataset (2020–2022)"),
                    tags$div(class="lc-compare-sub", style="margin-bottom:24px;", "Complete state-wise records sourced directly from india_crime_data.json"),
                    DTOutput("dataset_table_full")
           )
  ),
  
  # ── JS ─────────────────────────────────────────────────────
  tags$script(HTML("
    Shiny.addCustomMessageHandler('lcSwitchTab', function(msg) {
      ['over','trnd','poll','cmp','data'].forEach(function(x) {
        var btn = document.getElementById('pt_'+x);
        var con = document.getElementById('pc_'+x);
        if(btn) btn.classList.remove('active');
        if(con) con.classList.remove('active');
      });
      ['loc','anal','comp','rep','data'].forEach(function(x) {
        var tb = document.getElementById('tab_'+x);
        if(tb) tb.classList.remove('active');
      });
      
      var ab = document.getElementById('pt_'+msg.pt);
      var ac = document.getElementById('pc_'+msg.pt);
      var topBtn = document.getElementById('tab_'+msg.top);
      if(ab) ab.classList.add('active');
      if(ac) ac.classList.add('active');
      if(topBtn) topBtn.classList.add('active');
      
      // Toggle full-screen dataset viewer vs standard dashboard
      var parts = document.querySelectorAll('.lc-sidebar, .lc-map-area, .lc-analytics');
      var area = document.getElementById('dataset_full_area');
      var mapArea = document.querySelector('.lc-map-area');
      var analyticsArea = document.querySelector('.lc-analytics');
      
      if(msg.pt === 'data') {
        parts.forEach(function(el) { el.style.display = 'none'; });
        if(area) area.style.display = 'block';
        setTimeout(function() { window.dispatchEvent(new Event('resize')); }, 100);
      } else {
        if(area) area.style.display = 'none';
        parts.forEach(function(el) { el.style.display = ''; }); // Reset default flex
        
        // Map vs Analytics logic
        if (msg.top === 'loc') {
          if (analyticsArea) analyticsArea.style.display = 'none';
          if (mapArea) mapArea.style.display = '';
        } else if (msg.top === 'anal' || msg.top === 'comp' || msg.top === 'rep') {
          if (mapArea) mapArea.style.display = 'none';
          if (analyticsArea) {
            analyticsArea.style.display = '';
            analyticsArea.style.width = '100%';
            analyticsArea.style.flex = '1';
            analyticsArea.style.maxWidth = '100%';
          }
        }
        setTimeout(function() { window.dispatchEvent(new Event('resize')); }, 100);
      }
    });

    Shiny.addCustomMessageHandler('lcSwitchNav', function(m) {
      var n1 = document.getElementById('nav_crime');
      var n2 = document.getElementById('nav_poll');
      if(n1) n1.classList.remove('active');
      if(n2) n2.classList.remove('active');
      var n3 = document.getElementById('nav_' + m);
      if(n3) n3.classList.add('active');
    });

    // Show float card on Shiny message
    Shiny.addCustomMessageHandler('showFloatCard', function(msg) {
      var cd = document.getElementById('float_card');
      if(cd) cd.classList.add('visible');
    });

    // Dirty-filter button pulse
    Shiny.addCustomMessageHandler('lcDirtyBtn', function(msg) {
      var btn = document.getElementById('apply_btn');
      if(!btn) return;
      if(msg.dirty) { btn.classList.add('dirty'); }
      else          { btn.classList.remove('dirty'); }
    });

    // Search bar: add has-value CSS class for glow
    document.addEventListener('input', function(e) {
      if(e.target && e.target.id === 'state_search') {
        e.target.classList.toggle('has-value', e.target.value.length > 0);
      }
    });
  "))
)

# ══════════════════════════════════════════════════════════════
# SERVER
# ══════════════════════════════════════════════════════════════
server <- function(input, output, session) {
  
  selected_state   <- reactiveVal(NULL)
  aqi_data         <- reactiveVal(NULL)
  aqi_fetched_time <- reactiveVal(NULL)   # timestamp of last AQI fetch
  search_query     <- reactiveVal("")     # live search text
  
  # ── Panel tab switching via server ──────────────────────────
  observeEvent(input$pt_over, { session$sendCustomMessage("lcSwitchTab", list(pt="over", top="loc")) })
  observeEvent(input$pt_trnd, { session$sendCustomMessage("lcSwitchTab", list(pt="trnd", top="anal")) })
  observeEvent(input$pt_poll, { session$sendCustomMessage("lcSwitchTab", list(pt="poll", top="rep")) })
  observeEvent(input$pt_cmp,  { session$sendCustomMessage("lcSwitchTab", list(pt="cmp", top="comp")) })
  
  observeEvent(input$tab_loc,  { session$sendCustomMessage("lcSwitchTab", list(pt="over", top="loc")) })
  observeEvent(input$tab_anal, { session$sendCustomMessage("lcSwitchTab", list(pt="trnd", top="anal")) })
  observeEvent(input$tab_comp, { session$sendCustomMessage("lcSwitchTab", list(pt="cmp",  top="comp")) })
  observeEvent(input$tab_rep,  { session$sendCustomMessage("lcSwitchTab", list(pt="poll", top="rep"))  })
  observeEvent(input$tab_data, { session$sendCustomMessage("lcSwitchTab", list(pt="data", top="data")) })
  
  map_mode <- reactiveVal("crime")
  observeEvent(input$nav_crime, {
    map_mode("crime")
    session$sendCustomMessage("lcSwitchNav", "crime")
    session$sendCustomMessage("lcSwitchTab", list(pt="over", top="loc"))
  })
  observeEvent(input$nav_poll, {
    map_mode("poll")
    session$sendCustomMessage("lcSwitchNav", "poll")
    session$sendCustomMessage("lcSwitchTab", list(pt="poll", top="rep"))
  })
  
  # ── Fetch AQI ───────────────────────────────────────────────
  fetch_all_aqi <- function() {
    withProgress(message="Fetching live AQI...", value=0, {
      results <- lapply(names(city_coords), function(city) {
        incProgress(1/length(city_coords), detail=city)
        fetch_aqi(city)
      })
      df <- do.call(rbind, lapply(results, function(r) {
        data.frame(city=r$city,
                   aqi=ifelse(is.null(r$aqi)||is.na(r$aqi),NA_real_,as.numeric(r$aqi)),
                   pm25=ifelse(is.null(r$pm25),NA_real_,as.numeric(r$pm25)),
                   pm10=ifelse(is.null(r$pm10),NA_real_,as.numeric(r$pm10)),
                   no2=ifelse(is.null(r$no2),NA_real_,as.numeric(r$no2)),
                   label=r$label, color=r$color, stringsAsFactors=FALSE)
      }))
      aqi_data(df)
      aqi_fetched_time(format(Sys.time(), "%H:%M:%S"))  # record fetch time
    })
  }
  observe({ fetch_all_aqi() })
  observeEvent(input$refresh_aqi, { fetch_all_aqi() })

  # ── Instant reactive filters (no Apply needed) ────────────────
  confirmed_filters <- reactive({
    list(trend = input$trend_sel, year = input$year_sel, sort = input$sort_sel)
  })

  # Pulse Apply button when filters are dirty
  observeEvent(list(input$year_sel, input$trend_sel, input$sort_sel), {
    session$sendCustomMessage("lcDirtyBtn", list(dirty=TRUE))
  }, ignoreInit = TRUE)
  observeEvent(input$apply_btn, {
    session$sendCustomMessage("lcDirtyBtn", list(dirty=FALSE))
  })

  # ── Search: pan map to matching state ────────────────────────
  observeEvent(input$state_search, {
    q <- trimws(tolower(input$state_search))
    search_query(q)
    if(nchar(q) >= 2) {
      hits <- india_crime[grepl(q, tolower(india_crime$state)), ]
      if(nrow(hits) > 0)
        leafletProxy("main_map") %>% setView(lng=hits$lng[1], lat=hits$lat[1], zoom=6)
    }
  }, ignoreInit = TRUE)

  fdata <- reactive({
    df <- india_crime
    f  <- confirmed_filters()
    
    y <- f$year
    if(is.null(y)) y <- "2022"
    yr_col <- paste0("crimes_", y)
    if(!yr_col %in% names(df)) yr_col <- "crimes_2022"
    
    df$active_crimes <- df[[yr_col]]
    df$active_rate   <- round(df$active_crimes / df$population_lakhs, 1)
    df$active_year   <- y
    
    if(!is.null(f$trend) && f$trend != "All") {
      df <- df %>% filter(trend == f$trend)
    }
    
    if(!is.null(f$sort)) {
      sort_col <- "active_rate"
      if(f$sort == "crimes_2022") sort_col <- "active_crimes"  # active_crimes already set to year-specific column
      if(f$sort == "crime_rate_2022") sort_col <- "active_rate"
      if(f$sort == "chargesheeting_rate") sort_col <- "chargesheeting_rate"
      df <- df[order(-df[[sort_col]]), ]
    } else {
      df <- df[order(-df$active_rate), ]
    }
    df
  })
  
  # ── MAP ──────────────────────────────────────────────────────
  # ── Marker highlighting: re-render with selected state emphasised ────────────────────
  output$main_map <- renderLeaflet({
    leaflet(options=leafletOptions(zoomControl=FALSE)) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng=82, lat=22, zoom=5)
  })
  
  observe({
    df   <- fdata()
    mode <- map_mode()
    
    leafletProxy("main_map") %>% clearMarkers()
    
    # BUG 2 FIX: Guard against empty filtered data
    if(nrow(df) == 0) return()
    
    if(mode == "crime") {
      df$col    <- get_crime_color(df$active_rate)
      df$radius <- pmax(8, pmin(26, df$active_rate/38))
      
      leafletProxy("main_map") %>%
        addCircleMarkers(data=df, lng=~lng, lat=~lat,
                         radius=~radius, color="rgba(255,255,255,0.5)", weight=1.5,
                         fillColor=~col, fillOpacity=0.82, layerId=~state,
                         popup=~paste0(
                           "<div style='font-family:Inter,sans-serif;padding:6px;min-width:180px;'>",
                           "<b style='font-size:15px;color:#1e293b;'>",state,"</b><br>",
                           "<span style='color:#586064;font-size:11px;'>Click marker for full details</span><br><br>",
                           "<b style='color:#0061a4;'>Crime Rate (",active_year,"):</b> ",active_rate,"/lakh<br>",
                           "<b style='color:#0061a4;'>Crimes (",active_year,"):</b> ",
                           format(active_crimes,big.mark=","),"<br>",
                           "<b style='color:#0061a4;'>Trend:</b> ",trend,
                           "</div>"
                         )
        )
    } else {
      # mode == "poll"
      aqi_df <- aqi_data()
      if(!is.null(aqi_df)) {
        aqi_df_valid <- aqi_df[!is.na(aqi_df$aqi), ]
        if(nrow(aqi_df_valid) > 0) {
          coords_list <- do.call(rbind, lapply(names(city_coords), function(n) {
            data.frame(city=n, lat=city_coords[[n]][1], lng=city_coords[[n]][2], stringsAsFactors=FALSE)
          }))
          aqi_df_valid <- merge(aqi_df_valid, coords_list, by="city")
          aqi_df_valid$radius <- pmax(10, pmin(30, aqi_df_valid$aqi/10))
          
          leafletProxy("main_map") %>%
            addCircleMarkers(data=aqi_df_valid, lng=~lng, lat=~lat,
                             radius=~radius, color="rgba(255,255,255,0.5)", weight=1.5,
                             fillColor=~color, fillOpacity=0.82, layerId=~paste0("AQI_", city),
                             popup=~paste0(
                               "<div style='font-family:Inter,sans-serif;padding:6px;min-width:180px;'>",
                               "<b style='font-size:15px;color:#1e293b;'>",city,"</b><br>",
                               "<b style='color:",color,";'>AQI: ",aqi,"</b> (",label,")<br>",
                               "PM2.5: ",pm25," | PM10: ",pm10,"<br>",
                               "NO2: ",no2,
                               "</div>"
                             )
            )
        }
      }
    }
  })
  
  observeEvent(input$main_map_marker_click, {
    click <- input$main_map_marker_click
    # BUG 4 FIX: Ignore AQI city marker clicks — only handle state crime markers
    if(!is.null(click$id) && !startsWith(click$id, "AQI_")) {
      s <- fdata() %>% filter(state == click$id)
      if(nrow(s) > 0) {
        selected_state(s)
        session$sendCustomMessage("showFloatCard", list())
        # ── Fly to clicked state ─────────────────────────────────────
        leafletProxy("main_map") %>%
          flyTo(lng=s$lng, lat=s$lat, zoom=6)
      }
    }
  })
  
  # ── FLOAT CARD ───────────────────────────────────────────────
  output$float_name <- renderUI({
    s <- selected_state()
    if(is.null(s)) return(tags$span("—"))
    tags$span(s$state)
  })
  
  output$float_badge <- renderUI({
    s <- selected_state()
    if(is.null(s)) return(NULL)
    cls <- if(s$active_rate>600) "lc-alert-badge badge-high" else if(s$active_rate>300) "lc-alert-badge badge-med" else "lc-alert-badge badge-low"
    lbl <- if(s$active_rate>600) "HIGH ALERT" else if(s$active_rate>300) "ELEVATED" else "NORMAL"
    tags$span(class=cls, lbl)
  })
  
  output$float_metrics <- renderUI({
    s <- selected_state()
    if(is.null(s)) return(NULL)
    tags$div(class="lc-metrics-grid",
             tags$div(class="lc-metric",
                      tags$div(class="lc-metric-val", s$active_rate),
                      tags$div(class="lc-metric-lbl","Rate/Lakh")),
             tags$div(class="lc-metric",
                      tags$div(class="lc-metric-val", format(s$active_crimes,big.mark=",")),
                      tags$div(class="lc-metric-lbl",paste0("Crimes ",s$active_year))),
             tags$div(class="lc-metric",
                      tags$div(class="lc-metric-val", paste0(s$chargesheeting_rate,"%")),
                      tags$div(class="lc-metric-lbl","Chargesheeting"))
    )
  })
  
  output$float_trend_row <- renderUI({
    s <- selected_state()
    if(is.null(s)) return(NULL)
    cls  <- if(s$trend=="increasing") "lc-trend-pill pill-inc" else "lc-trend-pill pill-dec"
    arrow <- if(s$trend=="increasing") "↑ Increasing" else "↓ Decreasing"
    pct <- round((s$active_crimes - s$crimes_2020)/s$crimes_2020*100, 1)
    tags$div(class="lc-trend-row",
             tags$span(class=cls, arrow),
             tags$span(paste0(ifelse(pct>=0,"+",""), pct, "% since 2020"),
                       style="font-size:11px;color:#586064;align-self:center;")
    )
  })
  
  output$float_insight_text <- renderUI({
    s <- selected_state()
    if(is.null(s)) return(tags$p("Click a state on the map.", style="font-size:12px;color:#586064;"))
    diff_pct <- round((s$active_rate - NATIONAL_AVG)/NATIONAL_AVG*100)
    dir <- if(diff_pct>=0) "above" else "below"
    tags$p(paste0(s$state, " is ", abs(diff_pct), "% ", dir, " the national average of ", NATIONAL_AVG, "/lakh. Population: ", s$population_lakhs, " lakh."))
  })
  
  # ── SIDEBAR STATS ────────────────────────────────────────────
  output$sidebar_stats_ui <- renderUI({
    df <- fdata()
    if(nrow(df)==0) return(NULL)
    st <- compute_stats(df$active_rate)
    tags$div(class="lc-stat-grid",
             tags$div(class="lc-stat-card",
                      tags$div(class="lc-stat-val", st$mean),
                      tags$div(class="lc-stat-lbl","Mean Rate")),
             tags$div(class="lc-stat-card",
                      tags$div(class="lc-stat-val", st$median),
                      tags$div(class="lc-stat-lbl","Median")),
             tags$div(class="lc-stat-card",
                      tags$div(class="lc-stat-val", st$sd),
                      tags$div(class="lc-stat-lbl","Std Dev")),
             tags$div(class="lc-stat-card",
                      tags$div(class="lc-stat-val", style="color:#0061a4;", NATIONAL_AVG),
                      tags$div(class="lc-stat-lbl","Nat'l Avg"))
    )
  })
  
  # ── SIDEBAR INSIGHTS ─────────────────────────────────────────
  output$sidebar_insights_ui <- renderUI({
    df <- fdata()
    if(nrow(df)==0) return(tags$div("No data for current filters."))
    top  <- df[which.max(df$active_rate),]
    bot  <- df[which.min(df$active_rate),]
    inc  <- sum(df$trend=="increasing")
    pct  <- round((top$active_rate/NATIONAL_AVG-1)*100)
    mk   <- function(txt) tags$div(class="lc-insight-item",
                                   tags$span(class="lc-insight-arrow","→"),
                                   tags$span(class="lc-insight-text", txt))
    tags$div(class="lc-insight-box",
             tags$div(class="lc-insight-title",
                      tags$span(class="material-symbols-outlined","lightbulb"), "Auto Analysis"),
             mk(paste0(top$state,": ",top$active_rate,"/lakh (",pct,"% above avg)")),
             mk(paste0(bot$state," lowest: ",bot$active_rate,"/lakh")),
             mk(paste0(inc," of ",nrow(df)," states show ↑ trend")),
             mk("Kerala high rate reflects strict reporting norms")
    )
  })
  
  # ── OVERVIEW: Stats 2x2 ──────────────────────────────────────
  output$stats_2x2 <- renderUI({
    df <- fdata()
    if(nrow(df)==0) return(NULL)
    st <- compute_stats(df$active_rate)
    tags$div(class="lc-stats-2x2",
             tags$div(class="lc-scard",
                      tags$div(class="lc-scard-val", st$mean),
                      tags$div(class="lc-scard-lbl","Mean Crime Rate")),  # BUG 5 FIX: was "Mean AQI / Rate"
             tags$div(class="lc-scard",
                      tags$div(class="lc-scard-val", st$median),
                      tags$div(class="lc-scard-lbl","Median Crime Rate"))
    )
  })
  
  # ── ANOMALY WARNING ──────────────────────────────────────────
  output$anomaly_warning <- renderUI({
    tags$div(class="lc-anomaly",
             "⚠️ Tamil Nadu: 78% drop in reported crimes (2020→2022) — likely a reporting change, not an actual reduction.")
  })
  
  # ── OVERVIEW: Trend chart ─────────────────────────────────────
  output$trend_chart_main <- renderPlotly({
    totals <- c(sum(india_crime$crimes_2020), sum(india_crime$crimes_2021), sum(india_crime$crimes_2022))
    plot_ly(x=c("2020","2021","2022"), y=totals, type="scatter", mode="lines+markers",
            line=list(color="#0061a4",width=2.5),
            marker=list(color="#0061a4",size=7),
            fill="tozeroy", fillcolor="rgba(0,97,164,0.08)") %>%
      layout(paper_bgcolor="white", plot_bgcolor="white",
             font=list(family="Inter",color="#586064",size=11),
             xaxis=list(title="",color="#adb5bd",gridcolor="#eaeff1",showgrid=TRUE),
             yaxis=list(title="",color="#adb5bd",gridcolor="#eaeff1",tickformat=".2s"),
             margin=list(t=4,b=30,l=50,r=4), showlegend=FALSE,
             hovermode="x unified")
  })
  
  # ── OVERVIEW: Distribution bars ───────────────────────────────
  output$dist_bars <- renderUI({
    # BUG 6 FIX: Labels were incorrectly mixing AQI pollutant names with crime-rate buckets
    cats <- list(
      list(lbl="High Rate (>300/lakh)",     pct=round(sum(india_crime$crime_rate_2022>300)/36*100)),
      list(lbl="Medium Rate (150–300)",      pct=round(sum(india_crime$crime_rate_2022>150 & india_crime$crime_rate_2022<=300)/36*100)),
      list(lbl="Low Rate (≤150/lakh)",       pct=round(sum(india_crime$crime_rate_2022<=150)/36*100)),
      list(lbl="Increasing Trend",           pct=round(sum(india_crime$trend=="increasing")/36*100))
    )
    tagList(lapply(cats, function(c) {
      tags$div(class="lc-dist-bar",
               tags$div(class="lc-dist-bar-header",
                        tags$span(class="lc-dist-bar-label", c$lbl),
                        tags$span(class="lc-dist-bar-val",   paste0(c$pct,"%"))),
               tags$div(class="lc-dist-bar-track",
                        tags$div(class="lc-dist-bar-fill", style=paste0("width:",c$pct,"%;")))
      )
    }))
  })
  
  # ── OVERVIEW: Status box ─────────────────────────────────────
  output$status_box <- renderUI({
    tags$div(class="lc-status-box",
             tags$div(class="lc-status-header",
                      tags$span(class="material-symbols-outlined lc-status-icon","verified"),
                      tags$span(class="lc-status-title","Data Status")),
             tags$p(class="lc-status-text",
                    "NCRB 2022 dataset loaded — 36 states/UTs. Open-Meteo AQI live feed active for 9 cities. Last refreshed on app load.")
    )
  })
  
  # ── TRENDS: Top states chart ──────────────────────────────────
  output$top_states_chart <- renderPlotly({
    metric <- input$sort_sel
    if(is.null(metric)) metric <- "crime_rate_2022"
    df  <- india_crime %>% arrange(desc(.data[[metric]])) %>% head(10)
    val <- df[[metric]]
    plot_ly(x=val, y=reorder(df$state,val), type="bar", orientation="h",
            marker=list(color="#0061a4", opacity=0.85,
                        line=list(color="rgba(0,97,164,0.2)",width=1))) %>%
      layout(paper_bgcolor="white", plot_bgcolor="white",
             font=list(family="Inter",color="#586064",size=11),
             xaxis=list(title="",gridcolor="#eaeff1"),
             yaxis=list(title="",gridcolor="rgba(0,0,0,0)"),
             margin=list(t=4,b=30,l=130,r=4), showlegend=FALSE)
  })
  
  output$national_chart <- renderPlotly({
    yrs <- c("2020","2021","2022")
    tot <- c(sum(india_crime$crimes_2020),sum(india_crime$crimes_2021),sum(india_crime$crimes_2022))
    plot_ly(x=yrs, y=tot, type="bar",
            marker=list(color=c("rgba(0,97,164,0.4)","rgba(0,97,164,0.6)","rgba(0,97,164,0.9)"),
                        line=list(color="#0061a4",width=1))) %>%
      layout(paper_bgcolor="white", plot_bgcolor="white",
             font=list(family="Inter",color="#586064",size=11),
             xaxis=list(title="",gridcolor="#eaeff1"),
             yaxis=list(title="",gridcolor="#eaeff1",tickformat=".2s"),
             margin=list(t=4,b=30,l=50,r=4), showlegend=FALSE)
  })
  
  output$top_states_list <- renderUI({
    df  <- india_crime %>% arrange(desc(crime_rate_2022)) %>% head(10)
    mx  <- max(df$crime_rate_2022)
    tagList(lapply(seq_len(nrow(df)), function(i) {
      r   <- df[i,]
      pct <- round(r$crime_rate_2022/mx*100)
      tags$div(class="lc-top-state",
               tags$span(class="lc-top-rank", i),
               tags$span(class="lc-top-name", r$state),
               tags$div(class="lc-top-bar-wrap",
                        tags$div(class="lc-top-bar-fill", style=paste0("width:",pct,"%"))),
               tags$span(class="lc-top-val", r$crime_rate_2022)
      )
    }))
  })
  
  # ── POLLUTION ────────────────────────────────────────────────
  output$aqi_cards_ui <- renderUI({
    df <- aqi_data()
    if(is.null(df)) return(tags$p("Fetching AQI data...", style="color:#586064;font-size:13px;"))
    tags$div(class="lc-aqi-grid",
             lapply(seq_len(nrow(df)), function(i) {
               r   <- df[i,]
               val <- if(is.na(r$aqi)) "—" else r$aqi
               col <- r$color
               tags$div(class="lc-aqi-card",
                        tags$div(class="lc-aqi-val", style=paste0("color:",col,";"), val),
                        tags$div(class="lc-aqi-city", r$city),
                        tags$div(class="lc-aqi-label",
                                 style=paste0("background:",col,"22;color:",col,";"), r$label),
                        if(!is.na(r$pm25))
                          tags$div(class="lc-aqi-pm", paste0("PM2.5: ",r$pm25))
               )
             })
    )
  })
  
  output$aqi_bar <- renderPlotly({
    df <- aqi_data()
    # IMP 3 FIX: Show a helpful loading message instead of a blank plot
    if(is.null(df)) return(plotly_empty() %>% layout(title=list(text="Fetching AQI data...", font=list(color="#586064", size=13, family="Inter"))))
    df <- df[!is.na(df$aqi),]
    plot_ly(x=df$city, y=df$aqi, type="bar",
            marker=list(color=df$color, line=list(color="white",width=1))) %>%
      layout(paper_bgcolor="white", plot_bgcolor="white",
             font=list(family="Inter",color="#586064",size=11),
             xaxis=list(title="",gridcolor="#eaeff1"),
             yaxis=list(title="AQI",gridcolor="#eaeff1"),
             margin=list(t=4,b=50,l=40,r=4), showlegend=FALSE,
             shapes=list(list(type="line",x0=-0.5,x1=nrow(df)-0.5,y0=100,y1=100,
                              line=list(color="#FC4E2A",dash="dash",width=1.5))))
  })
  
  # ── COMPARE ──────────────────────────────────────────────────
  output$compare_bars_ui <- renderUI({
    a <- india_crime %>% filter(state==input$cmp_a)
    b <- india_crime %>% filter(state==input$cmp_b)
    if(nrow(a)==0||nrow(b)==0) return(NULL)
    
    mx   <- max(a$crime_rate_2022, b$crime_rate_2022)
    pct_a <- round(a$crime_rate_2022/mx*80)
    pct_b <- round(b$crime_rate_2022/mx*80)
    diff  <- round((b$crime_rate_2022 - a$crime_rate_2022)/a$crime_rate_2022*100,1)
    
    tags$div(
      tags$div(class="lc-compare-cols",
               tags$div(class="lc-compare-col-a",
                        tags$div(class="lc-compare-col-label","Region A"),
                        tags$div(class="lc-compare-col-name", a$state)),
               tags$div(class="lc-compare-col-b",
                        tags$div(class="lc-compare-col-label","Region B"),
                        tags$div(class="lc-compare-col-name", b$state))
      ),
      
      tags$div(class="lc-comp-bar-section",
               tags$div(class="lc-comp-bar-header",
                        tags$span(class="lc-comp-bar-title","Crime Rate (per lakh)"),
                        tags$div(class=paste0("lc-comp-delta ",ifelse(diff>=0,"delta-up","delta-dn")),
                                 ifelse(diff>=0,"↑","↓"), paste0(abs(diff),"%"))
               ),
               tags$div(class="lc-comp-bar-track",
                        tags$div(class="lc-comp-bar-a", style=paste0("width:",pct_a,"%;"),
                                 tags$span(class="lc-comp-bar-label lc-comp-bar-label-a", a$crime_rate_2022)),
                        tags$div(class="lc-comp-bar-b", style=paste0("width:",pct_b,"%;"),
                                 tags$span(class="lc-comp-bar-label lc-comp-bar-label-b", b$crime_rate_2022))
               )
      )
    )
  })
  
  output$compare_deltas_ui <- renderUI({
    a <- india_crime %>% filter(state==input$cmp_a)
    b <- india_crime %>% filter(state==input$cmp_b)
    if(nrow(a)==0||nrow(b)==0) return(NULL)
    
    cs_diff   <- round((b$chargesheeting_rate - a$chargesheeting_rate)/a$chargesheeting_rate*100,1)
    pop_diff  <- round((b$population_lakhs - a$population_lakhs)/a$population_lakhs*100,1)
    rate_diff <- round((b$crime_rate_2022 - a$crime_rate_2022)/a$crime_rate_2022*100,1)
    
    mk_delta <- function(val, lbl) {
      col <- if(val>=0) "#bb1b21" else "#006d41"
      tags$div(class="lc-delta-card",
               tags$div(class="lc-delta-val", style=paste0("color:",col,";"),
                        paste0(ifelse(val>=0,"+",""),val,"%")),
               tags$div(class="lc-delta-lbl", lbl)
      )
    }
    tags$div(class="lc-delta-grid",
             mk_delta(cs_diff,   "Chargesheeting"),
             mk_delta(pop_diff,  "Population"),
             mk_delta(rate_diff, "Crime Rate")
    )
  })
  
  output$compare_insight_ui <- renderUI({
    a <- india_crime %>% filter(state==input$cmp_a)
    b <- india_crime %>% filter(state==input$cmp_b)
    if(nrow(a)==0||nrow(b)==0) return(NULL)
    higher <- if(a$crime_rate_2022>b$crime_rate_2022) a else b
    lower  <- if(a$crime_rate_2022>b$crime_rate_2022) b else a
    diff   <- round((higher$crime_rate_2022-lower$crime_rate_2022)/lower$crime_rate_2022*100)
    txt    <- paste0('"', higher$state, ' shows a ', diff, '% higher crime rate than ',
                     lower$state, ' (', higher$crime_rate_2022, ' vs ', lower$crime_rate_2022,
                     ' per lakh). National average is ', NATIONAL_AVG, '/lakh."')
    tags$div(class="lc-comp-insight",
             tags$div(class="lc-comp-insight-label",
                      tags$span(class="material-symbols-outlined","auto_awesome"), "Comparative Insight"),
             tags$p(txt)
    )
  })
  
  # ── CSV DOWNLOAD ─────────────────────────────────────────────
  output$dl_csv <- downloadHandler(
    filename = function() paste0("GeoInsight_NCRB_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(india_crime, file, row.names=FALSE)
  )
  output$dl_compare <- downloadHandler(
    filename = function() paste0("GeoInsight_Compare_", Sys.Date(), ".csv"),
    content  = function(file) {
      a <- india_crime %>% filter(state==input$cmp_a)
      b <- india_crime %>% filter(state==input$cmp_b)
      write.csv(rbind(a,b), file, row.names=FALSE)
    }
  )
  
  # ── outputOptions: must come AFTER all output$* definitions ──
  # Float card outputs (inside display:none div)
  outputOptions(output, "float_name",          suspendWhenHidden = FALSE)
  outputOptions(output, "float_badge",         suspendWhenHidden = FALSE)
  outputOptions(output, "float_metrics",       suspendWhenHidden = FALSE)
  outputOptions(output, "float_trend_row",     suspendWhenHidden = FALSE)
  outputOptions(output, "float_insight_text",  suspendWhenHidden = FALSE)
  # Hidden tab panel outputs
  outputOptions(output, "compare_bars_ui",     suspendWhenHidden = FALSE)
  outputOptions(output, "compare_deltas_ui",   suspendWhenHidden = FALSE)
  outputOptions(output, "compare_insight_ui",  suspendWhenHidden = FALSE)
  outputOptions(output, "top_states_list",     suspendWhenHidden = FALSE)
  outputOptions(output, "aqi_cards_ui",        suspendWhenHidden = FALSE)
  outputOptions(output, "anomaly_warning",     suspendWhenHidden = FALSE)
  outputOptions(output, "stats_2x2",           suspendWhenHidden = FALSE)
  outputOptions(output, "dist_bars",           suspendWhenHidden = FALSE)
  outputOptions(output, "status_box",          suspendWhenHidden = FALSE)
  outputOptions(output, "sidebar_stats_ui",    suspendWhenHidden = FALSE)
  outputOptions(output, "sidebar_insights_ui", suspendWhenHidden = FALSE)
  # Plotly charts in hidden panels (Trends + Pollution tabs)
  outputOptions(output, "top_states_chart",    suspendWhenHidden = FALSE)
  outputOptions(output, "national_chart",      suspendWhenHidden = FALSE)
  outputOptions(output, "aqi_bar",             suspendWhenHidden = FALSE)
  outputOptions(output, "trend_chart_main",    suspendWhenHidden = FALSE)

  # ── AQI timestamp display ─────────────────────────────────────────────────
  output$aqi_timestamp_ui <- renderUI({
    ts <- aqi_fetched_time()
    if(is.null(ts)) return(tags$div(class="lc-aqi-ts", "Fetching data..."))
    tags$div(class="lc-aqi-ts", paste0("Last updated: ", ts))
  })

  # ── Search hint (shown below Apply button) ────────────────────────────────
  output$search_hint_ui <- renderUI({
    q <- search_query()
    if(nchar(q) < 2) return(tags$div(class="lc-search-result-hint", style="opacity:0;", "\u00a0"))
    hits <- india_crime[grepl(q, tolower(india_crime$state)), ]
    if(nrow(hits) == 0)
      return(tags$div(class="lc-search-result-hint", style="color:#bb1b21;", paste0('No match for "', q, '"')))
    tags$div(class="lc-search-result-hint",
             paste0("\u2192 ", nrow(hits), " match(es): ", paste(hits$state, collapse=", ")))
  })

  outputOptions(output, "aqi_timestamp_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "search_hint_ui",   suspendWhenHidden = FALSE)
  
  # ── DATASET TABLE ────────────────────────────────────────────
  output$dataset_table_full <- renderDT({
    # BUG 1 FIX: Was reading a missing external JSON file. Now uses the in-memory india_crime data frame.
    display_df <- india_crime %>%
      select(State=state, `Crimes 2020`=crimes_2020, `Crimes 2021`=crimes_2021,
             `Crimes 2022`=crimes_2022, `Pop (Lakh)`=population_lakhs,
             `Rate/Lakh`=crime_rate_2022, `Chargesheeting %`=chargesheeting_rate,
             Trend=trend)
             
    datatable(display_df,
              options = list(
                pageLength  = 15,
                scrollX     = TRUE,
                dom         = "frtip",
                columnDefs  = list(list(className="dt-center", targets=1:7)),
                initComplete = JS("function(settings, json) {
                  $(this.api().table().header()).css({'font-size':'12px','font-family':'Inter,sans-serif','color':'#586064','text-transform':'uppercase','letter-spacing':'0.5px'});
                }")
              ),
              rownames   = FALSE,
              class      = "stripe hover compact",
              selection  = "none") %>%
      formatStyle("Trend",
                  color = styleEqual(c("increasing","decreasing"), c("#bb1b21","#006d41")),
                  fontWeight = "700") %>%
      formatStyle("Rate/Lakh",
                  background = styleColorBar(india_crime$crime_rate_2022, "rgba(0,97,164,0.12)"),
                  backgroundSize = "100% 80%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center") %>%
      formatRound(columns = c("Rate/Lakh","Chargesheeting %"), digits = 1) %>%
      formatCurrency(columns = c("Crimes 2020","Crimes 2021","Crimes 2022"),
                     currency = "", interval = 3, mark = ",", digits = 0)
  }, server = FALSE)
  
  outputOptions(output, "dataset_table_full", suspendWhenHidden = FALSE)
}

shinyApp(ui=ui, server=server)