# ============================================================
# GeoInsight — Setup & Run Script
# Run this file ONCE to install all packages, then run app.R
# ============================================================

# Step 1: Install all required packages
packages <- c(
  "shiny",
  "shinydashboard",
  "leaflet",
  "plotly",
  "DT",
  "httr",
  "jsonlite",
  "dplyr"
)

cat("Installing required packages...\n")
for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat(paste0("Installing: ", pkg, "\n"))
    install.packages(pkg, dependencies = TRUE)
  } else {
    cat(paste0("Already installed: ", pkg, "\n"))
  }
}

cat("\n✅ All packages ready!\n")
cat("Now run the app with:\n")
cat('  shiny::runApp("app.R")\n')
cat("Or open app.R in RStudio and click Run App\n")
