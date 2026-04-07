# ============================================================
# Geo InSight - One-step startup script
# Installs required packages (if missing) and launches the app
# Usage: Rscript start_project.R
# ============================================================

required_packages <- c(
  "shiny",
  "shinydashboard",
  "leaflet",
  "plotly",
  "DT",
  "httr",
  "jsonlite",
  "dplyr"
)

install_if_missing <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]

  if (length(missing) == 0) {
    cat("All required packages are already installed.\n")
    return(invisible(TRUE))
  }

  cat("Installing missing packages:\n")
  for (pkg in missing) {
    cat(" -", pkg, "\n")
  }

  install.packages(missing, dependencies = TRUE, repos = "https://cloud.r-project.org")
  invisible(TRUE)
}

run_geo_insight <- function() {
  app_file <- "app.R"
  if (!file.exists(app_file)) {
    stop("app.R not found. Run this script from the project root folder.")
  }

  shiny::runApp(app_file, launch.browser = TRUE)
}

cat("Geo InSight startup initiated...\n")
install_if_missing(required_packages)
cat("Starting app...\n")
run_geo_insight()
