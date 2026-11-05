# dev_setup.R — run once in Positron to set up the full local dev environment.
# After this completes, open app.R and click Run (or shiny::runApp()).

# Step 1: CRAN packages (includes gmed's dependencies)
pkgs <- c(
  "shiny", "bslib", "shinyjs", "future", "promises", "shinycssloaders",
  "DT", "reactable",
  "dplyr", "tidyr", "purrr", "lubridate", "rlang", "data.table", "tibble",
  "plotly", "ggplot2", "htmltools",
  "httr", "jsonlite", "readr",
  "pins", "memoise",
  "config",
  "REDCapR"
)

missing <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if (length(missing) > 0) {
  message("Installing ", length(missing), " CRAN packages...")
  install.packages(missing)
} else {
  message("All CRAN packages already installed.")
}

# Step 2: GitHub packages. Installed from GitHub (not a local source build)
# so rsconnect::writeManifest() records a GitHub source it can deploy from —
# a local build of gmed once broke writeManifest for every app sharing the
# system library (see imslu.ind.dash/CLAUDE.md).
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
message("Installing roundsui, gmed and amiontools from GitHub...")
renv::install(c("fbuckhold3/roundsui", "fbuckhold3/gmed", "fbuckhold3/amiontools"))

# Step 3: refresh the Posit Connect manifest after package changes.
# rsconnect::writeManifest()

message("\nSetup complete. Run shiny::runApp() or open app.R and click Run.")
