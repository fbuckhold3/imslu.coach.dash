# dev_setup.R — run once in Positron to set up the full local dev environment.
# After this completes, open app.R and click Run (or shiny::runApp()).

# Step 1: CRAN packages (includes gmed's dependencies)
pkgs <- c(
  "shiny", "shinydashboard", "bslib", "shinyjs",
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

# Step 2: Install gmed from local source (picks up your local edits)
message("Installing gmed from local source...")
install.packages("../gmed", repos = NULL, type = "source")
message("\nSetup complete. Run shiny::runApp() or open app.R and click Run.")
