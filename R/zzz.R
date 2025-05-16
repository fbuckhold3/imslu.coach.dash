#' @importFrom shiny addResourcePath
.onLoad <- function(libname, pkgname) {
  # Make sure shiny is loaded or load it
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("The shiny package is required for this package to work")
  }

  # This runs when the package is loaded
  # Set up resource paths only, avoid API calls here
  img_path <- system.file("www", package = "imres")
  if (dir.exists(img_path)) {
    message("Found imres www directory at: ", img_path)
    shiny::addResourcePath("imres-images", img_path)
  }
}


# Execute the function manually if not being loaded as a package
if (!isNamespaceLoaded("imres")) {
  tryCatch({
    img_path <- system.file("www", package = "imres")
    if (dir.exists(img_path)) {
      message("Manually setting up imres-images resource path")
      shiny::addResourcePath("imres-images", img_path)
    }
  }, error = function(e) {
    message("Error setting up resource path: ", e$message)
  })
}
