# Install dependencies for Llama Throughput Lab Shiny App

# Required packages
required_packages <- c(
  "shiny",
  "bslib",
  "DT",
  "ggplot2",
  "jsonlite",
  "httr",
  "future",
  "future.apply",
  "promises"
)

# Optional packages (for better functionality)
optional_packages <- c(
  "processx"  # Better process management
)

# Install missing required packages
install_if_missing <- function(packages, optional = FALSE) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message("Installing ", pkg, "...")
      tryCatch({
        install.packages(pkg)
      }, error = function(e) {
        if (optional) {
          message("Optional package ", pkg, " could not be installed: ", e$message)
        } else {
          stop("Required package ", pkg, " could not be installed: ", e$message)
        }
      })
    } else {
      message(pkg, " is already installed")
    }
  }
}

message("Installing required packages...")
install_if_missing(required_packages)

message("\nInstalling optional packages...")
install_if_missing(optional_packages, optional = TRUE)

message("\nAll dependencies installed!")
message("Run the app with: shiny::runApp('shiny')")
