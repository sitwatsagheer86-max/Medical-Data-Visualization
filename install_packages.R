# install_packages.R

# List of required packages
packages <- c("shiny", "shinydashboard", "plotly", "DT", 
              "dplyr", "tidyr", "ggplot2", "readr")

# Install missing packages
install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# Install all packages
invisible(lapply(packages, install_if_missing))

cat("All required packages installed successfully!\n")