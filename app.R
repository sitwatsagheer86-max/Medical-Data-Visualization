options(repos = c(CRAN = "https://cloud.r-project.org"))

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(
  shiny, shinydashboard, plotly, DT,
  dplyr, tidyr, ggplot2,
  readr, janitor, stringr,
  RColorBrewer
)

source("hospital_data_cleaning.R")
source("hospital_dashboard.R")
shinyApp(ui = ui, server = server)