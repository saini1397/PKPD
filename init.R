# List All Libraries needed
my_packages <- c("usethis", "devtools", "testthat", "labelled", "highcharter", "htmlwidgets", "expss", "purrr", "DT", "testthat", "shiny", "tidyverse", "sqldf", "leaflet", "shinydashboard", "plotly", "shinythemes", "shinyBS", "DT", "shinycssloaders", "shinyjs", "corrplot", "shinyWidgets", "stringr", "foreign")

install_if_missing <- function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p, dependencies = TRUE)
  }
}

invisible(sapply(my_packages, install_if_missing))