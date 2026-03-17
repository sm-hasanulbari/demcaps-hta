# app.R — Entry point
required_pkgs <- c(
  "shiny", "shinydashboard", "shinyWidgets", "shinycssloaders",
  "waiter", "DT", "plotly", "ggplot2", "dplyr", "tidyr",
  "MASS", "truncnorm", "scales", "RColorBrewer", "ggrepel",
  "reshape2", "survival", "nnet", "mice", "lme4"
)

missing_pkgs <- required_pkgs[!required_pkgs %in% installed.packages()[, "Package"]]
if (length(missing_pkgs) > 0) {
  stop("Missing packages: ", paste(missing_pkgs, collapse = ", "))
}

options(shiny.maxRequestSize = 50 * 1024^2)

source("app/ui.R")
source("app/server.R")

shinyApp(ui = ui, server = server)