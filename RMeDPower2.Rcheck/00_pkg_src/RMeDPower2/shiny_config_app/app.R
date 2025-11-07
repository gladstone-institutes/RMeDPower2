# RMeDPower2 Configuration Generator
# A Shiny app to help users create JSON configuration files for RMeDPower2 classes

# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(jsonlite)

# Source UI and Server
source("ui.R")
source("server.R")

# Run the app
shinyApp(ui = ui, server = server)