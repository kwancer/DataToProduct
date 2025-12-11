# app.R ---------------------------------------------------------------

library(shiny)
library(leaflet)
library(jsonlite)
library(later)

# Load helpers and main UI/server definitions
source("R/ui_helpers.R")
source("R/model_helpers.R")
source("R/ui_main.R")
source("R/server_main.R")

# Launch the app
shinyApp(ui = ui_main, server = server_main)
