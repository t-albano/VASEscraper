library(shiny)
library(shinydashboard)

library(data.table)
library(tidyverse)
library(dplyr)
library(ggiraph)

vase <- fread("VASE_cleandata.csv")
geo <- fread("GEO_cleandata.csv")

ui <- dashboardPage(
  dashboardHeader(title = "State VASE Results"),
  dashboardSidebar(),
  dashboardBody()
)

server <- function(input, output) { }

shinyApp(ui, server)