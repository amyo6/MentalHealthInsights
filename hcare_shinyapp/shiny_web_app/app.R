#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(leaflet)
library(corrplot)
library(plotly)
library(tidyverse)

#Sources
source("server.R")
source("ui.R")


# Run the application 
shinyApp(ui = ui, server = server)
