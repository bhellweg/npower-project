library(readxl)
library(tidycensus)
library(tidyverse)
library(ggplot2)
library(shiny)
library(readxl)
library(tidycensus)
library(acs)
library(tidyverse)
library(ggplot2)
library(shiny)
library(leaflet)
library(stringr)
library(sf)
library(leaflet.extras)

setwd("~/GitHub/npower-project")

source('preset-npower.R',local = T)
source('ui.R',local = T)
source('server.R',local = T)


shinyApp(ui = ui, server = server)
