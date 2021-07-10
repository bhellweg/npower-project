library(htmltools)
library(readxl)
library(tidycensus)
library(acs)
library(tidyverse)
library(ggplot2)
library(shiny)
library(ggmap)
library(leaflet)
library(tigris)
library(stringr)
library(sf)
library(leaflet.extras)

ui <- function(req){
  fluidPage(
  
  # Application title
  verticalLayout(
   hr(),
   column(12,div(img(src = "npower-logo.png",
           height = 50)),
   h2(strong("NPower Maryland Program Enrollees"))),
   column(8,p(h4("This webpage provides a snapshot into NPower Maryland's alumni body and helps showcase ",
   "the equitable recruitment of NPower students throughout Maryland. The map below allows you to ",
   "see how their places of residence compare to Maryland as a whole in terms of socioeconomic and ",
   "demographic characteristics, and the charts below provide detail into the status of the program ",
   "over time."))),
   hr(style = "border-top: 1px solid #fdb927;"),
    # Show a plot of the generated distribution
    div(
      fluidRow(
      column(9,leafletOutput("myMap",height = 600)),
      column(3,uiOutput("geography"),
      uiOutput("demographic"),
      uiOutput("year"),
      uiOutput("race"),
      uiOutput("gender"))
      )),
   hr(style = "border-top: 1px solid #fdb927;"),
   column(8,p(h3(strong("Participant Overview: "))),
              p(h4("The following charts provide a basic overview of the demographics and city of ",
              "residence for NPower Maryland participants over time. The three pie charts show ",
              "the demographics of NPower Maryland participants since program inception, while ",
              "the bar chart below shows the number of participants over time, broken down by ",
              "class topic."))),
  # Sidebar with a slider input for number of bins 
  hr(style = "border-top: 1px solid #FFFFFF;"),
  fluidRow(
    column(8,plotOutput("plot4")),
    column(4,uiOutput("gender1"),
           uiOutput("race1"),
           uiOutput("county1"),
           uiOutput("year1"))
  ),
    fluidRow(
    column(3,plotOutput("plot1")),
    column(3,plotOutput("plot2")),
    column(3,plotOutput("plot3"))
  ),

  hr(style = "border-top: 1px solid #fdb927;"),
  p(h5("Produced by for NPower Maryland by Brendan Hellweg on 5/27/2020. Draft. ",
       "Note: all participant addresses have been rounded to the 100 block to preserve privacy."))
  )
)}
