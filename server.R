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
library(stats)
library(leaflet.extras)

# Define server logic 
server <- function(input, output, session) {

  #Maps of NPower Graduates
  
  #BASIC SETTINGS
  opacity <- 0.5
  rad     <- 3
  outercolor <- "white"
  innercolor <- "black"
  
  #DROPDOWN FOR GEOGRAPHY TYPE
  output$geography <- renderUI({
    
    choices <- c("Census Tract", "Zip Code", "County")
    selectInput(inputId = "geography", label = "Select Geographic Level", 
                choices = choices, selected = "Census Tract")
    
  })
  
  #DROPDOWN FOR DEMOGRAPHIC LAYER REPRESENTED
  output$demographic <- renderUI({
    
    choices <- c("Population", "Median Home Value","Poverty Rate",
                 "Percent Black or African American","Percent Hispanic or Latino")
    selectInput(inputId = "demographic", label = "Select Overlay", 
                choices = choices, selected = "Poverty Rate")
    
  })
  
  #DROPDOWN FOR ENROLLEE YEAR
  output$year <- renderUI({
    
    choices <- c("2016", "2017", "2018","2019","2020","2021","All")
    selectInput(inputId = "year", label = "Filter to Year", 
                choices = choices, selected = "All")
    
  })
  
  #DROPDOWN FOR ENROLLEE RACE/ETHNICITY
  output$race <- renderUI({
    
    choices <- c("African American/Black", "Asian", "Caucasian","Hispanic/Latinx","Multiracial",
                 "Hawaiian/Pacific Islander","Other","Prefer not to answer","All")
    selectInput(inputId = "race", label = "Filter to Race/Ethnicity", 
                choices = choices, selected = "All")
    
  })
  
  #DROPDOWN FOR ENROLLEE GENDER
  output$gender <- renderUI({
    
    choices <- c("Female", "Male", "Prefer not to answer","All")
    selectInput(inputId = "gender", label = "Filter to Gender Identity", 
                choices = choices, selected = "All")
    
  })
  
  #BASIC MAP SETUP. THIS IS THEN MANIPULATED BY EACH DROPDOWN 
  output$myMap <- renderLeaflet({
    leaflet(width = "100%") %>%
      addMapPane("polygons",zIndex = 410) %>%
      addMapPane("points",zIndex = 440) %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      addCircleMarkers(lng = npower$lon,lat = npower$lat, popup = npower$Year,
                       fillOpacity = 1,
                       radius = rad,
                       weight = 1,
                       options = leafletOptions(pane = "points"),
                       color = outercolor,
                       fillColor = innercolor)
  })
  
  #MODIFIES THE LAYER REPRESENTED ON THE MAP BASED ON THE DEMOGRAPHIC BUTTON
  observeEvent(input$demographic, {
    
    DEMO = input$demographic
    GEOG = input$geography
    
    if(GEOG == 'Census Tract'){
      if(DEMO == 'Population'){
        md = md_pop
      }else{if(DEMO == 'Median Home Value'){
        md <- md_housing
      }else{if(DEMO == "Percent Black or African American"){
        md <- md_race
      }else{if(DEMO == "Percent Hispanic or Latino"){
        md <- md_latinx
      }else{
        md <- md_pov
      }}}}}else{if(GEOG == 'Zip Code'){
        if(DEMO == 'Population'){
          md = zmd_pop
        }else{if(DEMO == 'Median Home Value'){
          md <- zmd_housing
        }else{if(DEMO == "Percent Black or African American"){
          md <- zmd_race
        }else{if(DEMO == "Percent Hispanic or Latino"){
          md <- zmd_latinx
        }else{
          md <- zmd_pov
        }}}}}else{if(GEOG == 'County'){
          if(DEMO == 'Population'){
            md = cmd_pop
          }else{if(DEMO == 'Median Home Value'){
            md <- cmd_housing
          }else{if(DEMO == "Percent Black or African American"){
            md <- cmd_race
          }else{if(DEMO == "Percent Hispanic or Latino"){
            md <- cmd_latinx
          }else{
            md <- cmd_pov
          }}}}}
        }}
    
    pal <- colorNumeric(palette = "viridis", domain = md$estimate)
    
    leafletProxy("myMap") %>%
      clearShapes() %>% 
      clearControls() %>%
      addPolygons(data = md %>%
                    st_transform(crs = "+init=epsg:4326"),
                  popup = ~ paste("Geography: ", NAME,". Value: ",estimate),
                  stroke = FALSE,
                  smoothFactor = 1,
                  weight = 1,
                  fillOpacity = 0.5,
                  color = ~ pal(estimate),
                  options = leafletOptions(pane = "polygons")) %>%
      addLegend("bottomright", 
                pal = pal, 
                values = md$estimate,
                title = "Selected Overlay Values",
                opacity = opacity)
  })
  
  #MODIFIES THE LAYER REPRESENTED BY THE GEOGRAPHIC LAYER SELECTED
  observeEvent(input$geography, {
    
    DEMO = input$demographic
    GEOG = input$geography
    
    if(GEOG == 'Census Tract'){
      if(DEMO == 'Population'){
        md = md_pop
      }else{if(DEMO == 'Median Home Value'){
        md <- md_housing
      }else{if(DEMO == "Percent Black or African American"){
        md <- md_race
      }else{if(DEMO == "Percent Hispanic or Latino"){
        md <- md_latinx
      }else{
        md <- md_pov
      }}}}}else{if(GEOG == 'Zip Code'){
        if(DEMO == 'Population'){
          md = zmd_pop
        }else{if(DEMO == 'Median Home Value'){
          md <- zmd_housing
        }else{if(DEMO == "Percent Black or African American"){
          md <- zmd_race
        }else{if(DEMO == "Percent Hispanic or Latino"){
          md <- zmd_latinx
        }else{
          md <- zmd_pov
        }}}}}else{if(GEOG == 'County'){
          if(DEMO == 'Population'){
            md = cmd_pop
          }else{if(DEMO == 'Median Home Value'){
            md <- cmd_housing
          }else{if(DEMO == "Percent Black or African American"){
            md <- cmd_race
          }else{if(DEMO == "Percent Hispanic or Latino"){
            md <- cmd_latinx
          }else{
            md <- cmd_pov
          }}}}}
        }}
    
    pal <- colorNumeric(palette = "viridis", domain = md$estimate)
    
    leafletProxy("myMap") %>%
      clearControls() %>%
      clearShapes() %>% 
      addPolygons(data = md %>%
                    st_transform(crs = "+init=epsg:4326"),
                  popup = ~ paste("Geography: ", NAME,". Value: ",estimate),
                  stroke = FALSE,
                  smoothFactor = 1,
                  weight = 1,
                  fillOpacity = 0.5,
                  color = ~ pal(estimate),
                  options = leafletOptions(pane = "polygons")) %>%
      addLegend("bottomright", 
                pal = pal, 
                values = md$estimate,
                title = "Selected Overlay Values",
                opacity = opacity)
  })
  
  #FILTERS THE ENROLLES REPRESENTED ON THE MAP BY YEAR
  observeEvent(input$year, {
    YEAR = input$year
    GEND = input$gender
    RACE = input$race
    
    if(GEND != 'All'){
      npower <- npower %>% filter(`Gender Identity` == input$gender)}
    if(RACE != 'All'){
      npower <- npower %>% filter(`Race/Ethnicity` == input$race)}
    ifelse(YEAR == "All",{
      leafletProxy("myMap") %>%
        addCircleMarkers(lng = npower$lon,lat = npower$lat, 
                         popup = npower$Year,
                         fillOpacity = 1,
                         radius = rad,
                         weight = 1,
                         options = leafletOptions(pane = "points"),
                         color = outercolor,
                         fillColor = innercolor
        )
    },{filtyear <- npower %>% filter(Year == YEAR)
    leafletProxy("myMap") %>%
      clearMarkers() %>%
      addCircleMarkers(lng = filtyear$lon,lat = filtyear$lat, 
                       fillOpacity = 1,
                       radius = rad,
                       weight = 1,
                       options = leafletOptions(pane = "points"),
                       color = outercolor,
                       fillColor = innercolor)})
  })
  
  #FILTERS THE ENROLLES REPRESENTED ON THE MAP BY RACE/ETHNICITY  
  observeEvent(input$race, {
    YEAR = input$year
    GEND = input$gender
    RACE = input$race
    
    if(GEND != 'All'){
      npower <- npower %>% filter(`Gender Identity` == GEND)}
    if(YEAR != 'All'){
      npower <- npower %>% filter(Year == YEAR)}
    ifelse(RACE == "All",{
      leafletProxy("myMap") %>%
        addCircleMarkers(lng = npower$lon,lat = npower$lat, 
                         fillOpacity = 1,
                         radius = rad,
                         weight = 1,
                         options = leafletOptions(pane = "points"),
                         color = outercolor,
                         fillColor = innercolor
        )
    },{filtrace <- npower %>% filter(`Race/Ethnicity` == RACE)
    leafletProxy("myMap") %>%
      clearMarkers() %>%
      addCircleMarkers(lng = filtrace$lon,lat = filtrace$lat, 
                       fillOpacity = 1,
                       radius = rad,
                       weight = 1,
                       options = leafletOptions(pane = "points"),
                       color = outercolor,
                       fillColor = innercolor)})
  })
  
  #FILTERS THE ENROLLES REPRESENTED ON THE MAP BY GENDER
  observeEvent(input$gender, {
    YEAR = input$year
    GEND = input$gender
    RACE = input$race
    
    if(YEAR != 'All'){
      npower <- npower %>% filter(Year == input$year)}
    if(RACE != 'All'){
      npower <- npower %>% filter(`Race/Ethnicity` == input$race)}
    ifelse(GEND == "All",
           {leafletProxy("myMap") %>%
               addCircleMarkers(lng = npower$lon,lat = npower$lat, 
                                fillOpacity = 1,
                                radius = rad,
                                weight = 1,
                                options = leafletOptions(pane = "points"),
                                color = outercolor,
                                fillColor = innercolor
               )},{filtgend <- npower %>% filter(`Gender Identity` == GEND)
               leafletProxy("myMap") %>%
                 clearMarkers() %>%
                 addCircleMarkers(lng = filtgend$lon,lat = filtgend$lat, 
                                  popup = filtgend$Year,
                                  fillOpacity = 1,
                                  radius = rad,
                                  weight = 1,
                                  options = leafletOptions(pane = "points"),
                                  color = outercolor,
                                  fillColor = innercolor)})
  })

  #HISTOGRAMS AND PIE CHARTS
    
    # Visualizations of NPower Graduates
    
    output$gender1 <- renderUI({
      
      choices <- c("Female", 
                   "Male", 
                   "Prefer not to answer",
                   "All")
      selectInput(inputId = "gender1", label = "Filter to Gender Identity", 
                  choices = choices, selected = "All")
      
    })
    
    output$county1 <- renderUI({
      
      choices <- c('All',
                   'Alexandria',
                   'Anne Arundel',
                   'Baltimore City',
                   'Baltimore County',
                   'Charles',
                   'District of Columbia',
                   'Fairfax',
                   'Harford',
                   'Howard',
                   'Montgomery',
                   'Prince George\'s',
                   'Washington')
      selectInput(inputId = "county1", label = "Filter to County", 
                  choices = choices, selected = "All")
      
    })
    
    output$race1 <- renderUI({
      
      choices <- c("African American/Black", 
                   "Asian", 
                   "Caucasian",
                   "Hispanic/Latinx",
                   "Multiracial",
                   "Hawaiian/Pacific Islander",
                   "Other",
                   "Prefer not to answer",
                   "All")
      selectInput(inputId = "race1", label = "Filter to Race/Ethnicity", 
                  choices = choices, selected = "All")
      
    })
    
    output$year1 <- renderUI({
      
      choices <- c(2016:2021,"All")
      selectInput(inputId = "year1", label = "Filter to Enrollment Year", 
                  choices = choices, selected = "All")
      
    })
    
    #PLOT OF RACE/ETHNICITY
    output$plot1 <- renderPlot({
      
      GEND1 = input$gender1
      COUNT1 = input$county1
      RACE1 = input$race1
      YEAR1 = input$year1
      
      if(RACE1 != 'All'){
        npower <- npower %>% filter(`Race/Ethnicity` == RACE1)}
      if(GEND1 != 'All'){
        npower <- npower %>% filter(`Gender Identity` == GEND1)}
      if(COUNT1 != 'All'){
        npower <- npower %>% filter(County == COUNT1)}
      if(YEAR1 != 'All'){
        npower <- npower %>% filter(Year == YEAR1)}
      if(nrow(npower)>0){
      
      npower %>%
        group_by(`Race/Ethnicity`) %>% 
        summarise(Count = sum(`Contact Count`)) %>% 
        ggplot(.,aes(x = 2,
                     y = Count, 
                     fill = `Race/Ethnicity`)) +
        geom_bar(width = 1,stat = "identity",color = "white") +
        coord_polar("y", start = 0)+
        theme_void()+
        xlim(0.5, 2.5)+ 
        theme(text = element_text(size = 16))  +
        theme(legend.direction = "vertical",legend.position="bottom") +
        scale_fill_brewer(palette="Dark2")}
      
      
    })
    
    #PLOT OF GENDER IDENTITY
    output$plot2 <- renderPlot({
      
      GEND1 = input$gender1
      COUNT1 = input$county1
      RACE1 = input$race1
      YEAR1 = input$year1
      
      if(RACE1 != 'All'){
        npower <- npower %>% filter(`Race/Ethnicity` == RACE1)}
      if(COUNT1 != 'All'){
        npower <- npower %>% filter(County == COUNT1)}
      if(GEND1 != 'All'){
        npower <- npower %>% filter(`Gender Identity` == GEND1)}
      if(YEAR1 != 'All'){
        npower <- npower %>% filter(Year == YEAR1)}
      if(nrow(npower)>0){
        
      npower %>%
        group_by(`Gender Identity`) %>% 
        summarise(Count = sum(`Contact Count`)) %>% 
        ggplot(.,aes(x = 2,
                     y = Count, 
                     fill = `Gender Identity`)) +
        geom_bar(stat = "identity",width = 1,color = "white") +
        coord_polar("y", start = 0) +
        theme_void()+
        xlim(0.5, 2.5)+ 
        theme(legend.direction = "vertical",legend.position="bottom") +
        theme(text = element_text(size = 16))  +
        scale_fill_brewer(palette="Dark2")}
      
    })
    
    #PLOT OF COUNTY OF RESIDENCE
    output$plot3 <- renderPlot({
      
      GEND1 = input$gender1
      COUNT1 = input$county1
      RACE1 = input$race1
      YEAR1 = input$year1
      
      if(GEND1 != 'All'){
        npower <- npower %>% filter(`Gender Identity` == GEND1)}
      if(RACE1 != 'All'){
        npower <- npower %>% filter(`Race/Ethnicity` == RACE1)}
      if(COUNT1 != 'All'){
        npower <- npower %>% filter(County == COUNT1)}
      if(YEAR1 != 'All'){
        npower <- npower %>% filter(Year == YEAR1)}
      if(nrow(npower)>8){
        
      npower %>%
        group_by(`County`) %>% 
        summarise(Count = sum(`Contact Count`)) %>% 
        mutate(County1 = ifelse(Count>8,County,"Other")) %>% 
        group_by(County = County1) %>% 
        summarise(Count = sum(Count)) %>% 
        ggplot(.,aes(x = 2,
                     y = Count, 
                     fill = County)) +
        geom_bar(stat = "identity",width = 1,color = "white") +
        coord_polar("y", start = 0) +
        theme_void()+
        xlim(0.5, 2.5)+ 
        theme(text = element_text(size = 16))  +
        theme(legend.direction = "vertical",legend.position="bottom") +
        scale_fill_brewer(palette="Dark2")}else{
          if(nrow(npower)>0){
          npower %>%
            group_by(`County`) %>% 
            summarise(Count = sum(`Contact Count`)) %>% 
            ggplot(.,aes(x = 2,
                         y = Count, 
                         fill = County)) +
            geom_bar(stat = "identity",width = 1,color = "white") +
            coord_polar("y", start = 0) +
            theme_void()+
            xlim(0.5, 2.5)+ 
            theme(text = element_text(size = 16))  +
            theme(legend.direction = "vertical",legend.position="bottom") +
            scale_fill_brewer(palette="Dark2")
          
        }}
      
    })
    
    #PLOT OF ENROLLMENT YEAR
    output$plot4 <- renderPlot({
      GEND1 = input$gender1
      COUNT1 = input$county1
      RACE1 = input$race1
      
      if(GEND1 != 'All'){
        npower <- npower %>% filter(`Gender Identity` == GEND1)}
      if(RACE1 != 'All'){
        npower <- npower %>% filter(`Race/Ethnicity` == RACE1)}
      if(COUNT1 != 'All'){
        npower <- npower %>% filter(County == COUNT1)}
  
      if(nrow(npower)>0){
      
      npower %>%
        ggplot(.,aes(x = Year,
                     y = `Contact Count`, 
                     fill = Class)) +
        geom_bar(stat = "identity") +
        theme_minimal()+
        ylab("Count") +
        scale_fill_brewer(palette="Dark2") +
        ggtitle("NPower Enrollees by Program and Year") +
        theme(text = element_text(size = 16),
              axis.text = element_text(size = 14))+
        theme(legend.direction = "vertical",legend.position="bottom") }
  
      
    })
    
  }
