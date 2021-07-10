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

api.key.install("e598bc95dd62b82fa87e928529f2f6072c7dfa3e")

npower <- read_excel("npower_map.xlsx")

acsvars <- load_variables(2019,"acs5")

topercent <- function(DATA,DATASET){
  st_join(DATA,DATASET,by = GEOID) %>%
    filter(NAME.x == NAME.y) %>% 
    mutate(estimate = ifelse((estimate.x/estimate.y > 1)|
                               (estimate.x/estimate.y == Inf),
                             NA,round(estimate.x/estimate.y,3)),
           moe = moe.x/moe.y) %>%
    select(-c(4:10)) %>%
    rename(GEOID = GEOID.x,
           NAME = NAME.x,
           variable = variable.x)
}

  #CENSUS TRACT POLYGONS
  
  md_pop <- get_acs(geography = "tract", 
                       variables = "B01001_001", 
                       state = "MD",
                       year = 2019,
                       geometry = TRUE) 
  
  md_housing <- get_acs(geography = "tract", 
                    variables = "B25077_001", 
                    state = "MD",
                    year = 2019,
                    geometry = TRUE) 
  
  md_income <- get_acs(geography = "tract", 
                        variables = "B19013_001", 
                        state = "MD",
                       year = 2019,
                        geometry = TRUE) 
  
  md_pov <- get_acs(geography = "tract", 
                      variables = "B17001_002", 
                      state = "MD",
                      year = 2019,
                      geometry = TRUE) %>% 
    topercent(.,md_pop)

  
  md_race <- get_acs(geography = "tract", 
                    variables = "B01001B_001", 
                    state = "MD",
                    year = 2019,
                    geometry = TRUE) %>%
    topercent(.,md_pop)
  
  md_latinx <- get_acs(geography = "tract", 
                     variables = "B01001I_001", 
                     state = "MD",
                     year = 2019,
                     geometry = TRUE) %>%
    topercent(.,md_pop)
  
  #ZIP CODE LEVEL DATA
  
  zmd_pop <- get_acs(geography = "zcta", 
                    variables = "B01001_001", 
                    state = "MD",
                    year = 2019,
                    geometry = TRUE) 
  
  zmd_housing <- get_acs(geography = "zcta", 
                        variables = "B25077_001", 
                        state = "MD",
                        year = 2019,
                        geometry = TRUE) 
  
  zmd_income <- get_acs(geography = "zcta", 
                       variables = "B19013_001", 
                       state = "MD",
                       year = 2019,
                       geometry = TRUE) 
  
  zmd_pov <- get_acs(geography = "zcta", 
                    variables = "B17001_002", 
                    state = "MD",
                    year = 2019,
                    geometry = TRUE) %>%
    topercent(.,zmd_pop)
  
  zmd_race <- get_acs(geography = "zcta", 
                     variables = "B01001B_001", 
                     state = "MD",
                     year = 2019,
                     geometry = TRUE) %>%
    topercent(.,zmd_pop)
  
  zmd_latinx <- get_acs(geography = "zcta", 
                       variables = "B01001I_001", 
                       state = "MD",
                       year = 2019,
                       geometry = TRUE) %>%
    topercent(.,zmd_pop)
  
  #COUNTY LEVEL POLYGONS
  
  cmd_pop <- get_acs(geography = "county", 
                    variables = "B01001_001", 
                    state = "MD",
                    year = 2019,
                    geometry = TRUE) 
  
  cmd_housing <- get_acs(geography = "county", 
                        variables = "B25077_001", 
                        state = "MD",
                        year = 2019,
                        geometry = TRUE) 
  
  cmd_income <- get_acs(geography = "county", 
                       variables = "B19013_001", 
                       state = "MD",
                       year = 2019,
                       geometry = TRUE) 
  
  cmd_pov <- get_acs(geography = "county", 
                    variables = "B17001_002", 
                    state = "MD",
                    year = 2019,
                    geometry = TRUE) %>%
    topercent(.,cmd_pop)
  
  cmd_race <- get_acs(geography = "county", 
                     variables = "B01001B_001", 
                     state = "MD",
                     year = 2019,
                     geometry = TRUE) %>%
    topercent(.,cmd_pop)
  
  cmd_latinx <- get_acs(geography = "county", 
                       variables = "B01001I_001", 
                       state = "MD",
                       year = 2019,
                       geometry = TRUE) %>%
    topercent(.,cmd_pop)

