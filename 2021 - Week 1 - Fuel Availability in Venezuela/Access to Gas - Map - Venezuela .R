library(tidyverse)
library(googlesheets4)
library(scales)
library(lubridate)


#### Data Load ####
 
fuel <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1Mp6wHtSZt_BXumcOUbMqKG3GH96sPL9ZvND1ou5iB28/edit#gid=1223982608")


#### Data Wrangling - Quick Insights #### 
fuel_clean <- fuel %>% 
  mutate(observed_time = as.Date(observed_time),
         month = month(ymd(observed_time), label = TRUE, abbr = FALSE),
         Open = case_when(Status == "Open" ~ 1,
                          TRUE ~0))

fuel_round_coordinates <- fuel %>% 
  mutate(observed_time = as.Date(observed_time),
         month = month(ymd(observed_time), label = TRUE, abbr = FALSE),
         Open = case_when(Status == "Open" ~ 1,
                          TRUE ~0),
         Latitude = signif(Latitude, digits = 3),
         Longitude = signif(Longitude, digits = 3),
         Location = str_c(Latitude,Longitude))           


# Data used for visualization
pct_open_coord <- fuel_round_coordinates %>% 
  group_by(Longitude, Latitude) %>% 
  summarise(total_count = n(),
            pct_open = sum(Open)/total_count) %>%  
  filter(total_count > 30) %>% # At least 30 surveys received
  mutate(fuel_availability = case_when(pct_open < 0.3 ~ "Low",
                                       pct_open < 0.6 ~ "Medium",
                                       TRUE ~ "High")) %>% 
  ungroup() 

# Not great insights
# Type of Area - Not great insights
fuel_clean %>% 
  group_by(geography, month) %>% 
  summarise(total_count = n(),
            pct_open = sum(Open)/total_count)

  
####  Map Visualization ####
library(leaflet)
library(htmlwidgets)
library(htmltools)

av_color <- colorNumeric("RdYlBu", pct_open_coord$pct_open, n = 3)

gas_vzla <- leaflet(options = leafletOptions(zoomControl = FALSE), data = pct_open_coord) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addCircleMarkers(
    color = ~av_color(pct_open),
    opacity = 0.5,
    fillOpacity = 0.5) 


gas_vzla <- gas_vzla %>% 
  addLegend(title =  "% Open",
            position = "bottomright",
            pal = av_color,
            values = pct_open_coord$pct_open) 

rr <- tags$div(
  HTML("<b> Gas Availability in Venezuela </b> <br> Data: Aug - Oct 20'")
)  

map_leaflet <- gas_vzla %>% 
  addControl(rr, position = "topleft")
  
map_leaflet

