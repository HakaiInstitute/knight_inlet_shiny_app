
# -------------------------------------------------------------------------
# PRODUCTION VERSION
# for demonstration purposes only
# -------------------------------------------------------------------------



# Calvert Samples Shiny Practice -------------------------------------------


library(shiny)
library(dplyr)
library(leaflet)
library(reactable)
library(stringr)
library(MetBrewer)
library(shinyalert)


# load data ---------------------------------------------------------------
source(here::here("00_initialize_data.R"))

site_coords <- site_coords 


# Define server logic required to draw a histogram
function(input, output, session) {  

# render leaflet ----------------------------------------------------------
  
  output$knightmap <- renderLeaflet({
    
    leaflet(
      options = leaflet::leafletOptions()) %>% 
      #setView(lng = -128.15, lat = 51.75, zoom = 8) %>%
      addProviderTiles(providers$CartoDB.Voyager) %>%
      #addMapPane("yellow outlines", zIndex=600) %>% 
      #addMapPane("depth points",zIndex=500) %>%
      #addMapPane("habitat points",zIndex=450) %>%
      addCircleMarkers(data = site_coords,
                       lat = ~lat,
                       lng = ~long,
                       weight = 0,
                       radius = 5,
                       layerId = ~site,
                       fillColor = "darkgreen",
                       color = "black",
                       popup = paste0("<b>Site: </b>", site_coords$site," <br>
                                    <b>Survey Method: </b>", site_coords$survey, "<br>
                                    <b>Site Name: </b>", site_coords$area, "<br>
                                    <b>Max Depth: </b>", site_coords$max_depth,"m","<br>
                                    <b>Survey Objective: </b>", site_coords$objective),
                       fillOpacity = .8,
                       group = "all_sites")#,
    #clusterOptions = markerClusterOptions((freezeAtZoom = 1))) 
    
    
    
  }) # end render leaflet
  
  
  
  
}  