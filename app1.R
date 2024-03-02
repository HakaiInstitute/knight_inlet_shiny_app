#### Map with informative labels ####

library(shiny)
library(leaflet)
library(RColorBrewer)

# load data ---------------------------------------------------------------

site_coords <- site_coords 

getwd()

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("knightmap", width = "100%", height = "100%"),
)

server <- function(input, output, session) {  
  
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
                                    <b>Survey Objective: </b>", site_coords$objective, "<br>
                                    <b>Total Species: </b>", site_coords$sppr),
                       fillOpacity = .8,
                       group = "all_sites")#,
    #clusterOptions = markerClusterOptions((freezeAtZoom = 1))) 
    
    
    
  }) # end render leaflet
  
} 

shinyApp(ui, server)


# Observe clicks on the map and update the species table
observeEvent(input$map_marker_click, {
  click <- input$map_marker_click
  siteName <- site_coords$site[which(site_coords$lat == click$lat & site_coords$long == click$long)]
  speciesList <- full_spp_list[[siteName]]
  speciesDF <- data.frame(Species = unlist(speciesList))
  
  output$speciesTable <- renderDT({
    datatable(speciesDF, options = list(pageLength = 5))
  })
})
