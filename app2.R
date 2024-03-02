
library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)
library(dplyr)

site_coords <- site_coords
site_spp_list <- readRDS("~/knight inlet eDNA/shinyapp/App-1/data/knight-site-spp-tables.rds")
full_spp_list


# Assuming site_coords and full_spp_list are preloaded
# site_coords <- data.frame(...)
# full_spp_list <- list(...)

ui <- dashboardPage(
  dashboardHeader(title = "Site Observations"),
  dashboardSidebar(disable = TRUE), # Disable sidebar if not needed
  dashboardBody(
    fluidRow(
      box(title = "Map", status = "primary", solidHeader = TRUE,
          leafletOutput("map", width = "100%", height = "600px"), width = 8),
      box(title = "Species List", status = "warning", solidHeader = TRUE,
          DTOutput("speciesTable", width = "100%"), width = 4)
    )
  )
)

server <- function(input, output, session) {
  # Render the Leaflet map
  output$map <- renderLeaflet({
    leaflet(data = site_coords) %>%
      addTiles() %>%
      addMarkers(~long, ~lat, popup = ~site, group = "sites", layerId = ~site)
  })
  
  # Initialize species table as empty, allowing HTML content
  output$speciesTable <- renderDT({
    datatable(data.frame(Species = character(0)), options = list(pageLength = 5, scrollX = TRUE), escape = FALSE)
  }, server = FALSE)
  
  # Update table based on map click
  observeEvent(input$map_marker_click, {
    clicked_site <- input$map_marker_click$id
    
    if (!is.null(clicked_site) && clicked_site %in% names(full_spp_list)) {
      species_list <- full_spp_list[[clicked_site]]
      
      # Assuming species_list contains strings formatted as "<i>Scientific name</i> - Common Name"
      species_data <- data.frame(Species = species_list, stringsAsFactors = FALSE)
      
      # Render the species table with HTML formatting for scientific names
      output$speciesTable <- renderDT({
        datatable(species_data, options = list(pageLength = 5, scrollX = TRUE), escape = FALSE)
      }, server = FALSE)
    }
  })
}


shinyApp(ui = ui, server = server)

