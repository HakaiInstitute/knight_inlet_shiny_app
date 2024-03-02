
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(leaflet)
library(DT)
library(dplyr)
library(viridis)

site_coords <- site_coords
site_spp_list <- readRDS("~/knight inlet eDNA/shinyapp/App-1/data/knight-site-spp-tables.rds")

# establish pallete
pal_rich <- colorNumeric(
  palette = "viridis",
  domain = site_coords$sppr)


ui <- dashboardPage(
  dashboardHeader(title = "Biodiveristy in the Gwaxdlala/Nalaxdlala IPCA Marine Refuge", 
                  titleWidth = 600),  # since we have a long title, we need to extend width element in pixels
  dashboardSidebar(disable = TRUE),  # Disable the sidebar
  dashboardBody(
    
    fluidRow(
      box(title = "Map", status = "primary", solidHeader = TRUE,
          leafletOutput("map", width = "100%", height = "600px"), width = 8),
      box(title = "Species Selector", 
          status = "warning",           
          collapsible = T,
          solidHeader = T,
          width = 4,
          ### 1. Species Select Box -----
          pickerInput(
            inputId = "selectcalvertspp",
            label = "Select a species", 
            # choices = full_spp_list,
            choices = split(site_spp_list,  # need to make a full_spp_list for this to work
                            names(site_spp_list)),  # was "full_spp_list"
            multiple = TRUE,
            options = pickerOptions(liveSearch=T,
                                    maxOptions = 1),
            choicesOpt = list(content = site_spp_list),  # was full_spp_list
            
          )),  # Placeholder for species choices
      box(title = "Color Code by Species Richness", status = "warning", solidHeader = TRUE,
          radioGroupButtons(
            inputId = "colorCode",
            label = "Color by:", 
            choices = list("Default" = "default", 
                           "Total Species" = "richness"),
            status = "primary",
            selected = "default",
            individual = TRUE,
            checkIcon = list("yes" = icon("check"))
          ),
          width = 4),
      box(title = "Species List", status = "warning", solidHeader = TRUE,
          DTOutput("speciesTable", width = "100%"), width = 4)
    ),
    
    fluidRow(id = "footer",
             column(2),
             column(8,
                    HTML("<center><p style='size:18px';>
                     App Development by Alex Schmill
                     </p></center>")),
             column(2)
             
    ) # end footer fluidrow
  )
)

server <- function(input, output, session) {
  
  # Populate the species selector dropdown with species names
  # Assuming site_spp_list is a named list with species per site
  all_species <- unique(unlist(site_spp_list))
  
  all_species_formatted <- sapply(all_species, function(name) {
    sprintf("<i>%s</i>", name)
  })
  
  observe({
    updateSelectInput(session, "speciesSelector", choices = all_species_formatted)
    session$sendCustomMessage('formatDropdown', list(id = 'speciesSelector'))
  })
  
  # Render map 
  
  output$map <- renderLeaflet({
    
    leaflet(
      options = leaflet::leafletOptions()) %>%
      addProviderTiles(providers$CartoDB.Voyager) %>%
      addMapPane("depth points",zIndex=500) %>%
      
      
      addCircleMarkers(data = site_coords,
                       lat = ~lat,
                       lng = ~long,
                       weight = 0,
                       radius = 5,
                       layerId = ~site,
                       fillColor = "black",
                       color = "black",
                       popup = paste0("<b>Site: </b>", site_coords$site," <br>
                                    <b>Survey Method: </b>", site_coords$survey, "<br>
                                    <b>Site Name: </b>", site_coords$area, "<br>
                                    <b>Max Depth: </b>", site_coords$max_depth,"m","<br>
                                    <b>Survey Objective: </b>", site_coords$objective, "<br>
                                    <b>Total Species: </b>", site_coords$sppr),
                       fillOpacity = .8,
                       group = "all_sites") %>%
      
      # add and hide richness dots
      addCircleMarkers(data = site_coords,
                       lat = ~lat,
                       lng = ~long,
                       weight = 0,
                       radius = 5,
                       fillColor = ~pal_rich(sppr),
                       color = "black",
                       popup = paste0("<b>Site: </b>", site_coords$site," <br>
                                    <b>Survey Method: </b>", site_coords$survey, "<br>
                                    <b>Site Name: </b>", site_coords$area, "<br>
                                    <b>Max Depth: </b>", site_coords$max_depth,"m","<br>
                                    <b>Survey Objective: </b>", site_coords$objective, "<br>
                                    <b>Total Species: </b>", site_coords$sppr),
                       fillOpacity = 1,
                       options = markerOptions(interactive = FALSE,
                                               pane = "depth points"),
                       group = "richness") %>%
      hideGroup("richness")
    
  })  # end render leaflet
  
  
  
  
  myLeafletProxy <- leafletProxy(mapId = "map", session)
  
  # add richness layer when group button is clicked
  observe({
    req("richness" %in% input$colorCode)
    leafletProxy("map") %>%
      #hideGroup(c("total_spp","habitat_type")) %>% will add to this as more layers get added
      clearControls() %>%
      showGroup("richness") %>%
      leaflet::addLegend(data = site_coords,
                         pal = pal_rich,
                         title = paste0("Species<br>Richness"),
                         values = ~sppr,
                         layerId = "sppr",
                         opacity = 1)
  })
  
  
  # when button is default, remove all others options
  observe({
    req("default" %in% input$colorCode)
    leafletProxy("map") %>%
      clearControls() %>%
      #removeControl(layerId = c("total_spp","depthlegend")) %>%
      
      hideGroup(c("total_spp"))
  })
  

  
  # track clicked dot -------------------------------------------------------
  # Highlight sites based on selected species
  observeEvent(input$speciesSelector, {
    selected_species <- input$speciesSelector
    
    sites_containing_selected_species <- names(site_spp_list)[sapply(site_spp_list, function(species_list) {
      selected_species %in% species_list
    })]
    
    leafletProxy("map") %>%
      clearGroup("selected_sites") %>%
      addCircleMarkers(data = site_coords[site_coords$site %in% sites_containing_selected_species, ],
                       ~lat, ~long,
                       weight = 3,
                       radius = 5,
                       fillColor = 'transparent',
                       color = "yellow",
                       opacity = 1,
                       fillOpacity = .8,
                       options = markerOptions(interactive = FALSE),
                       group = "selected_sites")
  }, ignoreNULL = FALSE)
  
  
  # Initialize species table as empty, allowing HTML content
  output$speciesTable <- renderDT({
    datatable(data.frame(Species = character(0)), options = list(pageLength = 5, scrollX = TRUE), escape = FALSE)
  }, server = FALSE)
  
  # Update table based on map click
  observeEvent(input$map_marker_click, {
    clicked_site <- input$map_marker_click$id
    
    if (!is.null(clicked_site) && clicked_site %in% names(site_spp_list)) {
      species_list <- site_spp_list[[clicked_site]]
      
      # Assuming species_list contains strings formatted as "<i>Scientific name</i> - Common Name"
      species_data <- data.frame(Species = species_list, stringsAsFactors = FALSE)
      
      # Render the species table with HTML formatting for scientific names
      output$speciesTable <- renderDT({
        datatable(species_data, 
                  options = list(
                    pageLength = -1, 
                    scrollX = TRUE,
                    scrollY = "200px",  # Set the height of the table's vertical viewport
                    lengthChange = FALSE, 
                    searching = FALSE,
                    dom = 't'  # This option removes pagination controls
                    ), 
                  escape = FALSE)
      }, server = FALSE)
    }
  })
}


shinyApp(ui = ui, server = server)

