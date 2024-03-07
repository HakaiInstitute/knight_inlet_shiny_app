
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(leaflet)
library(DT)
library(dplyr)
library(viridis)
library(reactable)
library(readr)
library(here)
library(readxl)
library(MetBrewer)

getwd()
# load data ---------------------------------------------------------------

# Load in these objects then return to comments before running app
#site_coords <- read_csv("~/knight inlet eDNA/shinyapp/App-1/data/site-coords.csv")
#site_spp_list <- readRDS("~/knight inlet eDNA/shinyapp/App-1/data/knight-site-spp-tables.rds")
#species_list <- readRDS("~/knight inlet eDNA/shinyapp/App-1/data/knight-species-list.rds")

# establish pallete
pal_rich <- colorNumeric(
  palette = "viridis",
  domain = site_coords$sppr)

pal_depth <- colorNumeric(
  palette = viridis::mako(100, direction = -1)[10:100],
  domain = site_coords$max_depth)

pal_coralpres <- colorFactor(
  palette = c("red","grey90"),
  levels = c("Yes","No")
)

pal_method <- colorFactor(
  palette = c(met.brewer("Signac",2)),  # adjust the number as the number survey types increase
  domain = site_coords$survey)




ui <- dashboardPage(
  dashboardHeader(title = "Biodiveristy in the Gwaxdlala/Nalaxdlala IPCA Marine Refuge", 
                  titleWidth = 600),  # since we have a long title, we need to extend width element in pixels
  dashboardSidebar(disable = TRUE),  # Disable the sidebar
  dashboardBody(
    
    fluidRow(
      box(title = "Map", status = "primary", solidHeader = TRUE,
          leafletOutput("map", width = "100%", height = "600px"), width = 8),
      box(title = "Species Selector", status = "warning", solidHeader = TRUE,
          pickerInput(
            inputId = "speciesSelector",
            label = "Select a species:", 
            # choices = full_spp_list,
            choices = species_list, 
            multiple = TRUE,
            options = pickerOptions(liveSearch=T,
                                    maxOptions = 1),
            choicesOpt = list(content = species_list),
            
          ), choices = NULL, width = 4),  # Placeholder for species choices
      box(title = HTML("Color Code"), status = "warning", solidHeader = TRUE,
          radioGroupButtons(
            inputId = "colorCode",
            label = "Color by:", 
            choices = list("Default" = "default", 
                           "Total Species" = "richness",
                           "Survey Method" = "method",
                           "Red Tree Coral" = "coralpres",
                           "Depth" = "depth"),
            status = "primary",
            selected = "default",
            individual = TRUE,
            checkIcon = list("yes" = icon("check"))
          ),
          width = 4),
      box(title = "Species List", status = "warning", solidHeader = TRUE,
          reactableOutput("speciesTable", width = "100%", height= '250px'), width = 4)
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
  
  
  
  # Render map -----------------------------------------------------------------
  
  output$map <- renderLeaflet({
    
    leaflet(
      options = leaflet::leafletOptions()) %>%
      addProviderTiles(providers$CartoDB.Voyager) %>%
      addMapPane("yellow outlines", zIndex=600) %>% 
      addMapPane("depth points",zIndex=500) %>%
      addMapPane("coral points",zIndex=450) %>%
      
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
                       clusterOptions = markerClusterOptions(maxClusterRadius = 8),
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
                       clusterOptions = markerClusterOptions(maxClusterRadius = 8),
                       options = markerOptions(interactive = FALSE,
                                               pane = "depth points"),
                       group = "richness") %>%
      hideGroup("richness") %>%
      
      # add and hide richness dots
      addCircleMarkers(data = site_coords,
                       lat = ~lat,
                       lng = ~long,
                       weight = 0,
                       radius = 5,
                       fillColor = ~pal_depth(max_depth),
                       color = "black",
                       popup = paste0("<b>Site: </b>", site_coords$site," <br>
                                    <b>Survey Method: </b>", site_coords$survey, "<br>
                                    <b>Site Name: </b>", site_coords$area, "<br>
                                    <b>Max Depth: </b>", site_coords$max_depth,"m","<br>
                                    <b>Survey Objective: </b>", site_coords$objective, "<br>
                                    <b>Total Species: </b>", site_coords$sppr),
                       fillOpacity = 1,
                       clusterOptions = markerClusterOptions(maxClusterRadius = 8),
                       options = markerOptions(interactive = FALSE,
                                               pane = "depth points"),
                       group = "depth") %>%
      hideGroup("depth") %>%  
      
      # add and hide coralpres dots
      addCircleMarkers(data = site_coords,
                       lat = ~lat,
                       lng = ~long,
                       weight = 0,
                       radius = 5,
                       fillColor = ~pal_coralpres(`Primnoa present`),
                       color = "black",
                       popup = paste0("<b>Site: </b>", site_coords$site," <br>
                                    <b>Survey Method: </b>", site_coords$survey, "<br>
                                    <b>Site Name: </b>", site_coords$area, "<br>
                                    <b>Max Depth: </b>", site_coords$max_depth,"m","<br>
                                    <b>Survey Objective: </b>", site_coords$objective, "<br>
                                    <b>Total Species: </b>", site_coords$sppr),
                       fillOpacity = 1,
                       clusterOptions = markerClusterOptions(maxClusterRadius = 8),
                       options = markerOptions(interactive = FALSE,
                                               pane = "coral points"),
                       group = "coralpres") %>%
      hideGroup("coralpres") %>%
      
      # add and hide method dots
      addCircleMarkers(data = site_coords,
                       lat = ~lat,
                       lng = ~long,
                       weight = 0,
                       radius = 5,
                       fillColor = ~pal_method(survey),
                       color = "black",
                       popup = paste0("<b>Site: </b>", site_coords$site," <br>
                                    <b>Survey Method: </b>", site_coords$survey, "<br>
                                    <b>Site Name: </b>", site_coords$area, "<br>
                                    <b>Max Depth: </b>", site_coords$max_depth,"m","<br>
                                    <b>Survey Objective: </b>", site_coords$objective, "<br>
                                    <b>Total Species: </b>", site_coords$sppr),
                       fillOpacity = 1,
                       clusterOptions = markerClusterOptions(maxClusterRadius = 8),
                       options = markerOptions(interactive = FALSE,
                                               pane = "coral points"),
                       group = "method") %>%
      hideGroup("method")
    
  })  # end render leaflet
  
  
  site_coords_nona <- site_coords %>%  # create a dataframe that doesn't contain NAs in the sppr column 
    filter(!is.na(sppr))
  
  # Build new map when color code buttons are pressed
  myLeafletProxy <- leafletProxy(mapId = "map", session)
  
  # add species richness layer when button is clicked
  observe({
    req("richness" %in% input$colorCode)
    leafletProxy("map") %>%
      hideGroup(c("coralpres","depth")) %>% 
      clearControls() %>%
      showGroup("richness") %>%
      leaflet::addLegend(data = site_coords_nona,
                         pal = pal_rich,
                         title = paste0("Species<br>Richness"),
                         values = ~sppr,
                         layerId = "sppr",
                         opacity = 1)
  })
  
  # add depth layer when button is clicked
  observe({
    req("depth" %in% input$colorCode)
    leafletProxy("map") %>%
      hideGroup(c("richness","coralpres","method")) %>% 
      clearControls() %>%
      showGroup("depth") %>%
      leaflet::addLegend(data = site_coords,
                         pal = pal_depth,
                         title = paste0("Depth"),
                         values = ~max_depth,
                         layerId = "depth",
                         opacity = 1)
  })
  
  # add coralpres layer when group button is clicked
  observe({
    req("coralpres" %in% input$colorCode)
    leafletProxy("map") %>%
      hideGroup(c("richness","depth","method")) %>%
      clearControls() %>%
      #removeControl(layerId = c("depthlegend","habitat_type")) %>%
      showGroup("coralpres") %>%
      leaflet::addLegend(data = site_coords_nona,
                         pal = pal_coralpres,
                         title = paste0("Red Tree<br>Coral Present?"),
                         values = ~`Primnoa present`,
                         layerId = "coralpres",
                         opacity = 1)
  })
  
  # add method layer when group button is clicked
  observe({
    req("method" %in% input$colorCode)
    leafletProxy("map") %>%
      hideGroup(c("richness","depth", "coralpres")) %>%
      clearControls() %>%
      #removeControl(layerId = c("depthlegend","habitat_type")) %>%
      showGroup("method") %>%
      leaflet::addLegend(data = site_coords,
                         pal = pal_method,
                         title = paste0("Survey Method"),
                         values = ~survey,
                         layerId = "method",
                         opacity = 1)
  })
  
  
  # when button is default, remove all others options
  observe({
    req("default" %in% input$colorCode)
    leafletProxy("map") %>%
      clearControls() %>%
      #removeControl(layerId = c("total_spp","depthlegend")) %>%
      
      hideGroup(c("richness","depth","coralpres","method"))
  })
  
  
  # Species selector -----------------------------------------------------------
  # this section creates yellow borders around sites where a
  # selected species is found. 
  sites_containing_spp <- reactive({
    req(input$speciesSelector)
    names(site_spp_list)[sapply(seq_along(site_spp_list),function(x){input$speciesSelector %in% site_spp_list[[x]]})]
  })
  
  observeEvent(input$speciesSelector, {
    
    leafletProxy("map") %>% 
      clearGroup( "selected_sites")
    
    if(!is.null(input$speciesSelector))
    {# %>%
      
      #clearMarkerClusters()
      #   this works but try to make reactive
      #    sites_containing_spp <- 
      #      names(site_spp)[sapply(seq_along(site_spp),function(x){input$selectcalvertspp %in% site_spp[[x]]$species})]
      
      leafletProxy("map") %>% 
        addCircleMarkers(data = site_coords[site_coords$site %in% sites_containing_spp(),],
                         lat = ~lat,
                         lng = ~long,
                         weight = 3,
                         radius = 5,
                         #layerId = ~site,
                         fillColor = 'transparent',
                         color = "yellow",
                         opacity = 1,
                         #popup = paste("<b>Site:</b>", site_coords$site," <br>
                         #           <b>Species:</b>", site_coords$total_spp),
                         fillOpacity = .8,
                         clusterOptions = markerClusterOptions(maxClusterRadius = 8),
                         options = markerOptions(interactive = FALSE,
                                                 pane = "yellow outlines"),
                         group = "selected_sites") 
      
    }
  }, ignoreNULL = F) 
  
  
  
  
  # Initialize species table as empty, allowing HTML content -------------------
  # Reactive value to store selected site
  selectedSite <- reactiveVal()
  
  # Observe map click events
  observeEvent(input$map_marker_click, {
    selectedSite(input$map_marker_click$id)
  })
  
  # Render species list in reactable based on selected site
  output$speciesTable <- renderReactable({
    site <- selectedSite()  # Get the currently selected site
    if (is.null(site)) {
      species <- data.frame(Species = character(0))  # Empty data frame if no site is selected
    } else {
      species <- data.frame(Species = site_spp_list[[site]])
    }
    reactable(species,
              pagination = F,
              compact = T,
              borderless = TRUE,
              defaultPageSize = nrow(species), # Show all rows, adjust as needed
              columns = list(Species = colDef(html = TRUE)))
  })
  
  
  
  
  
  
  
  
  
  
}


shinyApp(ui = ui, server = server)

