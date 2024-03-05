
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



# Footer  -----
fluidRow(id = "footer",
         column(2),
         column(8,
                HTML("<center><p style='size:18px';>
                     App Development by <a href='https://jakelawlor.github.io/' target='_blank'>Jake Lawlor</a><br>
                     </p></center>")),
         column(2)
         
) # end footer fluidrow


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
          reactableOutput("speciesTable", width = "100%"), width = 4)
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
  
  # add sampledepth layer when group button is clicked
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
      
      hideGroup(c("richness"))
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
                         options = markerOptions(interactive = FALSE,
                                                 pane = "yellow outlines"),
        ) 
      
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
      species <- data.frame(Species = full_spp_list[[site]])
    }
    reactable(species,
              pagination = F,
              compact = T,
              borderless = TRUE,
              columns = list(Species = colDef(html = TRUE)))
  })
  
  
  
  
  
  
  
  
  
  
}


shinyApp(ui = ui, server = server)

