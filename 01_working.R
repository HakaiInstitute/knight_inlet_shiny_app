
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
library(sp)
library(maps)
library(htmltools)
library(raster)
library(RColorBrewer)


# load data ---------------------------------------------------------------
#hillshade <- raster("data/Bathymetry/Hoeya_1m_NAD83_CSRS_UTMz10_CGVD2013a_HS.tif")
#classified_hillshade <- raster("data/Hillshade/ClassifiedHillshade.tif")
#backscatter <- raster("data/Backscatter/BACKSCATTER_KNIGHTINLET_2023_v3.tiff")
#depth <- raster("data/Bathymetry/Hoeya_1m_NAD83_CSRS_UTMz10_CGVD2013a.tif")
#substrate_class <- raster("data/Backscatter/SubstrateClassification.tif")

#depth_small <- aggregate(depth, fact=20, fun=mean)   # adjust factor for tiny size
#hillshade_small <- aggregate(hillshade, fact=20, fun=mean)

#saveRDS(depth_small, "data/depth_small.rds")
#saveRDS(hillshade_small, "data/hillshade_small.rds")

site_coords <- read_csv("data/site-coords.csv", col_types = cols(...1 = col_skip()))
site_spp_list <- readRDS("data/knight-site-spp-tables.rds")
species_list <- readRDS("data/knight-species-list.rds")
depth <- readRDS("data/depth_small.rds")
hillshade <- readRDS("data/hillshade_small.rds")


#site_coords_2023 <- site_coords %>%
#filter(year == 2023)
#site_coords <- site_coords_2023


ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      html, body {height: 100%; margin: 0;}
      #map {position: absolute; top: 0; bottom: 0; right: 0; left: 0;}
      .control-panel {
        background: rgba(255,255,255,0.95);
        padding: 22px;
        border-radius: 15px;
        box-shadow: 0 4px 14px rgba(0,0,0,0.3);
        width: 520px;                /* nice wide panel */
        overflow-y: auto;            /* scroll if content is taller than screen */
        font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
        position: absolute;
        top: 20px;
        bottom: 20px;                /* anchor to bottom for full-height stretch */
        left: 70px;                  /* moves it to the right of zoom buttons (~50px wide) */
      }
      .control-panel h4 {
        margin-top: 0;
        font-weight: 600;
        font-size: 20px;
        margin-bottom: 15px;
      }
      .footer {
        position: absolute;
        bottom: 12px;
        right: 15px;
        background: rgba(255,255,255,0.85);
        padding: 6px 16px;
        border-radius: 8px;
        font-size: 13px;
        color: #555;
        box-shadow: 0 2px 6px rgba(0,0,0,0.2);
      }
    "))
  ),
  
  # --- Map canvas ---
  leafletOutput("map", width = "100%", height = "100%"),
  
  # --- Floating controls (docked to right of zoom buttons) ---
  div(class = "control-panel",
      h4("Biodiveristy in the Gwaxdlala/Nalaxdlala IPCA Marine Refuge"),
      
      pickerInput(
        inputId = "speciesSelector",
        label = "Select a species:",
        choices = species_list,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE, maxOptions = 1),
        choicesOpt = list(content = species_list)
      ),
      hr(),
      
      # --- Raster layers temporarily disabled ---
      # checkboxGroupInput(
      #   inputId = "rasterSelect",
      #   label = "Raster Layers:",
      #   choices = list("Hillshade" = "hillshade", "Depth" = "depth")
      # ),
      # hr(),
      
      radioGroupButtons(
        inputId = "colorCode",
        label = "Color sites by:",
        choices = list(
          "Default" = "default",
          "Total Species" = "richness",
          "Substrate" = "habitat",
          "Red Tree Coral" = "coralpres",
          "Depth" = "depth"
        ),
        status = "primary",
        selected = "default",
        individual = TRUE,
        direction = "vertical",   # stacked neatly
        checkIcon = list("yes" = icon("check"))
      ),
      hr(),
      
      h5("Species at Selected Site"),
      reactableOutput("speciesTable", height = "220px")
  ),
  
  # --- Footer (bottom-right corner) ---
  div(class = "footer", "App Development by Alex Schmill")
)


server <- function(input, output, session) {
  
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
  
  pal_habitat <- colorFactor(
    palette = c(met.brewer("Signac",2)),  # adjust the number as the number habitat types increase
    domain = site_coords$substrate)
  
  # Render map -----------------------------------------------------------------
  output$map <- renderLeaflet({
    
    leaflet(
      options = leaflet::leafletOptions()) %>%
      #setView(zoom = 13) %>%
      
      # Add basemaps with groups
      addProviderTiles(providers$CartoDB.Positron, group = "Light") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addProviderTiles(providers$OpenStreetMap, group = "OSM") %>%
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Topographic") %>%
      
      # Add control to switch between them
      addLayersControl(
        baseGroups = c("Light", "Satellite", "OSM", "Topographic"),
        overlayGroups = c("all_sites","richness","depth","coralpres","habitat","selected_sites"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      
      # Set default basemap (Light)
      hideGroup(c("richness","depth","coralpres","habitat")) %>%
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
                                    <b>Habitat Complexity: </b>", site_coords$complexity, "<br>
                                    <b>Total Species: </b>", site_coords$sppr),
                       fillOpacity = .8,
                       #site_coords_allyears$long <- as.numeric(site_coords_allyears$long)
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
                                    <b>Habitat Complexity: </b>", site_coords$complexity, "<br>
                                    <b>Total Species: </b>", site_coords$sppr),
                       fillOpacity = 1,
                       #site_coords_allyears$long <- as.numeric(site_coords_allyears$long)
                       options = markerOptions(interactive = FALSE,
                                               pane = "depth points"),
                       group = "richness") %>%
      hideGroup("richness") %>%
      
      # add and hide depth dots
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
                                    <b>Habitat Complexity: </b>", site_coords$complexity, "<br>
                                    <b>Total Species: </b>", site_coords$sppr),
                       fillOpacity = 1,
                       #site_coords_allyears$long <- as.numeric(site_coords_allyears$long)
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
                                    <b>Habitat Complexity: </b>", site_coords$complexity, "<br>
                                    <b>Total Species: </b>", site_coords$sppr),
                       fillOpacity = 1,
                       #site_coords_allyears$long <- as.numeric(site_coords_allyears$long)
                       options = markerOptions(interactive = FALSE,
                                               pane = "coral points"),
                       group = "coralpres") %>%
      hideGroup("coralpres") %>%
      
      
      
      # add and hide substrate dots
      addCircleMarkers(data = site_coords,
                       lat = ~lat,
                       lng = ~long,
                       weight = 0,
                       radius = 5,
                       fillColor = ~pal_habitat(substrate),
                       color = "black",
                       popup = paste0("<b>Site: </b>", site_coords$site," <br>
                                    <b>Survey Method: </b>", site_coords$survey, "<br>
                                    <b>Site Name: </b>", site_coords$area, "<br>
                                    <b>Max Depth: </b>", site_coords$max_depth,"m","<br>
                                    <b>Survey Objective: </b>", site_coords$objective, "<br>
                                    <b>Habitat Complexity: </b>", site_coords$complexity, "<br>
                                    <b>Total Species: </b>", site_coords$sppr),
                       fillOpacity = 1,
                       #site_coords_allyears$long <- as.numeric(site_coords_allyears$long)
                       options = markerOptions(interactive = FALSE,
                                               pane = "coral points"),
                       group = "habitat") %>%
      hideGroup("habitat") %>%
      
      # --- Automatically fit bounds with padding ---
      fitBounds(
        lng1 = min(site_coords$long, na.rm = TRUE) - 0.4,   # expand west
        lat1 = min(site_coords$lat, na.rm = TRUE) - 0.1,   # expand south
        lng2 = max(site_coords$long, na.rm = TRUE) ,   # expand east
        lat2 = max(site_coords$lat, na.rm = TRUE) + 0.1    # expand north
      )
    
  })  # end render leaflet
  
  
  
  # Adding raster layers to map ------------------------------------------------
  observe({
    # Remove any existing rasters and legends
    leafletProxy("map", data = NULL) %>%
      clearImages() %>%
      clearControls()
    
    # Define the range for the "hillshade" raster
    hillshadeRange <- range(values(hillshade), na.rm = TRUE)
    
    if ("hillshade" %in% input$rasterSelect) {
      # Create a greyscale palette that spans the full range of hillshade values
      paletteHillshade <- colorNumeric(palette = grey.colors(256), domain = hillshadeRange, na.color = "transparent")
      
      leafletProxy("map") %>%
        addRasterImage(hillshade, colors = paletteHillshade, opacity = 1)
    }
    
    if ("depth" %in% input$rasterSelect) {
      paletteDepth <- colorNumeric(palette = viridis::mako(100)[10:100], 
                                   domain = values(depth), na.color = "transparent")
      leafletProxy("map") %>%
        addRasterImage(depth, colors = paletteDepth, opacity = 0.6)
      
      # Add legend for "depth"
      leafletProxy("map") %>%
        addLegend(pal = paletteDepth, values = values(depth), opacity = 1,
                  title = "Depth (m)", position = "bottomright")
    }
  })
  
  
  
  
  # Change point colours -------------------------------------------------------
  # add species richness layer when button is clicked
  observe({
    req("richness" %in% input$colorCode)
    leafletProxy("map") %>%
      hideGroup(c("coralpres","depth")) %>% 
      clearControls() %>%
      showGroup("richness") %>%
      leaflet::addLegend(data = site_coords,
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
      hideGroup(c("richness","coralpres","habitat")) %>% 
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
      hideGroup(c("richness","depth","habitat")) %>%
      clearControls() %>%
      #removeControl(layerId = c("depthlegend","habitat_type")) %>%
      showGroup("coralpres") %>%
      leaflet::addLegend(data = site_coords,
                         pal = pal_coralpres,
                         title = paste0("Red Tree<br>Coral Present?"),
                         values = ~`Primnoa present`,
                         layerId = "coralpres",
                         opacity = 1)
  })
  
  # add method layer when group button is clicked
  observe({
    req("habitat" %in% input$colorCode)
    leafletProxy("map") %>%
      hideGroup(c("richness","depth", "coralpres")) %>%
      clearControls() %>%
      #removeControl(layerId = c("depthlegend","habitat_type")) %>%
      showGroup("habitat") %>%
      leaflet::addLegend(data = site_coords,
                         pal = pal_habitat,
                         title = paste0("Substrate"),
                         values = ~substrate,
                         layerId = "habitat",
                         opacity = 1)
  })
  
  
  # when button is default, remove all others options
  observe({
    req("default" %in% input$colorCode)
    leafletProxy("map") %>%
      clearControls() %>%
      #removeControl(layerId = c("total_spp","depthlegend")) %>%
      
      hideGroup(c("richness","depth","coralpres","habitat"))
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
                         #site_coords_allyears$long <- as.numeric(site_coords_allyears$long)
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

