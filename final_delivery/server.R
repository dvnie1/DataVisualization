#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(leaflet)
library(dplyr)

source("global.R")

# Render initial map with NO filters
initial_attacked_countries <- calculate_geojson_data(valid_attacks)

# Create a color palette based on attack count (yellow to redish colors. Gray is used as default)
palette <- colorBin("YlOrRd", domain = initial_attacked_countries$attack_count, bins = 5, na.color = "gray")

# Create the Leaflet map
basemap <- leaflet(initial_attacked_countries, options = leafletOptions(minZoom = 2)) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~palette(attack_count),
    color = "black", # Border color
    weight = 1, # Border thickness
    fillOpacity = 0.7, # Transparency
    popup = ~paste0("<b>Country:</b> ", GEOUNIT, "<br>",
                    "<b>Attack Count:</b> ", ifelse(is.na(attack_count), "0", attack_count))
  ) %>%
  addLegend(
    pal = palette,
    values = ~attack_count,
    title = "Attack Count",
    position = "bottomright"
  ) %>%
  setView(lng = 14.91946, lat = 12.02342, zoom = 2) %>%
  setMaxBounds(
    lng1 = -180, lat1 = -90,  # Southwest corner of Europe
    lng2 = 180, lat2 = 90    # Northeast corner of Europe
  )

render_options <- I(
  '
    {
      option: function(item, escape) {
          if (item.value === \'All\') {
            return \'<div class="option"><strong>\'+ escape(item.value) + \' (Select All)</strong></div>\';
          }
          return \'<div class="option">\'+ escape(item.value) + \'</div>\';
      },
      item: function(item, escape) {
          return \'<button type="button" class="btn btn-outline-dark" style="font-size: 11px">\' + escape(item.value) + "</button>";
      }
    }'
)

# Define server logic required to draw a histogram
function(input, output, session) {

  filtered_geo_json <- reactiveVal(initial_attacked_countries)
  
  output$mymap <- renderLeaflet({ 
    basemap
  })
  
  all_attacks <- reactiveVal(TRUE)
  
  # Only calls it once since valid_attacks is only processed once
  updateSelectizeInput(session, 'attackType', choices = c("All", unique(valid_attacks$attack_type)), selected = "All", server = TRUE, options = list(render = render_options))
  
  observeEvent(input$attackType, {
    
    if(length(input$attackType) == 1 && all_attacks()){return ()}
    
    if ("All" %in% input$attackType) {
      
      selected_value <- "All"
      
      if(length(input$attackType) != 1 && all_attacks()){
        selected_value <- input$attackType[[which(unlist(input$attackType) != "All")[1]]]
        print(paste("Selecting: ", selected_value))
        all_attacks(FALSE)
      }else if(!all_attacks()){
        all_attacks(TRUE)
      }
      
      updateSelectizeInput(session, 'attackType', selected = selected_value)
    }else{
      filtered_geo_json(calculate_geojson_data(valid_attacks %>% filter(attack_type == "Brute Force")))
    }
  })
  
  # Update map with filtered GeoJSON
  observe({
    
    print(paste("Current length: ", length(filtered_geo_json())))
    
    leafletProxy("mymap", data = filtered_geo_json()) %>%
      clearShapes() %>%  # Clear existing polygons
      addPolygons(
        fillColor = ~palette(attack_count),
        color = "black", # Border color
        weight = 1, # Border thickness
        fillOpacity = 0.7, # Transparency
        popup = ~paste0("<b>Country:</b> ", GEOUNIT, "<br>",
                        "<b>Attack Count:</b> ", ifelse(is.na(attack_count), "0", attack_count))
      )
  })
  
}
