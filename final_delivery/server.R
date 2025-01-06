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
library(purrr)
library(ggplot2)

source("global.R")

# Render initial map with NO filters for Visualization 1
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

# Define server logic
function(input, output, session) {
  # ===================
  # Visualization 1 Logic
  # ===================
  
  # Reactive flag to control observer activation
  observer_active <- reactiveValues(active = FALSE)
  
  applied_filters <- reactiveValues(data = list(
    'attackType' = NULL,
    'protocol' = NULL,
    'affectedSystems' = NULL
  ))
  
  output$mymap <- renderLeaflet({ 
    basemap
  })
  
  all_attacks <- reactiveVal(TRUE)
  all_protocol <- reactiveVal(TRUE)
  all_targets <- reactiveVal(TRUE)
  
  # Only calls it once since valid_attacks is only processed once
  updateSelectizeInput(session, 'attack_type', choices = c("All", unique(valid_attacks$attack_type)), selected = "All", server = TRUE, options = list(render = render_options))
  updateSelectizeInput(session, 'protocol', choices = c("All", unique(valid_attacks$protocol)), selected = "All", server = TRUE, options = list(render = render_options))
  updateSelectizeInput(session, 'affected_system', choices = c("All", unique(valid_attacks$affected_system)), selected = "All", server = TRUE, options = list(render = render_options))
  
  observeEvent(input$attack_type, {
    computed <- determine_filter_expr(input$attack_type, "attack_type", session, all_attacks)
    
    if(!identical(computed, applied_filters$data$attackType)){
      applied_filters$data$attackType <- computed
    }
  })
  
  observeEvent(input$protocol, {
    computed <- determine_filter_expr(input$protocol, "protocol", session, all_protocol)
    
    if(!identical(computed, applied_filters$data$protocol)){
      applied_filters$data$protocol <- computed
    }
  })
  
  observeEvent(input$affected_system, {
    computed <- determine_filter_expr(input$affected_system, "affected_system", session, all_targets)
    
    if(!identical(computed, applied_filters$data$affectedSystem)){
      applied_filters$data$affectedSystems <- computed
    }
  })
  
  # Update map with filtered GeoJSON
  observe({
    
    active_filters <- Filter(Negate(is.null), applied_filters$data)
    
    filtered_geo_json <- calculate_geojson_data(reduce(
      .x = active_filters, 
      .f = function(data, filter_func){
        filter_func(data)
      },
      .init = valid_attacks
    ))
    
    # Create a color palette based on attack count (yellow to redish colors. Gray is used as default)
    palette <- colorBin("YlOrRd", domain = filtered_geo_json$attack_count, bins = 5, na.color = "gray")
    
    leafletProxy("mymap", data = filtered_geo_json) %>%
      clearShapes() %>%  # Clear existing polygons
      clearControls() %>%
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
      )
  })
  
  # ===================
  # Visualization 3 Logic
  # ===================
  
  data_for_vis3 <- reactive({
    # Prepare data for visualization 3
    filtered_data <- data
    
    # Calculate confidence levels based on payload_size_kb
    filtered_data <- filtered_data %>% 
      mutate(payload_size_kb = payload_size_bytes / 1024) %>%
      mutate(
        confidence_level = case_when(
          payload_size_kb <= quantile(payload_size_kb, 0.33, na.rm = TRUE) ~ "Low",
          payload_size_kb <= quantile(payload_size_kb, 0.66, na.rm = TRUE) ~ "Medium",
          TRUE ~ "High"
        )
      )
    
    # Apply confidence level filtering if not "All"
    if (!"All" %in% input$confidence_levels) {
      filtered_data <- filtered_data %>%
        filter(confidence_level %in% input$confidence_levels)
    }
    
    data <- filtered_data %>% 
      group_by(ml_model, confidence_level) %>% 
      summarise(mean_payload = mean(payload_size_kb, na.rm = TRUE), .groups = "drop")
    
    return(data)
  })
  
  output$vis3_plot <- renderPlot({
    data <- data_for_vis3()
    
    ggplot(data, aes(x = ml_model, y = mean_payload, fill = ml_model)) +
      geom_bar(stat = "identity") +
      facet_wrap(~ confidence_level) +  # Separate plot for each confidence level
      theme_minimal() +
      labs(
        title = "Payload Size by ML Model and Confidence Level",
        x = "ML Model",
        y = "Payload Size (kB)"
      ) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none"
      )
  })
  
}
