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
library(ggplot2)
library(lubridate)
library(purrr)

source("global.R")

# Create the Leaflet map
basemap <- leaflet(options = leafletOptions(minZoom = 2)) %>%
  addTiles() %>%
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
  
  #################################################################
  ##                    Visualization 1                          ##
  #################################################################
  
  valid_attacks <- merge(data, countries_list, by.x = "destination_country", by.y = "country") %>% select(source_country, destination_country, attack_type, protocol, affected_system, alpha_3)
  
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
  
  #################################################################
  ##                    Visualization 2                          ##
  #################################################################
  
  attacks_with_timestamp <- data %>% filter(!is.na(timestamp)) %>% select(destination_country, source_country, timestamp, attack_type, affected_system)
  attacks_with_timestamp$timestamp <- as.POSIXlt(attacks_with_timestamp$timestamp, format="%m/%d/%Y %H:%M")
  
  observe({
    min_date <- as.Date(min(attacks_with_timestamp$timestamp))
    max_date <- as.Date(max(attacks_with_timestamp$timestamp))
    
    updateDateRangeInput(session, "date_selector",
                    start = min_date,
                    end = max_date,
                    min   = min_date,
                    max   = max_date)
  })
  
  # Filter data based in input
  filtered_attacks <- reactive(attacks_with_timestamp %>% filter(timestamp > as.POSIXlt(input$date_selector[[1]]) & timestamp < as.POSIXlt(input$date_selector[[2]])))
  
  output$heatmap <- renderPlotly({
    df <- filtered_attacks()
    
    # Process data for heatmap
    df_week <- df %>%
      mutate(
        x_axis = week(timestamp),
        y_axis = wday(timestamp, label = TRUE, week_start = 1),
        date = as.Date(timestamp)
      ) %>% 
      count(x_axis, y_axis, date, name="incidents") %>% 
      mutate(
        value_category = cut(incidents, breaks = 5, labels = c("Minimum", "Low", "Medium", "High", "Very High"))
      )
    
    # Process data for heatmap
    df_hour <- df %>%
      mutate(
        x_axis = hour(timestamp),
        y_axis = wday(timestamp, label = TRUE, week_start = 1),
      ) %>% 
      count(x_axis, y_axis, name="incidents") %>% 
      mutate(
        value_category = cut(incidents, breaks = 5, labels = c("Minimum", "Low", "Medium", "High", "Very High"))
      )
    
    g_plot <- NULL
    
    if(input$select_type == "Week"){
      # Tooltip message
      df_week$tooltip <- paste("Date:", df_week$date, "<br>Attack Count:", df_week$incidents)
      
      week_heatmap <- render_heatmap(df=df_week, x_axis_name="Week")
      g_plot <- ggplotly(week_heatmap, tooltip = "text")
    }else{
      # Tooltip message
      df_hour$tooltip <- paste("Attack Count:", df_hour$incidents)
      
      hour_heatmap <- render_heatmap(df=df_hour, x_axis_name="Hour")
      g_plot <- ggplotly(hour_heatmap, tooltip = "text")
    }
    
    g_plot %>%
      layout(
        hoverlabel = list(
          bgcolor = "white",   # Tooltip background color
          font = list(
            color = "black"    # Tooltip text color
          )
        )
      )
    
  })
}
