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
library(ggplot2)
library(cluster)

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

filter_attacks_window <- function(df, default_value){
  if(nrow(df) != 0){
    attack_count <- df %>% filter(!is.na(attack_type)) %>% count(attack_type) %>% mutate(percentage = (n / sum(n)) * 100)
    attack_count <- attack_count %>% filter(percentage == max(percentage)) %>% pull(attack_type)
    
    if (length(attack_count) > 1) {
      paste0(attack_count[1], " + ", length(attack_count[-1]), " more")
    } else {
      attack_count
    }
  }else{
    default_value
  }
}

filter_detection_label <- function(df, default_value){
  if(nrow(df) != 0){
    detection_percentage <- df %>% filter(!is.na(detection_label)) %>% count(detection_label) %>% mutate(percentage = (n / sum(n)) * 100)
    return(detection_percentage %>% filter(detection_label == "Detected") %>% pull(percentage) %>% round(2) %>% paste0("%"))
  }else{
    default_value
  }
}

# Define server logic required to draw a histogram

function(input, output, session) {
  # ===================
  # Visualization 1 Logic
  # ===================

  observeEvent(input$tabs, {
    
    # Clean unused variables, free space
    gc()
    
    if (input$tabs == "vis1") {
      
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
      
      all_attacks <- reactiveVal(FALSE)
      all_protocol <- reactiveVal(FALSE)
      all_targets <- reactiveVal(FALSE)
      
      # # Only calls it once since valid_attacks is only processed once
      updateSelectizeInput(session, 'attack_type', choices = c("All", unique(valid_attacks$attack_type)), server = TRUE)
      updateSelectizeInput(session, 'protocol', choices = c("All", unique(valid_attacks$protocol)), server = TRUE)
      updateSelectizeInput(session, 'affected_system', choices = c("All", unique(valid_attacks$affected_system)), server = TRUE)
      
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
    } else if (input$tabs == "vis2") {
      
      #################################################################
      ##                    Visualization 2                          ##
      #################################################################
      
      attacks_with_timestamp <- data %>% filter(!is.na(timestamp)) %>% select(destination_country, source_country, timestamp, attack_type, affected_system, detection_label)
      attacks_with_timestamp$timestamp <- as.POSIXlt(attacks_with_timestamp$timestamp, format="%m/%d/%Y %H:%M")
      
      # Filter data based in input
      filtered_attacks <- reactive(attacks_with_timestamp %>% filter(timestamp > as.POSIXlt(input$date_selector[[1]]) & timestamp < as.POSIXlt(input$date_selector[[2]])))
      # Keeps track of which dataset is used
      selected_set <- reactiveValues(data = NULL)
      
      output$attack_detection_count <- renderText(filter_detection_label(filtered_attacks(), input$attack_detection_count))
      output$frequent_attack_count <- renderText(filter_attacks_window(filtered_attacks(), input$frequent_attack_count))
      
      observe({
        min_date <- as.Date(min(attacks_with_timestamp$timestamp))
        max_date <- as.Date(max(attacks_with_timestamp$timestamp))
        
        updateDateRangeInput(session, "date_selector",
                             start = min_date,
                             end = max_date,
                             min   = min_date,
                             max   = max_date)
      })
      
      output$heatmap <- renderPlotly({
        df <- filtered_attacks()
        
        # Race condition, initializes before data load
        if(nrow(df) != 0){
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
            selected_set$data <- df_week
          }else{
            # Tooltip message
            df_hour$tooltip <- paste("Timeframe:", sprintf("%02d:00", df_hour$x_axis)," - ", sprintf("%02d:59", df_hour$x_axis), "<br>Attack Count:", df_hour$incidents)
            
            hour_heatmap <- render_heatmap(df=df_hour, x_axis_name="Hour")
            g_plot <- ggplotly(hour_heatmap, tooltip = "text")
            selected_set$data <- df_hour
          }
          
          g_plot %>%
            event_register("plotly_click") %>%
            event_register("plotly_selected")  %>%
            layout(
              hoverlabel = list(
                bgcolor = "white",   # Tooltip background color
                font = list(
                  color = "black"    # Tooltip text color
                )
              ),
              margin = list(t = 40)
            )
        }
        
      })
      
      # When clicked on the house reset removes the selected
      observeEvent(event_data("plotly_relayout"), {
        d <- event_data("plotly_relayout")
        if (!is.null(d) && !is.null(d$yaxis.showspikes) && !d$yaxis.showspikes) {
          plotlyProxy("heatmap", session) %>%
            plotlyProxyInvoke("relayout", list(
              shapes = list()
            ))
          
          output$attack_detection_count <- renderText(filter_detection_label(attacks_with_timestamp))
          output$frequent_attack_count <- renderText(filter_attacks_window(attacks_with_timestamp))
          
          min_date <- as.Date(min(attacks_with_timestamp$timestamp))
          max_date <- as.Date(max(attacks_with_timestamp$timestamp))
          
          updateDateRangeInput(session, "date_selector",
                               start = min_date,
                               end = max_date,
                               min   = min_date,
                               max   = max_date)
        }
      })
      
      observeEvent(event_data("plotly_click"), {
        click_data  <- event_data("plotly_click")
        if (!is.null(click_data) && !is.null(click_data$x) && !is.null(click_data$x)) {
          selected_tile <- NULL
          if("date" %in% colnames(selected_set$data)){
            cell_position <- (if(click_data$x == 1) 0 else ((click_data$x-1) * 7)) + click_data$y
            
            by_date <- selected_set$data %>% arrange(date)
            selected_tile <- by_date[cell_position, ] 
          }else{
            cell_position <- (click_data$x * 7) + click_data$y
            selected_tile <- selected_set$data[cell_position, ]
          }
          
          selected_tile <- filtered_attacks() %>%
            mutate(
              week = week(timestamp),
              day = wday(timestamp, label = TRUE, week_start = 1),
              hour = hour(timestamp)
            ) %>% 
            filter(
              day == selected_tile$y_axis & 
                (week == selected_tile$x_axis | hour == selected_tile$x_axis)
            )
          
          output$attack_detection_count <- renderText(filter_detection_label(selected_tile))
          output$frequent_attack_count <- renderText(filter_attacks_window(selected_tile))
          
          plotlyProxy("heatmap", session) %>%
            plotlyProxyInvoke("relayout", list(
              shapes = list(
                list(
                  type = "rect",
                  x0 = click_data$x - 0.5,
                  x1 = click_data$x + 0.5,
                  y0 = click_data$y - 0.5,
                  y1 = click_data$y + 0.5,
                  line = list(color = "red", width = 3)
                )
              )
            ))
        }
      })
    } else if (input$tabs == "vis3") {
      
      #################################################################
      ##                    Visualization 3                          ##
      #################################################################
      
      ml_attack_prediction <- data %>% 
        filter(!is.na(payload_size_bytes) & !is.na(confidence_score) & !is.na(ml_model)) %>% 
        select(payload_size_bytes, ml_model, confidence_score, attack_type, ml_model)
      
      data_for_vis3 <- reactive({
        min <- (input$prediction_confidence[1]/100)
        max <- (input$prediction_confidence[2]/100)
        grouped_attacks <- ml_attack_prediction %>% filter(confidence_score >= min & confidence_score < max)
        
        if(input$cluster_models){
          clara_results <- clara(grouped_attacks$confidence_score, k=input$clusters_num)
          grouped_attacks$cluster <- as.factor(paste("Cluster", clara_results$clustering))
        }
        
        grouped_attacks
      })
      
      output$confidence_interval_cluster <- renderPlotly({
        df <- data_for_vis3()
        
        if(nrow(df) != 0){
          g_plot <- ggplot(df, aes(x = ml_model, y = payload_size_bytes, fill = ml_model)) + theme_minimal() 
          
          if(input$cluster_models){
            g_plot <- g_plot + facet_wrap(~cluster, ncol = input$clusters) + 
              theme(
                axis.text.x = element_text(angle = 90, hjust = 1),  # Rotate x-axis text
                legend.position = "right"
              ) 
          }
          
          g_plot <- g_plot + geom_boxplot() +
            labs(title = "Customized Box Plot",
                 x = "Group",
                 y = "Value") +
            scale_fill_brewer(palette = "Set2")
          
          ggplotly(g_plot)
        }else{
          NULL 
        }
      })
    }
  })
}
