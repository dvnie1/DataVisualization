library(bslib)
library(readr)
library(snakecase)
library(dplyr)
library(sf)
library(rlang)

# All data is loaded from the root of the Github repo

# Geo information to define polygon figure of map
countries_geo = st_read("data/50m.geojson")
# Country naming is different, so we used an intermediate converter to map to the geo-information
#   Ej:
#     UK -> GBR -> United Kingdom
countries_list <- read_csv("data/countries_codes_and_coordinates.csv", name_repair = as_function(to_snake_case))

# We remap column names to snake_case to avoid dealing with spaces
data <- read_csv("data/cyberattacks_detection.csv", name_repair = as_function(to_snake_case))

link_github <- tags$a(shiny::icon("github"),
                     "Source Code",
                     href = "https://github.com/dvnie1/DataVisualization",
                     target = "_blank")

link_kaggle <- tags$a(shiny::icon("kaggle"),
                     "Kaggle Dataset",
                     href = "https://www.kaggle.com/datasets/lastman0800/cyberattacks-detection",
                     target = "_blank")

theme <- bs_theme(bootswatch = "sandstone")

# Adds the attack counter to the geojson data to aggregate data
calculate_geojson_data <- function(data){
  
  # Since we have multiple coincidences we just group them for easier rendering
  aggregated_data <- data %>%
    group_by(alpha_3) %>%
    summarise(
      attack_count = n(), # Count total attacks for each country
      .groups = 'drop'
    )
  
  # Add attack_count column to geo information
  countries_geo %>% left_join(aggregated_data, by = c("ADM0_A3" = "alpha_3"))
}

firstUp <- function(x) {
  paste(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)), sep="")
}

render_heatmap <- function(df, x_axis_name){
  
  # Plot heatmap
  gplot <- ggplot(df, aes(x = x_axis, y = y_axis, fill = value_category, text = tooltip)) +
    # Heat-Map directive(white border lines)
    geom_tile(color = "white", lwd = 0.25, linetype = 1) +
    geom_point(aes(x = x_axis, y = y_axis), alpha = 0) +
    # Color Palette - BluGrn from color-palette-finder
    scale_fill_manual(values = c(
      "Minimum" = "#C4E6C3FF", 
      "Low" = "#96D2A4FF", 
      "Medium" = "#6DBC90FF", 
      "High" = "#4DA284FF", 
      "Very High" = "#36877AFF"),
    ) +
    # Adds some spacing
    theme_minimal() +
    # Titles and labels
    labs(
      x = x_axis_name,
      y = "Day of Week",
      fill = "Activity Level\n"
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank(),
      panel.background = element_blank()
    )
}

determine_filter_expr <- function(selector, key_name, session, oper_tracker){
  
  if(length(selector) == 1 && oper_tracker()){
    
    return(NULL)
    
  }else if ("All" %in% selector) {
    
    selected_value <- "All"
    
    if(length(selector) != 1 && oper_tracker()){
      selected_value <- selector[[which(unlist(selector) != "All")[1]]]
      oper_tracker(FALSE)
    }else if(!oper_tracker()){
      oper_tracker(TRUE)
    }
    
    updateSelectizeInput(session, key_name, selected = selected_value)
  }else{
    return(function(data){
      data %>% filter(.data[[key_name]] %in% as.list(selector))
    })
    
  }
  
  return(NULL)
}