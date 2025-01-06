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
valid_attacks <- merge(data, countries_list, by.x = "destination_country", by.y = "country") %>% select(source_country, destination_country, attack_type, protocol, affected_system, alpha_3)

print("PRINTING HERE ...")
print(colnames(data))

link_shiny <- tags$a(shiny::icon("github"),
                     "Shiny",
                     href = "https://github.com/rstudio/shiny",
                     target = "_blank")

link_posit <- tags$a(shiny::icon("r-project"),
                     "Posit",
                     href = "https://posit.co",
                     target = "_blank")

theme <- bs_theme(bootswatch = "litera")

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