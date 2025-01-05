# Visualization 1 - Is there any geographical pattern based on attack type, protocol or targeted system?
library(leaflet)

source("global.R")

render_sidebar_first <- function(){
  sidebar(
    helpText("Filter options to analyze different countries"),
    selectizeInput(
      "attack_type",
      label = "Attack Type",
      multiple = TRUE,
      choices = NULL
    ),
    selectizeInput(
      "protocol",
      label = "Protocol",
      multiple = TRUE,
      choices = NULL
    ),
    selectizeInput(
      "affected_system",
      label = "Affected Systems",
      multiple = TRUE,
      choices = NULL
    )
  )
}

render_first <- function(){
  card(
    leafletOutput("mymap", width="100%", height="100%")
  )
}
