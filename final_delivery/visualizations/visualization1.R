# Visualization 1 - Is there any geographical pattern based on attack type, protocol or targeted system?
library(leaflet)

source("global.R")

render_sidebar_first <- function(){
  card(
    card_header(helpText("Filter options to analyze different countries")),
    card_body(selectizeInput(
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
    )),
    min_height = "100vh"
  )
}

render_first <- function(){
  card(
    leafletOutput("mymap")
  )
}
