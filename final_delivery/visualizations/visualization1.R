# Visualization 1 - Is there any geographical pattern based on attack type, protocol or targeted system?
library(leaflet)
library(shinycssloaders)

source("global.R")

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

render_sidebar_first <- function(){
  card(
    card_header(helpText("Filter options to analyze different countries")),
    card_body(selectizeInput(
      "attack_type",
      label = "Attack Type",
      multiple = TRUE,
      choices = c("All"),
      options = list(placeholder = "All (Select All)", render = render_options)
    ),
    selectizeInput(
      "protocol",
      label = "Protocol",
      multiple = TRUE,
      choices = c("All"),
      options = list(placeholder = "All (Select All)", render = render_options)
    ),
    selectizeInput(
      "affected_system",
      label = "Affected Systems",
      multiple = TRUE,
      choices = c("All"),
      options = list(placeholder = "All (Select All)", render = render_options)
    )),
    min_height = "100vh"
  )
}

render_first <- function(){
  card(
    withSpinner(leafletOutput("mymap", height = "100vh"), proxy.height = "200px")
  )
}
