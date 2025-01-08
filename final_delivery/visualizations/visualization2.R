# Visualization 2 - Is there any geographical pattern based on attack type, protocol or targeted system?

library(plotly)
library(shinycssloaders)

render_sidebar_second <- function(){
  card(
    card_header(helpText("GitHub-Style Heatmap")),
    card_body(
      dateRangeInput("date_selector", "Select Date Range:"),
    )
  )
}

render_second <- function(){
  card(
    selectInput( 
      "select_type",
      selected = "Week",
      label = "Filter Type:",
      choices = list("Filter by Week" = "Week", "Filter by Hour" = "Hour") 
    ), 
    withSpinner(plotlyOutput("heatmap"))
  )
}