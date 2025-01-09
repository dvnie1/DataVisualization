# Visualization 2 - Is there any geographical pattern based on attack type, protocol or targeted system?

library(plotly)
library(shinycssloaders)

render_sidebar_second <- function(){
  card(
    card_header(helpText("Available Filters")),
    card_body(
      helpText(
        tagList(
          "To reset the view click on the: ",
          icon("house")
        )
      ),
      dateRangeInput("date_selector", "Date Range:"),
    )
  )
}

render_second <- function(){
  card(
    layout_column_wrap(
      card(value_box( 
        title = "% of Attacks Detected", 
        value = withSpinner(textOutput("attack_type_count"), proxy.height = "30px", size = 0.5),
        showcase = icon("user-secret"), 
        theme = "bg-gradient-indigo-purple"
      )),
      card(value_box( 
        title = "Most frequent Attack", 
        value = withSpinner(textOutput("frequent_attack_count"), proxy.height = "30px", size = 0.5),
        showcase = icon("bomb"), 
        theme = "bg-gradient-indigo-purple"
      )), 
    ),
    selectInput( 
      "select_type",
      selected = "Week",
      label = "Filter Type:",
      choices = list("Filter by Week" = "Week", "Filter by Hour" = "Hour") 
    ), 
    withSpinner(plotlyOutput("heatmap"))
  )
}
