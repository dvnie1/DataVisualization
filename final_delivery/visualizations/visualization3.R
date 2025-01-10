# visualization3.R

# Sidebar for Visualization 3
render_sidebar_third <- function() {
  card(
    helpText("Payload size and ML model visualization."),
    sliderInput( 
      "prediction_confidence", "ML prediction confidence level threshold", 
      min = 0, max = 100, 
      value = c(0, 100) 
    ),
    tags$head(
      tags$style(HTML("hr {border-top: 1px solid #000000;}"))
    ),
    helpText("Adjust the number of clusters to see changes in the grouping of ML models."),
    sliderInput("clusters", "Number of Clusters:", min = 2, max = 5, value = 3),
  )
}

# Main content for Visualization 3
render_third <- function() {
  card(
    plotOutput("vis3_plot", width = "100%", height = "100%")
  )
}
