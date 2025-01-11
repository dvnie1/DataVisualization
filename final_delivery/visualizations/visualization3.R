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
    conditionalPanel(
      condition = "input.cluster_models == true",  # JavaScript condition
      helpText("Adjust the number of clusters to see changes in the grouping of ML models."),
      sliderInput("clusters_num", "Number of Clusters:", min = 2, max = 5, value = 3)
    )
  )
}

# Main content for Visualization 3
render_third <- function() {
  card(
    input_switch("cluster_models", "Group similar confidence intervals"),
    withSpinner(plotlyOutput("confidence_interval_cluster"))
  )
}
