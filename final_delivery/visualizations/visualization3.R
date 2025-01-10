# visualization3.R

# Sidebar for Visualization 3
render_sidebar_third <- function() {
  card(
    helpText("Payload size and ML model visualization."),
    selectInput(
      inputId = "confidence_levels",
      label = "Confidence Levels",
      choices = c("All", "High", "Medium", "Low"),
      selected = "All",
      multiple = TRUE
    )
  )
}

# Main content for Visualization 3
render_third <- function() {
  card(
    plotOutput("vis3_plot", width = "100%", height = "100%")
  )
}
