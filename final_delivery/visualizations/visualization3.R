# visualization3.R

# Sidebar for Visualization 3
render_sidebar_third <- function() {
  card(
    helpText("Payload size and ML model visualization.")
  )
}

# Main content for Visualization 3
render_third <- function() {
  card(
    plotOutput("vis3_plot", width = "100%", height = "100%")
  )
}
