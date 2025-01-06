# Visualization 3 - Does the payload size play a critical role in the distribution of confidence levels when classifying attacks based on ML Models or Affected systems?
library(ggplot2)

source("global.R")

# Sidebar for Visualization 3
render_sidebar_third <- function() {
  card(
    helpText("Filter options to analyze payload size and confidence levels"),
    selectizeInput(
      "confidence_model",
      label = "Select ML Model or Affected System:",
      multiple = TRUE,
      choices = NULL
    )
  )
}

# Main content for Visualization 3
render_third <- function() {
  card(
    plotOutput("payload_confidence_chart", width = "100%", height = "500px")
  )
}
