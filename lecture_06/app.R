# Load packages ----
library(shiny)
library(bslib)
library(quantmod)

# Source helpers ----
source("helpers.R")

# User interface ----
ui <- page_sidebar(
  title = "stockVis",
  sidebar = sidebar(
    helpText(
      "Select a stock to examine.

        Information will be collected from Yahoo finance."
    ),
    textInput("symb", "Symbol", "SPY"),
    dateRangeInput(
      "dates",
      "Date range",
      start = "2013-01-01",
      end = as.character(Sys.Date())
    ),
    br(),
    br(),
    checkboxInput(
      "log",
      "Plot y axis on log scale",
      value = FALSE
    ),
    checkboxInput(
      "adjust",
      "Adjust prices for inflation",
      value = FALSE
    )
  ),
  card(
    card_header("Price over time"),
    plotOutput("plot")
  )
)

# Server logic
server <- function(input, output) {
  
  dataInput <- reactive({
    if (nzchar(input$symb)) {  # Check if input$symb is not an empty string
      data = getSymbols(input$symb, src = "yahoo",
                        from = input$dates[1],
                        to = input$dates[2],
                        auto.assign = FALSE)
      if (input$adjust) data <- adjust(data)
      data
    } else {
      return(NULL)  # Return NULL if no symbol is provided
    }
  })
  
  output$plot <- renderPlot({
    
    data <- dataInput()
    
    if (!is.null(data)) {
      chartSeries(data, theme = chartTheme("white"),
                  type = "line", log.scale = input$log, TA = NULL)  
    } else {
      plot.new()  # Create an empty plot
      text(0.5, 0.5, "Please enter a stock symbol", col = "red", cex = 1.5)  # Display message
    }
    
  })
  
}


# Run the app
shinyApp(ui, server)
