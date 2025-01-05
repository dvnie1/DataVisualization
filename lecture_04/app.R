library(shiny)
library(bslib)

ui <- page_sidebar(
  title = "censusVis",
  sidebar = sidebar(
    helpText("Create demographic maps with information from the 2010 US Census"),
    selectInput(
      "var",
      label = "Choose a variable to display",
      choices = c("Percent White",
		  "Percent Black",
		  "Percent Hispanic",
		  "Percent Asian"),
      selected = "Percent White"
    ),
    sliderInput(
      "rangeSlider",
      "Range of interest:",
      min = 0,
      max = 100,
      value = c(0, 100)
    )
  ),
  textOutput("selection"),
  textOutput("range")
)



# Define server logic ----
server <- function(input, output) {
  
  output$selection <- renderText({
    paste("You have selected ", input$var)
    })
  output$range <- renderText({
    paste("You have chosen a range that goes from ", input$rangeSlider[1], " to ", input$rangeSlider[2])
    })
  
}


# Run the app ----
shinyApp(ui = ui, server = server)

