library(shiny)
library(bslib)
library(maps)
library(mapproj)

source("helpers.R")

counties <- readRDS("data/counties.rds")

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
  
  card(plotOutput("map"))
)



# Define server logic ----
server <- function(input, output) {
  
  output$map <- renderPlot({
    data <- switch(input$var,
                   "Percent White" = list(counties$white, "#16325B", "% White"),
                   "Percent Black" = list(counties$black, "#640D5F", "% Black"),
                   "Percent Hispanic" = list(counties$hispanic, "#654520", "% Hispanic"),
                   "Percent Asian" = list(counties$asian, "#343131", "% Asian"))
    percent_map(data[[1]], data[[2]], data[[3]], input$rangeSlider[1], input$rangeSlider[2])
    # Why is the first index of a list 1 and not 0 !?!? Who came up with this? Put your hands up right now!
  })
  
}


# Run the app ----
shinyApp(ui = ui, server = server)