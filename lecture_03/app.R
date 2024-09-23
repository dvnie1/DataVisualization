library(shiny)
library(bslib)

# Define UI ----
# ui <- page_fluid(
#   titlePanel("Basic widgets"),
#   layout_columns(
#     col_width = 3,
#     card(
#       card_header("Buttons"),
#       actionButton("action", "Action"),
#       submitButton("Submit")
#     ),
#     card(
#       card_header("Single checkbox"),
#       checkboxInput("checkbox", "Choice A", value = TRUE)
#     ),
#     card(
#       card_header("Checkbox group"),
#       checkboxGroupInput(
#         "checkGroup",
#         "Select all that apply",
#         choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),
#         selected = 1
#       )
#     ),
#     card(
#       card_header("Date input"),
#       dateInput("date", "Select date", value = "2014-01-01")
#     ),
#     card(
#       card_header("Date range input"),
#       dateRangeInput("dates", "Select dates")
#     ),
#     card(
#       card_header("File input"),
#       fileInput("file", label = NULL)
#     ),
#     card(
#       card_header("Help text"),
#       helpText(
#         "Note: help text isn't a true widget,",
#         "but it provides an easy way to add text to",
#         "accompany other widgets."
#       )
#     ),
#     card(
#       card_header("Numeric input"),
#       numericInput("num", "Input number", value = 1)
#     ),
#     card(
#       card_header("Radio buttons"),
#       radioButtons(
#         "radio",
#         "Select option",
#         choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),
#         selected = 1
#       )
#     ),
#     card(
#       card_header("Select box"),
#       selectInput(
#         "select",
#         "Select option",
#         choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),
#         selected = 1
#       )
#     ),
#     card(
#       card_header("Sliders"),
#       sliderInput(
#         "slider1",
#         "Set value",
#         min = 0,
#         max = 100,
#         value = 50
#       ),
#       sliderInput(
#         "slider2",
#         "Set value range",
#         min = 0,
#         max = 100,
#         value = c(25, 75)
#       )
#     ),
#     card(
#       card_header("Text input"),
#       textInput("text", label = NULL, value = "Enter text...")
#     )
#   )
# )
# 


# My turn

ui <- page_sidebar(
  title = "censusVis",
  sidebar = sidebar(
    helpText("Create demographic maps with information from the 2010 US Census"),
    selectInput(
      "select",
      "Choose a variable to display",
      choices = list("Percent White" = 1, "Percent Black" = 2, "Percent Hispanic" = 3, "Percent Asian" = 4)
    ),
    sliderInput(
      "rangeSlider",
      "Range of interest:",
      min = 0,
      max = 100,
      value = c(0, 100)
    )
  ),
  card(
    card_title("Want a random number? :3"),
    textOutput("displayNumber"),
    actionButton("generateButton", "Generate")
  )
)



# Define server logic ----
server <- function(input, output) {
  
  output$displayNumber <- renderText({"42"})
  
  # the below code is not Lesson 4!
  # just wanted to try this out on my own already in lesson 3
  observeEvent(input$generateButton, {
    output$displayNumber <- renderText({sample(1:9999, 1)})
  })
 }
 

# Run the app ----
shinyApp(ui = ui, server = server)
