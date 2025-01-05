library(shiny)
library(bslib)

# Define UI ----

#ui <- page_sidebar(
#  title = "Title panel :3",
#  sidebar = sidebar("sidebar", position = "right"),
#  "main contents :3"
#)

#ui <- page_fluid(
#  layout_sidebar(
#    sidebar = sidebar("sidebar :p"),
#    "Main Contents"
#  )
#)

# ui <- page_sidebar(
#  title = "Titale :3",
#  sidebar = sidebar("sidebar"),
#  card(
#    card_header("This is a card"),
#    "Hihi",
#    card_footer("that is it...")
#  ),
#  value_box(
#    title = "Value box",
#    value = 69,
#    showcase = bsicons::bs_icon("bar-chart"),
#    theme = "teal"
#  )
# )

# Recreation the Shiny app

ui <- page_sidebar(
  title = "My Shiny App",
  sidebar = sidebar(
    "Shiny is available on CRAN, so you can install it in the usual way from your R console:",
    code("install.packages(\"shiny\")")
    ),
  card(
    card_header("Introducing Shiny"),
    "Shiny is a package from Posit that ...",
    card_image("https://shiny.posit.co/images/shiny-solo.png", height = "100px", width = "200px"),
    card_footer("Shiny is a product of Posit.")
  )
)

# Define server logic ----
server <- function(input, output) {
  
}

# Run the app ----
shinyApp(ui = ui, server = server)