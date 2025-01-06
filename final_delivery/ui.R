#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Global components regardless of the current visualization
source("global.R")

# Separate each visualization to make it more manageable
source("visualizations/visualization1.R")
source("visualizations/visualization2.R")
source("visualizations/visualization3.R")

# Define UI for application that draws a histogram
fillPage(
  # Application title
  wellPanel(titlePanel("Final Delivery - Data Visualization")),
  
  navset_tab(
    nav_panel(
      title = "Visualization 1",
      page_sidebar(
        title = p("Is there any geographical pattern based on attack type, protocol or targeted system?", class="lead"),
        theme = theme,
        sidebar = sidebar(
          render_sidebar_first(),
          width = 400,
        ),
        render_first()
      )
    ),
    nav_panel(
      title = "Visualization 2",
      page_sidebar(
        theme = theme,
        title = p("Are there specific days of the week or hours where trends occur (attack type, targeted system)?", class="lead"),
        sidebar = sidebar(
          "Shiny is available on CRAN, so you can install it in the usual way from your R console:",
          code('install.packages("shiny")'),
        ),
        render_second()
      )
    ),
    nav_panel(
      title = "Visualization 3",
      page_sidebar(
        title = p("Does the payload size play a critical role in the distribution of confidence levels when classifying attacks based on ML Models or Affected systems?", class="lead"),
        theme = theme,
        sidebar = sidebar(
          "Shiny is available on CRAN, so you can install it in the usual way from your R console:",
          code('install.packages("shiny")'),
        ),
        render_third()
      )
    ),
    nav_spacer(),
    nav_menu(title = "Links", nav_item(link_shiny), nav_item(link_posit))
  )
)
