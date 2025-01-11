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
page_fluid(
  tags$style(HTML("
    .full-sidebar {
      min-height: 100vh; /* Full viewport height */
    }
  ")),
  
  # Application title
  wellPanel(titlePanel("Final Delivery - Data Visualization")),
  
  navset_tab(
    nav_panel(
      title = "Introduction",
      value = "intro",
      fillPage(
        tags$head(
          tags$style(
            HTML("
            .content-container {
              padding: 40px;
            }
          ")
          )
        ),
        div(
          class = "content-container",
          title = p("Cyberattack Analysis Overview", class = "lead"),
          p("The following paragraph presents three key visualizations derived from a cyberattack dataset obtained from Kaggle. The dataset and source code can be adquire in the links section."),
          p("To ensure the quality and accuracy of the visualizations, the data has been filtered to exclude null values that could otherwise compromise the results. The visualizations aim to address three critical questions:"),
          h4("1. Is there any geographical pattern based on attack type, protocol or targeted system?"),
          p("This question is addressed using a geographical map that highlights hotspots based on user-defined search parameters. The map illustrates the most vulnerable countries, enabling comparisons with others to identify effective countermeasures implemented elsewhere."),
          h4("2. Are there specific specific days of weeks or hours where trends occurs (attack type)?"),
          p("A heatmap is employed to analyze attack frequencies across different time frames, such as hours or days of the week. This insight allows countries to proactively strengthen cybersecurity measures by aligning their policies with the time periods preferred by attackers."),
          h4("3. Does the payload size play a critical role in the distribution of confidence levels when classifying attacks based on ML Models?"),
          p("To investigate this, a box-and-whisker plot is used to analyze the relationship between payload size and the confidence levels (using clusters) of machine learning model predictions. This visualization highlights which models are preferred against package size as an initial filter. This offers insights into potential counterstrategies."),
          br(),
          h4("Team members:"),
          tags$ol(
            tags$li("Daniel Cser"),
            tags$li("Hector Flores"),
          )
          
        )
      )
    ),
    nav_panel(
      title = "Visualization 1",
      value = "vis1",
      page_sidebar(
        title = p("Is there any geographical pattern based on attack type, protocol or targeted system?", class="lead"),
        sidebar = sidebar(
          render_sidebar_first(),
          width = 400
        ),
        fillable=TRUE, 
        fill=TRUE,
        render_first()
      )
    ),
    nav_panel(
      title = "Visualization 2",
      value = "vis2",
      page_sidebar(
        title = p("Are there specific specific days of weeks or hours where trends occurs (attack type)?", class="lead"),
        sidebar = sidebar(
          render_sidebar_second(),
          width = 400
        ),
        fillable=TRUE, 
        fill=TRUE,
        render_second()
      )
    ),
    nav_panel(
      title = "Visualization 3",
      value = "vis3",
      page_sidebar(
        title = p("Does the payload size play a critical role in the distribution of confidence levels when classifying attacks based on ML Models?", class="lead"),
        sidebar = sidebar(
          render_sidebar_third(),
          width = 400
        ),
        fillable=TRUE, 
        fill=TRUE,
        render_third()
      )
    ),
    nav_spacer(),
    nav_menu(title = "Links", nav_item(link_github), nav_item(link_kaggle)),
    id = "tabs"
  ),
  theme = theme
)
