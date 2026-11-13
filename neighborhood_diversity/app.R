#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(sf)
library(plotly)

data_by_dist <- read_rds("data/diverse_data_by_dist.rds")
data_by_year <- read_csv("data/diverse_data_by_year.csv")
metro_names <- data_by_dist |>
  pull(metro_name) |>
  unique()
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
          sidebarPanel(selectInput(inputId = "city", label = "Choose a city:", choices = metro_names),
          sliderInput(inputId = "span", label = "Choose span:", min = 0, max = 1, value = 0.5, step = 0.05)
        ),

        # Show a plot of the generated distribution
        mainPanel(plotlyOutput(outputId = "scatterplot"), plotOutput(outputId = "map"), plotlyOutput(outputId = "barplot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$scatterplot <- renderPlotly({
    city_data <- data_by_dist |>
      filter(metro_name == input$city) |>
      st_drop_geometry()
    p <- ggplot(city_data, aes(x = distmiles, y = entropy)) +
      geom_point() +
      geom_smooth(method = "loess", span = max(input$span, 0.01)) +
      theme_minimal()
    ggplotly(p)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
