
library(shiny)
library(bslib)
library(tidyverse)

counties <- readRDS("counties.rds")

source("helper.R")

# Define ui
ui <- page_sidebar(
  title = "censusVis", 
  sidebar = sidebar(
    helpText("Create demographic maps with information from the 2010 US Census."), 
    selectInput("var", 
                "Choose a variable to display", 
                choices = list("Percent White", 
                               "Percent Black", 
                               "Percent Asian", 
                               "Percent Hispanic"), 
                selected = 1), 
    sliderInput("slider", 
                "Range of interest:", 
                min = 0, 
                max = 100, 
                value = c(0, 100))
  ), 
  # main panel
  plotOutput("map")
)

# Define server
server <- function(input, output) {
  
  output$map <- renderPlot({
    data <- switch(input$var,
                   "Percent White" = counties$white,
                   "Percent Black" = counties$black,
                   "Percent Hispanic" = counties$hispanic,
                   "Percent Asian" = counties$asian)
    
    color <- switch(input$var,
                    "Percent White" = "darkgreen",
                    "Percent Black" = "dodgerblue3",
                    "Percent Hispanic" = "darkturquoise",
                    "Percent Asian" = "mediumpurple2")
    
    title <- switch(input$var,
                    "Percent White" = "% White",
                    "Percent Black" = "% Black",
                    "Percent Hispanic" = "% Hispanic",
                    "Percent Asian" = "% Asian")
    
    percent_map(data, color, title, 
                min = input$slider[1], max = input$slider[2])
  })
  
}

# Run app
shinyApp(ui = ui, server = server)
