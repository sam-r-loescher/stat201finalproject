library(shiny)
library(bslib)


ui <- page_sidebar(
  title = "censusVis", 
  sidebar = sidebar(
    helpText("Create demographic maps"),
    selectInput("var",
                "Choose a variable to display",
                choices = list("Choice 1", "Choice 2", "Choice 3"),
                      selected = 1),
          sliderInput("slider2",
            "Range of interest:",
              min = 0,
              max = 100,
              value = c(0, 100))
  )
)

# Define server logic ----
server <- function(input, output) {
  
  output$selected_var <- renderText({
    paste("You have selected", input$var)
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)