
library(shiny)
library(bslib)
library(tidyverse)
library(ggplot2)
library(dplyr)

percentiles <- read.csv("pip_global_percentiles.csv")
percentiles <- percentiles%>%
  mutate(income = thr * 365 * .85)
#source("helper.R")

# Define ui
ui <- page_sidebar(
  title = "Global Income Percentiles (adjusted for Purchasing Power)", 
  sidebar = sidebar(
    numericInput(
      "number",
      "Enter Your Income (USD)",
      value = 20000),
    sliderInput(
      "don_per",
      "Donation Percentage",
      min = 0,
      max = 100,
      value = 10
    )
    ),
  mainPanel(
    plotOutput("percent_map", width = "150%", height = "300px"),
    tags$div(
      style = "white-space: nowrap; font-weight: bold;",
      textOutput("textoutput")),
    plotOutput("percent_map_new", width = "150%", height = "300px"),
    tags$div(
      style = "white-space: nowrap; font-weight: bold;",
      textOutput("textoutput2"),
      textOutput("textoutput3")
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  user_percentile <- reactive({
    cutoff <- input$number
    matching <- percentiles%>%
      filter(income <= cutoff)%>%
      summarise(percentile = max(percentile))
  })
  new_percentile <- reactive({
    cutoff <- input$number - (input$don_per*input$number*.01)
    matching <- percentiles%>%
      filter(income <= cutoff)%>%
      summarise(percentile = max(percentile))
  })
  time <- reactive({
    3000 / ((input$number * input$don_per * .01) / 365)
  })
  output$percent_map <- renderPlot({
    cutoff <- input$number
    percentiles %>% 
      mutate(below = income < cutoff) %>%
      ggplot(aes(x = factor(percentile), y = income, fill = below)) +
      geom_col(width = 0.8) +              # or geom_bar(stat="identity")
      scale_fill_manual(
        values = c("TRUE"  = "red",
                   "FALSE" = "grey")
      )+
      labs(
        x = "Income Percentile",
        y = "Income Threshold",
        title = "Income by Global Percentile"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
      )})
  output$textoutput <- renderText({
    per <- user_percentile()
    paste0("Your income is higher than ", per, "% of people worldwide.")
  })
  
  output$percent_map_new <- renderPlot({
    cutoff <- input$number - (input$don_per*input$number*.01)
    percentiles %>% 
      mutate(below = income < cutoff) %>%
      ggplot(aes(x = factor(percentile), y = income, fill = below)) +
      geom_col(width = 0.8) +              # or geom_bar(stat="identity")
      scale_fill_manual(
        values = c("TRUE"  = "red",
                   "FALSE" = "grey")
      )+
      labs(
        x = "Income Percentile",
        y = "Income Threshold",
        title = "Income by Global Percentile"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
      )})
  output$textoutput2 <- renderText({
    new_per <- new_percentile()
    paste0("Even if you donated ", input$don_per, "% of your income, you would still be richer than ", new_per, "% of people worldwide.")
  })
  output$textoutput3 <- renderText({
    life_saved_time <- time()
    paste0("This donation, if given to the most effective charities, would save one person's life every ", life_saved_time, " days on average.")
  })
}



# Run app
shinyApp(ui = ui, server = server)
