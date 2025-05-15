library(shiny)
library(bslib)
library(tidyverse)
library(ggplot2)
library(scales)
library(dplyr)
library(plotly)
library(sf)
library(leaflet)
library(rnaturalearth)
library(ggrepel)





data <- read.csv("2010.csv")

gdppercapnew <- read.csv("gdp_per_capita.csv")

charities <- read.csv("charities_top10.csv")







data_long <- data %>%
  
  select(Country, Total.Score) %>%
  
  mutate(Total.Score = as.numeric(gsub("%", "", Total.Score)))





world <- st_as_sf(rnaturalearth::ne_countries(scale = "medium", returnclass = "sf"))

map_data <- left_join(world, data_long, by = c("name" = "Country"))




gdp_data <- gdppercapnew %>%
  
  select(Country = Country.Name, GDP.per.capita = X2010)





merged_data <- inner_join(data_long, gdp_data, by = "Country") %>%
  
  filter(!is.na(GDP.per.capita), !is.na(Total.Score)) %>%
  
  mutate(
    
    GDP.group = case_when(
      
      GDP.per.capita < 5000 ~ "Low (<$5k)",
      
      GDP.per.capita < 15000 ~ "Lower-Mid ($5k–15k)",
      
      GDP.per.capita < 30000 ~ "Upper-Mid ($15k–30k)",
      
      TRUE ~ "High (>$30k)"
      
    )
    
  )





label_data <- merged_data %>%
  
  filter(Country %in% c("Sri Lanka", "Malta", "Greece", "Japan", "Australia", "Luxembourg"))







percentiles <- read.csv("pip_global_percentiles.csv")

percentiles <- percentiles %>%
  
  mutate(income = thr * 365 * .85)

#source("helper.R")



# Define uii

ui <- navbarPage("Tabs", id = "nav",
                 
                 theme = bs_theme(bg = "#D0E4F2",  # light‑green background
                                  
                                  fg = "#000000"),
                 
                 
                 
                 # Intro Tab
                 
                 tabPanel("Introduction",
                          
                          wellPanel(
                            
                            tags$h4("Introduction"),
                            
                            tags$p(
                              
                              "This shiny app's goal is to help you think about charity. It will help you see how rich you are, how much you could give, and what you giving's impacts could be.",
                              
                              style = "font-size:17px; line-height:1.4;"
                              
                            )
                            
                          ),
                          
                          fluidRow(
                            
                            column(
                              
                              width = 12,
                              
                              tags$img(
                                
                                src    = "Carbon River_ Empowering Africa with Clean Water.jpeg",       # file in www/
                                
                                style  = "width:100%; max-height:500px; object-fit:cover; margin-top:15px;",
                                
                                alt    = "Big intro graphic"
                                
                              )
                              
                            )
                            
                          )
                          
                          
                          
                 ),
                 
                 # 1st tab
                 
                 tabPanel("Income Comparison",
                          
                          sidebarLayout(
                            
                            sidebarPanel(
                              
                              wellPanel(
                                
                                tags$h4("Overview"),
                                
                                tags$p(
                                  
                                  "This page's goal is to compare your income to the worldwide income distribution. 

           This distribution is adjusted for currency differences, and for purchasing power.",
                                  
                                  style = "font-size:15px; line-height:1.4;"
                                  
                                )
                                
                              ),
                              
                              numericInput("number","Enter Your Income (USD)",20000),
                              
                              sliderInput("don_per","Donation Percentage",0,100,10)
                              
                            ),
                            
                            mainPanel(
                              
                              plotOutput("percent_map", width="100%", height="300px"),
                              
                              textOutput("textoutput"),
                              
                              plotOutput("percent_map_new", width="100%", height="300px"),
                              
                              textOutput("textoutput2"),
                              
                              textOutput("textoutput3")
                              
                            )
                            
                          )
                          
                 ),
                 
                 
                 
                 # 2nd tab: Quantifying the Good
                 
                 tabPanel("Quantifying Effects",
                          
                          sidebarLayout(
                            
                            sidebarPanel(
                              
                              h3("Adjust the relative weight you give to each charity here:"),
                              
                              tableOutput("alloc"),
                              
                              sliderInput("w1","Against Malaria Foundation (Bed Nets)",0,10,2.5),
                              
                              sliderInput("w2","Helen Keller International (Vitamin A)",0,10,2.5),
                              
                              sliderInput("w3","Malaria Consortium (Malaria Medicine)",0,10,2.5),
                              
                              sliderInput("w4","New Incentives (Childhood Vaccines)",0,10,2.5)
                              
                            ),
                            
                            mainPanel(
                              
                              wellPanel(
                                
                                tags$h4("Overview"),
                                
                                tags$p(
                                  
                                  "This page's goal is to demonstrate the tangible impacts your donation could have, if donated to some of the most effective charities.",
                                  
                                  style = "font-size:15px; line-height:1.4;"
                                  
                                )
                                
                              ),
                              
                              plotlyOutput("effects_plot"),
                              
                              textOutput("amf_text"),
                              
                              textOutput("hki_text"),
                              
                              textOutput("mc_text"),
                              
                              textOutput("ni_text")                            )
                            
                          )
                          
                 ), 
                 
                 
                 
                 # 3rd tab
                 
                 tabPanel("Charitability Score Map",
                          
                          mainPanel(
                            
                            wellPanel(
                              
                              tags$h4("Overview"),
                              
                              tags$p(
                                
                                "This is an interactive map displaying the charitability scores of different countries. Charitability scores are calculated using variables such as average volunteering hours spent and average number of dollars donated. Obviously, the very poorest countries cannot be expected to have high charitability scores. This map shows significant differences in charitable practices between wealthy nations as well, though, demonstrating the importance of making giving a community norm.",
                                
                                style = "font-size:15px; line-height:1.4;"
                                
                              )
                              
                            ),
                            
                            leafletOutput("my_map", height = 600)
                            
                          )
                          
                 ),
                 
                 
                 
                 tabPanel("Charity and GDP Scatter Plot",
                          
                          mainPanel(
                            
                            wellPanel(
                              
                              tags$h4("Overview"),
                              
                              tags$p(
                                
                                "This scatterplot compares charity scores to GDP, demonstrating that while a loose correlation exists, some very poor countries still have high charity scores, and some wealthy countries have low ones. This further demonstrates the influence of culture, and the importance of emphasizing effective giving as an essential part of life.",
                                
                                style = "font-size:15px; line-height:1.4;"
                                
                              )
                              
                            ),
                            
                            plotOutput("gdp_plot", height = 600)
                            
                          )
                          
                 ),
                 
                 
                 
                 # 5th tab panel
                 
                 tabPanel("Assets by State & County",
                          
                          sidebarLayout(
                            
                            sidebarPanel(
                              
                              selectInput("state", "Select State:",
                                          
                                          choices = sort(unique(charities$CENSUS_STATE_ABBR))),
                              
                              
                              
                              
                              
                              selectInput("county", "Select County:",
                                          
                                          choices = NULL)
                              
                            ),
                            
                            
                            
                            mainPanel(
                              
                              wellPanel(
                                
                                tags$h4("Overview"),
                                
                                tags$p(
                                  
                                  "While the other graphics and statistics were fascinating, and we feel some of the charities described are great to donate to, they are often very far away geographically. We thus also organized charitable organizations by state and county, ordered by size of total assets. You can use this to find organizations near you, to start getting involved!",
                                  
                                  style = "font-size:15px; line-height:1.4;"
                                  
                                )
                                
                              ),
                              
                              plotlyOutput(outputId = 'revenue_plot', width = "100%", height = "60vh")
                              
                            )
                            
                          )
                          
                 ),
                 
                 
                 
                 
                 
                 # 6th tab: Sources
                 
                 tabPanel("Sources",
                          
                          mainPanel(
                            
                            p("Our charity effectiveness data comes from GiveWell, one of the most highly regarded charity evaluators. (https://www.givewell.org/) While they use the most rigorous tools available to make their estimates, high margins of error still exist. While the calculations are inexact, we still beleive they are incredibly useful. We hope you agree!"),
                            
                            p("The basis for the income comparison came from Giving What We Can, another highly effective charity evaluator. (https://www.givingwhatwecan.org/)"),
                            
                            h3("Works Cited"),
                            
                            p("Cheng, Joe, et al. SuperZIP Example: Mapping Demographics with Leaflet and Shiny. Shiny App Gallery, RStudio, https://gallery.shinyapps.io/063-superzip-example/."),
                            
                            p("Giving What We Can. Giving What We Can, https://www.givingwhatwecan.org/. Accessed 14 May 2025."),
                            
                            p("GiveWell. GiveWell, https://www.givewell.org/. Accessed 14 May 2025."),
                            
                            p("World Bank. Poverty and Inequality Platform, https://pip.worldbank.org/. Accessed 14 May 2025."),
                            
                            p("Global Change Data Lab. Our World in Data, https://ourworldindata.org/. Accessed 14 May 2025.")
                            
                          )
                          
                 )
                 
)





# Define server logic

server <- function(input, output, session) {
  
  
  
  ## Tab 1
  
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
    
    round(3000 / ((input$number * input$don_per * .01) / 365), digits = 1)
    
  })
  
  output$percent_map <- renderPlot({
    
    cutoff <- input$number
    
    percentiles %>%
      
      mutate(below = income < cutoff) %>%
      
      ggplot(aes(x = factor(percentile), y = income, fill = below)) +
      
      geom_col(width = 0.8) +              # or geom_bar(stat="identity")
      
      scale_fill_manual(
        
        values = c("TRUE"  = "darkgreen",
                   
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
        
        values = c("TRUE"  = "darkgreen",
                   
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
    
    paste0("Even if you donated ", input$don_per, "% of your income, your income would still be higher than ", new_per, "% of people worldwide.")
    
  })
  
  output$textoutput3 <- renderText({
    
    life_saved_time <- time()
    
    paste0("This donation, if given to the most effective charities, would save one person's life every ", life_saved_time, " days on average.")
    
  })
  
  
  
  ## Tab 2
  
  
  
  # reactive that grabs raw weights and computes normalized percentages
  
  alloc <- reactive({
    
    w <- c(input$w1, input$w2, input$w3, input$w4)
    
    total <- sum(w)
    
    pct   <- if (total == 0) rep(0, 4) else round(w/total * 100, 1)
    
    data.frame(
      
      Charity = c("AMF", "HKI", "MC", "NI"),
      
      Weight  = w,
      
      Percent = pct,
      
      Amount = paste0("$", pct*.01*input$number*input$don_per*.01),
      
      stringsAsFactors = FALSE
      
    )
    
  })
  
  
  
  output$alloc <- renderTable({
    
    df <- alloc()
    
    # ensure the printed percentages sum exactly to 100
    
    df$Percent[nrow(df)] <- df$Percent[nrow(df)] + (100 - sum(df$Percent))
    
    df
    
  }, digits = 1)
  
  
  
  # Generate Plotly Pie Chart
  
  output$effects_plot <- renderPlotly({
    
    df <- alloc()
    
    plot_ly(df,
            
            labels = ~Charity,
            
            values = ~Percent,
            
            type   = 'pie',
            
            text = ~paste0(Amount, " (", Percent, "%)"),
            
            hoverinfo = 'text',
            
            textinfo = 'label+percent',
            
            insidetextorientation = 'radial'
            
    ) %>%
      
      layout(title = "Charity Allocation",
             
             margin = list(t = 100))
    
  })
  
  # Calculate dollars to each charity
  
  dollars_amf <- reactive({
    
    df <- alloc()
    
    req(df)
    
    df$Percent[1]*.01*input$number*input$don_per*.01}) 
  
  
  
  dollars_hki <- reactive({
    
    df <- alloc()
    
    req(df)
    
    df$Percent[2]*.01*input$don_per*input$number*.01})
  
  
  
  dollars_mc <- reactive({
    
    df <- alloc()
    
    req(df)
    
    df$Percent[3]*.01*input$don_per*input$number*.01})
  
  
  
  dollars_ni <- reactive({
    
    df <- alloc()
    
    req(df)
    
    df$Percent[4]*.01*input$don_per*input$number*.01})
  
  
  
  
  
  ## Calculate effects of each donation
  
  
  
  # amf
  
  bednets <- reactive({
    
    df <- alloc()
    
    req(df)
    
    (df$Percent[1]*.01*input$don_per*input$number*.01)/5})
  
  
  
  # hki
  
  supps <- reactive({
    
    df <- alloc()
    
    req(df)
    
    (df$Percent[2]*.01*input$don_per*input$number*.01)/2})
  
  
  
  # mc
  
  meds <- reactive({
    
    df <- alloc()
    
    req(df)
    
    (df$Percent[3]*.01*input$don_per*input$number*.01)/7})
  
  
  
  inscentives <- reactive({
    
    df <- alloc()
    
    req(df)
    
    (df$Percent[4]*.01*input$don_per*input$number*.01)/148})
  
  
  
  # Generate Text Outputs
  
  output$amf_text <- renderText({
    
    amf_amt  <- round(dollars_amf(), 2)
    
    beds     <- round(bednets(), 1)
    
    paste0("Your donation of $", amf_amt,
           
           " to AMF will provide ", beds, " bed nets.")
    
  })
  
  
  
  output$hki_text <- renderText({
    
    hki_amt  <- round(dollars_hki(), 2)
    
    supps    <- round(supps(), 1)
    
    paste0("Your donation of $", hki_amt,
           
           " to HKI will provide ", supps,
           
           " year‑long courses of vitamin A supplements.")
    
  })
  
  
  
  output$mc_text <- renderText({
    
    mc_amt   <- round(dollars_mc(), 2)
    
    meds     <- round(meds(), 1)
    
    paste0("Your donation of $", mc_amt,
           
           " to MC would protect ", meds,
           
           " children with malaria medicine.")
    
  })
  
  
  
  output$ni_text <- renderText({
    
    ni_amt   <- round(dollars_ni(), 2)
    
    vacs     <- round(inscentives(), 1)
    
    paste0("Your donation of $", ni_amt,
           
           " to NI ensures ", vacs,
           
           " infants receive their essential vaccines.")
    
  })
  

  
  output$my_map <- renderLeaflet({
    
    leaflet(map_data) %>%
      
      addTiles() %>%
      
      addPolygons(
        
        fillColor = ~colorNumeric("Blues", map_data$Total.Score, na.color = "#ccc")(Total.Score),
        
        weight = 1,
        
        color = "white",
        
        fillOpacity = 0.7,
        
        highlightOptions = highlightOptions(weight = 2, color = "#666", fillOpacity = 0.9),
        
        label = ~paste0("Charity Score: ", Total.Score, "%")
        
      ) %>%
      
      addLegend("bottomright",
                
                pal = colorNumeric("Blues", map_data$Total.Score, na.color = "#ccc"),
                
                values = map_data$Total.Score,
                
                title = "Charity Score (%)",
                
                opacity = 0.7)
    
  })
  
  
  
  
  
  output$gdp_plot <- renderPlot({
    
    ggplot(merged_data, aes(x = GDP.per.capita, y = Total.Score, color = GDP.group)) +
      
      geom_point(size = 2, alpha = 0.7) +
      
      geom_text_repel(
        
        data = label_data,
        
        aes(label = Country),
        
        size = 3
        
      ) +
      
      scale_color_manual(values = c(
        
        "Low (<$5k)" = "red",
        
        "Lower-Mid ($5k–15k)" = "orange",
        
        "Upper-Mid ($15k–30k)" = "blue",
        
        "High (>$30k)" = "darkgreen"
        
      )) +
      
      labs(
        
        title = "Charity Score vs GDP per Capita (2010)",
        
        x = "GDP per Capita (USD)",
        
        y = "Charity Score (%)",
        
        color = "GDP Group"
        
      ) +
      
      theme_minimal()
    
  })
  
  
  
  # 5th Tab:
  
  # Observe changes to the state input and update counties dynamically
  
  observeEvent(input$state, {
    
    # Filter the dataset by the selected state
    
    counties_in_state <- charities %>%
      
      filter(CENSUS_STATE_ABBR == input$state) %>%
      
      pull(CENSUS_COUNTY_NAME) %>%
      
      unique()
    
    
    
    # Update the county dropdown with filtered counties
    
    updateSelectInput(session, "county",
                      
                      choices = sort(counties_in_state),
                      
                      selected = NULL) # Reset county selection
    
  })
  
  
  
  # Filter the data and create the plot based on state and county selection
  
   # Filter the data and create the plot based on state and county selection
  
  output$revenue_plot <- renderPlotly({
    
    # Filter the dataset based on the selected state and county
    
    filtered_data <- charities %>%
      filter(CENSUS_STATE_ABBR == input$state, CENSUS_COUNTY_NAME == input$county)
    
    
    
    # Create the plot
    filtered_data$tooltip_text <- paste(
      "Charity:", filtered_data$ORG_NAME_CURRENT,
      "<br>Total Assets: $", comma(filtered_data$Total_Assets * 1000)
      )
    plot <- ggplot(filtered_data, aes(x = reorder(ORG_NAME_CURRENT, -Total_Assets),
                              
                              y = Total_Assets,
                              
                              fill = ORG_NAME_CURRENT,
                              
                              text = tooltip_text) 
                              ) +
      
      geom_bar(stat = "identity", show.legend = F) +
      
      labs(x = "Charity",
           
           y = "Total Assets (in thousands of dollars)",
           
           title = "Total Assets by Charity") +
      
      scale_fill_discrete(name = "Charity Name") +
      
      
      
      theme_bw() +
      
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)
      )
    
    ggplotly(plot, tooltip = "text")
    
  })
  
}


# Run app

shinyApp(ui = ui, server = server)
