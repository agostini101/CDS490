
library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)
library(tidyverse)




# develop interface
ui1 <- fluidPage(
  titlePanel("Customer breakdown"),
  
  # Sidebar
  #sidebarLayout(
    # Add inouts
  #  sidebarPanel(
      # Input to Select which set
    #   radioButtons("thevariables", "Select Feature:",
    
    # Main Panel options 
    mainPanel(
      # Output set up
      tabsetPanel(type = "tabs",
                  # display tab name in left "", output name for func in right ""
                  
                  tabPanel("Status Bar Chart", plotlyOutput("plotly")),
                  tabPanel("Months Chart Bi status", plotlyOutput("plotly1")),
                  tabPanel("Months Chart Multi status", plotlyOutput("plotly2")),
                  #tabPanel("Months Box plot", plotlyOutput("plotly3")),
                  #tabPanel("Age Box plot", plotlyOutput("plotly4")),
                  #tabPanel("Employment Box plot", plotlyOutput("plotly5")),
                  tabPanel("Income Box plot", plotlyOutput("plotly6")),
                 # tabPanel("Income vs Age and Employment", plotlyOutput("plotly7")),
                  tabPanel("Income, Age, and Employment 3D", plotlyOutput("plotly8")),
                  tabPanel("Bubble", plotlyOutput("plotly9")),
                  tabPanel("Stacked Histogram", plotlyOutput("plotly10")),
                 
                 tabPanel("Random Forest Result Multi", plotlyOutput("plotly11")),
                 tabPanel("Random Forest Result Bi", plotlyOutput("plotly12"))

                  #,
                  # extra tabs, interesting to see value changes over long time period here
                 # tabPanel("Summary", verbatimTextOutput("summary")),
                #  tabPanel("Table", tableOutput("table"))
      )
    )
#))
)

# function to create output 
server1 <- function(input, output) {
  
  # allow swith of datasets listed in radio button input
  # reactive assigns data options to d
  
  # d <- reactive({
  # 
  #   thevariables <- switch(input$thevariables,
  #                          dgroup1 = data %>%
  #                            filter(group == '1'),
  #                          dgroup2 = data %>%
  #                            filter(group == '2'),
  #                          dgroup3 = data %>%
  #                            filter(group == '3')
  #   )
  # })
  
  
  # use output name from main panel section to assign which tab 
  # output$summary <- renderPrint({
  #   summary(d())
  # })
  # 
  # output$table <- renderTable({
  #   head(d(), input$n)
  # })
  
  
  output$plotly <- renderPlotly({
    #STATUS bar
    
    ggplotly(plot, height = as.numeric(700))
  
    })
  
  
  output$plotly1 <- renderPlotly({
    #monthly balance bar with binary
    ggplotly(Plot, height = as.numeric(700))
    
     })
  
  output$plotly2 <- renderPlotly({
    #monthly balance bar with STATUS
    ggplotly(Plot1, height = as.numeric(700))
  })
  
  output$plotly3 <- renderPlotly({
    #month faceted box ranges
    ggplotly(box, height = as.numeric(700))
  })
  
  output$plotly4 <- renderPlotly({
    # age faceted box ranges
    ggplotly(box1, height = as.numeric(700))
  })
  
  output$plotly5 <- renderPlotly({
    # employment faceted box ranges
    ggplotly(box2, height = as.numeric(700))
  })

  output$plotly6 <- renderPlotly({
    # income range
    ggplotly(box3, height = as.numeric(700))
  })
  
  output$plotly7 <- renderPlotly({
    # age, emp, and inc scatter
    ggplotly(scatter, height = as.numeric(700))
  })
  
  output$plotly8 <- renderPlotly({
    # age, emp, and inc scatter 3d
    ggplotly(record3, height = as.numeric(700))
  })
  
  output$plotly9 <- renderPlotly({
    # bubble scatter
    ggplotly(p, height = as.numeric(700))
  })
  
  output$plotly10 <- renderPlotly({
    # stacked hist on acct status from cust ages
    ggplotly(hist, height = as.numeric(700))
  })
  
  output$plotly11 <- renderPlotly({
    # 8 levels rf result on importance
    ggplotly(rfSTATUS, height = as.numeric(700))
  })
  
  output$plotly12 <- renderPlotly({
    # binary rf result on importance
    ggplotly(rf, height = as.numeric(700))
  })
  
  }

# Run app to display info
shinyApp(ui1, server1)

