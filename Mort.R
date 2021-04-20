
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(reshape2)
library(plotly)
library(tidyverse)

library(DT)
library(htmltools)


# Make loan/ credit card prediction calculator based off data set information
application_record<- read.csv('application_record.csv')
credit_record<- read.csv('credit_record.csv')
record<- inner_join(application_record, credit_record, by = 'ID')


# "Current", 'Past Due'
#Stats_binary aka Current
record <- record %>%
  mutate(Status_binary = ifelse(as.numeric(STATUS) >= 5 , 1, 0))

record$Status_binary <- as.factor(record$Status_binary)

###### change credit
#if (credit >= 700) {
#  print('great')
#}
#else if (credit <= 600) {
#  print('bad')
#}
#else {
#  print('good')
#}

# credit card max limit is 15% of yearly income if other good factors
# negatives slowly decrease it
# can increase to 20% with another user



# Have loan option of mortgage or credit card 
# Mortgage

MonthlyPayments <- function(Price, down, rate, years){
  loan = Price - down
  rateMonth = (rate/12)
  term = years * 12
  payment = loan *((rateMonth(1+rateMonth)^term) / (1+rateMonth)^term-1)
}

P <- readline(prompt="Enter home price: ")
D <- readline(prompt="Enter down payment: ")
APR <- readline(prompt="Enter APR: ")
Y <- readline(prompt="Enter loan term in years: ")
P <- as.integer(P)
D <- as.integer(D)
APR <- as.integer(APR)
Y <- as.integer(Y)

loan = P - D
rateMonth = (APR*.01)/12
term = Y*12
payment1 = rateMonth*((1+rateMonth)^term)
payment2 = ((1+rateMonth)^term)-1
payment = loan *(payment1/payment2)
#payment = payment * -1
total = payment * term
IntMonth = (total - loan)/term
PrinMOnth = payment - IntMonth

print(paste("The Monthly Mortgage Payments(Principal and Interest) will be $", round(payment,2) ))
print(paste("The Mortgage will total $", round(total,2), " in total with principal and interest over the term"
            #, when the original amount was $ ", loan 
))
print(paste("The Payments will contain $", round(IntMonth,2), 'in interest and the principal is $ ',  round(PrinMOnth,2), 'each month initially.'))


# how much should your monthly payment be
inc <- readline(prompt="Enter yearly income: ")

# from 1 to 45%, 30 is standard
# have conservative to aggresive option 
P <- readline(prompt="Enter Percent toward mortgage: ")

inc <- as.numeric(inc)
P <- as.numeric(P)
P <- P*.01

pay = inc * P
MonthlyMax = pay/12

print(paste("Your monthly payments should not exceed $", round(MonthlyMax,2) ))






# begin application setup 
ui <- fluidPage(
  titlePanel("Mortgage Calculator"),
  sidebarLayout(
    sidebarPanel(
      numericInput("principal", "Principal after Down Payment", 500000, min = 0, step = 1000),
      numericInput("interest", "Interest", 3, min = 0, max = 100, step = 0.01),
      sliderInput("length", "Length (in Years)", min = 0, max = 30, value = 30, step = 1
      )),
    
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  # display tab name in left "", output name for func in right ""
                  # tabPanel("How much can I Afford?", plotOutput("howmuch")),
                  tabPanel("Summary", uiOutput("Summary")),
                  tabPanel("Table", dataTableOutput("table")),
                  
                  #  tabPanel("Plot", plotOutput("Plot")),
                  tabPanel("Interactive Plot", plotlyOutput("Plot1")),
                  tabPanel("Interest Pay", plotlyOutput("Plot2")),
                  tabPanel("Principal Pay", plotlyOutput("Plot"))
                  #tabPanel("Text", textOutput("text"))#,
                  # tabPanel("Table", tableOutput("table"))
      )
    )))


# pre define starter values for plotly graphing
P <- 500000
I <- 3
L <- 30
mortgage(500000,3,30)

server <- function(input, output) {
  mortgage <- function(P = 500000, I = 4, L = 30, amort = T, plotData = T) {
    # rate as decimal
    J <- I /  100
    # monthly rate
    JJ <- J / 12
   # term as months
    N <- 12 * L
    
    M1 <- JJ * ((1 + JJ)^-N)
    M2 <- ((1 + JJ)^N)-1
    M <- P * (M1/M2)
    Pay <- M
    
    # Calculate Amortization for each Month
    if (amort == TRUE) {
      # principal initial
      Prin <- P 
      # when loan is initially started
      currP <- NULL 
      while (Prin >= 0) {
        # amt of interest
        int <- Prin * J
        # diff of pay and int
        C <- M - int 
        # new balance
        D <- Prin - C
        # principal updates
        Prin <- D
        currP <- c(currP, Prin)
      }
      
      # update as pay made , length as t
      monthP <- c(P, currP[1:(length(currP) - 1)]) - currP
      
      month <- data.frame(
        # time changes over term
        Month = 1:length(currP),
        Year = sort(rep(1:ceiling(N / 12), 12))[1:length(monthP)],
        # value changes
        Balance = c(currP[1:(length(currP))]),
        Payment = monthP + c((Pay - monthP)[1:(length(monthP))]),
        Principal = monthP,
        Interest = c((Pay - monthP)[1:(length(monthP))])
      )
      
      # breakdown by month
      month <- subset(month, Year <= L * 12)
      
      year <- data.frame(
        Amortization = tapply(month$Balance, month$Year, max),
        
        # annnuals always sums
        Annual_Payment = tapply(month$Payment, month$Year, sum),
        Annual_Principal = tapply(month$Principal, month$Year, sum),
        Annual_Interest = tapply(month$Interest, month$Year, sum),
        Year = as.factor(na.omit(unique(month$Year)))
      )
      }
    
    { year2 <- year 
      
      year2$Year <- as.factor(year2$Year)
      # reshape for columns
      year2 <- melt(year2[, c("Annual_Interest", "Annual_Principal", "Year")], id.vars = "Year")
      
      ggplot(year2, aes(x = Year, y = value, fill = variable)) +
        geom_bar(position = "fill", stat = "identity") +
        labs(y = "Payment") 
    }}
  
  # print overall loan details
  output$Summary <- renderUI({
    HTML(paste0(
      "Summary", 
      "<br>",
      "Loan Amount: ", format(round(input$principal, 2)),
      "<br>",
      "Interest rate: ", input$interest, "%",
      "<br>",
      "Term: ", input$length, " years (", input$length * 12, " months)",
      "<br>",
      "Payment: ", format(round(monthPay, digits = 2)), 
      "<br>",
      "Total cost: ",  format(round(input$principal, 2)), 
      "<br>",
      "Principal over term ", format(round(monthPay * 12 * input$length - input$principal, 2)),
      "<br>",
      "Interest over term = ", format(round(monthPay * 12 * input$length, digits = 2))
    ))
  })
  
  

  output$Plot <- renderPlot({
    mortgage(P = input$principal, I = input$interest, L = input$length, plotData = T)
  })
  
  
  output$Plot1 <- renderPlotly({
    # 
    #   m <-  ggplot(year, aes(x = Year, y = mean(Annual_Principal))) +
    #                  #, fill = variable
    #       geom_bar(position = "fill", stat = "identity") +
    #       labs(y = "Payment") +
    #       #scale_y_continuous(labels = percent) +
    #       theme_minimal() +
    #       theme(legend.title = element_blank(), legend.position = "top")
    # 
    #     ggplotly(m)
    #m <- ggplot(
    m <- mortgage(P = input$principal, I = input$interest, L = input$length, plotData = T)
    #)
    ggplotly(m, height = 700)
  })
  
  
  month$Year <- as.factor(month$Year)
  
  output$Plot <- renderPlotly({
    
    m2 <-  ggplot(drop_na(month), aes(x = Year, y = Principal)) +
      geom_bar(#position = "fill", 
        stat = "identity", fill = 'royal blue') +
      labs(y = "Payment") +
      labs(title = "Principal pay down from monthly payment",
           x = 'Term years', y = 'Stacked Months of amount')   
    ggplotly(m2, height = 750)
    
  })
  
  output$Plot2 <- renderPlotly({
    
    m3 <-  ggplot(drop_na(month), aes(x = Year, y = Interest)) +
      geom_bar(#position = "fill", 
        stat = "identity", fill = 'cadetblue'
      ) +
      labs(y = "Payment") +
      labs(title = "Interest Collection from monthly payment",
           x = 'Term years', y = 'Stacked Months of Amount')   
    ggplotly(m3, height = 750)
    
  })
  
  
  output$table <-  renderDataTable({
      mortgage(P = input$principal, I = input$interest, L = input$length, plotData = F)
    
      df_month <- datatable(data.frame(round(month, 2)),
                  extensions = "Buttons",
                  options = list(
                    lengthChange = T,
                    lengthMenu = list(c(-1, 10, 12, 15, 25, 50, 100), c("All", "10", "12", "15", "25", "50", "100"))), rownames = F) %>%
        formatCurrency(c("Balance", "Payment", "Principal", "Interest"), currency = "", interval = 3, mark = ",")
    })
}

# Run App
shinyApp(ui, server)


