
library(tidyverse)
library(shiny)
library(htmltools)
library(ggplot2)
library(highcharter)
library(shinydashboard)

# 
# application_record<- read_csv('application_record.csv')
# credit_record<- read_csv('credit_record.csv')
# 
# record <- inner_join(application_record, credit_record, by = 'ID')
# 
# # potential approvals
# borrowers <- record %>%
#   filter(AMT_INCOME_TOTAL < 500000) %>% 
#   mutate(STATUS = ifelse(STATUS %in% c('0', '1', '2', '3', '4', '5'), '>=0', 'CX')) %>% 
#   group_by(ID) %>% 
#   count(STATUS) %>%          
#   mutate(pct = n/sum(n)) %>%        
#   filter((STATUS == 'CX' & pct >= .75 & pct < 1
#           ))
# 
# record <- record %>%
#   mutate(Status_binary = ifelse(STATUS %in% c('0', '1', '2', '3', '4', '5'), '0', '1')) %>%
#   mutate(Status = ifelse(STATUS %in% c('0', '1', '2', '3', '4', '5'), 'Past Due', 'Current'))
# 
# record$Status_binary <- as.factor(record$Status_binary)
# 
# summary(record$DAYS_EMPLOYED)
# 
# record1 <- record %>% 
#   filter(AMT_INCOME_TOTAL < 500000) %>% 
#   mutate(Status_binary = ifelse(ID %in% unique(borrowers$ID), 1, 0),
#          yrs_employed = abs(DAYS_EMPLOYED)/360,
#          yrs_old = abs(DAYS_BIRTH)/360) 
# write.csv(record, 'C://Users//agost//Desktop//GMU//Spring 2021//Cds 490//Data//pair//record1.csv')


record1<- read_csv('record1.csv')


# record <- record %>%
#   mutate(Status_binary = ifelse(ID %in% unique(record$ID), 1, 0),
#          yrs_employed = (DAYS_EMPLOYED/360)*-1,
#          yrs_old = (DAYS_BIRTH/360)*-1)
# 
# 
# 
# record$yrs_employed <- replace(record$yrs_employed, which(record$yrs_employed <= 0), 0)
# record <- filter(record, yrs_employed != 0)
# 
# summary(record$yrs_employed)


# logreg2 <- glm(Status_binary ~ AMT_INCOME_TOTAL*CNT_FAM_MEMBERS + 
#                  #FLAG_OWN_CAR + 
#                  FLAG_OWN_REALTY +
#                  #NAME_INCOME_TYPE +
#                  #NAME_HOUSING_TYPE +
#                  MONTHS_BALANCE +
#                  yrs_employed + 
#                yrs_old #+
#                ####NAME_FAMILY_STATUS +
#                ####CNT_FAM_MEMBERS
#                ,   data = record %>% 
#                  filter(AMT_INCOME_TOTAL < 200000) %>% 
#                  group_by(Status_binary))

# logreg2 <- glm(Status_binary ~ AMT_INCOME_TOTAL + 
#                  #FLAG_OWN_CAR + 
#                  FLAG_OWN_REALTY +
#                  #NAME_INCOME_TYPE +
#                  #NAME_HOUSING_TYPE +
#                  #MONTHS_BALANCE +
#                  yrs_employed + 
#                  yrs_old +
#                  NAME_FAMILY_STATUS +
#                  CNT_FAM_MEMBERS
#                ,   data = record %>% 
#                  filter(AMT_INCOME_TOTAL < 200000) %>% 
#                  group_by(Status_binary) %>% sample_n(5500))
#                
# summary(logreg2)


logreg3 <- glm(Status_binary ~ AMT_INCOME_TOTAL + FLAG_OWN_REALTY + NAME_INCOME_TYPE + NAME_HOUSING_TYPE + yrs_employed 
               ,data = record1)

summary(logreg3)


# graph potential log
# record1 %>% 
#   filter(AMT_INCOME_TOTAL < 99000) %>% 
#   distinct(ID, AMT_INCOME_TOTAL, Status_binary) %>% 
#   group_by(Status_binary) %>% 
#   sample_n(100) %>% 
#   ggplot(aes(x = AMT_INCOME_TOTAL, y = Status_binary)) +
#   geom_point(alpha = .2) +
#   stat_smooth(method="glm", se=FALSE, method.args = list(family = binomial)) 
# 
# predictions
# record1$pred1 <- predict(logreg2, record1, type = 'response') 
# 
# hist(record1$pred1) 
# 
# 
# ## test pred
# pred_data1 <- data.frame(AMT_INCOME_TOTAL = seq(30000, 100000, 1000),
#                          FLAG_OWN_CAR = rep('Y', 71),
#                          FLAG_OWN_REALTY = rep('N', 71),
#                          NAME_INCOME_TYPE = rep('Commercial associate', 71),
#                          NAME_HOUSING_TYPE = rep('House / apartment', 71),
#                          yrs_employed = seq(1,2,3))
# 
# 
# pred_data1$preds <- predict(logreg2, newdata = pred_data1, type = 'response')
# pred_data1











ui2 <- fluidPage(
  titlePanel("Credit Application"),
  sidebarLayout(
    
    sidebarPanel(
      # radioButtons('credit_type', 'Credit_Type',
      #             choices = c('Credit card', 'Mortgage'),
      #             selected = NULL),
      
      #  selectInput('selec_gen', 'Gender', choices = c('M', 'F')),
      textInput('name', 'Enter your Full Name'),
      selectInput('selec_car', 'Do you own a car?', choices = c('Y', 'N')),
      selectInput('selec_rea', 'Do you own real estate?', choices = c('Y', 'N')),
      #  numericInput('selec_chi', 'How many children do you have?', value = 0),
      numericInput('selec_inc', 'How much do you earn?', value = 0),
      selectInput('selec_it', 'What is your income type?', choices = unique(application_record$NAME_INCOME_TYPE),
                  selected = NULL),
      selectInput('selec_edu', 'Whats your education level?', choices = unique(application_record$NAME_EDUCATION_TYPE),
                  selected = NULL),
      selectInput('selec_fam', 'Whats your marital status?', choices = unique(application_record$NAME_FAMILY_STATUS),
                  selected = NULL),
      selectInput('selec_hou', 'Whats your housing situation?', choices = unique(application_record$NAME_HOUSING_TYPE),
                  selected = NULL),
      numericInput('age', 'Age in years', value = 0),
      numericInput('work', 'Years at current Employer', value = 0),
      
      
      #  selectInput('selec_pho', 'Can you provide a work phone?', choices = c(1, 0)),
      #  selectInput('selec_mob', 'Do you have a mobile phone?', choices = c(1, 0)),
      #  selectInput('selec_ema', 'Can you provide an email?', choices = c(1, 0)),
      #  selectInput('selec_et', 'What is your employment type?', choices = unique(application_record$OCCUPATION_TYPE),
      #              selected = NULL),
      numericInput('selec_fm', 'How many members are in your household?', value = 0),
      #actionButton("clear", "Reset/Clear"),
      actionButton("enter", "Enter"), 
      # submitButton("enter, icon("enter")),
      
    ),
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  # display tab name in left "", output name for func in right ""
                  
                  tabPanel("Mortgage", uiOutput("mort")),
                  tabPanel("Credit Card"
                           , uiOutput("cc")
                  )
                  #tabPanel("Interactive Plot", plotlyOutput("Plot1")),
                  
                  # tabPanel("Table", dataTableOutput("table")),
                  #tabPanel("Text", textOutput("text"))#,
                  # tabPanel("Text", uiOutput("text"))#,
                  # tabPanel("Table", tableOutput("table1"))
                  
      )
    )))



server2 <- function(input, output, session) {
  
  values <- reactiveValues(df_data = NULL) 
  
  observeEvent(input$enter, { 
    
    values$df_data <- data.frame(AMT_INCOME_TOTAL = c(input$selec_inc),
                                 # FLAG_OWN_CAR = c(input$selec_car),
                                 FLAG_OWN_REALTY = c(input$selec_rea),
                                 NAME_INCOME_TYPE = c(input$selec_it),
                                 NAME_HOUSING_TYPE = c(input$selec_hou),
                                 yrs_employed = c(input$work)#,
                                 #  yrs_old = c(input$age)
    )
    
  })
  
  output$mort <- renderUI({
    list(
      tags$h2('Your Application for Credit is Currently in Progress'),
      tags$h6('Please hit Enter below to continue'),
      
      tags$img(src = "Home.png", width = "350"),
      
      textOutput('logRegLoan')#,
      ###imageOutput("houseImg"),
      
      ###tags$div(img(src = "Images/Home.png"))
    )
    
  })
  
  
  
  output$logRegLoan <- renderPrint({  
    req(input$enter)
    
    pct <- predict(logreg3, newdata = values$df_data, type = 'response') 
    
    if(pct > .46 & input$selec_inc >= 20000){  
      cat('Congratulations! You are approved')
    
      } else {
      cat('Thank you for applying! We must look into your account and will contact you shortly')
    } 
  })
    
    
    
    
  #   tags$h2('Your Application for is Currently in Progress')
  #   tags$h6('Please hit Enter below to continue')
  #   
  #   tags$img(src = "Home.png", width = "350")
  #   
  #   pred_data <- data.frame(AMT_INCOME_TOTAL = c(input$selec_inc),
  #                           FLAG_OWN_CAR = c(input$selec_car),
  #                           yrs_old = c(input$age),
  #                           yrs_employed = c(input$work),
  #                           CNT_FAM_MEMBERS = c(input$selec_fm),
  #                           
  #                           FLAG_OWN_REALTY = c(input$selec_rea))
  #   
  #   
  #   rf_pred <- predict(rf_model, record_test)
  #   
  #   rf_pred_label <- rf_pred$predictions
  #   c_matrix_rf <- table(observed = record_test[,"Status_binary"], predicted = rf_pred_label)
  #   #c_matrix_rf
  #   
  #   #p.rpart<- predict(m.rpart, record_test)
  #   #p.rpart_label <- ifelse(p.rpart >= 0.5, 1, 0)
  #   
  #   label <- ifelse(rf_pred == 1 & rf_pred_label == 1, 1, 0)
  #   
  #   if(label == 1){  # If statement to define loan approval message based on model results
  #     print('Congratulations! You are approved')
  #   } else {
  #     print('Sorry')
  #   }
  #   
  # })
    
    
    
    
    # pct <- predict(logreg2, newdata = values$df_data, type = 'response') #predictions based on model and custom data
    # if(pct > .46){  # If statement to define loan approval message based on model results
    #   print('Congratulations! You are approved')
    # } else {
    #   print('Sorry')
    # }
    
  #})
  
  output$cc <- renderUI({
  
    tags$img(src = "card.png", width = "250")
      
  })
}

shinyApp(ui2, server2)
