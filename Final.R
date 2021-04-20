# main

library(dplyr)
library(tidyverse)
library(ggplot2)
library(corrplot)
library(GGally)
library(car)
library(plotly)
library(caret)
library(stats)

# forest
library(ranger)
library(randomForest)

#multinom
#library(nnet)
#library(class)
#library(gmodels)


application_record<- read.csv('application_record.csv')
credit_record<- read.csv('credit_record.csv')

record<- inner_join(application_record, credit_record, by = 'ID') #%>%
  #sample_n(50000)


# "Current", 'Past Due'
#Stats_binary aka Current
# record <- record %>%
#   mutate(Status_binary = ifelse(as.numeric(STATUS) >= 5 , 1, 0))

record <- record %>%
  mutate(Status_binary = ifelse(STATUS %in% c('0', '1', '2', '3', '4', '5'), 0, 1)) %>%
  mutate(Status = ifelse(STATUS %in% c('0', '1', '2', '3', '4', '5'), 'Past Due', 'Current'))

record$Status_binary <- as.factor(record$Status_binary)

### do not use with forest
record <- record %>% 
  mutate(Status_binary = ifelse(ID %in% unique(record$ID), 1, 0),
         yrs_employed = (DAYS_EMPLOYED/360)*-1,
         yrs_old = (DAYS_BIRTH/360)*-1) 

#record$yrs_employed <- replace(record$yrs_employed, which(record$yrs_employed <= 0), 0)
#summary(record$yrs_employed)


set.seed(11)
thesample <- sample(777715, 583286)
record_train <- record[thesample, ]
record_test  <- record[-thesample, ]

###################### smaller sample, change to original
#thesample <- sample(50000, 37500)
#record_train <- record[thesample, ]
#record_test  <- record[-thesample, ]



library(psych)
library(rpart)
library(RWeka)
library (ROCR)


# use r part
m.rpart <- rpart(Status_binary ~ ID  + MONTHS_BALANCE  + DAYS_BIRTH  + DAYS_EMPLOYED  + AMT_INCOME_TOTAL 
                   #CODE_GENDER + MONTHS_BALANCE + AMT_INCOME_TOTAL +DAYS_EMPLOYED 
                 ,data = record_train) 
# m.rpart
p.rpart<- predict(m.rpart, record_test)

summary(p.rpart)
summary(record_test$Status_binary)

p.rpart_label <- ifelse(p.rpart >= 0.5, 1, 0)
test <- table(p.rpart_label, as.numeric(p.rpart))
test

# overall accuracy at current prediction is 40.17%
print(sum(diag(test)) / sum(test))

################################## odd with binary
#cor(p.rpart, as.numeric(record_test$Status_binary))



###############
# acc, recall, pres, f score
matrix <- confusionMatrix(p.rpart[1], reference = record_test$Status_binary)






library(C50)
# select numeric columns except one of interest
record_model <- C5.0(record_train[c(1,5,6,11,12,18,19)], as.factor((record_train$Status_binary)))
record_model

# shows accuracy 
# error at 14 %
summary(record_model)
 
# Attribute usage:
#   100.00%	MONTHS_BALANCE
# 99.99%	DAYS_BIRTH
# 98.95%	DAYS_EMPLOYED
# 98.91%	AMT_INCOME_TOTAL
# 94.29%	ID
# 86.31%	CNT_CHILDREN
# 67.04%	CNT_FAM_MEMBERS


# see how well it did with confusion matrix
#record_predict <- predict(record_model, record_test)

library(gmodels)
# improve from boosting
# overfit? 91% on confusion matrix
record_boost <- C5.0(record_train[c(1,5,6,11,12,18,19)], as.factor(record_train$Status_binary), trials = 8)
record_boost
summary(record_boost)

# Trial	    Decision Tree   
# -----	  ----------------  
#   Size      Errors  
# 
# 0	  23507 83450(14.3%)
# 1	  10014 148692(25.5%)
# 2	  7552 170172(29.2%)
# 3	  10233 161601(27.7%)
# 4	  9784 167464(28.7%)
# 5	  8551 161626(27.7%)
# 6	  14435 107281(18.4%)
# 7	  10258 132247(22.7%)
# boost	       52688( 9.0%)   <<
#   
#   (a)    (b)    <-classified as
# -----  -----
#   192827  33027    (a): class 0
# 19661 337771    (b): class 1
# 
# Attribute usage:
#   100.00%	ID
# 100.00%	AMT_INCOME_TOTAL
# 100.00%	DAYS_BIRTH
# 100.00%	MONTHS_BALANCE
# 99.92%	CNT_CHILDREN
# 99.89%	DAYS_EMPLOYED
# 99.80%	CNT_FAM_MEMBERS


# OneR <- OneR(as.factor(STATUS) ~ AMT_INCOME_TOTAL, data = record)
# OneR
# 
# # makes matrix after entries above and show accuracy and error %s
# summary(OneR)


 OneR2 <- OneR(as.factor(Status_binary) ~ ID  + MONTHS_BALANCE  + DAYS_BIRTH  + DAYS_EMPLOYED  + AMT_INCOME_TOTAL , data = record)
 OneR2
# makes matrix after entries above and show accuracy and error %s
 summary(OneR2)

 # === Summary ===
 #   Correctly Classified Instances      626169               80.5139 %
 # Incorrectly Classified Instances    151546               19.4861 %
 # Kappa statistic                          0.577 
 # Mean absolute error                      0.1949
 # Root mean squared error                  0.4414
 # Relative absolute error                 41.0944 %
 # Root relative squared error             90.6581 %
 # Total Number of Instances           777715     
 # 
 # === Confusion Matrix ===
 #   a      b   <-- classified as
 # 201504  98984 |      a = 0
 # 52562 424665 |      b = 1
 
 
# improve the model
# breaks down accuracy by single variable or var groups to see how well it performs
#JRip <- JRip(as.factor(Status_binary) ~ ID  + MONTHS_BALANCE  + DAYS_BIRTH  + DAYS_EMPLOYED  + AMT_INCOME_TOTAL, data = record) 
#JRip
#summary(JRip)




# use many decision trees in forest with mult rparts
set.seed(12) 
# 20 min to run without ID removal
rf_setup <- record_train %>% select(-c(STATUS, Status, FLAG_MOBIL))
#rf_setup <- record_train %>% select(-c(STATUS, ID, FLAG_MOBIL))

# n tree 500 same result as 100
rf <- randomForest(x = rf_setup[ , -19], y = rf_setup[, 19],
                   importance = T, proximity = F, ntree = 50,
                   keepForest = T)

rf_imp <- importance(rf)
rf_imp
# > rf_imp
# 0         1 MeanDecreaseAccuracy MeanDecreaseGini
# ID                   79.66629  76.19140             80.59738        44632.347
# CODE_GENDER          21.03140  25.60680             24.35897         2160.534
# FLAG_OWN_CAR         46.28709  50.44435             51.92550         2517.092
# FLAG_OWN_REALTY      47.64513  51.15719             53.20172         2555.014
# CNT_CHILDREN         41.13063  43.92231             47.10112         2641.894
# AMT_INCOME_TOTAL     83.44967  89.85867             94.66986        13796.783
# NAME_INCOME_TYPE     43.40126  41.16784             48.65387         3785.971
# NAME_EDUCATION_TYPE  29.70436  28.99040             29.61637         2549.320
# NAME_FAMILY_STATUS   49.39263  49.13807             52.52270         4175.019
# NAME_HOUSING_TYPE    61.70952  52.09472             62.07193         2479.902
# DAYS_BIRTH           65.22037  71.00668             74.99247        19053.217
# DAYS_EMPLOYED        54.63916  62.79348             62.50193        15274.236
# FLAG_WORK_PHONE      27.64275  24.34967             25.78587         2029.496
# FLAG_PHONE           48.30274  45.67719             48.25087         2648.701
# FLAG_EMAIL           23.15084  24.96607             24.88112         1421.833
# OCCUPATION_TYPE     137.96241  74.34579            110.56017        10135.515
# CNT_FAM_MEMBERS      48.77371  40.40307             46.57674         3699.558
# MONTHS_BALANCE      164.46336 239.73959            212.32757        24668.619

varImpPlot(rf, sort = T, cex = .8)

conf <- rf$confusion
conf
# > conf
#         0      1 class.error
# 0 157160  68334   0.3030413
# 1  36772 321020   0.1027748

# at 81.98%
accuracy <- sum(diag(conf)) / sum(conf)
accuracy

# 
# # continue matrix set up
# rf_pred <- predict(rf_model, record_test)
# 
# rf_pred_label <- rf_pred$predictions
# c_matrix_rf <- table(observed = record_test[,"Status_binary"], predicted = rf_pred_label)
# 
# # accuracy at 81% for binary
# rf_accuracy <- sum(diag(c_matrix_rf)) / sum(c_matrix_rf)
# print(paste0("The Random Forest regression model has an accuracy of: ", round(rf_accuracy,4)*100, '%'))
# 








set.seed(12)
record_train_rf <- record_train
record_train_rf <- record_train_rf %>% select(-c(STATUS, Status, FLAG_MOBIL))
rf_model <- ranger(Status_binary ~ . , data = record_train_rf, num.trees = 100, importance = "impurity",
                   classification = T)

conf1 <- rf_model$confusion
conf1
# > conf1
# predicted  true 
#       0      1
# 0 157270  69887
# 1  36338 319791

table(conf1[1:2,1], conf1[2, 1:2])
conf1[1:2,1]

Matrix1 <- confusionMatrix(conf1[1],conf1[2])
Matrix1 <- confusionMatrix()








rf_importance <- data.frame(Variable = names(rf_model$variable.importance),
                            Importance = rf_model$variable.importance)

row.names(rf_importance) <- NULL

rf_importance <- rf_importance %>% arrange(desc(Importance))
print(rf_importance)

rf_importance_top <- rf_importance %>% arrange(desc(Importance)) %>% top_n(10)
print(rf_importance_top)

# > print(rf_importance)
# Variable Importance
# 1                   ID  44685.226
# 2       MONTHS_BALANCE  24775.996
# 3           DAYS_BIRTH  19335.811
# 4        DAYS_EMPLOYED  15730.148
# 5     AMT_INCOME_TOTAL  14090.833
# 6      OCCUPATION_TYPE   7764.268
# 7   NAME_FAMILY_STATUS   4160.275
# 8      CNT_FAM_MEMBERS   3661.944
# 9     NAME_INCOME_TYPE   3355.383
# 10     FLAG_OWN_REALTY   2754.232
# 11          FLAG_PHONE   2656.710
# 12        CNT_CHILDREN   2589.282
# 13        FLAG_OWN_CAR   2574.306
# 14         CODE_GENDER   2433.621
# 15 NAME_EDUCATION_TYPE   2377.226
# 16   NAME_HOUSING_TYPE   2207.620
# 17     FLAG_WORK_PHONE   1969.968
# 18          FLAG_EMAIL   1290.797


# record_train_rf <- record_train
# record_train_rf <- record_train_rf %>% select(-c(STATUS, ID, FLAG_MOBIL))
# rf_model <- ranger(Status_binary ~ . , data = record_train_rf, num.trees = 100, importance = "impurity",
#                    classification = T)
# 
# 
# rf_importance <- data.frame(Variable = names(rf_model$variable.importance), 
#                             Importance = rf_model$variable.importance)
# 
# row.names(rf_importance) <- NULL
# 
# rf_importance <- rf_importance %>% arrange(desc(Importance))
# print(rf_importance)
# 
# rf_importance_top <- rf_importance %>% arrange(desc(Importance)) %>% top_n(10)
# print(rf_importance_top)




rf_all <- ggplot(rf_importance, aes(x = reorder(Variable,-Importance), y = Importance, fill = Variable)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  ggtitle("Every Variable importance from data")+
  labs(x = 'Variables') +
  scale_fill_viridis_d(option = "viridis") +
  theme_light() 
# ggplotly(rf_all)

# amount the first 5 explaned of total from model
percent <- sum(rf_importance_top$Importance)/ sum(rf_importance$Importance)
percent
# 86 percent result
# remaining 7 var is 13 %
print(paste0("The Random Forest top 10 important variables accounts for: ", round(percent,4)*100, '%'))


# ordered for pareto chart
rf <-ggplot(rf_importance_top, aes(x = reorder(Variable,-Importance), y = Importance, fill = Variable, weight = Importance)) + 
  geom_bar(stat = "identity") +
  ggtitle("Top 10 Var in order of Importance with Binary") +
  labs(x = 'Variables') +
  scale_fill_viridis_d(option = "viridis") +
  theme_light() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

rf
ggplotly(rf)




# continue matrix set up
rf_pred <- predict(rf_model, record_test)

rf_pred_label <- rf_pred$predictions
c_matrix_rf <- table(observed = record_test[,"Status_binary"], predicted = rf_pred_label)

# accuracy at 81.75% for binary, 76.16 when no id
rf_accuracy <- sum(diag(c_matrix_rf)) / sum(c_matrix_rf)
print(paste0("The Random Forest regression model has an accuracy of: ", round(rf_accuracy,4)*100, '%'))


