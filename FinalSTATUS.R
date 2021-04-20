# main for STATUS

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
library(nnet)
library(class)
library(gmodels)


application_record<- read.csv('application_record.csv')
credit_record<- read.csv('credit_record.csv')

record<- inner_join(application_record, credit_record, by = 'ID') %>%
  sample_n(50000)


# "Current", 'Past Due'
#Stats_binary aka Current
# record <- record %>%
#   mutate(Status_binary = ifelse(as.numeric(STATUS) >= 5 , 1, 0))

record <- record %>%
  mutate(Status_binary = ifelse(STATUS %in% c('0', '1', '2', '3', '4', '5'), 0, 1)) %>%
  mutate(Status = ifelse(STATUS %in% c('0', '1', '2', '3', '4', '5'), 'Past Due', 'Current'))

record$Status_binary <- as.factor(record$Status_binary)


set.seed(11)
thesample <- sample(777715, 583286)
record_train <- record[thesample, ]
record_test  <- record[-thesample, ]

###################### smaller sample, change to original
thesample <- sample(50000, 37500)
record_train <- record[thesample, ]
record_test  <- record[-thesample, ]



library(psych)
library(rpart)
library(RWeka)

# use r part
m.rpart <- rpart(STATUS ~ ID  + MONTHS_BALANCE  + DAYS_BIRTH  + DAYS_EMPLOYED  + AMT_INCOME_TOTAL 
                 #CODE_GENDER + MONTHS_BALANCE + AMT_INCOME_TOTAL +DAYS_EMPLOYED 
                 ,data = record_train) 
# m.rpart
p.rpart<- predict(m.rpart, record_test)

summary(p.rpart)
summary(record_test$STATUS)

##############
# overall accuracy at 
print(sum(diag(test)) / sum(test))





library(C50)
# select numeric columns except one of interest
record_model <- C5.0(record_train[c(1,5,6,11,12,18,19)], as.factor((record_train$STATUS)))
record_model

# shows accuracy 
# error at 8%
summary(record_model)

# Attribute usage:
#   
#   100.00%	CNT_FAM_MEMBERS
# 99.98%	MONTHS_BALANCE
# 99.85%	DAYS_BIRTH
# 98.93%	AMT_INCOME_TOTAL
# 98.92%	ID
# 95.64%	DAYS_EMPLOYED
# 81.40%	CNT_CHILDREN




library(gmodels)
# improve from boosting
# overfit?  on confusion matrix
record_boost <- C5.0(record_train[c(1,5,6,11,12,18,19)], as.factor(record_train$STATUS), trials = 5)
record_boost
# Classification Tree
# Number of samples: 583286 
# Number of predictors: 7 
# 
# Number of boosting iterations: 5 
# Average tree size: 39635.6 

summary(record_boost)
# Trial	    Decision Tree   
# -----	  ----------------  
#   Size      Errors  
# 0	  51217 48057( 8.2%)
# 1	  33229 111616(19.1%)
# 2	  39403 108137(18.5%)
# 3	  37116 97411(16.7%)
# 4	  37213 102566(17.6%)
# boost	       27675( 4.7%)

# Attribute usage:
#   
#   100.00%	ID
# 100.00%	DAYS_BIRTH
# 100.00%	CNT_FAM_MEMBERS
# 100.00%	MONTHS_BALANCE
# 100.00%	AMT_INCOME_TOTAL
# 99.95%	DAYS_EMPLOYED
# 99.93%	CNT_CHILDREN



OneR <- OneR(as.factor(STATUS) ~ ID  + MONTHS_BALANCE  + DAYS_BIRTH  + DAYS_EMPLOYED  + AMT_INCOME_TOTAL, data = record)
OneR

# makes matrix after entries above and show accuracy and error %s
summary(OneR)

#(600325/777715 instances correct)

# === Summary ===
#   Correctly Classified Instances      600325               77.1909 %
# Incorrectly Classified Instances    177390               22.8091 %
# Kappa statistic                          0.639 
# Mean absolute error                      0.057 
# Root mean squared error                  0.2388
# Relative absolute error                 35.339  %
# Root relative squared error             84.0703 %
# Total Number of Instances           777715     
# 
# === Confusion Matrix ===
#   a      b      c      d      e      f      g      h   <-- classified as
# 196785    198      5      0      0    205  80255  13206 |      a = 0
# 4819    391      5      0      0     57   3056    419 |      b = 1
# 306     37      8      0      0     39    333     78 |      c = 2
# 103      3      1      0      0     36    111     32 |      d = 3
# 73      3      0      0      0     37     69     32 |      e = 4
# 257     21      0      0      0    845    251    153 |      f = 5
# 29701     74      0      0      0     80 295520   4161 |      g = C
# 25185     20      1      0      0    103  13865 106776 |      h = X






set.seed(12)
record_train_rf <- record_train
record_train_rf <- record_train_rf %>% select(-c(Status_binary, Status, FLAG_MOBIL, ID))
rf_model <- ranger(STATUS ~ . , data = record_train_rf, num.trees = 100, importance = "impurity",
                   classification = T)


rf_importance <- data.frame(Variable = names(rf_model$variable.importance),
                            Importance = rf_model$variable.importance)

row.names(rf_importance) <- NULL

rf_importance <- rf_importance %>% arrange(desc(Importance))
print(rf_importance)

rf_importance_top <- rf_importance %>% arrange(desc(Importance)) %>% top_n(10)
print(rf_importance_top)

# > print(rf_importance)
# Variable Importance
# 1                   ID  57991.466
# 2       MONTHS_BALANCE  27483.201
# 3           DAYS_BIRTH  26694.876
# 4        DAYS_EMPLOYED  21686.778
# 5     AMT_INCOME_TOTAL  19250.552
# 6      OCCUPATION_TYPE  11559.042
# 7   NAME_FAMILY_STATUS   6014.310
# 8      CNT_FAM_MEMBERS   5519.339
# 9     NAME_INCOME_TYPE   5279.798
# 10     FLAG_OWN_REALTY   4129.536
# 11 NAME_EDUCATION_TYPE   3957.250
# 12        CNT_CHILDREN   3923.011
# 13        FLAG_OWN_CAR   3837.868
# 14         CODE_GENDER   3418.926
# 15   NAME_HOUSING_TYPE   3259.622
# 16     FLAG_WORK_PHONE   2909.621
# 17          FLAG_PHONE   2827.729
# 18          FLAG_EMAIL   2142.893



# record_train_rf <- record_train
# record_train_rf <- record_train_rf %>% select(-c(Status_binary, ID, FLAG_MOBIL))
# rf_model <- ranger(STATUS ~ . , data = record_train_rf, num.trees = 100, importance = "impurity",
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

# remaining 7 var is 13 %
print(paste0("The Random Forest top 10 important variables accounts for: ", round(percent,4)*100, '%'))


# ordered for pareto chart
rfSTATUS <-ggplot(rf_importance_top, aes(x = reorder(Variable,-Importance), y = Importance, fill = Variable, weight = Importance)) + 
  geom_bar(stat = "identity") +
  ggtitle("Top 10 Var in order of Importance") +
  labs(x = 'Variables') +
  scale_fill_viridis_d(option = "viridis") +
  theme_light() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

rfSTATUS
ggplotly(rfSTATUS)




# continue matrix set up
# get predictions
rf_pred <- predict(rf_model, record_test)

rf_pred_label <- rf_pred$predictions
c_matrix_rf <- table(observed = record_test[,"Status_binary"], predicted = rf_pred_label)

# accuracy at 27.96% for all 7 status, 24.91 when no id
rf_accuracy <- sum(diag(c_matrix_rf)) / sum(c_matrix_rf)
print(paste0("The Random Forest regression model has an accuracy of: ", round(rf_accuracy,4)*100, '%'))





# ordinal regression
# choose several var
important_var <- c("CNT_CHILDREN","DAYS_BIRTH","CNT_FAM_MEMBERS","MONTHS_BALANCE","OCCUPATION_TYPE","STATUS")
record_train_ml <- record_train %>% 
  #dplyr::
  select(important_var)


# ordinal log reg
library(MASS)
ordinal_model <- polr(STATUS ~ ., data = record_train_ml, Hess = T)
summary(ordinal_model)

ordinal_pred <- predict(ordinal_model, newdata = record_test)
c_matrix_ordinal <- table(observed = record_test[,"STATUS"], predicted = ordinal_pred)

# accuracy of 47.15%
ordinal_accuracy <- sum(diag(c_matrix_ordinal)) / sum(c_matrix_ordinal)

print(paste0("The rate of Ordinal Logistic regression model is: ", 
             round(ordinal_accuracy,4)))


# ordered log reg
multi_model <- multinom(STATUS ~ ., data = record_train_ml)
# donot run summary(multi_model)
# weights:  192 (161 variable)
# initial  value 1212909.139076 
# iter  10 value 717236.760231
# iter  20 value 704115.251596
# iter  30 value 698139.720973
# iter  40 value 694307.470316
# iter  50 value 670748.978171
# iter  60 value 658300.101576
# iter  70 value 654893.832862
# iter  80 value 652155.981885
# iter  90 value 648562.645688
# iter 100 value 646062.840561
# final  value 646062.840561 
# stopped after 100 iterations




multi_pred <- predict(multi_model, newdata = record_test)
c_matrix_multi <- table(observed = record_test[,"STATUS"], predicted = multi_pred)

# acc rate is 46.7 %
multi_accuracy <- sum(diag(c_matrix_multi)) / sum(c_matrix_multi)
# slightly worse than ordinal
print(paste0("Accuracy of the multi nomial regression model is: ", 
             round(multi_accuracy,4)))






############## issue?
# SVM for STATUS 7 level variable
library(kernlab)
# vanilla specifies linear
############## change from linear?
classifier <- ksvm(STATUS ~  ID  + MONTHS_BALANCE  + DAYS_BIRTH  + DAYS_EMPLOYED  + AMT_INCOME_TOTAL, data = record_train, kernel = "vanilladot") 
#classifier

predictions <- predict(classifier, record_test)
head(predictions)

# compare predicted to actual
table(predictions, record_test$STATUS) 
agreement <- predictions == record_test$STATUS

# how many were correct
table(agreement) 
# accuracy
prop.table(table(agreement)) 

## Improving Model with RBF-based SVM

classifier_rbf <- ksvm(STATUS ~  ID  + MONTHS_BALANCE  + DAYS_BIRTH  + DAYS_EMPLOYED  + AMT_INCOME_TOTAL , data = record_train, kernel = "rbfdot")
predictions_rbf <- predict(classifier_rbf, record_test)
agreement_rbf <- predictions_rbf == record_test$STATUS
table(agreement_rbf)
prop.table(table(agreement_rbf))




