# CDS490

## Goal

Through this project there will be multiple goals such as to predict payment default and balance amounts based off data for credit cards or mortgages. This is based off financial attributes and possibly additional characteristics. 

Also, investigate the inverse to make a calculation for approval prediction with a loan amount if approved based off many factors. Credit score similarities and risk weight manipulation in prediction.


## Info

Topics of loans, credit, and debt are of great importance. These subjects I believe have not had enough attention as needed over the past several years. I have noticed trends with those around me typically taking two polarizing stances when it comes to debt and credit scores. This would be for an individual to have more of an emphasis on credit scoring and achieve a value which is in a good standing. 

While on the other hand, I have personally seen for my generation with the millennials, there are many people totally against having a score. The split on those who wish to take on debt then is quite evenly split. I was seen some change their minds on debt with time to acquire some or vice versa being they received student loans early on and that caused them to stop adding to this debt. There is a whole spectrum for pro and anti-loans. 

I decided to investigate further into this with data for a history of loan tendencies with default and amounts of total or carried balances. I also felt it would be important to work on possible alternatives with credit scoring from generation trends. A credit score is a good figure to base off a financial decision for the bank however for those just getting started or individuals with more challenged circumstances are heavily penalized. I wanted to work with category weights such as income to see how to modify this.

## Method
Classification and regression techniques are to be used to achieve borrower groups and credit amounts.
Best classification methods for the data are decision trees, K nearest neighbor, and Support Vector. I will look also at Neural net applications. 
Regression analysis applied for a binary outcome with logistic. Otherwise for amounts a multivariate regression is investigated. 

In regard to Classification, error will range depending on variable complexity. With binary variables the algorithm is more easily able to categorise while I have additionally been working with an option of seven groups.
Currently, error is higher than desired especially with resursive partitioning to get total estimates. The balance between adding enough variable to better explain model, yet not to overfit with characteristics is crutial.

# Results
## Losistic
Based off of Status Binary for customer grouping of current or past due.
This can be compared to different customaer groups in test sets to get varying probabibilities of defualt y customer characteristics.

## Multinomial/Ordinal 
With the status variable with multiple levels
This was able close to double the accuracy with the Forest model, however the outcome then reached approximately 46%.

## Knn
Based off of Status Binary for customer grouping of current or past due.


## Partitioning with boosting
Based off of Status Binary for customer grouping of current or past due.
Initially the recursive partitioning achieves a 62% accuracy on average with the true posiitves and negatives.

## Decision Trees
Error of status binary with several numeric variables in turn caused error to 28% (record_model)
When trials are increased the error was a result of a range from 21 to 39 % in error depending on level. (record_boost)

This level of error can be helped with more iterations through trail however the scew in the observations for the lower count in levels 2,3, and 4 past due time intervals. 


## Random Forest
Based off of Status Binary for customer grouping of current or past due.

Also applied with ordinal variable of STATUS for comparison. 

The use of Status for seven factors did hinder the model so after the multinomial was applied as an alternative. When Binary was applied the accuracy went from a level of 25% to 81%.

The top 5 variables with highest importance to the model are by far the consumer Age, Time with balance, Employment time, Income, and Occupation. 



# Final







