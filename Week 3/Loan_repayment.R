loan = read.csv("loans.csv")
str(loan)

# What proportion of the loans in the dataset were not paid in full? 
table(loan$not.fully.paid)

# Which of the following variables has at least one missing observation? Select all that apply.
summary(loan)

# Preparing the Dataset 
loans_imputed = read.csv("loans_imputed.csv")

# Prediction models 
set.seed(144) 
library(caTools)
split = sample.split(loans_imputed$not.fully.paid, SplitRatio = 0.7)
train = subset(loans_imputed, split == TRUE)
test = subset(loans_imputed, split == FALSE)
Model1 = glm(not.fully.paid~., data=train, family="binomial")
summary(Model1)

# Prediction Models 
test$predicted.risk = predict(Model1, type = "response", newdata = test)
table(test$not.fully.paid, test$predicted.risk >=0.5 )
# What is the accuracy of the logistic regression model? 

# Use the ROCR package to compute the test set AUC
loan_pred = prediction(test$predicted.risk, test$not.fully.paid) 
as.numeric(performance(loan_pred,"auc")@y.values)

# bivariate 
bivariate = glm(not.fully.paid~int.rate, data=train, family="binomial")
summary(bivariate)

# Predict bivariate 
pred.bivariate = predict(bivariate, newdata=test, type="response")
summary(pred.bivariate)

# Computing the Profitability of an Investment 
test$profit = 1 * exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1  # lender looses all the money 
summary(test$profit)

# An Investment Strategy Based on Risk 
# Choose those test  data having high interest rate greater than or equal to 1 
highInterest = subset(test, int.rate >= 0.15)
mean(highInterest$profit)
table(highInterest$not.fully.paid)

# Choose the 100'th cutoff predicted risk from the smallest limit
cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]
selectedLoans = subset(highInterest, predicted.risk <= cutoff) 
sum(selectedLoans$profit)

# How many of 100 selected loans were not paid back in full?
table(selectedLoans$not.fully.paid)


