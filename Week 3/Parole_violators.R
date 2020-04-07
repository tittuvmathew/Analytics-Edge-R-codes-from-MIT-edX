parole = read.csv("parole.csv")
nrow(parole)
str(parole)
nrow(subset(parole,violator==1))

# as.factor() 
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)
summary(parole)

# Splitting into a Training and Testing Set 
set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)

#  Building a Logistic Regression Model 
Model1 = glm(violator ~ ., data=train, family=binomial)
summary(Model1)

# Building a Logistic Regression Model 
male=1 
race=1
age=50
state2=0
state3=0
state4=0
time.served=3
max.sentence=12
multiple.offenses=0
crime2=1
crime3=0
crime4=0
logodds =-4.2411574 + 0.3869904*male + 0.8867192*race - 0.0001756*age + 0.4433007*state2 + 0.8349797*state3 - 3.3967878*state4 - 0.1238867*time.served + 0.0802954*max.sentence + 1.6119919*multiple.offenses + 0.6837143*crime2 - 0.2781054*crime3 - 0.0117627*crime4

# Prediction 
pred1 = predict(Model1, type = "response", newdata = test)
summary(pred1)

# Evaluating the Model on the Testing Set 
table(test$violator, pred1 >= 0.5)

# What is the accuracy of a simple model that predicts that every parolee is a non-violator? 
table(test$violator)

# AUC 
library("ROCR")
violator_pred = prediction(pred1, test$violator) 
as.numeric(performance(violator_pred,"auc")@y.values)

# Identifying Bias in Observational Data 



