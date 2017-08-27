# Project Name: Titanic Prediction
# Author: Gopika Jayadev

df <- read.csv('train.csv')
summary(df)

# Loading gg plot for visualizing the data
library(ggplot2)
library(fields)
library(caret)

# Analyzing the relationship between Fields

# Between Survived and PClass
table(df[,c('Survived', 'Pclass')])
# It is evident that the 1st class passengers have a higher probability of survival

# Between Survived and Sex
table(df[,c('Survived', 'Sex')])
# It is evident that the Female passengers have a higher probability of survival

# Using Box plot to visualize continuous data
bplot(df$Age, df$Survived)
# Median looks the same, looks like Age is not a major factor in predicting Survival

bplot(df$Fare, df$Survived)
# The Fare seems to have effect on the Survival, it should be interesting to include this variable

# Training the model
model <- train(Survived ~ Pclass + Sex + SibSp + Parch + Fare + Embarked, data = df, 
               method = 'rf', trControl = trainControl(method = "cv", 5) )

plot(model)
# Model Summary
summary(model)
# Insample CV
model

# Checking out of sample CV
# Loading the test data set
test <- read.csv("test.csv")
# Summary of the test set
# RF does not provide prediction if NA values exist
summary(test)
# Immuting the NA with the mean of the coulumn
test$Fare <- ifelse(is.na(test$Fare), mean(test$Fare, na.rm = TRUE), test$Fare)

# Predicting the Survival
test$Survived <- predict(model, newdata = test)

# Lets look at the submission details
submission <- read.csv('gender_submission.csv')

submission_ready <- data.frame('PassengerId' = test$PassengerId, 'Survived' = test$Survived)

submission_ready$SurvivedRounded <- ifelse(submission_ready$Survived > 0.5, 1, 0)

submissions <- data.frame('PassengerId' = submission_ready$PassengerId, 'Survived' = submission_ready$SurvivedRounded)
