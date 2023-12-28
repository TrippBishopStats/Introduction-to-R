# Random Forests: From DataDaft - https://www.youtube.com/watch?v=uXIIk7suD6c

rm(list=ls())

library(randomForest)
library(caret)

#Let's use the randomForest package's randomForest() function to build a
#predictive model with the Titanic training data. First we'll load and
#preprocess the data.

titanic_train <- read.csv("data/train.csv")

titanic_train$Pclass <- ordered(titanic_train$Pclass,   # Convert to ordered factor
                                levels=c("3","2","1"))  

impute <- preProcess(titanic_train[,c(6:8,10)],        # Impute missing ages
                     method=c("knnImpute"))

titanic_train_imp <- predict(impute, titanic_train[,c(6:8,10)])     

titanic_train <- cbind(titanic_train[,-c(6:8,10)], titanic_train_imp)

titanic_train$Survived <- as.factor(titanic_train$Survived) # Convert target to factor

# Next we'll build our random forest model.

# This forest will have 1000 trees and at each decision point, only 2 of the
# predictors can be considered for selection.

set.seed(12)
rf_model <- randomForest(Survived ~ Sex + Pclass + Age + SibSp + Fare,
                         data= titanic_train,   # Data set
                         ntree=1000,            # Number of trees to grow
                         mtry=2)                # Number of branch variables

rf_model


set.seed(12)

# Create a trainControl object
train_control <- trainControl(method = "repeatedcv",   # use cross validation
                              number = 10,             # Use 10 partitions
                              repeats = 2)             # Repeat the cross validation 2 times

# Set required model parameters
tune_grid = expand.grid(mtry=c(2))

# Use the training control and parameter grid to train the model using the caret
# training infrastructure. Note that the parameters are the same as before, they're just specified differently.


# Train model
validated_rf <- train(Survived ~ Sex + Pclass + Age + SibSp + Fare + Embarked, 
                      data=titanic_train,                # Data set
                      method="rf",                       # Model type
                      trControl= train_control,          # model control options
                      tuneGrid = tune_grid,              # Required model parameters
                      ntree = 1000)                      # Additional parameters***


validated_rf          # View a summary of the model



