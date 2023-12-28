# Decision Trees: From DataDaft - https://www.youtube.com/watch?v=uXIIk7suD6c
rm(list=ls())
# Decision trees are like flow charts that classify observations based on a
# series of if/else decisions that assign a probability or class.

library(rpart)
library(rpart.plot)
library(caret)

options(repr.plot.width = 6, repr.plot.height = 5)

# read in the data and do a little bit of prep work.
titanic_train <- read.csv("data/train.csv")
titanic_train$Pclass <- ordered(titanic_train$Pclass,     # Convert to ordered factor
                                levels=c("3","2","1"))  

impute <- preProcess(titanic_train[,c(6:8,10)],  # Impute missing ages
                     method=c("knnImpute"))

titanic_train_imp <- predict(impute, titanic_train[,c(6:8,10)])     

titanic_train <- cbind(titanic_train[,-c(6:8,10)], titanic_train_imp)

# now run the decision tree
gender_tree <- rpart(Survived ~ Sex,
                     data = titanic_train)

# plot a diagram of the decision tree
prp(gender_tree,
    space=4,
    split.cex = 1.5,
    nn.border.col = 0)

# Super simple tree that only uses Sex as a predictor.

# now run the decision tree adding a new predictor, Pclass.
gender_tree <- rpart(Survived ~ Sex + Pclass,
                     data = titanic_train)

# plot a diagram of the decision tree
prp(gender_tree,
    space=4,
    split.cex = 1.5,
    nn.border.col = 0)

# Below is a good example of a complex tree that is very likely to overfit the
# data. It is adapting itself to closely to the training data so it will
# probably not generalise very well.

complex_tree <- rpart(Survived ~ Sex + Pclass + Age + SibSp + Fare + Embarked,
                      cp = 0.001,                 # Set complexity parameter*
                      data = titanic_train)       # Use the titanic training data

options(repr.plot.width = 8, repr.plot.height = 8)

prp(complex_tree, 
    type = 1,
    nn.border.col=0, 
    border.col=1, 
    cex=0.4)

# This tree retains some of the complexity of the previous model, it uses the
# same set of predictors, but it has some additional parameters that have been
# set. The maximum depth has been set to 5 levels and at least 5 observations
# must be left in the terminal nodes. These two settings prevent the tree from
# becoming too closely turned to the particular data.

limited_complexity_tree <- rpart(Survived ~ Sex + Pclass + Age + SibSp + Fare + Embarked,
                                 cp = 0.001,              # Set complexity parameter
                                 maxdepth = 5,            # Set maximum tree depth
                                 minbucket = 5,           # Set min number of obs in leaf nodes
                                 method = "class",        # Return classifications instead of probs
                                 data = titanic_train)    # Use the titanic training data

options(repr.plot.width = 6, repr.plot.height = 6)

prp(limited_complexity_tree,
    space=4,          
    split.cex = 1.2,
    nn.border.col=0)
