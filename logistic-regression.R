# Logistic Regression: From DataDaft - https://www.youtube.com/watch?v=rSCCic0X0UE

rm(list = ls())

# Logistic regression is generally used as a binary classifier. It is a tool for
# making predictions about which of two outcomes an observation can belong in. A
# threshold is applied to then classify an observation based on the probability
# that it belongs in the "positive" class. The logistic function is used to
# determine the probabilities.

library(ggplot2)

sigmoid <- function(t){ 1/(1+exp(-t)) }     # Define the sigmoid function

dummy_frame <- data.frame(x=c(-6,6))

ggplot(data=dummy_frame) +                  # Plot the function 
  stat_function(fun=sigmoid, linewidth=1) +
  geom_hline(yintercept = 0.5, linetype="dashed", linewidth=1, colour="grey40") +
  annotate("text", x=-3,y=0.525,label="Threshold Probability") +
  xlim(-6,6)

titanic_train <- read.csv("data/train.csv")

titanic_train$PassengerId  <- NULL             # Remove PassengerId
titanic_train$Ticket  <- NULL                  # Remove Ticket
titanic_train$Name <- as.character(titanic_train$Name)    # Convert name to character

titanic_train$Pclass <- ordered(titanic_train$Pclass,     # Convert to ordered factor
                                levels=c("3","2","1"))  

# Reduce cabin factor levels
char_cabin <- as.character(titanic_train$Cabin)     

new_Cabin <- ifelse(char_cabin == "",          
                    "",                        
                    substr(char_cabin,1,1))    

new_Cabin <- factor(new_Cabin )                
titanic_train$Cabin <- new_Cabin

# we have a lot of NA values (177) in age. Logistic regression cannot handle
# missing values so we have two options: throw out the observations with missing
# data or impute the missing values. Given that nearly 20% of our records are
# missing an age, we need to choose the second option.

impute <- preProcess(titanic_train[,c(5:8)],  # Impute missing ages*
                     method=c("knnImpute"))

titanic_train_imp <- predict(impute, titanic_train[,c(5:8)])     

titanic_train <- cbind(titanic_train[,c(1:4)], titanic_train_imp, titanic_train[,c(9:10)])

# Note: we use only use the numeric variables, Age, sibSp, Parch and Fare for
# imputation because the impute function we learned only accepts numeric
# variables. If we wanted more accurate imputation, we could convert categorical
# variables like Sex and Pclass into several numeric 0,1 numeric indicator
# variables (dummy variables.). Converting a factor into 0,1 dummy variables is
# known as one-hot encoding. Also note that the preProcess() function scales and
# centers the data before imputation.

titanic_model <- glm(Survived ~ Sex,      # Formula
                     data= titanic_train, # Data set
                     family="binomial")   # family="binomial" for binary logistic

summary(titanic_model) 

# This shows us that males are less likely to survive than females since the
# Sexmale coefficient is negative.

# We can use our new model to make predictions and a confusion matrix to
# evaluate the results.
train_preds <- predict(titanic_model,           # Model to use
                       newdata=titanic_train,   # Data to use for predictions
                       type="response")         # Return predicted probabilities

table(train_preds, titanic_train$Sex)

# This simple model shows that based on Sex alone, females have a 74% of
# survival while males have a 19% chance of survival. So if we set our threshold
# at 0.5 (the default), what this model predicts is that all females survive and
# all males dies.This is a good baseline model, it's simple but Sex is clearly
# an important predictor.

titanic_model <- glm(Survived ~ Sex + Pclass + Age + SibSp + Cabin,
                     data= titanic_train,       
                     family="binomial")         

predicted_probabilities <- predict(titanic_model,              
                                   newdata=titanic_train,      
                                   type="response")            

summary(titanic_model)


# Convert to 0, 1 preds
class_preds <- ifelse(predicted_probabilities >= 0.5, 1, 0)  

# Make a table of predictions vs. actual
result_table <- table(class_preds,             
                      titanic_train$Survived)  

result_table

(472+238)/sum(result_table)

confusionMatrix(data = factor(class_preds), 
                reference = factor(titanic_train$Survived),
                positive = "1")      # Set the positive class to Survived
