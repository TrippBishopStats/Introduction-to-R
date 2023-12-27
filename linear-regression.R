# Linear Regression in R: From DataDaft - https://www.youtube.com/watch?v=NTGw6AIM8H4

rm(list=ls())

library(ggplot2)
library(broom)

# Root mean squared is a method that can be used to compare different models for
# the same data. The actual value producted by the funciton doesn't have any
# intrinsic meaning. What is important is how different model residuals compare
# to each other. Smaller values are better.
root_mean_squared <- function(predicted, targets) {
  sqrt(mean((targets - predicted)^2))
}

ggplot(data=mtcars, aes(x=wt, y=mpg)) +
  geom_point()

mpg_model <- lm(mpg ~ wt,       # Formula of the form response ~ explanatory
                data=mtcars)    # Data set to use

summary(mpg_model)              # View a summary of the regression model
tidy(mpg_model)                 # A tidyverse approach to viewing the results.

# plot the model fit.

ggplot(data=mtcars, aes(x=wt, y=mpg)) +
  geom_point() +
  geom_abline(slope = -5.3445, intercept = 37.2851, colour="red", linewidth=1)

# plotting the residuals is an important step in model evaluation. Ideally,
# there will be no pattern to the residuals and they will be normally
# distributed.

# There looks to be a curvature in the residuals that indicates that there is
# more signal in the data that we could be capturing with a better model.
plot(mpg_model$residuals)

# The distribution of the residuals not great, but it's not terrible.
hist(mpg_model$residuals)

# This QQ plot shows that the model fit is really good in the "heart" of the
# data, but at the extremes it doesn't perform well. This is another indication
# that we should look for a better fitting model.
qqnorm(mpg_model$residuals)
qqline(mpg_model$residuals)

# compute the RMSE for this model so that we can compare it to other models.
lm_preds <- predict(mpg_model, newdata = mtcars)
root_mean_squared(lm_preds, mtcars$mpg)

# The caret package has a built in RMSE function that we can use.
library(caret)

RMSE(lm_preds, mtcars$mpg)

# The RMSE for this model is 2.95 (using either function) and this will serve as
# our baseline to which we can compare other models.

# A side note about outliers and leverage. This quick example shows how outliers
# that are far from the other data can have a massive influence on a linear
# model. Adding a super car that is much heavier than the other cars in the
# dataset puts it far from the other data on the independent axis. This data
# point is also a big outlier in the dependent variable, so it exerts a lot over
# "leverage" on the model fit.

super_car <- c(50,10)                                  # Outlier car 
new_cars <- rbind(mtcars[,c("mpg","wt")], super_car)   # Add outlier to mtcars

ggplot(data=new_cars, aes(x=new_cars$wt, y=new_cars$mpg))+
  geom_point() +
  geom_smooth(method=lm,      # Add a linear regression line
              se=FALSE,       # Don't add shaded confidence region
              color="red")

# Notice how in this example, a car with a similarly extreme mpg but a much more
# typical weight doesn't influence the model nearly as much. The influence is
# there, if you compare the model to the original dataset, but the change in
# slope and intercept is much less pronounced.

super_car <- c(50,3)                                  # Outlier car 
new_cars <- rbind(mtcars[,c("mpg","wt")], super_car)  # Add outlier to mtcars

ggplot(data=new_cars, aes(x=new_cars$wt, y=new_cars$mpg))+
  geom_point() +
  geom_smooth(method=lm,      # Add a linear regression line
              se=FALSE,       # Don't add shaded confidence region
              color="red")


## Polynomial regression

# Linear models can, perhaps surprisingly, have non-linear elements in them. The
# model is linear in the coefficients, not the predictors, so we can raise the
# predictors to powers if we wish. We can also create interaction terms by using
# the product of two predictors.

# Here, we'll create a really simple quadratic model using the square of weight.
quadratic_model <- lm(mpg ~ wt + I(wt^2), data = mtcars)

summary(quadratic_model)

# Right away, we can see that the R squared has improved quite a bit by the
# addition of the quadratic term.

# Create a function based on the model
quadratic_function <- function(x){        
  49.9308 + -13.3803*x + 1.1711*x^2
}

# Let's plot the fit of the data.
ggplot(data=mtcars, aes(x=wt, y=mpg)) +
  geom_point() +
  # Plot the function
  stat_function(fun=quadratic_function, color="red", linewidth=1)  


# Now take a quick look at the diagnostics for this new model.
plot(quadratic_model$residuals)

hist(quadratic_model$residuals)

quad_preds <- predict(quadratic_model, newdata = mtcars)
RMSE(quad_preds, mtcars$mpg)

# The RMSE has improved a bit with this model. Overall, this model is an
# improvement, but we might improve it by adding adding more predictors, but
# first, a cautionary tale.

# Simply adding higher order terms will allow the model to fit the data points
# more closely, but this can often lead to models that fit the noise and are not
# generalisable. The following model is a good demonstration of these.

tenth_order_model <- lm(mpg ~ wt + I(wt^2) + I(wt^3) + I(wt^4) + I(wt^5) +
                          I(wt^6) + I(wt^7) + I(wt^8) + I(wt^9) + +I(wt^10),
                        data=mtcars)

tenth_order_model$coefficients                  # Check model coefficients

summary(tenth_order_model)

# Create a function based on the model coefficients
model_function2 <- function(x){    
  -14921.1256679509 + 64581.3723400433*x - 
    120086.155632163*x^2 + 126931.950142918*x^3 - 
    84659.8581288786*x^4 + 37315.5247656017*x^5 - 
    11033.4768062176*x^6 +  2165.90426790932*x^7 - 
    270.730570029588*x^8 + 19.4974178769716*x^9 - 
    0.61551548538557*x^10
}

ggplot(data=mtcars, aes(x=wt, y=mpg)) +               
  geom_point() +
  stat_function(fun=model_function2, color="red", linewidth=1)

preds <- predict(tenth_order_model, newdata = mtcars)
RMSE(preds, mtcars$mpg)

# Notice that we have knocked the RMSE down even more, but the model is clearly
# overfit and makes ridiculous, unrealistic predictions for weights between
# about 4 and 5.5 thousand pounds. The Runge phenomenon is a play here and it is
# a big reason why trying to fit high-degree polynomials to data is generally a
# bad idea.

## Multiple Linear Regression Now we'll introduce more predictors to our model.
# Here, we'll also include horsepower as a predictor.
mpg_model <- lm(mpg ~ wt + hp, # add horsepower
                data=mtcars)

summary(mpg_model)

# This looks promising. We can see that the R squared has improved quite a bit
# and the p-values of the predictors are small.

# This implies that hp has a linear relationship with mpg. Let's examine this.

ggplot(data=mtcars, aes(x=hp,y=mpg)) +
  geom_point()

# mpg certainly decreases has hp increases but there appears to be a some
# curvature to the trend, so adding a quadratic term might be a reasonable next
# step.

quadratic_model <- lm(mpg ~ wt + hp + I(wt^2) + I(hp^2), data = mtcars)

summary(quadratic_model)

# accessing model performance with RMSE
sqrt(mean((quadratic_model$residuals)^2))

# This model looks like the best fit yet. Let's take a quick look at some
# diagnostics.
plot(quadratic_model$residuals)
hist(quadratic_model$residuals)

qqnorm(quadratic_model$residuals)
qqline(quadratic_model$residuals)

# The residuals look more random now and the QQplot is pretty good. We seem to
# be fitting some of the more extreme observations better.

# A final model. Simply throwing the kitchen sink at the data is generally not
# a good idea. Not only is not parsimonious, but it often leads to poorer
# performance than a carefully considered model.

mpg_model <- lm(mpg ~ .,         # use all predictors
                data = mtcars)

summary(mpg_model)

# Notice that adjusted R-squared is considerable lower than the R-squared. The
# model is getting penalised significantly for including so many predictors.
# None of the predictors appear to be significant in this model.