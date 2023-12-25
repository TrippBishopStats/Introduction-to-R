# Descriptive Statistics: From DataDaft - https://www.youtube.com/watch?v=tPqt8QUg1U0
rm(list=ls())
# Measures of centre. Values that are typical or usual in a population.

# Base R has lots of tools for computing the mean of data.

cars <- mtcars

# compute the mean of a vector of data.
mean(cars$mpg)

# get the means for all columns in a dataframe
colMeans(cars)

# get the mean for each row of data.
head(rowMeans(cars))

# can also compute median easily.
median(cars$mpg)

# there is no colMedians function in base R, but the apply function can get us
# what we need.

colMedians <- apply(cars, 
                    MARGIN = 2,   # operate on columns
                    FUN = median) # use the median function
colMedians

## Measures of spread

# The range of a variable is the most basic measure of spread.
max(cars$mpg) - min(cars$mpg)
rng <- range(cars$mpg)
rng[2] - rng[1]

# We can also compute quantiiles on the data.
quantile(cars$mpg)

# Get the five number summary
fivenum(cars$mpg)

#Summary shows the 5 number summary plus the mean
summary(cars$mpg)

# can also use quantile with specific quantiles
quantile(cars$mpg, probs = c(0.1,0.9)) # get 10th and 90th percentiles.

# Interquartile range, IQR, gives you the range between the 25th and 75th
# percentiles.
IQR(cars$mpg)

# The good ole variance
var(cars$mpg)

# and the standard deviation
sd(cars$mpg)

# Both the variance and standard deviation are susceptible to outliers. The
# median absolute deviation is an alternative measure of spread based on the
# median which inherits the median's robustness to outliers.
mad(cars$mpg)

