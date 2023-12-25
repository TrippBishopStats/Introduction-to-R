# Probability Distributions: From DataDaft - https://www.youtube.com/watch?v=Rxe086csxTU

library(tidyverse)
theme_set(theme_minimal())

# Base R has many functions for handling probability distributions and they all
# a basic naming convention. Each distribution has a "nickname" that is prefixed
# by one of the following characters:
#
# r - for generating a random draw from the distribution.
# p - for the cumulative density (or mass) function of the distribution.
# d - for the probability density (or mass) function of the distribution.
# q - for the quantile that represents the value of the corresponding CDF.

## Uniform Distribution

# The random draw function
unif_data <- runif(1000000, 0, 10)

# This call tells R to pull 1 million random draws from a uniform distribution
# with a minimum of 0 and a maximum of 10.

plot(density(unif_data))

# cumulative density function.
punif(4, 0, 10)

# if we provide a quantile of 4, we will get the cumulative probability up to
# that quantile. In this case, that will be 0.4, so 40% of the values in the
# support will fall at or below this quantile.

# density function
dunif(4, 0, 10)   

# since this is the uniform distribution, any value between 0 and 10 will return
# a value of 0.1. Values outside of this range will return 0.

dunif(1.4, 0, 10)
dunif(9.889, 0, 10)
dunif(15, 0, 10)

# the anti-CDF of the uniform distribution
qunif(0.4, 0, 10)

# Here we provide the probability and the funtion will return the associated
# quantile. This function is like the inverse of punif.

## Sampling

random_ints <- sample(1:10,             # vector to sample from 
                      100000,           # number of samples
                      replace = TRUE)   # sample with/without replacement

table(random_ints)

# This is a good way to sample integers from a vector of integers. It can be
# used in conjunction with the set.seed function to make the output
# reproducible.

set.seed(12)
runif(5,1,5)

set.seed(12)
runif(5,1,5)

# Notice that the output of runif is the same for both calls. If we change one
# of the seeds, the results will not be the same, but will still be
# reproducible.

set.seed(12)
runif(5,1,5)

set.seed(3)
runif(5,1,5)
