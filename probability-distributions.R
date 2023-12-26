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

# The R nickname for the uniform distribution is "unif".

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

## The Normal Distribution

# The R nickname for the normal distribution is "norm".

normally_distributed <- rnorm(1000000, mean=0, sd=1)

# create a data frame containing density data for normal distribution
norm_frame <- with(density(normally_distributed), data.frame(x,y))

# Get prob of observing a value less than -1
prob_under_minus1 <- pnorm(q=-1,        
                           mean=0,
                           sd=1)

# Get prob of observing a value over 1
prob_over_1 <-  1-pnorm(q=1,            
                        mean=0,
                        sd=1)

# Prob between -1 and 1
between_prob <- 1-(prob_under_minus1+prob_over_1) 

norm_frame |> 
  ggplot(aes(x=x,y=y)) +
    geom_line() +
    geom_ribbon(data=subset(norm_frame, x < -1), 
                aes(ymax=y, ymin=0), 
                fill="red", 
                alpha=0.4) +
    geom_ribbon(data=subset(norm_frame, x > 1),
                aes(ymax=y,ymin=0),
                fill="red",
                alpha=0.4) +
    geom_ribbon(data = subset(norm_frame, x > -1 & x < 1),
                aes(ymax=y,ymin=0),
                fill="skyblue",
                alpha=0.4) +
    geom_text(x=-1.6,y=0.03, label=round(prob_under_minus1, 4), size=4) +
    geom_text(x=1.6,y=0.03, label=round(prob_over_1, 4), size=4) +
    geom_text(x=0,y=0.1, label=round(between_prob, 4), size=4) +
    xlim(-4,4)


qnorm(p = 0.025)      # Find the quantile at the 2.5% cutoff
qnorm(p = 0.975)      # Find the quantile at the 97.5% cutoff


## Binomial Distribution

# The R nickname for the Binomial distribution is "binom"
fair_coin_flips <- rbinom(1000000, # generate draws from binomal dist
                          size=10, # with 10 trials
                          prob=0.5) # success probability

table(fair_coin_flips)

hist(fair_coin_flips, breaks=seq(-0.5, 10.5, 1))

# Note how similar this result is to the normal distribution. This is the result
# of the probability of success being 0.5. The normal distribution approximates
# the binomial distribution to a fair degree of accuracy when n*p > 10 and
# n(1-p) > 10. The normal distribution to use is one where mu = np and sigma =
# sqrt(np(1-p)).

# If the probability is not 0.5, then there will be skewness in the
# distribution.

unfair_coin_flips <- rbinom(1000000, # generate draws from binomal dist
                          size=10, # with 10 trials
                          prob=0.8) # success probability

hist(unfair_coin_flips, breaks=seq(-0.5, 10.5, 1))

# Notice here how the results are skewed in favour of more successes. For
# example, it is extremely unlikely that 0 successes will be realised in 10
# trials of this unfair coin.

## Geometric distribution - It's a cousin of the binomial distribution. It is
## also a discrete distribution and it models the amount of time it takes for
## the first successful trial to occur given a specified probability of
## success. The nickname of the distribution in R is "geom".

set.seed(12)
flips_til_heads <- rgeom(n=1000000, prob=0.5) + 1

table(flips_til_heads)

hist(flips_til_heads, breaks=seq(-0.5, max(flips_til_heads)+0.5, 1))

# Here, we have counted the results from 1M experiments and tallied the number
# of Bernoulli trials required to get the first head in each experiment.

# The exponential distribution is the continuous version of the geometric
# distribution. It models the time until the first event based on a specified
# occurrence rate. The nickname in R for the exponential distribution is "exp".

exponential_data <- rexp(n=1000000, 
                         rate=1)     # Success/arrival rate

# Note that for the arrival rate, the mean arrival time is equal to 1/rate.

prob_longer_1 <- pexp(q=1,
                      rate=1,
                      lower.tail = FALSE)

prob_longer_1

exp_data <- with(density(exponential_data), data.frame(x,y))
exp_data |> 
  ggplot(aes(x=x,y=y)) +
  geom_line(linewidth=0.75) +
  geom_ribbon(data = subset(exp_data, x < 1),
              aes(ymax=y, ymin=0),
              fill = "skyblue",
              alpha = 0.4) +
  geom_ribbon(data = subset(exp_data, x >= 1),
              aes(ymax=y, ymin=0),
              fill = "red",
              alpha = 0.4) +
  geom_text(x=1.75, y=0.0625, label=round(prob_longer_1,2)) +
  geom_text(x=0.5, y=0.25, label=round(1-prob_longer_1,2)) +
  xlim(-0.5,6)

# Notice how the probability of the first event occurring before 1 unit of time
# is higher than that for the event happening after 1 unit of time has passed.

## Poisson Distribution

# The Poisson distribution is used to model the probability of X number of
# events occurring within an interval where the time to the next success is
# governed by the exponential distribution. The nickname for the Poisson
# distribution in R is "pois".

set.seed(12)
arrival_rate_1 <- rpois(n=1000000,
                        lambda = 1) #Arrival rate

table(arrival_rate_1)

# The frequency table and histogram show that 0 or 1 arrivals is by far the most
# likely, and that the probability drops off considerably after 4 arrivals.

hist(arrival_rate_1, breaks=seq(-0.5,max(arrival_rate_1) + 0.5, 1))

# If we increase the arrival rate, we get something that looks a lot more like a
# normal distribution, which is interesting because the Poission distribution is
# associated with the exponential distribution.
set.seed(12)
arrival_rate_1 <- rpois(n=1000000,
                        lambda = 10) #Arrival rate

table(arrival_rate_1)

# The frequency table and histogram show that 0 or 1 arrivals is by far the most
# likely, and that the probability drops off considerably after 4 arrivals.

hist(arrival_rate_1, breaks=seq(-0.5,max(arrival_rate_1) + 0.5, 1))
