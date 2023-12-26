# Confidence intervals - From DataDaft: https://www.youtube.com/watch?v=NynxWXP_AU0
rm(list=ls())

# We can investigate sampling and making point estimates by first create a
# simulated population. Here we're considering the scenario where we have a
# population of 2.5 million voters. We begin by creating the entire population.

set.seed(12)

# Generate a population
population_ages <- c(rexp(1000000,0.015)+18,   
                     rpois(500000,20)+18,
                     rpois(500000,32.5)+18,
                     rpois(500000,45)+18)

# correct for voters whose ages are over 100
population_ages <- ifelse(population_ages<100, 
                          population_ages, population_ages%%100+18)


true_mean <- mean(population_ages)   # Check the population mean

sprintf("The true population mean is %0.5f", true_mean)

# Now, we will draw a sample from this population and compare its mean to that
# of the population.
set.seed(12)
sample_ages <- sample(population_ages, size=1000)
sample_mean <- mean(sample_ages)
sprintf("The sample mean is %0.5f", sample_mean)
sprintf("The difference between the sample and population means is %0.5f", sample_mean-true_mean)

# This sample mean is pretty close to the true mean, but it's definitely not
# exact. We can do another demonstration using simulated demographic data.
set.seed(12)

# Generate some dummy demographic data
population_races <- c(rep("white",1000000), 
                      rep("hispanic",500000),
                      rep("black",500000),
                      rep("asian",250000),
                      rep("other",250000))

demographic_sample <- sample(population_races, size=1000)   # Take a sample

# Print the estimated proportion for each, unique will create a vector of all of
# the distinct values for race.
for (race in unique(demographic_sample)){       
  print(paste(race," proportion estimate:"))       
  print(sum(demographic_sample==race)/1000)   
}

# These point estimates are close, but again, it is clear that they are not
# exact, there is some sampling error here.

# we can improve our estimate by leveraging the central limit theorem.
library(e1071)

hist(population_ages, breaks = 20)

#note, this population isn't quite normal, there is a distinct tail. We can
#quantify the skewness in the data with the skewness function from the e1071
#package.
skewness(population_ages)

# The data are moderately, skewed. Let's check our sample's skewness
skewness(sample_ages)

# This data has a similar skew. A normally distributed random variable has a
# skewness of 0, so values for skewness near 0 indicate a normally distributed
# dataset.

# We'll create 200 samples, each with 1000 observations. We will then compute
# and store the mean for each sample and then we will analyse the distribution
# of these sample means. The distribution of the sample means, also known as the
# sampling distribution, should be close to normal due to the central limit
# theorem.
set.seed(12)
num_samples <- 1000
point_estimates <- numeric(num_samples)

for(i in 1:num_samples) {
  sample_ages <- sample(population_ages, size=1000)
  point_estimates[i] <- mean(sample_ages)
}

plot(density(point_estimates))
skewness(point_estimates)

# This result shows that the sampling distribution is close to normally
# distributed. And the more sample means we generate the closer to normal the
# result will be. Increasing the sample size would also improve the result.

mean(point_estimates) - true_mean

# This result shows that the mean of the point estimates is much closer to the
# true population mean than the mean of a single sample.

# The downside is that taking loads of samples, particularly ones with many
# observations is often not practice. It can be expensive and time consuming, so
# most likely, a bootstrap approach would be used. This technique will help
# uncover the spread of the data.

## Confidence intervals

# The confidence interval uses the sample mean and knowledge of the standard
# deviation in the population to build a region about the sample mean that is
# likely to contain the true mean. The larger the confidence level we want, the
# wider the confidence interval we need to create.

# The margin of error is used to create the confidence interval about the sample
# mean, it is computed using the z-statistic that represents the desired level
# of confidence, the population standard deviation, and the sample size.

# generate our sample


# Capture the quantile that encompasses 97.5% of density of the standard normal distribution.
set.seed(10)
z_critical <- qnorm(0.975)
sample_size <- 1000
sample_ages <- sample(population_ages, size=sample_size)
sample_mean <- mean(sample_ages)
pop_stdev <- sd(population_ages)
margin_of_error <- z_critical*pop_stdev/sqrt(sample_size)
conf_interval <- c(sample_mean - margin_of_error,
                   sample_mean + margin_of_error)
sprintf("The 95%% confidence interval is [%0.2f, %0.2f]", 
        conf_interval[1], 
        conf_interval[2])

# we can do this exercise a number of times to demonstrate the idea of the
# confidence interval "capturing" the true mean.

intervals <- numeric(50)
set.seed(12)
for(i in 1:25) {
  sample_ages <- sample(population_ages, size=sample_size)
  sample_mean <- mean(sample_ages)
  conf_interval <- c(sample_mean - margin_of_error,
                     sample_mean + margin_of_error)
  j <- 2*i - 1
  intervals[j] <- conf_interval[1]
  intervals[j+1] <- conf_interval[2]
}

interval_df <- data.frame(matrix(intervals,25,2,byrow = TRUE))
library(ggplot2)
interval_df |> 
  ggplot(aes(x=1:nrow(interval_df))) +
    geom_errorbar(aes(ymax = X2, ymin = X1)) +
    geom_point(aes(y=rowMeans(interval_df)), shape=1, size=3) +
    geom_abline(intercept=true_mean, slope=0,color="red",lwd=1) +
    ylab("Interval Range (Red Line=True Mean)") +
    xlab("Interval Number")

# This plot shows how the population mean falls in all of the 25 confidence
# intervals except 1 which does not.

# If the population standard deviation is unknown, which is often the case, then
# we must use the t-distribution instead of the normal distribution to build the
# confidence interval. We use the standard deviation of the sample instead of
# the population and use a t critical value instead of a z critical value.
sample_size <- 25
smaller_sample <- sample(population_ages, size=sample_size)
t_critical <- qt(0.975, df=sample_size-1)
sample_stdev <- sd(smaller_sample)
sample_mean <- mean(smaller_sample)

margin_of_error <- t_critical*sample_stdev/sqrt(sample_size)
conf_interval <- c(sample_mean - margin_of_error,
                   sample_mean + margin_of_error)
sprintf("The 95%% confidence interval is [%0.2f, %0.2f]", 
        conf_interval[1], 
        conf_interval[2])

# Notice that this confidence interval is a lot wider than the previous ones.
# This is largely due to the fact that the sample size is so much smaller. The
# smaller sample size as a big effect on the margin of error.