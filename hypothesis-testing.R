# Hypothesis testing: From DataDaft - https://www.youtube.com/watch?v=4eNkmnxFwwQ

rm(list=ls())

# Hypothesis testing is a framework that can be used to determine if an observed
# sample deviates from what is expected. R has a number of tools for performing
# hypothesis tests on data. To begin, we will create a population of national
# voter ages and then a separate population representing Minnesota voters. We
# can then ask the question "is the mean age of these population the same?". The
# null hypothesis states that "nothing is going on here", and in this case, that
# means that there is no difference between the means of the two poulations. The
# alternate hypothesis is that there is a difference between the means of the
# two populations.

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


set.seed(12)

# Generate dummy sample age data
minnesota_ages <- c(rexp(100,0.015)+18,      
                    rpois(50,15)+18,
                    rpois(50,25)+18,
                    rpois(50,35)+18)

minnesota_ages <- ifelse(minnesota_ages<100, minnesota_ages, 
                         minnesota_ages%%100+18)

mean(minnesota_ages)

## One-sample T-test

# Tests whether a sample mean differs from a known population mean.

t.test(minnesota_ages,               # Sample data
       mu = true_mean,               # population mean
       alternative = "two.sided",    # computing a two sided test here
       conf.level = 0.95)            # level of significance we want to test at.

# We are looking to see where our sample mean falls in the range of quantiles.

# The confidence interval is constructed using a margin of error based on a
# t-distribution with 249 degrees of freedom b/c our sample size is 250. The
# quantile can be computed with the qt() function.

# These two quantiles represent the t-critical values in our test. In our test,
# the test statistic was determined to be -2.4506.
qt(p=0.025, df=249) # these quantiles are very similar to that of normal dist.
qt(p=0.975, df=249) # due to the large sample size.

# using our test statistic, we get a probability of 0.015. We multiply to by 2
# because we're doing a two-tailed test.
pt(q=-2.4506, df=249)*2

# If we increase the confidence level to 99%, will we still find a significant
# result?

t.test(minnesota_ages,
       mu=true_mean,
       conf.level = 0.99)

# Here the true mean IS in the confidence interval and so we cannot reject the
# null hypothesis at the alpha=0.01 level.


## Two-sample T-Test

# We can also compare two samples to each other to determine if there is a
# significant difference between them. Again, we choose our alpha value to
# determine the confidence level that we want to conduct the test at.

set.seed(12)

# Generate more dummy sample age data
wisconsin_ages <- c(rexp(50,0.015)+18,      
                    rpois(50,20)+18,
                    rpois(50,32.5)+18,
                    rpois(50,45)+18)

wisconsin_ages <- ifelse(wisconsin_ages<100, wisconsin_ages, 
                         wisconsin_ages%%100+18)

t.test(x=minnesota_ages,
       y=wisconsin_ages,
       conf.level = 0.95)

# In the output of the test, note that 0 is not contained in the confidence
# interval. In a two sample test, the test compares differences, so if the
# confidence interval includes 0, the null hypothesis cannot be rejected.

## Paired T-Test

# The standard t test compares samples that are assumed to be independent. In
# certain types of experiments, this is not the case. For example, a weight-loss
# programme might be tested and so subjects are tested before and after the
# programme. Clearly, these results are not independent b/c the subjects are the
# same in each measurement. A paired t-test can be used in a situation like
# this.

set.seed(80)

# Generate weights with mean 250lbs
before_treatment_weights <- rnorm(100,250,30)   

after_treatment_weights <- (before_treatment_weights + rnorm(100,-1.25,5)) 

# Pair the data in a data frame
weight_df <- data.frame(before=before_treatment_weights, 
                        after=after_treatment_weights,
                        change=after_treatment_weights-before_treatment_weights)

# Check a summary of the data
summary(weight_df)

t.test(before_treatment_weights,
       after_treatment_weights,
       conf.level= 0.95,
       paired=TRUE)

# The results of this test indicate that difference in sample means is very
# likely to be different and that the effect is real.

# to determine the power of a test that can detect specified differences in
# numeric variables, the power.t.test function can be used.
power.t.test(n=100,
             delta = 1.25,
             sd = 5,
             sig.level = 0.05,
             type = "paired")

# The power of this test is 70%, which is not great. This means that we have a
# 30% chance of making type 2 errors, false negatives. Basically the test might
# miss real differences in groups 30% of the time. To improve the power, we
# could increase the sample size, or accept a large delta between groups.

power.t.test(n=100,
             delta = 1.75,
             sd = 5,
             sig.level = 0.05,
             type = "paired")
