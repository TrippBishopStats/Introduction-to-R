# Chi-squared test: From DataDaft - https://www.youtube.com/watch?v=Ap1Kb-86Voc

rm(list=ls())

# Chi-squared goodness of fit The goodness of fit test is used to determine if
# sample categorical data matches some expected distribution.

# create some sample data. For categorical variables, we have to use the
# distribution of the counts of the observations in each category as the
# category labels have no mathematical meaning.

national_demographics <- c(rep("white",100000),      # Fake demographic data
                           rep("hispanic",60000),
                           rep("black",50000),
                           rep("asian",15000),
                           rep("other",35000))

minnesota_demographics <- c(rep("white", 600),      # Fake sample data
                            rep("hispanic", 300),
                            rep("black", 250),
                            rep("asian", 75),
                            rep("other", 150))

table(national_demographics)            # Check counts

table(minnesota_demographics)


# get the observed counts

observed <- table(minnesota_demographics)

# Get population ratios
national_ratios <- prop.table(table(national_demographics))

# Get expected counts
expected <- national_ratios * length(minnesota_demographics)

expected        # Check expected counts

# Calculate the statistic
chi_squared_statistic <- sum(((observed-expected)^2)/expected)

chi_squared_statistic

# Now, we compare this statistic to the quantiles of the appropriate chi-squared
# distribution.
qchisq(p=0.95, df=4) # we have 5 groups, so df = 5 - 1

# The above quantile is the critical value, so our computed statistic is way
# outside of this value. To compute our p-value, we can subtract the pchisq
# probability for our statistic from 1.
1 - pchisq(chi_squared_statistic, df=4) # or
pchisq(chi_squared_statistic, df=4, lower.tail = FALSE)

# It's a very small value, so we conclude that the sample data does not fit the
# distribution of the population.

# The R chisq.test() function makes this much easier.
chisq.test(x=observed,          # table of observed counts
           p=national_ratios)   # expected proportions

# These are the same results we got doing the test manually.

## Chi-Squared Test of Independence

# We can also test if two categorical variables are independent of each other.
# For example, there should be no correlation between your birth month and your
# race. Below, we'll create fictitious voter demographic data. Notice that the
# two vectors we create are not connected. Neither one informs the other so we
# expect that the test for independence should reflect this fact.

set.seed(12)

# Generate race data
voter_race <- sample(c("white", "hispanic", 
                       "black", "asian", "other"),                
                     prob = c(0.5, 0.25 ,0.15, 0.05, 0.15), 
                     size=1000,
                     replace=TRUE)

table(voter_race)         # Check counts

# Generate party data
voter_party <- sample(c("democrat","republican","independent"),  
                      prob = c(0.4, 0.4, 0.2), 
                      size=1000,
                      replace=TRUE)

voter_table <- table(voter_race, voter_party) 
voter_table

chisq.test(x=voter_race,
           y=voter_party)

# This result shows us that the two datasets are, in fact, independent.