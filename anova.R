# ANOVA: From DataDaft - https://www.youtube.com/watch?v=FLyCZnjuJVA

rm(list=ls())

# ANOVA (analysis of variance) is a way to look for differences between many
# groups without doing multiple pairwise t-tests. The problem with doing so many
# t-test is that the likelihood of encountering a false positive increases
# quickly. The one-way ANOVA solves this problem.

set.seed(12)

# Generate some demographic data
voter_race <- sample(c("white", "hispanic", 
                       "black", "asian", "other"),              
                     prob = c(0.5, 0.25 ,0.15, 0.1, 0.1), 
                     size=1000,
                     replace=TRUE)

# Generate age data (equal means)
voter_age <- rnorm(1000,50,20)                  
voter_age <- ifelse(voter_age<18,18,voter_age)

#NOTE: The two sets of data were generated independently, so we expect that we
#will not see a difference between groups.

# Conduct the ANOVA and store the result
av_model <- aov(voter_age ~ voter_race)  
# Check a summary of the test result
summary(av_model)

# That is indeed the case. The p-value is quite large. Now, let's modify one of
# the groups to have a different mean age.

set.seed(12)

# Draw ages from a different distribution for white voters
white_dist <- rnorm(1000,55,20)
white_dist <- ifelse(white_dist<18,18,white_dist)

new_voter_ages <- ifelse(voter_race == "white", white_dist, voter_age)

# Conduct the ANOVA and store the result
av_model <- aov(new_voter_ages ~ voter_race)   
# Check a summary of the test result
summary(av_model)

# Now, the white voters age is generated from a different distribution and we
# detect that difference at the 95% confidence level. We know if was the white
# voters who are different because we "cooked" the data. In a real world
# example, we wouldn't know this and so we would need to perform a post-hoc test
# to determine which groups are different.

pairwise.t.test(new_voter_ages,
                voter_race,
                p.adjust.method = "none") # don't adjust for multiple tests.

# The resulting table shows that the white group seems different to all groups
# except the other group (p-value is small, but not significant at the 95%
# level).

# we can limit the probability of a false positive, due to multiple tests, by
# using an adjustment method. The Bonferroni correction to account for this.
pairwise.t.test(new_voter_ages,
                voter_race,
                p.adjust.method = "bonferroni") # use bonferroni correction.

# Using this correction method, only the hispanic/white groups show a
# significant difference. Bonferroni adjusts the correction by dividing the
# alpha by the number of groups, in this case there are 5. That means we are now
# using a hypothesis test with alpha=0.01. Bonferroni is a pretty conservative
# adjustment and it can lead to false negatives.

# The Tukey method is less conservative than Bonferroni. It's very easy to
# implement in R.

TukeyHSD(av_model)

# Using this method, see that white/asian is close to significant, but the
# white/hispanic is still significant. These results are not a slamdunk, so in a
# real world scenario, this test would serve as a starting point for further
# investigation into if the white group is truly different to the others.

## Two-way ANOVA

# A two-way ANOVA test allows us to see how two categorical variables influence
# a numerical outcome variable. We can also test for interactions between the
# two categorical variables to see if the interaction is important in influencing
# the outcome variable.

# Generate simulated gender data.

set.seed(10)
voter_gender <- sample(c("male","female"),  # Generate genders
                       size=1000, 
                       prob=c(0.5,0.5),
                       replace = TRUE)

# Alter age based on gender
voter_age2 <- ifelse(voter_gender=="male", voter_age-1.5, voter_age+1.5) 
voter_age2 <- ifelse(voter_age2<18,18,voter_age2)

# the anova function uses formulas like many other R functions.
av_model <- aov(voter_age2 ~ voter_race + voter_gender)    # Perform ANOVA
summary(av_model)         

# This output suggests that race is less important in determining voter age than
# gender. Now we can test for interactions.
av_model <- aov(voter_age2 ~ voter_race + 
                             voter_gender + 
                             voter_race*voter_gender
                )
summary(av_model)

# based on the output, it doesn't look like there is any interaction. This makes
# sense based on the way the simulated data was created.

# Increase the age of Asian female voters by 10
interaction_age <- ifelse((voter_gender=="female")&(voter_race=="asian"), 
                          (voter_age + 10), voter_age)

# Repeat the test
av_model <- aov(interaction_age ~ voter_race + voter_gender +     
                  (voter_race * voter_gender))                       

summary(av_model)

# Now we can see that the race predictor is significant and that the interaction
# term is too. We can't tell from this model what group or groups are causing
# significant differences, but we can use the TukeyHSD function to gain
# insights.

TukeyHSD(av_model)

# This output shows us that almost every interaction involving Asian females
# results in a low p-value, so we would want to investigate further. Plotting
# our data would be a great way to visualise what's going on which might be a
# very quick way to grasp what is going on.