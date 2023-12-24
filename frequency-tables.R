# Frequency Tables: From DataDaft - https://www.youtube.com/watch?v=BX60iXae6k4

rm(list=ls())

# Frequency tables are a way to explore relationships between variables in a
# dataset. We'll use the titanic dataset here.

titanic_train <- read.csv("data/train.csv")

char_cabin <- as.character(titanic_train$Cabin)
new_cabin <- ifelse(char_cabin == "",
                    "",
                    substr(char_cabin,1,1))

titanic_train$Cabin <- factor(new_cabin)

# Create some one-way frequency tables of categorical variables contained in the
# dataset.
table(titanic_train$Survived)
table(titanic_train$Pclass)
table(titanic_train$Sex)
table(titanic_train$Cabin)

# These one-way tables give us some basic insight into the data by showing the
# distribution of observations across each level of the categorical variable.

cabin_table <- table(titanic_train$Cabin)
# The table object is a vector of integers, so we can use all of the typical
# vector functions and index access methods on a table.
typeof(cabin_table)
sum(cabin_table)
length(cabin_table)
cabin_table[2]
cabin_table[3:8]

# NA values are dropped from a table by default. Use the exclude parameter to
# include them.
age_table <- table(titanic_train$Age, exclude = NULL)
age_table

# This is a pretty messy output, because age is a numeric variable, but you can
# quickly see that there are 177 NA values in this feature.

# We can use exclude to remove specific values from a table. For example, in the
# Cabin data, we could exclude empty string values with the following.
table(titanic_train$Cabin, exclude = c(""))

# One-way tables great for determining proportions in the data.
gender_table <- table(titanic_train$Sex)
gender_table/sum(gender_table)
# This gives the proportion on male and female passengers on the ship.

# A more convenient way to do this is with prop.table
prop.table(gender_table)

### Two-way tables


survived_sex <- table(titanic_train$Survived, titanic_train$Sex)
survived_sex
# rename the rows so that they're easier to read.
rownames(survived_sex) <- c("died","survived") 

# This simple table gives us some useful information. We can quickly see that
# most of the males on board perished and that most females survived. This could
# be used as a simple baseline for assessing a predictive model's performance.

# We can use base R functions to perform basic computational tasks on tables.
rowSums(survived_sex)

colSums(survived_sex)

survived_sex[2,1] # we can also just access an index like we could with a matrix

# The prop.table function works very where here too.
prop.table(survived_sex)

# This output is a little funky to read. What is says is that 52% of
# observations are of males who died, 10% are females who died, 26% are females
# who survived, and 12% are males who survived.

# In general we are probably more interested in the proportion of those who died
# who were male or female and likewise for those who survive. We want the margin
# probabilities. Fortunately, prop.table as an argument for this.

prop.table(survived_sex, margin = 1) # get margin probabilities for the rows

# This shows us the marginal probabilities, which is what we're probably
# interested in. This shows us that males account for 85% of those who died
# while females accounted for 70% of those who survived.

prop.table(survived_sex, margin = 2) # get margin probabilities for the columns

## Higher dimensional tables
surv_sex_class <- table(titanic_train$Survived,
                        titanic_train$Sex,
                        titanic_train$Pclass)

dimnames(surv_sex_class)

# this is hard to interpret, so we'll rename the classes

dimnames(surv_sex_class)[[1]] <- c("died","survived")
dimnames(surv_sex_class)[[3]] <- c("class1","class2","class3")
surv_sex_class

# There are 3 tables now, one for each level of the Pclass variable, and the
# output is a bit easier to interpret now. This output shows that most females
# in 1st and 2nd class survived and that 50% of females in third class survived.
# The situation for males was not nearly so good. This tells us that sex and
# class are important predictors when trying to classify passengers as survivors
# or casualties.

# We can use prop.table on this data to understand the marginal probabilities in
# our table.
prop.table(surv_sex_class, margin = c(2,3))

#This output shows the probabilities for survival for the two sexes in each
#class aboard the Titanic.