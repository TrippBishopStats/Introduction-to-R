# Merging data - From DataDaft: https://www.youtube.com/watch?v=OOiZ-9jmizA

# Create some simulated data about patients

genders <- sample(c("male","female"), 8, replace=TRUE)

get_height <- function(x) {
  if(x == "male") {
    return(round(rnorm(1, 70, 4),0))
  } else {
    return(round(rnorm(1,63,2),0))
  }
}

get_weight <- function(x) {
  if(x == "male") {
    return(round(rnorm(1, 175, 25),0))
  } else {
    return(round(rnorm(1,125,25),0))
  }
}

table1 <- data.frame(
  P_ID = 1:8,
  gender = genders,
  height = sapply(genders, get_height),
  weight = sapply(genders, get_weight)
)

table2 <- data.frame(
  P_ID = c(1,2,4,5,7,8,9,10),
  sex = c("female", "female", "male", "male", "female", "male", "male", "female"),
  visits = c(1,2,4,12,2,2,1,1),
  checkup = c(1,1,1,1,1,1,0,0),
  follow_up = c(0,0,1,2,0,0,0,0),
  illness = c(0,0,2,7,1,1,0,0),
  surgery = c(0,0,0,2,0,0,0,0),
  ER = c(0,1,0,0,0,0,1,1)
)

# This is an inner join. Note that some patient records will be missing because
# they're not in both dataframes.
combine_data <- merge(x=table1, y=table2, by = "P_ID")

# we can do a left join to keep all patient records from table 1, but there will
# be some missing data for patients 3 & 6.
left_join <- merge(
  x=table1,
  y=table2,
  by="P_ID",
  all.x =TRUE
)
View(left_join)

# We can do a right join to keep all patient records from table2, but there will
# be missing data for patients 9 & 10.
right_join <- merge(
  x=table1,
  y=table2,
  by="P_ID",
  all.y =TRUE
)
View(right_join)

# A full (outer) will keep all records from both tables but the unique patient
# records from each table will have missing data.
full_join <- merge(
  x=table1,
  y=table2,
  by="P_ID",
  all = TRUE
)

# Now we're going to merge the tables again, but we're going to use both P_ID
# feature and the gender/sex features. These two features represent the same
# data, so merging on them is a way to condense down to one feature.

combined_data2 <- merge(
  x=table1,
  y=table2,
  by.x = c("P_ID","gender"),
  by.y = c("P_ID", "sex"),
  all = TRUE
)

# In this case the gender column is kept and sex is dropped.
