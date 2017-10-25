### Cleaning data - Restructuring with tidyr

library(tidyr)
library(dplyr)


bugs = Bug.Frequency

head(bugs)

names(bugs) <- c("Region", "<10 g", "10-20 g", "20-30 g", "30-40 g", ">40 g")

bugs

?gather


gather(data = bugs, key = weight, value = counts, -Region)
# we are specifying the dataset, our key are the different weights in one column
# we put all the counts in 1 column, and we suppress a gathering of Region

# we can store it as a table
bugs.table = tbl_df(gather(data = bugs, key = weight, value = counts, -Region))

# we can of course change the order with the arrange function
arrange(bugs.table, Region)

## alternative with package reshape2

library(reshape2)

bugs.melt = melt(data = bugs, measure.vars = c(2:6), # here we use col IDs
                 variable.name = "weight",
                 value.name = "counts"); bugs.melt

## too much information in 1 column

age.sex <- data.frame(
  name = c("Paul", "Kim", "Nora", "Sue", "Paul", "Kim"),
  biometrics = c("179m", "173f", "174f", "159f", "188m", "163f"),
  measurement = rnorm(6)); age.sex

# here the column biometrics contains height in cm and sex at the same time
# this requires separation in many cases

# we can use separate to split the biometrics column in 2
separate(data = age.sex, col = biometrics,
         into = c("height", "sex"), 3)
# number 3 indicates split, pos starts from left, - from right

# What can you do if you have two variables in the same column
# going from long form to wide form

sports <- data.frame(
  name = c("Paul", "Paul", "Nora", "Nora", "Kim", "Kim"),
  performance = c("top", "low", "top", "low", "top", "low"),
  counts = c(11,3,18,2,9,1)); sports

# here we have 2 variables the top and low performance in 1 column
# how do we split it up?

spread(data=sports, key = performance, value = counts)

## lets check the alternative from the reshape2 package

# the argument names differ a bit, but the function structure looks very similar
dcast(data = sports, name ~ performance, value.var = "counts" )
