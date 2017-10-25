install.packages('data.table')
install.packages('tidyr')
install.packages('reshape2')
install.packages('dplyr')
install.packages('ggplot2')

# Data Import

numbers = scan()

characters = scan(what = "character")

library(data.table)

mydata = fread("Bug Frequency.csv")

mydata

df = fread(" ")

df

df = fread(" ",
           col.names =c("Rank", "Country", "Population", "Area", "Density"))

df; class(df)

### Comparison of classes



# activation
library(data.table)

set.seed(34)

# standard data.frame from R base
mydf = data.frame(a = c("Paul", "Kim", "Nora", "Sue", "Paul", "Kim"),
                  b = c("A", "A", "B", "B", "B", "C"),
                  c = rnorm(2)); mydf

head(mydf)

sapply(mydf, class)

# data.table from the package

library(data.table)

set.seed(34)

mytable = data.table(a = c("Paul", "Kim", "Nora", "Sue", "Paul", "Kim"),
                     b = c("A", "A", "B", "B", "B", "C"),
                     c = rnorm(2)); mytable

head(mytable)

sapply(mytable, class)

# data_frame

library(dplyr)

my_df = data_frame(a = c("Paul", "Kim", "Nora", "Sue", "Paul", "Kim"),
                   b = c("A", "A", "B", "B", "B", "C"),
                   c = rnorm(6)); my_df # length 6

head(my_df)

sapply(my_df, class)

# checking the class
class(mydf); class(mytable); class(my_df)
