library(ggplot2)
library(dplyr)
library(psych)
my_var <- describe(mtcars)
my_var
write.csv(my_var, file = 'my_var.csv')
str(my_var)

my_var
save(my_var, file = 'my_vars.Rdata')
load(file='my_vars.Rdata')
