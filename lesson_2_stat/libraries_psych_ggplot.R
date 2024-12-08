library(psych)
library(ggplot2)
df <- mtcars
describe(df)
df$am <- factor(df$am, labels = c("Auto", "Manual"))
df$vs <- factor(df$vs, labels = c('V', 'S'))
str(df)
describeBy(x=df[,2:6], group=df$vs, mat=T, digits=2)

names(df)
describeBy(df$qsec, group=list(df$am, df$vs), mat=T, digits=2)
