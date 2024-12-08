library(ggplot2)
suppressMessages(library(dplyr))
library(psych)
df <- mtcars

df$am <- factor(df$am, labels=c('Auto', 'Manual'))
df$vs <- factor(df$vs, labels=c('V', 'S'))

ggplot(df)+
  geom_histogram(aes(x = mpg, y = ..density..), binwidth = 1.5, fill='purple', alpha=0.4 )+
  geom_density(aes(x = mpg), col = 'red', fill='lightblue', alpha=0.5, size=1.5)+
  geom_vline(aes(xintercept = mean(mpg)), col='blue', linetype='dashed', size=1)
