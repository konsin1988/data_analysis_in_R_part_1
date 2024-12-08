library(dplyr)
df <- mtcars

str(df)
# numeric to factor 
df$vs <- factor(df$vs, labels=c('V', 'S'))
df$am <- factor(df$am, labels=c('Auto', 'Manual'))

# mtcars. 
# Рассчитайте среднее значение времени разгона (qsec) для автомобилей, 
# число цилиндров (cyl) у которых не равняется 3 
# и показатель количества миль на галлон топлива (mpg) больше 20.
mean(df$qsec[df$cyl != 3 & df$mpg > 20])

aggregate(df$hp, by=list(df$vs), FUN=mean)

aggregate(hp ~ vs + am, df, mean)

aggregate(x = subset(df,,-c(vs, am)), by = list(df$vs, df$am), FUN=median)
agg_hp_disp <- aggregate(cbind(hp, disp) ~ vs + am, df, function(x) c('mean' = mean(x), 'sd' = sd(x)))



