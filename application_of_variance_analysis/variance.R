suppressMessages(library(dplyr))
library(ggplot2)
library(Hmisc)

df <- read.csv('../tables/shops.csv', stringsAsFactors = T)
str(df)

# boxplot
boxplot(price ~ origin, df)
ggplot(df)+
  aes(origin, price)+
  geom_boxplot()

# one way ANOVA
 fit_one_way <- aov(price ~ origin, df)
summary(fit_one_way) 

# two-way ANOVA, двух факторный дисперсионный анализ
fit_two_ways <- aov(price ~ origin + store, df)
summary(fit_two_ways)

fit_two_ways_with_interactions <- aov(price ~ (origin + store)^2, df)
summary(fit_two_ways_with_interactions)

model.tables(fit_two_ways_with_interactions, 'mean')

# visualisation 
pd = position_dodge(0.1)
ggplot(df, aes(x = store, y = price, color = origin, group= origin))+
  stat_summary(fun.data = mean_cl_boot, geom='errorbar', width = 0.2, lwd=0.8, position=pd)+
  stat_summary(fun.data = mean_cl_boot, geom='line', size = 1.5, position = pd)+
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 3, position=pd, pch=15)+
  theme_bw()





