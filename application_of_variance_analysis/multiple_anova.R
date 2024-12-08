library(ggplot2)

list.files('../tables/')
shops <- read.csv('../tables/shops.csv')

#boxplot multiple
ggplot(shops)+
  aes(x = food, y = price)+
  geom_boxplot()

fit_food <- aov(price ~ food, shops)
summary(fit_food)

# Попарное сравнение с поправкой Тьюки
TukeyHSD(fit_food)
