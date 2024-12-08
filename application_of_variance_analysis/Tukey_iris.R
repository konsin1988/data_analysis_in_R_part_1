library(ggplot2)

fit_iris <- aov(Sepal.Width ~ Species, iris)
summary(fit_iris)

sum_width <- TukeyHSD(fit_iris)
