library(ggplot2)

df <- npk
str(df)

fit <- aov(yield ~ N * P, df)
summary(fit)

fit_3_var <- aov(yield ~ N + P + K, df)
summary(fit_3_var)

