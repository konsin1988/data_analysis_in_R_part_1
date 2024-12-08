rm(list=ls())
df <- mtcars

fit <- lm(mpg ~ hp, df)
fit
summary(fit)
plot(fit)


library(ggplot2)
ggplot(df)+
  aes(hp, mpg)+
  geom_point(size=2)+
  geom_smooth()

# Реальные значения vs предсказанные 
fitted_values_mpg <- data.frame(mpg = df$mpg, 
        fitted_mpg = round(fit$fitted.values, 1))
fitted_values_mpg

# Предсказание значений predict 
new_hp <- data.frame(hp = c(100, 150, 129, 300))
new_hp$mpg <- round(predict(fit, new_hp),1)
new_hp

# Линейная регрессия с номинативной переменной 
# в качестве предиктора (cyl - колво цилиндров)
df
df$cyl <- factor(df$cyl, labels = c('four', 'six', 'eight'))
fit <- lm(mpg ~ cyl, df)
summary(fit)

aggregate(mpg ~ cyl, df, mean)

ggplot(df)+
  aes(cyl, mpg)+
  geom_point()+
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size = 12, face='bold'))

# Model hs ~ cyl
fit <- lm(hp ~ cyl, df)
summary(fit)  

aggregate(hp ~ cyl, df, mean)

ggplot(df)+
  aes(cyl, hp, col=cyl)+
  geom_point()+
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size = 12, face='bold'))+
  geom_hline(data = aggregate(hp ~ cyl, df, mean), 
             aes(yintercept = hp, col=cyl), linetype = 'dashed')
  
  


