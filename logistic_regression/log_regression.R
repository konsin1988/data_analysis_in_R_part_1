library(ggplot2)
df <- read.csv('train.csv', sep=';')
str(df)
df$hon <- factor(df$hon, labels = c('No', 'Yes'))

# Влияние read, write, and gender на то, 
# получит человек красный диплом или нет
ggplot(df, aes(read, math, col=gender))+
  geom_point()+
  facet_grid(.~hon)

fit <- glm(hon ~ read + math + gender, df, family='binomial')
summary(fit)

fit$coefficients
exp(fit$coefficients)

# Fitted values in natural logarifm
predict(object = fit)

# Fitted values in probability values
predict(object = fit, type='response')

# Probabilities to df
df$prob <- predict(object = fit, type='response')
str(df)

# ----------------TASKS------------------------
fit_cars <- glm(am ~ disp + vs + mpg, mtcars, family = 'binomial')
fit_cars$coefficients
str(mtcars)

# ToothGrowth
df_tooth <- ToothGrowth
str(df_tooth)


ggplot(df_tooth, aes(supp, len, fill = as.factor(dose)))+
  geom_boxplot()

# ROC curve (ROC кривая)
# Оценка качества логистической модели
library(ROCR)

df
pred_fit <- prediction(df$prob, df$hon)
perf_fit <- performance(pred_fit, 'tpr', 'fpr')
plot(perf_fit, colorize=T, print.cutoffs.at = seq(0,1, by=0.1))

# auc - area under curve, площадь под кривой
auc <- performance(pred_fit, 'auc')
str(auc)

# Specifies and Sensitivity, специфичность и чувствительность
# Специфичность - насколько часто мы ошибаемся
# Чувсвителоьность - насколько часто мы угадываем ответ
# Accurancy - общая эффективность модели
perf_spec <- performance(pred_fit, x.measure = 'cutoff', measure = 'spec')
perf_sens <- performance(pred_fit, x.measure = 'cutoff', measure = 'sens')
perf_acc <- performance(pred_fit, x.measure = 'cutoff', measure = 'acc')

plot(perf_spec, col='red', lwd=2)
plot(add=T, perf_sens, col='green', lwd=2)
plot(add=T, perf_acc, col='blue', lwd = 2.5)

legend(x = 0.2, y = 0.4, c('spec', 'sens', 'accur'),
       lty=1, col=c('red', 'green', 'blue'), bty = 'n', lwd=2, 
       y.intersp = 0.8)

abline(v = 0.225, lwd = 2)

# Create a prediction responde, либо сдал, либо не сдал
df$pred_resp <- factor(ifelse(df$prob > 0.225, 1, 0), labels=c('No', 'Yes'))
# Create variable показывает, насколько точно мы предсказали исход
df$correct <- ifelse(df$pred_resp == df$hon, 1, 0)

ggplot(df, aes(prob, fill=factor(correct)))+
  geom_dotplot()

mean(df$correct)

# Тестируем модель на новых данных
df_test <- read.csv('test.csv', sep=';')
head(df_test)

df_test$hon <- NA

df_test$hon <- predict(fit, newdata = df_test, type='response')
df_test

# --------- Task -------------------------------
df_task <- read.csv('data.csv')
str(df_task)
library(dplyr)
df_known <- df_task %>%           # not NA
  filter(!is.na(df_task$admit))

fit <- glm(admit ~ rank*gpa, df_known, family='binomial')
predict(object = fit, type = 'response')

df_unknown <- df_task %>% 
  filter(is.na(df_task$admit))

ifelse(predict(fit, newdata = df_unknown, type='response') >= 0.4, 1, 0)
df_new <- df_task %>%
  filter(is.na(admit)) %>%
  mutate(admit = ifelse(predict(fit, 
        newdata = df_unknown, type='response') >= 0.4, 1, 0))%>%
  rbind(df_task %>% filter(!is.na(admit)))
predict(fit, type='response')
df_task


