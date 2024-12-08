library(ggplot2)

# Воспользуемся еще одним встроенным набором данных в R  - ToothGrowth. 
# Данные позволяют исследовать рост зубов у морских свинок 
# в зависимости от дозировки витамина C и типа потребляемых продуктов.
# Сравните среднее значение длины зубов свинок, которые потребляли 
# апельсиновый сок (OJ) с дозировкой 0.5 миллиграмм, 
# со средним значением длины зубов свинок, которые потребляли 
# аскорбиновую кислоту (VC) с дозировкой 2 миллиграмма. 
# Значение t - критерия сохраните в переменную t_stat.

df <- ToothGrowth
names(df)
df %>% select('dose')
str(df)
df$dose <- factor(df$dose, labels=c('0.5', '1', '2'))
str(df)

df_sub <- subset(df,(dose == 0.5 & supp == 'OJ') | (supp == 'VC' & dose == 2), -dose)
t.test(df_sub$OJ, df_sub$VC)
t.test(len ~ supp, df_sub)$statistic

# Скачайте данные, посвященные влиянию различного типа лечения 
# на показатель артериального давления. 
# 
# https://stepik.org/media/attachments/lesson/11504/lekarstva.csv﻿
# 
# По всем испытуемым сравните показатель давления до начала 
# лечения (Pressure_before) с показателем давления 
# после лечения (Pressure_after) при помощи t - критерия 
# для зависимых выборок. 
# 
# В поле для ответа укажите значение t - критерия.
medicine <- read.csv('../tables/lekarstva.csv')
head(medicine)
suppressMessages(library(dplyr))
medicine %>%
  t.test(.$Pressure_before, .$Pressure_after, paired = T)

t.test(medicine$Pressure_before, medicine$Pressure_after, paired = T)$statistic



