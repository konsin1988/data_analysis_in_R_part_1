patient_temp <- read.csv('../tables/Pillulkin.csv', stringsAsFactors = T)
str(patient_temp)

patient_temp$patient <- factor(patient_temp$patient)

ggplot(patient_temp)+
  aes(x = pill, y = temperature)+
  geom_boxplot()+
  facet_grid(~patient)

fit_pill_temp <- aov(temperature ~ pill + Error(patient/pill), patient_temp)
summary(fit_pill_temp)

fit_pill_doctor <- aov(temperature ~ pill*doctor + Error(patient/(pill*doctor)), patient_temp)  
summary(fit_pill_doctor)

library(ggplot2)
obj <- ggplot(ToothGrowth, aes(x = as.factor(dose), y = len, col = supp, group = supp))+
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.1, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 3, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'line', position = position_dodge(0.2))
obj
