library(dplyr)
df <- read.table('dataset_11508_12.txt')
head(df)

fit <- lm(df$V1 ~ df$V2, df)
summary(fit)

# -------------------------------------
# Воспользуемся уже знакомыми данными diamonds 
# из библиотеки ggplot2. Только для бриллиантов 
# класса Ideal (переменная cut) c числом карат 
# равным 0.46 (переменная carat) постройте линейную 
# регрессию, где в качестве зависимой переменной 
# выступает price, в качестве предиктора - переменная  depth.
library(ggplot2)
library(dplyr)
df <- diamonds
df_filtered <- df %>%
  filter(cut == 'Ideal', carat == 0.46) %>%
  select(price, depth)
fit_dplyr <- lm(price ~ depth, data = df_filtered)  
fit_dplyr$coefficients

fit <- lm(price ~ depth, df, subset = (cut == 'Ideal'& carat == 0.46))  
fit$coefficients

# ----------------------------------------------------------------
# Напишите функцию regr.calc, которая на вход 
# получает dataframe c двумя переменными.
# Если две переменные значимо коррелируют 
# (p - уровень значимости для коэффициента 
# корреляции Пирсона меньше 0.05), то функция 
# строит регрессионную модель, где первая переменная - 
# зависимая, вторая - независимая. Затем создает 
# в dataframe новую переменную с назанием fit, 
# где сохраняет предсказанные моделью значения 
# зависимой переменной. В результате функция должна 
# возвращать исходный dataframe с добавленной новой переменной fit.
# Если две переменные значимо не коррелируют, 
# то функция возвращает строчку "There is no sense in prediction"
df <- iris[,c(1, 4)]
df = iris[,1:2]
regr.calc <- function(df){
if(cor.test(df[,1], df[,2], method = 'pearson')$p.value < 0.05) {
  fit <- lm(df[,1] ~ df[,2], df)
  df$fit <- fit$fitted.values
  return(df)
} else {
  "There is no sense in prediction"
}
}
regr.calc(df)

#----------------------------------------------------
# Постройте scatterplot по данным iris, 
# сохранив его в переменную my_plot : 
#   Ось X - переменная Sepal.Width
# Ось Y -  переменная Petal.Width
# Цвет точек - переменная Species
# Также добавьте линейное сглаживание 
# для каждой группы наблюдений по переменной Species.
ggplot(iris)+
  aes(iris$Sepal.Width, iris$Petal.Width, col=iris$Species)+
  geom_point()+
  geom_smooth(method = 'lm')

write.csv(iris, file='iris.csv')
  
  
  
  