library(ggplot2)
airq<- airquality
names(airq)
str(airq)
unique(airq$Month)
airq$Month <- factor(airq$Month, labels=unique(airq$Month))

# При помощи функции ggplot() или boxplot()
# постройте график boxplot, используя встроенные
# в R данные airquality. По оси x отложите номер
# месяца, по оси y — значения переменной Ozone.
# На графике boxplot отдельными точками отображаются
# наблюдения, отклоняющиеся от 1 или 3 квартиля больше
# чем на полтора межквартильных размаха. Сколько таких
# наблюдений присутствует в сентябре (месяц №9)?
# Обратите внимание, что для корректного отображения
# графика ggplot ожидает факторную переменную по оси x.
# 
# Comment multiple lines = Ctrl + Shift + C )))))
ggplot(airq, aes(x=Month, y = Ozone))+
  geom_boxplot()

# Нужно построить scatterplot с помощью ggplot из ggplot2, 
# по оси x которого будет mpg, по оси 
# y - disp, а цветом отобразить переменную (hp).
cars <- mtcars
ggplot(cars, aes(x = mpg, y = disp, col = hp))+
  geom_point()

# Укажите, при помощи какого варианта 
# кода мы можем построить следующий 
# график по данным iris:
# Гистограмма распределения переменной 
# Sepal.Length, в которой цвет заполнения столбцов 
# гистограммы зависит от значения переменной Species.
df_i <- iris
str(df_i)
ggplot(iris, aes(x = Sepal.Length, fill=Species))+
  geom_histogram(binwidth=0.1)

# Scatterplot (диаграмма рассеивания), где по оси X 
# будет отложена переменная Sepal.Length,  по оси Y 
# переменная  Sepal.Width. За цвет точек будет отвечать 
# переменная  Species, а за размер точек переменная Petal.Length.
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col=Species, size = Petal.Length)) +
  geom_point()+
  scale_size_continuous("Length")
