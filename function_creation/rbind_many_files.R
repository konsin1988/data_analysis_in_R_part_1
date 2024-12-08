# na position
my_vector <- c(1, 2, 3, NA, NA)
na_which <- function(x){
  which(is.na(x))
}

na_which(my_vector)

# df from many files
library(stringr)
dir('grants_data', pattern = '*.csv')
grants <- data.frame()

for(i in dir('grants_data', pattern='*.csv')){
  grants <- rbind(read.csv(str_c('grants_data/',i)), grants)
}
nrow(grants)

# filtered.sum
vect <- c(1, -2, 3, NA, NA)
filtered_sum <- function(x){
  sum(x[x > 0 & !is.na(x)])
}
filtered_sum(vect)

# outliers.rm, удаление выбросов
# Напишите функцию outliers.rm, которая находит 
# и удаляет выбросы. Для обнаружения выбросов 
# воспользуемся самым простым способом, 
# с которым вы не раз встречались, используя график Box plot. 
# Выбросами будем считать те наблюдения, которые 
# отклоняются от 1 или 3 квартиля больше чем на 1,5 *  IQR, 
# где  IQR  - межквартильный размах.
# На вход функция получает числовой вектор x. 
# Функция должна возвращать модифицированный 
# вектор x с удаленными выбросами. 
library(ggplot2)
names(mtcars)
ggplot(mtcars)+
  aes(y=qsec)+
  geom_boxplot()

outliers_remover <- function(x){
  quantiles = quantile(x, probs = c(0.25, 0.75))
  x_iqr <- 1.5 * IQR(x)
  x[(x >= quantiles[1] - x_iqr) & (x <= quantiles[2] + x_iqr)]
  
}

vect <- as.data.frame(outliers_remover(mtcars$mpg))
names(vect) <- "mpg_norm"
ggplot(vect, aes(y = mpg_norm))+
         geom_boxplot()

