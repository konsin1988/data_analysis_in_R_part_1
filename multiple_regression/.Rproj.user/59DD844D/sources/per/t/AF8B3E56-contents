library(dplyr)
library(tidyverse)
library(stringi)

str(swiss)
swiss <- data.frame(swiss)

hist(swiss$Fertility)

# Dependent var - Fertility, Independent vars - Examination + Catholic
fit <- lm(Fertility ~ Examination + Catholic, data=swiss)
summary(fit)

# The same vars as previous, but with interaction
fit2 <- lm(Fertility ~ Examination*Catholic, data=swiss)
summary(fit2)

# Confidence intervals
confint(fit2)

# ------------------------------------------------------
# TASKS
test_data <- read.csv("https://stepik.org/media/attachments/course/129/fill_na_test.csv")
str(test_data)
x <- test_data
fill_na <- function(x){
  fit_na <- lm(y ~ x_1 + x_2, x)
  x$full_y <- ifelse(is.na(x$y), predict(fit_na, x), x$y)
  return(x)
}
fill_na(test_data)

# ----------------------------------------------------
library(dplyr)
df <- mtcars %>% select(wt, mpg, disp, drat, hp)

get_df_adj <- function(x, list, df) {
  df_IV <- as.data.frame(combn(list, x))
  names(df_IV) <- combn(list, x, FUN = function(x) paste(x, collapse = '+'))
  result <- rbind(df_IV, lapply(df_IV, FUN = 
        function(x) round(summary(lm(data=df[c('wt', x)]))$adj.r.squared, 3)))
  result %>%
    slice(-c(1:nrow(result)-1))
}

get_max_adj.r.sqr <- function(df) {
  library(dplyr)
  df <- df %>%
    select_if(is.numeric)
  list_IV <- names(df[,c(2:length(df))])
  df_result <- data.frame(row.names = 1)
  
  list_all_vars <- 
    lapply(1:length(list_IV), FUN = function(x) get_df_adj(x, list_IV, df))
  df_result <- cbind(df_result, list_all_vars[1:length(list_all_vars)])
  df_result %>% 
    select(which.max(df_result))
}
get_max_adj.r.sqr(df)

# -----------------------------------------------------------------
# Воспользуйтесь встроенным датасетом attitude, 
# чтобы предсказать рейтинг (rating) по переменным 
# complaints и critical. Каково t-значение 
# для взаимодействия двух факторов?

df <- attitude
df
fit <- lm(rating ~ complaints*critical, df)
summary(fit)

# -----------------------------------------------------------------
# lection 'Multiple regression with categorical variable'

df <- swiss
df$religious <- as.factor(ifelse(df$Catholic > 60, 'Lots', 'Few'))
str(df)

fit_religious <- lm(Fertility ~ Examination + religious, df)
summary(fit_religious)
plot(df$religious)

# -----------------------------------------------
# Multiple regression with cathegorical variable and with
# interaction with variables
fit_religious_int <- lm(Fertility ~ Examination * religious, df)
summary(fit_religious_int)

# -------------------------------------------------------
# Graphics
library(ggplot2)
ggplot(df, aes(x = Examination, y = Fertility, col=religious))+
  geom_point()+
  geom_smooth(method='lm')

# -----------------------------------------------------
# multiple regression with one categorical and 2 numeric predictors
fit_mult_2_numeric <- lm(Fertility ~ religious*Infant.Mortality*Examination, df)
summary(fit_mult_2_numeric)

# -------------------------TASKS------------------------------
df_cars <- mtcars
df_cars$am <- factor(df_cars$am, labels = c('Auto', 'Manual'))
fit_task_1 <- lm(mpg ~ am*wt, df_cars)
summary(fit_task_1)
str(df_cars)

ggplot(df_cars)+
  aes(x = wt, y = mpg, color = am)+
  geom_smooth(method='lm')


