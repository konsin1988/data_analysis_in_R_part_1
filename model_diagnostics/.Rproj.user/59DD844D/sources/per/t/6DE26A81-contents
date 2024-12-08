library(ggplot2)
library(dplyr)

df <- swiss
str(df)

pairs(df)

ggplot(df)+
  aes(x=Examination, y=Education)+
  geom_point()+
  theme(axis.text = element_text(size=10),
        axis.title = element_text(size=20, face='bold'))

# Outliers (Выбросы)
ggplot(df)+
  aes(x=Examination, y=Education)+
  geom_point()+
  theme(axis.text = element_text(size=10),
        axis.title = element_text(size=20, face='bold'))+
  geom_smooth(method = 'lm')

# Normality of variable distribution 
# (нормальность распределения значений переменных)
library(MASS)
library(geomtextpath)

df$religious <- factor(ifelse(df$Catholic > 60, 'High', 'Low'))
ggplot(df, aes(x = Examination))+
  geom_histogram(aes(y = after_stat(density)),
                 color='black',
                 fill='white',
                 binwidth = 2)+
  geom_density(lwd=0, fill='#FF6666', alpha=0.5, adjust=0.9)+
  geom_textdensity(label='real density', adjust = 0.9, color='#AA1111')+
  geom_vline(aes(xintercept = mean(Examination)), 
             color='blue', 
             lwd=1.1,
             linetype='dashed',
             alpha=0.7)+
  stat_function(fun=dnorm,
              args=fitdistr(df$Examination,"normal")$estimate,
              color='black',
              lwd=1.3,
              alpha=0.8)

ggplot(diamonds, aes(carat, fill = cut)) +
  geom_density(position = "stack")+
  xlim(0.1, 3)

ggplot(df, aes(x=log(Education)))+
  geom_histogram(aes(y = after_stat(density)), color='wheat', fill='black')+
  geom_density(adjust=1, lwd=0, fill='red', alpha = 0.3)

# -----------------------TASKS ------------------------------
my_vector <- c(0.027, 0.079, 0.307, 0.098, 
               0.021, 0.091, 0.322, 0.211, 0.069, 
               0.261, 0.241, 0.166, 0.283, 0.041, 
               0.369, 0.167, 0.001, 0.053, 0.262, 
               0.033, 0.457, 0.166, 0.344, 0.139, 
               0.162, 0.152, 0.107, 0.255, 0.037, 
               0.005, 0.042, 0.220, 0.283, 0.050, 
               0.194, 0.018, 0.291, 0.037, 0.085, 
               0.004, 0.265, 0.218, 0.071, 0.213, 
               0.232, 0.024, 0.049, 0.431, 0.061, 0.523)
my_vector <- data.frame(my_vector)

ggplot(my_vector, aes(x = log(my_vector)))+
  geom_histogram(binwidth = 0.1)
shapiro.test(log(my_vector$my_vector))

# --------------------------------------------------
# Standartize regression coefficient
beta.coef <- function(x){
  summary(lm(scale(x[,1]) ~ scale(x[,2])))$coef[,1]
}
beta.coef(mtcars[,c(1,3)])

library(QuantPsyc)
fit <- lm(mtcars[,c(1,3)])
lm.beta(fit)
# -------- NORMALITY ----------------------------
normality.test  <- function(x){
  x %>%
    subset(sapply(., function(x) is.numeric(x))) %>%
    sapply(.,function(x) shapiro.test(x)$p.value)
}

df <- mtcars
normality.test(df)

# --------------------------------------------
df <- swiss
ggplot(df, aes(x=Examination, y = Education))+
  xlim(0, 30)+
  geom_point()+
  geom_smooth(method='lm')

lm1 <- lm(Education ~ Examination, df)
summary(lm1)

df$Examination.squared <- (df$Examination)^2
lm2 <- lm(Education ~ Examination + Examination.squared, df)
summary(lm2)

anova(lm2, lm1)


# ------------------------------------------
# Визуальное сравнение двух моделей
df$lm1.fitted <- lm1$fitted
df$lm2.fitted <- lm2$fitted
df$lm1.resid <- lm1$residuals
df$lm2.resid <- lm2$residuals

ggplot(df)+
  geom_point(aes(x=Examination, y = Education))+
  geom_line(aes(x = Examination, y = lm1.fitted), color='red', lwd=1.2)+
  geom_line(aes(x = Examination, y = lm2.fitted), color='blue', lwd=1.2)

# Проверка на нормальность распределения остатков
ggplot(df, aes(x = lm1.fitted, y = lm1.resid))+
  geom_point()+
  geom_hline(yintercept = 0, color='red', lwd=1.2)

ggplot(df, aes(x = lm2.fitted, y = lm2.resid))+
  geom_point()+
  geom_hline(yintercept = 0, color='blue', alpha=0.7, lwd=1.2)

# Проверка на нарушение независимости остатков
df$obs.number <- c(1:nrow(df))
ggplot(df, aes(x=obs.number, y = lm1.resid))+
  geom_point()+
  geom_smooth()

ggplot(df, aes(x = obs.number, y = lm2.resid))+
  geom_point()+
  geom_smooth()


# Homoscedasticity
#  Проверка на гомоскедастичность
ggplot(df, aes(x = lm1.fitted, y = lm1.resid))+
  geom_point()+
  geom_smooth()
  
  # geom_line(df_line, aes(x = x, y = y))

df_line <- data.frame(x = c(min(df$lm1.fitted), max(df$lm1.fitted)), y = c(df$lm1.resid[which.min(df$lm1.fitted)], df$lm1.resid[which.max(df$lm1.fitted)]))

which(df$lm1.fitted == min(df$lm1.fitted))


lm1.resid[which.min(df$lm1.fitted)]

# gvlma 
library(gvlma)
fit <- lm(Examination ~ Education, df)
fit_gvlma <- gvlma(fit)
summary(fit_gvlma)

# ------------------------------------------------
# Test of homoscedastisity  homosc.csv
df_task <- read.csv('homosc.csv')
str(df_task)
sum_homo <- summary(gvlma(DV ~ IV, df_task))
sum_homo$`p-value`

# Normality of residual distribution
library(ggplot2)
library(MASS)
ggplot(df, aes(x = lm1.resid))+
  geom_histogram(aes(y = after_stat(density)), binwidth = 2)+
  geom_density(color='red', fill='red', alpha=0.4, adjust = 0.7)+
  stat_function(fun=dnorm,
                args=fitdistr(df$lm1.resid,"normal")$estimate,
                color='black',
                lwd=1.3,
                alpha=0.8)

qqnorm(df$lm1.resid)
qqline(df$lm1.resid)

shapiro.test(df$lm1.resid)


# Normality of residuals distribution model 2
ggplot(df, aes(x = lm2.resid))+
  geom_histogram(aes(y = after_stat(density)), binwidth = 1.5)+
  geom_density(color='red', fill='red', alpha=0.4, adjust = 0.9)+
  stat_function(fun=dnorm,
                args=fitdistr(df$lm2.resid,"normal")$estimate,
                color='black',
                lwd=1.3,
                alpha=0.8)

qqnorm(df$lm2.resid)
qqline(df$lm2.resid)

shapiro.test(df$lm2.resid)

# ------------------------------------------
# Task function resid.norm
resid.norm <- function(x){
  library(ggplot2)
  x <- data.frame(fit.residuals = x$residuals)
  ggplot(x, aes(x = fit.residuals))+
    geom_histogram(fill = ifelse(shapiro.test(x$fit.residuals)$p.value < 0.05, 
                                 'red', 'green'))
}

fit <- lm(mpg ~ disp, mtcars)
fit <- lm(mpg ~ wt, mtcars)
my_plot <- resid.norm(fit)
my_plot

# ---------------------------------------------------------
# Multicollinearity test
high.corr <- function(x){
  x <- x[,sapply(x, function(x) is.numeric(x))]
  combinations <- as.data.frame(combn(names(x), 2))
  names(combinations) <- sapply(combinations, function(x) paste(x[1], x[2]))
  df_result <- rbind(combinations, sapply(combinations, function(y) abs(cor(x[y])[2])))
  df_result[which.max(df_result[3,])][c(1,2),]
}

typeof(high.corr(iris))
high.corr(swiss)

iris[,sapply(iris, function(x) is.numeric(x))]

test_data <- as.data.frame(list(V1 = c(-1, 1.2, 0.6, 0.6, -0.7, -0.1, 2, 0.4, 0.3, -0.1, 0.9, -1.4, -0.7, 0.4, 1, 0.1, -1.3, 1.4, -0.3, 0.9, 0.7, -1.4, -0.4, 0.7, 1.9), V2 = c(-2.2, 1.8, -0.8, 0.6, -0.4, 1.1, -1.3, 0.1, 0.2, -0.5, -0.3, -0.1, -1.1, 0.2, 1.7, 0.3, 1.7, -0.8, 1.1, 0.8, -0.2, -0.9, -1.7, -0.3, 1.5), V3 = c(-2.2, 1.8, -0.8, 0.6, -0.4, 1.1, -1.3, 0.1, 0.2, -0.5, -0.3, -0.1, -1.1, 0.2, 1.7, 0.3, 1.7, -0.8, 1.1, 0.8, -0.2, -0.9, -1.7, -0.3, 1.5)))
test_data <- as.data.frame(list(V1 = c(-0.5, -0.3, 1, 0.6, 0.3), V2 = c(-0.5, 1.2, 0, 1.9, 1.3), V3 = c(0.4, -1.5, 0.8, -0.3, 0.1), V4 = c(-0.9, -0.2, 0.1, -1, 0.9), V5 = c(0.9, -0.7, -0.4, 0.8, -1.2), V6 = c(-0.8, 0.7, 0.1, 0.6, 0.3), V7 = c(0, -0.5, 0.7, 0.2, 0.1), V8 = c(-0.5, -0.8, 0.7, 0.6, 0.1), V9 = c(-0.8, -0.1, 1.4, 1.6, -0.5), V10 = c(-1.6, -0.4, -1, 1.5, 1.2), V11 = c(-0.4, 1.5, -0.8, 0.3, -0.1)))
test_data <- as.data.frame(list(V1 = c(1.7, -1.9, 0.3, 0.6, 0.5), V2 = c(-0.6, -0.7, -1.1, -0.9, -0.1), V3 = c(-0.1, 0.2, 0.1, 1.2, -0.5), V4 = c(1, 0.3, 0, 0.6, -2.4), V5 = c(0, 0.1, 0.3, 2, -0.9), V6 = c(-0.7, 0.3, 0.7, -0.3, -1.8), V7 = c(-1.1, 1, -1, -1, -1), V8 = c(0.5, 0.9, 0.7, -0.3, -0.6), V9 = c(-0.3, 1, 0.2, 0.8, -0.3), V10 = c(-0.8, -0.5, 0.2, 0.7, -0.7), V11 = c(-1.7, 1.9, -0.3, -0.6, -0.5)))
test_data
high.corr(test_data)
combn(names(test_data), 2)

