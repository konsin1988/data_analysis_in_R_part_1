# library(ggplot2)

df <- mtcars
fit <- cor.test(df$mpg, df$hp)

names(fit)
cor.test(~mpg + hp, df)

# plot(hp ~ mpg, df)

# ggplot(df)+
#     aes(x = mpg, y = hp, col=factor(cyl))+
#     geom_point(size=3)

# df_numeric <- df[,c(1, 3:7)]

# pairs(df_numeric)

library(psych)
# fit_corr <- corr.test(df_numeric)
# fit_corr$r
# fit_corr$p

# fit <- corr.test(df$mpg, df$hp)
# names(fit)
# fit$r
# fit$p

# corr_p <- function(df){
#     fit <- corr.test(df[[1]], df[[2]])
#     c(fit$estimate, fit$p.value)
# }
# x = cor.test(mtcars[,1], mtcars[,5])
# x$estimate

max_corr <- function(df) {
    library(psych)
    df_num <- df[,sapply(df, is.numeric)]
    f<- corr.test(df_num)
    diag(f$r) <- 0
    f$r[which.max(abs(f$r))]
}

df <- iris
df_num <- df[,sapply(df, is.numeric)]

max_corr(iris)

test_data <- read.csv('test_data.csv')
smart_corr <- function(x) {
  if((shapiro.test(x[,1])$p.value > 0.05) &
     (shapiro.test(x[,2])$p.value > 0.05)) {
    cor.test(x[,1], x[,2], method = 'pearson')$estimate
  } else {
    cor.test(x[,1], x[,2], method = 'spearman')$estimate
  }  
}
smart_corr(test_data)

