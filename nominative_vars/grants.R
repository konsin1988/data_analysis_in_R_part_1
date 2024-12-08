library(ggplot2)
suppressMessages(library(dplyr))
df <- read.csv('../tables/grants.csv')
df$status <- factor(df$status, labels = c('Not founded', 'Founded'))

t1 <- table(df$status)
binom.test(t1)

t2 <- table(df$status, df$field)
t2
(t2 <- table(status = df$status, field = df$field))

prop.table(t2)        # Percent all
prop.table(t2, 1)     # Percent horizontal
prop.table(t2, 2)     # Percent vertical

(t3 <- table(Years = df$years_in_uni, Field = df$field, Status = df$status))
prop.table(t3, 2)

barplot(t2)
# Постройте столбчатую диаграмму распределения цвета 
# глаз по цвету волос только у женщин из
# таблицы HairEyeColor. По оси X должен 
# идти цвет волос, цвет столбиков должен отражать 
# цвет глаз. По оси Y - количество наблюдений.
df_female <- as.data.frame(HairEyeColor[,,'Female'])
(obj <- ggplot(df_female, aes(x = Hair, y=Freq, fill = Eye))+
  geom_bar(stat = 'identity', position='dodge')+
  scale_fill_manual(values=c('Brown', 'Blue', 'Darkgrey', 'Darkgreen')))

binom.test(t1) # How much theoretical value differ from real data
chisq.test(t1)

chi <- chisq.test(t1)
chi$exp       # expected values
chi$obs       # observed values

# t2 table
chisq.test(t2)

# Fisher exact test
fisher.test(t2)

# Eyes of women-browns
hyc <- HairEyeColor['Brown', ,'Female']
hyc
chisq.test(hyc)

# hypothesis about the relationship between 
# the quality of a diamond’s 
# cut (cut) and its color (color)
di_df <- as.data.frame(diamonds)
t_cut_color <- table(Cut = di_df$cut, Color = di_df$color)
t_cut_color
main_stat <- chisq.test(t_cut_color)
main_stat$statistic


# При помощи критерия Хи - квадрат проверьте гипотезу 
# о взаимосвязи цены (price) и каратов (carat) бриллиантов
di_df$price_factor <- factor(as.numeric(di_df$price >= mean(di_df$price)), labels = c('ls_mean', 'gt_mean'))
di_df$carat_factor <- factor(as.numeric(di_df$carat >= mean(di_df$carat)), labels = c('ls_mean', 'gt_mean'))

t2 <- table(di_df$price_factor, di_df$carat_factor)
chisq.test(t2)$statistic

# При помощи точного критерия Фишера проверьте гипотезу 
# о взаимосвязи типа коробки передач (am) и 
# типа двигателя (vs) в данных mtcars
df <- mtcars
df$am <- factor(df$am, labels = c('Auto', 'Manual'))
df$vs <- factor(df$vs, labels = c('V', 'S'))
fisher_test <- fisher.test(table(df$am, df$vs))$p.value
fisher_test
