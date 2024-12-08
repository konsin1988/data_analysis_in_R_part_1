suppressMessages(library(dplyr))
library(ggplot2)

(df <- iris)
str(iris)
(sub_df <- subset(df, Species != 'setosa'))

levels(sub_df$Species)[levels(sub_df$Species) == 'setosa'] <- NA
str(sub_df)
levels(sub_df$Species)[levels(sub_df$Species == 'setosa')] <- NA
table(sub_df$Species)

spec_mean <- tapply(sub_df$Sepal.Width, sub_df$Species, mean)[1]

names(sub_df)
ggplot(sub_df)+
  aes(x = Sepal.Width, fill = Species)+
  geom_histogram(binwidth = 0.06)+
  geom_vline(aes(xintercept = spec_mean), col='red', linetype='dashed', size=1)+
  facet_grid(Species ~ .)

sepal_length_means <- sub_df %>%
  group_by(Species) %>%
  summarize(mean = median(Sepal.Length))

ggplot(sub_df)+
  aes(x = Sepal.Length, fill=Species, col=Species)+
  geom_density(alpha=0.5)+
  geom_vline(data=sepal_length_means, 
             aes(xintercept=mean, color=Species), linetype='dashed', size=1)

df_counts <- as.data.frame(as.vector(c(3, 2.5)))
names(df_counts) <- 'count'


ggplot(sub_df)+
  aes(x = Sepal.Width, fill=Species, col=Species)+
  geom_density(alpha=0.5)+
  scale_fill_manual(values=c('red', 'Lightblue'))+
  scale_color_manual(values=c('black', 'white'))+
  geom_vline(aes(xintercept = mean(Sepal.Width)), 
             linetype='dashed', col='Darkgreen', alpha=0.4)+
  facet_grid(Species ~ .)

ggplot(sub_df)+
  aes(x= Species, y = Sepal.Length, fill = Species)+
  geom_boxplot(col='white')+
  geom_hline(aes(yintercept = mean(Sepal.Length)), col='red', linetype='dashed', alpha=0.7)

# Shapiro-Wilka test, p - уровень значимости отклонения от нормальности)
shapiro.test(sub_df$Sepal.Length) # for both groups
shapiro.test(sub_df$Sepal.Length[sub_df$Species == 'versicolor']) #for specie 'versicolor'
shapiro.test(sub_df$Sepal.Length[sub_df$Species == 'virginica'])  #for 'virginica'

# LeveneTest (dispersion homogenous)
library(car)
(result <- leveneTest(Sepal.Length ~ Species, sub_df, center=mean))

df <- mtcars
df$am <- factor(df$am, labels = c('Auto', 'Manual'))
df$vs <- factor(df$vs, labels = c('V', 'S'))
leveneTest(mpg ~ vs, df, center=mean)

# Bartlett test
bartlett.test(Sepal.Length ~ Species, sub_df)

# T.test
t.test(Sepal.Length ~ Species, sub_df)

# By()
df_means <- by(df$Sepal.Length, INDICES = df$Species, sum)
df_means
(as.data.frame(df_means))








