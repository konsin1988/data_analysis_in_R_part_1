library(ggplot2)
cars <- mtcars
cars$vs <- factor(cars$vs, labels=c("V", "S"))
cars$am <- factor(cars$am, labels=c("Auto", "Manual"))

# Histogram 
hist(cars$mpg, breaks=20, xlab="MPG", main="Histogram of MPG", cex.lab = 0.8, cex.axis = 0.7)
# cex.lab, cex.axis - size of labs and axis


#boxplot
boxplot(mpg ~ am, cars, ylab="MPG")

# Plot
plot(cars$mpg, cars$hp, xlab="MPG", ylab="hp", type="p", col='red', cex=.5, pch=19)
# Type: "p" for points, "l" for lines, "b" for both, 
# "c" for the lines part alone of "b", "o" for both ‘overplotted’,
# "h" for ‘histogram’ like (or ‘high-density’) vertical lines, "s" for stair steps,
# "S" for other steps, see ‘Details’ below, "n" for no plotting.
#---------------------------------------------
# cex: size of point, pch: appiarance

# -------------------------------------------
# GGPLOT
# Histogram
ggplot(cars, aes(x = mpg))+
  geom_histogram(col='black', fill='purple', alpha=0.4, binwidth=1)  

# Dotplot
ggplot(cars, aes(x = mpg, col=am))+
  geom_dotplot(binwidth=1, fill='white')+
  xlab('Mpg')+
  ggtitle('Dotplot')
# Density
ggplot(cars, aes(x=mpg, fill=am))+
  geom_density(alpha=0.5, col='grey')

#Boxplot
ggplot(cars, aes(x=am, y=mpg, fill=vs))+
  geom_boxplot()
# Geom_point
ggplot(cars, aes(x=mpg, y=hp, col=am, size=qsec))+
  geom_point()

# Work with variables
my_boxplot <- ggplot(cars, aes(x=am, y=mpg, fill=vs))+
  geom_boxplot()
my_boxplot

# Work with part of graphic design in variable
my_density_plot <- ggplot(cars, aes(x=mpg, fill=am))
my_density_plot + geom_density(alpha=0.5)

df <- mtcars
df$am <- factor(df$am, labels=c('Auto', 'Manual'))
df$vs <- factor(df$vs, labels=c('V', 'S'))
ggplot(df, aes(x = mpg, fill = am))+
  geom_dotplot(binwidth = 1)+
  xlab("Miles/(US) gallon")+
  ylab("Count")+
  scale_fill_discrete(name="Transmission type")+       #Title of the legend
  ggtitle("MPG dotplot")

ggplot(df, aes(x = mpg, y = hp, size = qsec))+
  geom_point()+
  xlab("Miles/(US) gallon")+
  ylab("Gross horsepower")+
  scale_size_continuous(name="1/4 mile time")+          #Title of the legend
  ggtitle("Miles/(US) gallon and Gross horsepower")


