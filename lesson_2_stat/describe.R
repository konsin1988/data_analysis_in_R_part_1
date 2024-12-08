df <- airquality

aggregate(Ozone ~ Month, subset(airquality, Month %in% c(7,8,9)), length)
describeBy(airquality, group='Month', mat=T)

df <- iris
subset(df, df['Species'] == 'virginica')
describe(subset(df, df['Species'] == 'virginica'))

my_vector <- rnorm(30)
my_vector[sample(1:30, 10)] <- NA
         