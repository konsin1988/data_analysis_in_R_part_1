df <- read.csv('evals.csv')
df_cars <- mtcars
df_cars$gear %% 2
df_cars$even_gear <- as.integer(!(df_cars$gear %% 2))

