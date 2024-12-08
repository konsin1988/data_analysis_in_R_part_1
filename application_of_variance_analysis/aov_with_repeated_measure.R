library(ggplot2)
therapy <- read.csv('../tables/therapy_data.csv')
str(therapy)
therapy$subject <- factor(therapy$subject)

# one way factor
fit_one_factor <- aov(well_being ~ therapy, therapy)
summary(fit_one_factor)

# one way with Error
fit_one_factor_with_error <- aov(well_being ~ therapy + Error(subject/therapy), therapy)
summary(fit_one_factor_with_error)

# two factors
fit_two_factors <- aov(well_being ~ therapy * price, therapy)
summary(fit_two_factors)

ggplot(therapy)+
  aes(x = price, y = well_being)+
  geom_boxplot()

# two factors with error
fit_two_factors_with_error <- 
  aov(well_being ~ therapy * price + Error(subject/(therapy * price)), therapy)
summary(fit_two_factors_with_error)

ggplot(therapy)+
  aes(x = price, y = well_being)+
  geom_boxplot()+
  facet_grid(~subject)

# 3 factors
fit_three_factors <- 
  aov(well_being ~ therapy*price*sex, therapy)
summary(fit_three_factors)

# 3 factors with error, add only in-grouping factors
fit_three_factors_with_error <-
  aov(well_being ~ therapy*price*sex + Error(subject/(therapy*price)), therapy)
summary(fit_three_factors_with_error)


