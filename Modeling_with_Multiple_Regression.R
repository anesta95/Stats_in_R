# Load packages
library(moderndive)
library(ggplot2)
library(dplyr)

data("house_prices")

house_prices <- house_prices %>% 
  mutate(
    log10_price = log10(price),
    log10_size = log10(sqft_living)
  )

# Create scatterplot with regression line
ggplot(house_prices, aes(x = bedrooms, y = log10_price)) +
  geom_point() +
  labs(x = "Number of bedrooms", y = "log10 price") +
  geom_smooth(method = "lm", se = FALSE)

# Remove outlier
house_prices_transform <- house_prices %>%
  filter(bedrooms < 30)

# Create scatterplot with regression line
ggplot(house_prices_transform, aes(x = bedrooms, y = log10_price)) +
  geom_point() +
  labs(x = "Number of bedrooms", y = "log10 price") +
  geom_smooth(method = "lm", se = FALSE)

# house_prices_tranform, which is available in your environment, 
# has the log base 10 transformed variables included and the outlier house with 33 bedrooms 
# removed. Let's fit a multiple regression model of price as a function of size and the
# number of bedrooms and generate the regression table. In this exercise, 
# you will first fit the model, and based on the regression table, in the second part, 
# you will answer the following question:
 
# Which of these interpretations of the slope coefficent for bedrooms is correct?

# Fit model
model_price_2 <- lm(log10_price ~ log10_size + bedrooms, 
                    data = house_prices)

# Get regression table
get_regression_table(model_price_2)

# Say you want to predict the price of a house using this model and you know it has:
#   
# 1000 square feet of living space, and
# 3 bedrooms
# What is your prediction both in log10 dollars and then dollars?
#   
# The regression model from the previous exercise is available in your workspace as 
# model_price_2.

# Make prediction in log10 dollars
2.69 + 0.941 * log10(1000) - 0.033 * 3

# Make prediction dollars
10^5.414

# Let's automate this process for all 21K rows in house_prices to obtain residuals, 
# which you'll use to compute the sum of squared residuals: 
# a measure of the lack of fit of a model. After computing the sum of squared residuals, 
# you will answer the following question:
#   
# Which of these statements about residuals is incorrect?

# Residuals are leftover points not accounted for in the our regression model.

# Automate prediction and residual computation
get_regression_points(model_price_2) %>%
  mutate(sq_residuals = residual^2) %>%
  summarize(sum_sq_residuals = sum(sq_residuals))

# Fit a multiple regression of log10_price using log10_size and waterfront as the predictors. 
# Recall that the data frame that contains these variables is house_prices.

# Fit model
model_price_4 <- lm(log10_price ~ log10_size + waterfront, 
                    data = house_prices)

# Get regression table
get_regression_table(model_price_4)

# Let's interpret the values in the regression table for the parallel slopes model you just fit. 
# Run get_regression_table(model_price_4) in the console to view the regression table again. The visualization for this model is below. 
# Which of these interpretations is incorrect?

# The intercept for houses with a view of the waterfront is 0.322.

# Using your model for log10_price as a function of log10_size and the binary variable 
# waterfront, let's make some predictions! Say you have the two following "new" houses, 
# what would you predict their prices to be in dollars?
# 
# House A: log10_size = 2.9 that has a view of the waterfront
# House B: log10_size = 3.1 that does not have a view of the waterfront


# After running the code on line 2 to get the regression table based 
# on model_price_4, compute the predicted prices for both houses. 
# First you'll use an equation based on values in this regression table to get 
# a predicted value in log10 dollars, then raise 10 to this predicted value to get 
# a predicted value in dollars.

# Get regression table
get_regression_table(model_price_4)

# House A log10 price predition:
2.96 + 0.322 + 0.825 * 2.9

# House B log10 price predition:
2.96 + 0 + 0.825 * 3.1

# Prediction for House A
10^(5.6745)

# Prediction for House B
10^(5.5175)

# Let's now repeat what you did in the last exercise, 
# but in an automated fashion assuming the information on these "new" houses 
# is saved in a dataframe.
# 
# Your model for log10_price as a function of log10_size and 
# the binary variable waterfront (model_price_4) 
# is available in your workspace, and so is new_houses_2, 
# a dataframe with data on 2 new houses. 
# While not so beneficial with only 2 "new" houses, this will save a lot of work 
# if you had 2000 "new" houses.

new_houses_2 <- data_frame(
  log10_size = c(2.9, 3.1),
  waterfront = c(1, 0)
)

new_houses_2$waterfront <- as.logical(new_houses_2$waterfront)
str(new_houses_2)
# View the "new" houses
new_houses_2

# Get predictions price_hat in dollars on "new" houses
get_regression_points(model_price_4, newdata = new_houses_2) %>% 
  mutate(price_hat = 10^log10_price_hat)


