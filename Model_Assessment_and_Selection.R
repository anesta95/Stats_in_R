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


# Model 2
model_price_2 <- lm(log10_price ~ log10_size + bedrooms, 
                    data = house_prices)

# Calculate squared residuals
get_regression_points(model_price_2) %>%
  mutate(sq_residuals = residual^2) %>%
  summarize(sum_sq_residuals = sum(sq_residuals))

# Model 4
model_price_4 <- lm(log10_price ~ log10_size + waterfront, 
                    data = house_prices)

# Calculate squared residuals
get_regression_points(model_price_4) %>%
  mutate(sq_residuals = residual^2) %>%
  summarize(sum_sq_residuals = sum(sq_residuals))

# Let's compute the R^2 summary value for the two numerical explanatory/predictor variable 
# model you fit in the Chapter 3, price as a function of size and the number of bedrooms.
# 
# Recall that R^2 can be calculated as:
# 
# 1 - Var(residuals) / Var(y)

# Compute R2 by summarizing the residual and log10_price columns.

# Fit model
model_price_2 <- lm(log10_price ~ log10_size + bedrooms,
                    data = house_prices)

# Get fitted/values & residuals, compute R^2 using residuals
get_regression_points(model_price_2) %>%
  summarize(r_squared = 1 - var(residual) / var(log10_price))


# Compute R^2 for model_price_4.
# Fit model
model_price_4 <- lm(log10_price ~ log10_size + waterfront,
                    data = house_prices)

# Get fitted/values & residuals, compute R^2 using residuals
get_regression_points(model_price_4) %>% 
  summarize(r_squared = 1 - var(residual) / var(log10_price))

# Since model_price_4 had a higher R2 of 0.470, it "fit" the data better.

# Mean squared error
# Get all residuals, square them, and take mean                    
# Get all residuals, square them, take the mean and square root
get_regression_points(model_price_2) %>%
  mutate(sq_residuals = residual^2) %>%
  summarize(mse = mean(sq_residuals)) %>% 
  mutate(rmse = sqrt(mse))

# You can think of this as the "typical" prediction error this model makes.

# MSE and RMSE for model_price_2
get_regression_points(model_price_2) %>%
  mutate(sq_residuals = residual^2) %>%
  summarize(mse = mean(sq_residuals), rmse = sqrt(mean(sq_residuals)))

# MSE and RMSE for model_price_4
get_regression_points(model_price_4) %>%
  mutate(sq_residuals = residual^2) %>%
  summarize(mse = mean(sq_residuals), rmse = sqrt(mean(sq_residuals)))

# Since model_price_4 had a lower rmse of 0.166, 
# this is suggestive that this model has better preditive power.

# Use slice() to set train to the first 10,000 rows of house_prices_shuffled 
# and test to the remainder of the 21,613 rows.
# Now fit a linear regression to predict log10_price using log10_size and bedrooms 
# using just the training data.

# Set random number generator seed value for reproducibility
set.seed(76)

# Randomly reorder the rows
house_prices_shuffled <- house_prices %>% 
  sample_frac(size = 1, replace = FALSE)

# Train/test split
train <- house_prices_shuffled %>%
  slice(1:10000)
test <- house_prices_shuffled %>%
  slice(10001:21613)

# Fit model to training set
train_model_2 <- lm(log10_price ~ log10_size + bedrooms, data = train)

# Make predictions on test set
get_regression_points(train_model_2, newdata = test)

# Compute RMSE
get_regression_points(train_model_2, newdata = test) %>% 
  mutate(sq_residuals = residual^2) %>%
  summarize(rmse = sqrt(mean(sq_residuals)))

