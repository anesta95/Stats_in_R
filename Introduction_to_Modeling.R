install.packages("moderndive")
# Load packages
library(moderndive)
library(ggplot2)
library(dplyr)

data("evals")

# Plot the histogram
ggplot(evals, aes(x = age)) +
  geom_histogram(binwidth = 5) +
  labs(x = "age", y = "count")

# Compute summary stats
evals %>%
  summarize(mean_age = mean(age),
            median_age = median(age),
            sd_age = sd(age))

data("house_prices")

# Exploratory visualization of house size 
# Plot the histogram
ggplot(house_prices, aes(x = sqft_living)) +
  geom_histogram() +
  labs(x = "Size (sq.feet)", y = "count")

# After plotting the histogram, what can you say about the distribution of the variable
# sqft_living?

# It is right-skewed

# Log-10 tranformation within ggplot
ggplot(house_prices, aes(x = sqft_living)) +
  geom_histogram() +
  scale_x_log10() +
  labs(x = "Size (sq.feet)", y = "count")

# Log-10 transformation to dataset
# Add log10_size
house_prices_2 <- house_prices %>%
  mutate(log10_size = log10(sqft_living))

# Plot the histogram  
ggplot(house_prices_2, aes(x = log10_size)) +
  geom_histogram() +
  labs(x = "log10 size", y = "count")

# Create a histogram of bty_avg "beauty scores" with bins of size 0.5.
# Plot the histogram
ggplot(evals, aes(bty_avg)) +
  geom_histogram(binwidth = 0.5) +
  labs(x = "Beauty score", y = "count")

# Create a scatterplot with the outcome variable score on the y-axis and the explanatory variable bty_avg on the x-axis.
# Scatterplot
ggplot(evals, aes(x = bty_avg, y = score)) +
  geom_point() +
  labs(x = "beauty score", y = "teaching score")

# Jitter plot
ggplot(evals, aes(x = bty_avg, y = score)) +
  geom_jitter() +
  labs(x = "beauty score", y = "teaching score")

# Compute correlation
evals %>%
  summarize(correlation = cor(score, bty_avg))

# Based on this, what can you say about the relationship between these two variables?
# Let's now perform an exploratory data analysis of the relationship between log10_price, the log base 10 house price, and the binary variable waterfront. Let's look at the raw values of waterfront and then visualize their relationship.

# The column log10_price has been added for you in the house_prices dataset.
house_prices <- house_prices %>% mutate(log10_price = log10(price))

# View the structure of log10_price and waterfront
house_prices %>%
  select(log10_price, waterfront) %>%
  glimpse()

# Plot 
ggplot(house_prices, aes(x = waterfront, y = log10_price)) +
  geom_boxplot() +
  labs(x = "waterfront", y = "log10 price")

# Calculate stats
house_prices %>%
  group_by(waterfront) %>%
  summarize(mean_log10_price = mean(log10_price), n = n())

# Prediction of price for houses with view
10^(6.12)

# Prediction of price for houses without view
10^(5.66)
