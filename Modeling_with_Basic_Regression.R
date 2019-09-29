# Load packages
library(ggplot2)
library(dplyr)
library(moderndive)

data("evals")

# Plot 
ggplot(evals, aes(x = bty_avg, y = score)) +
  geom_point() +
  labs(x = "beauty score", y = "score") +
  geom_smooth(method = "lm", se = F)

# You will do this by first "fitting" the model. 
# Then you will get the regression table, a standard output in many 
# statistical software packages. Finally, based on the output of get_regression_table(), 
# which interpretation of the slope coefficient is correct?

# Fit model
model_score_2 <- lm(score ~ bty_avg, data = evals)

# Output content
model_score_2

# Output regression table
get_regression_table(model_score_2)

# For every increase of one in beauty score, 
# you should observe an associated increase of on average 0.0670 units in teaching score.

# Say there is an instructor at UT Austin and you know nothing about them except that their beauty score is 5. 
# What is your prediction y^ of their teaching score y?
# Using the values of the intercept and slope from above, predict this instructor's score.
# Say it's revealed that the instructor's score is 4.7. 
# Compute the residual for this prediction, i.e., the residual y−y^.

# Use fitted intercept and slope to get a prediction
y_hat <- 3.88 + 5 * 0.0670
y_hat

# Compute residual y - y_hat
4.7 - 4.215

# Now say you want to repeat this for all 463 instructors in evals. 
# Doing this manually as you just did would be long and tedious, so as seen in the video, 
# let's automate this using the get_regression_points() function.
# 
# Add a new column score_hat_2 which replicates how score_hat is computed using the table's values.
# Add a new column residual_2 which replicates how residual is computed using the table's values.

# Get all fitted/predicted values and residuals
get_regression_points(model_score_2) %>% 
  mutate(score_hat_2 = 3.88 + 0.067 * bty_avg) %>% 
  mutate(residual_2 = score - score_hat)


# Let's perform an EDA of the relationship between an instructor's score and their rank. 
# You'll both visualize this relationship and compute summary statistics for each level of 
# rank: teaching, tenure track, and tenured.

ggplot(evals, aes(x = rank, y = score)) +
  geom_boxplot() +
  labs(x = "rank", y = "score")

evals %>%
  group_by(rank) %>%
  summarize(n = n(), mean_score = mean(score), sd_score = sd(score))


# Fit regression model
model_score_4 <- lm(score ~ rank, data = evals)

# Get regression table
get_regression_table(model_score_4)

# Based on the regression table, compute the 3 possible fitted values y^, 
# which are the group means. Since "teaching" is the baseline for comparison group, 
# the intercept is the mean score for the "teaching" group and 
# ranktenure track/ranktenured are relative offsets to this baseline for the 
# "tenure track"/"tenured" groups.

# teaching mean
teaching_mean <- 4.28

# tenure track mean
tenure_track_mean <- 4.28 - 0.13

# tenured mean
tenured_mean <- 4.28 - 0.145

# Run get_regression_table(model_score_4) in the console to regenerate the regression table 
# where you modeled score as a function of rank. Now say using this table you want to predict 
# the teaching score of an instructor about whom you know nothing except that they are a 
# tenured professor. Which of these statements is true?

get_regression_table(model_score_4)

# A good prediction of their score would be 4.28 - 0.145 = 4.135.

# Calculate predictions and residuals
model_score_4_points <- get_regression_points(model_score_4)
model_score_4_points

# Plot residuals
ggplot(model_score_4_points, aes(x = residual)) +
  geom_histogram() +
  labs(x = "residuals", title = "Residuals from score ~ rank model")
