# # Suppose you know that the average length of a guinea pigs odontoplasts is 18 micrometers. 
# Conduct a two-sided t-test on the ToothGrowth dataset. 
# Here, a two-sided t-test will check to see if the mean of len is not equal to 18.
# Load the ToothGrowth dataset
install.packages("tidyverse")
library(tidyverse)
data("ToothGrowth")

# Perform a two-sided t-test
t.test(x = ToothGrowth$len, alternative = "two.sided", mu = 18)

# Perform a t-test
ToothGrowth_ttest <- t.test(len ~ supp, data = ToothGrowth)

# Load broom
install.packages("broom")
library(broom)

# Tidy ToothGrowth_ttest
tidy(ToothGrowth_ttest)

# Count number of observations for each combination of supp and dose
ToothGrowth %>% 
  count(supp, dose)
str(ToothGrowth)
ToothGrowth$dose <- as.factor(ToothGrowth$dose)
str(ToothGrowth)

# Create a boxplot with geom_boxplot()
ggplot(ToothGrowth, aes(x = dose, y = len)) + 
  geom_boxplot()

# Create ToothGrowth_aov
ToothGrowth_aov <- aov(len ~ dose + supp, data = ToothGrowth)

# Examine ToothGrowth_aov with summary()
summary(ToothGrowth_aov)

# Less than
t.test(x = ToothGrowth$len,
       alternative = "less",
       mu = 18)

# Greater than
t.test(
  x = ToothGrowth$len,
  alternative = "greater",
  mu = 18)
install.packages("pwr")
library(pwr)
?pwr.t.test

# Calculate power using an effect size of 0.35, a sample size of 100 in each group, and a significance level of 0.10.
# Calculate power
pwr.t.test(n = 100, 
           d = 0.35,
           sig.level = 0.10,
           type = "two.sample", 
           alternative = "two.sided",
           power = NULL)

# Calculate sample size
pwr.t.test(n = NULL, 
           d = 0.25, 
           sig.level = 0.05, 
           type = "one.sample", alternative = "greater", 
           power = 0.8)
