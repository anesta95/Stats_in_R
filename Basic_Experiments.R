library(tidyverse)
library(ggplot2)
library(broom)
library(pwr)
lendingclub <- read_csv("lendingclub.csv")
View(lendingclub)

# Examine the variables with glimpse()
glimpse(lendingclub)

# Find median loan_amnt, mean int_rate, and mean annual_inc with summarise()
lendingclub %>% summarize(median(loan_amnt), mean(int_rate), mean(annual_inc))

# Use ggplot2 to build a bar chart of purpose
ggplot(data=lendingclub, aes(x = purpose)) + 
  geom_bar() +
  coord_flip()

# Use recode() to create the new purpose_recode variable
lendingclub$purpose_recode <- lendingclub$purpose %>% recode( 
  "credit_card" = "debt_related", 
  "debt_consolidation" = "debt_related",
  "medical" = "debt_related",
  "car" = "big_purchase", 
  "major_purchase" = "big_purchase", 
  "vacation" = "big_purchase",
  "moving" = "life_change", 
  "small_business" = "life_change", 
  "wedding" = "life_change",
  "house" = "home_related", 
  "home_improvement" = "home_related")

# Use ggplot2 to build a new bar chart of purpose_recode
ggplot(data=lendingclub, aes(x = purpose_recode)) + 
  geom_bar() +
  coord_flip()

# Build a linear regression model, purpose_recode_model
purpose_recode_model <- lm(funded_amnt ~ purpose_recode, data = lendingclub)

# Examine results of purpose_recode_model
summary(purpose_recode_model)

# Get anova results and save as purpose_recode_anova
purpose_recode_anova <- anova(purpose_recode_model)

# Print purpose_recode_anova
print(purpose_recode_anova)

# Examine class of purpose_recode_anova
class(purpose_recode_anova)

# Use aov() to build purpose_aov
purpose_aov <- aov(funded_amnt ~ purpose_recode, data = lendingclub)

# Conduct Tukey's HSD test to create tukey_output
tukey_output <- TukeyHSD(purpose_aov, "purpose_recode", conf.level = 0.95)

# Tidy tukey_output to make sense of the results
tidy(tukey_output)

# Use aov() to build purpose_emp_aov
purpose_emp_aov <- aov(funded_amnt ~ purpose_recode + emp_length, data = lendingclub)

# Print purpose_emp_aov to the console
print(purpose_emp_aov)

# Call summary() to see the p-values
summary(purpose_emp_aov)

# Examine the summary of int_rate
summary(lendingclub$int_rate)

# Examine int_rate by grade
lendingclub %>% 
  group_by(grade) %>% 
  summarise(mean = mean(int_rate), var = var(int_rate), median = median(int_rate))

# Make a boxplot of int_rate by grade
ggplot(lendingclub, aes(x = grade, y = int_rate)) +
  geom_boxplot()

# Use aov() to create grade_aov plus call summary() to print results
grade_aov <- aov(int_rate ~ grade, data = lendingclub)
summary(grade_aov)

# For a 2x2 grid of plots:
par(mfrow=c(2, 2))

# Plot grade_aov
plot(grade_aov)

# Bartlett's test for homogeneity of variance
bartlett.test(int_rate ~ grade, data = lendingclub)

# Conduct the Kruskal-Wallis rank sum test
kruskal.test(int_rate ~ grade,
             data = lendingclub)

?pwr.t.test
# Use the correct function from pwr to find the sample size
pwr.t.test(n = NULL, 
           d = 0.2, 
           sig.level = 0.05, 
           power = 0.8, 
           alternative = "two.sided")
?write_csv
?t.test

# Plot the A/B test results
ggplot(lendingclub_ab, aes(x = Group, y = loan_amnt)) + 
  geom_boxplot()

# Conduct a two-sided t-test
t.test(loan_amnt ~ Group, data = lendingclub_ab, alternative = "two.sided")

# Build lendingclub_multi
lendingclub_multi <-lm(loan_amnt ~ Group + grade + verification_status, data = lendingclub_ab)

# Examine lendingclub_multi results
tidy(lendingclub_multi)

