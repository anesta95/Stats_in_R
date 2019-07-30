library(openintro)
library(ggplot2)
library(tidyverse)
install.packages("HistData")
library(HistData)
data("bdims")
# Scatterplot with regression line
ggplot(data = bdims, aes(x = hgt, y = wgt)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)
bdims_summary <- data.frame(507, 0.7173011, 171.1438, 9.407205, 69.14753, 13.34576)
names(bdims_summary) <- c("N", "r", "mean_hgt", "sd_hgt", "mean_wgt", "sd_wgt")
#Print bdims_summary
print(bdims_summary)
#Add slope and intercept
bdims_summary %>% mutate(slope = (r * sd_hgt / sd_wgt),
                         intercept = mean_wgt - slope * mean_hgt)

data("Galton")
ggplot(data = Galton, mapping = aes(x = parent, y = child)) + geom_point() + geom_abline(slope = 1, intercept = 0) + geom_smooth(method = "lm", se = FALSE)
data("mlbBat10")
data("mammals")
# Linear model for weight as a function of height
lm(wgt ~ hgt, data = bdims)

# Linear model for SLG as a function of OBP
lm(SLG ~ OBP, data = mlbBat10)

# Log-linear model for body weight as a function of brain weight
lm(log(BodyWt) ~ log(BrainWt), data = mammals)

mod <- lm(wgt ~ hgt, data = bdims)
#Show the coefficients
coef(mod)
#Show the full output
summary(mod)

# Mean of weights equal to mean of fitted values?
mean(bdims$wgt) == mean(fitted.values(mod))
#Better to use
near(mean(bdims$wgt), mean(fitted.values(mod)))

# Mean of the residuals
mean(residuals(mod))

# Load broom
library(broom)

# Create bdims_tidy
bdims_tidy <- as.data.frame(augment(mod))

# Glimpse the resulting data frame
glimpse(bdims_tidy)
data("ben")
ben <- data.frame(wgt = 74.8, hgt = 182.8)

# Print ben
print(ben)

# Predict the weight of ben
predict(mod, ben)
coef(mod)
coefs <- as.data.frame(coef(mod))
coefs <- coefs %>% t() %>% data.frame()
names(coefs)
# Add the line to the scatterplot
ggplot(data = bdims, aes(x = hgt, y = wgt)) + 
  geom_point() + 
  geom_abline(data = coefs, 
              aes(intercept = `X.Intercept.`, slope = `hgt`),  
              color = "dodgerblue")
