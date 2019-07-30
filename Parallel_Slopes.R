install.packages("openintro")
library(openintro)
install.packages("tidyverse")
library(dplyr)
install.packages("broom")
library(broom)
library(ggplot2)
data("marioKart")

# Explore the data
glimpse(marioKart)
# fit parallel slopes
mod <- lm(totalPr ~ wheels + cond, data = marioKart)
augmented_mod <- augment(mod)
glimpse(augmented_mod)
augmented_mod2 <- augmented_mod %>% filter(totalPr < 100)

# scatterplot, with color
data_space <- ggplot(augmented_mod, aes(x = wheels, y = totalPr, color = cond)) + 
  geom_point()

# single call to geom_line()
data_space + 
  geom_line(aes(y = .fitted))

# scatterplot, with color
data_space2 <- ggplot(augmented_mod2, aes(x = wheels, y = totalPr, color = cond)) + 
  geom_point()

# single call to geom_line()
data_space2 + 
  geom_line(aes(y = .fitted))

data("babies")
glimpse(babies)

baby_mod <- lm(bwt ~ age + parity, data = babies)
smoke_mod <- lm(bwt ~ gestation + smoke, data = babies)
