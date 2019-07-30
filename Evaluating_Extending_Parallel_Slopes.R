mod <- lm(totalPr ~ wheels + cond, data = marioKart)
?rnorm
?mutate
library(ggplot2)
library(broom)
data("marioKart")
# R^2 and adjusted R^2
summary(mod)

# add random noise
mario_kart_noisy <- marioKart %>% mutate(noise = rnorm(nrow(marioKart)))

# compute new model
mod2 <- lm(totalPr ~ wheels + cond + noise, data = mario_kart_noisy)

# new R^2 and adjusted R^2
summary(mod2)

# return a vector
predict(mod)

# return a data frame
augment(mod)

names(marioKart)

# include interaction
(lm(totalPr ~ cond + duration + cond:duration, data = marioKart))
?geom_smooth
marioKart_filter <- marioKart %>% filter(totalPr < 100)
# interaction plot
ggplot(marioKart_filter, aes(x = duration, y = totalPr, color = cond)) + 
  geom_point() + 
  geom_smooth(se = FALSE, method = "lm")

slr <- ggplot(marioKart, aes(y = totalPr, x = duration)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = 0)

slr <- ggplot(marioKart_filter, aes(y = totalPr, x = duration)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = 0)

# model with one slope
(lm(totalPr ~ duration, data = marioKart))

# plot with two slopes
slr + aes(color = cond)
