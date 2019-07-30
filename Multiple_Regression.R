install.packages("plotly")
library(plotly)
library(tidyverse)
library(modelr)
library(broom)
data("marioKart")
mario_kart <- marioKart %>% filter(totalPr < 100)
names(marioKart)
mod_duration_sprice <- lm(totalPr ~ duration + startPr, data = mario_kart)
game_grid <- mario_kart %>% data_grid(duration = seq_range(duration, by = 1), start_price = seq_range(startPr, by = 1))
data_space <- ggplot(mario_kart, aes(x = duration, y = totalPr)) + geom_point(aes(color = startPr))
data_space
# add predictions to grid
price_hats <- augment(mod_duration_sprice, newdata = game_grid)

# tile the plane
data_space + 
  geom_tile(data = price_hats, aes(fill = .fitted), alpha = 0.5)


mod_duration_sprice
augment(mod_duration_sprice)

plane <- mario_kart$duration ~ (mario_kart$duration + mario_kart$startPr)^2
model.matrix(plane, data = mario_kart)

# draw the 3D scatterplot
p <- plot_ly(data = mario_kart, z = ~totalPr, x = ~duration, y = ~startPr, opacity = 0.6) %>%
  add_markers() 

# draw the plane
p %>%
  add_surface(x = ~x, y = ~y, z = ~plane, showscale = FALSE)

lm(totalPr ~ startPr + duration, data = mario_kart)


# draw the 3D scatterplot
p <- plot_ly(data = mario_kart, z = ~totalPr, x = ~duration, y = ~startPr, opacity = 0.6) %>%
  add_markers(color = ~cond) 

# draw two planes
p %>%
  add_surface(x = ~x, y = ~y, z = ~plane0, showscale = FALSE) %>%
  add_surface(x = ~x, y = ~y, z = ~plane1, showscale = FALSE)