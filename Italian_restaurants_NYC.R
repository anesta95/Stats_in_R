library(tidyverse)

nyc <- read_csv("italian_restaurants.csv", 
                col_types = cols(Case = col_integer(),
                                 Restaurant = col_factor(),
                                 Price = col_integer(),
                                 Food = col_integer(),
                                 Decor = col_integer(),
                                 Service = col_integer(),
                                 East = col_integer()
                                 ))
str(nyc)
pairs(nyc)

# Price by Food plot
ggplot(nyc, mapping = aes(x = Food, y = Price)) + geom_point()

# Price by Food model
lm(Price ~ Food, data = nyc)

#Price by Food and East model
lm(Price ~ Food + East, data = nyc)

# fit model
lm(Price ~ Food + Service, data = nyc)

# draw 3D scatterplot
p <- plot_ly(data = nyc, z = ~Price, x = ~Food, y = ~Service, opacity = 0.6) %>%
  add_markers() 

# draw a plane
p %>%
  add_surface(x = ~x, y = ~y, z = ~plane, showscale = FALSE)

# Price by Food and Service and East
lm(Price ~ Food + Service + East, data = nyc)


# draw 3D scatterplot
p2 <- plot_ly(data = nyc, z = ~Price, x = ~Food, y = ~Service, opacity = 0.6) %>%
  add_markers(color = ~factor(East)) 

# draw two planes
p2 %>%
  add_surface(x = ~x, y = ~y, z = ~plane0, showscale = FALSE) %>%
  add_surface(x = ~x, y = ~y, z = ~plane1, showscale = FALSE)

#Full model
lm(Price ~ Food + Service + Decor + East, data = nyc)

