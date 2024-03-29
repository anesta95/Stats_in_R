install.packages("openintro")
library(openintro)
install.packages("tidyverse")
library(tidyverse)
data("ncbirths")
# Scatterplot of weight vs. weeks
ncbirths %>% ggplot(mapping=aes(x=weeks, y=weight)) + geom_point()

# Boxplot of weight vs. weeks
ncbirths %>% ggplot(aes(x = cut(weeks, breaks = 5), y = weight)) + 
  geom_boxplot()

# Mammals scatterplot
ggplot(data=mammals, aes(x=BodyWt, y=BrainWt)) + geom_point()


# Baseball player scatterplot
ggplot(data=mlbBat10, aes(x=OBP, y=SLG)) + geom_point()


# Body dimensions scatterplot
ggplot(data=bdims, aes(x=hgt, y=wgt, color=factor(sex))) + geom_point()


# Smoking scatterplot
ggplot(data=smoking, aes(x=age, y=amtWeekdays)) + geom_point()

# Scatterplot with coord_trans()
ggplot(data = mammals, aes(x = BodyWt, y = BrainWt)) +
  geom_point() + 
  coord_trans(x = "log10", y = "log10")

# Scatterplot with scale_x_log10() and scale_y_log10()
ggplot(data = mammals, aes(x = BodyWt, y = BrainWt)) +
  geom_point() +
  scale_x_log10() + scale_y_log10()
# Scatterplot of SLG vs. OBP
mlbBat10 %>%
  filter(AB >= 200) %>%
  ggplot(aes(x = OBP, y = SLG)) +
  geom_point()

# Identify the outlying player
mlbBat10 %>%
  filter(AB >= 200, OBP < 0.2)
