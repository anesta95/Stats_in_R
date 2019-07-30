# View summary of model
summary(mod)

# Compute the mean of the residuals
mean(residuals(mod))

# Compute RMSE
sqrt(sum(residuals(mod)^2) / df.residual(mod))
?augment

#(as.data.frame(augment(mod))) will give you the bdims_tidy 
#dataframe with linear model statistics like such as residuals

# View model summary
summary(mod)

# Compute R-squared
bdims_tidy %>%
  summarize(var_y = var(wgt), var_e = var(.resid)) %>%
  mutate(R_squared = 1 - (var_e / var_y))

#Create the null linear model. Always set explanatory variable
#to one
mod_null <- lm(wgt ~ 1, data = bdims)

#Creating the null dataframe from null linear model
bdims_null <- as.data.frame(augment(mod_null))
glimpse(bdims_tidy)
glimpse(bdims_null)

# Compute SSE for null model
bdims_null %>%
  summarize(SSE = var(.resid))

# Compute SSE for regression model
bdims_tidy %>%
  summarize(SSE = var(.resid))

#Calculate the mlbBat10 mod for Slugging Percentage as a
#function of On-base percentage

mlb_mod <- lm(SLG ~ OBP, data = mlbBat10)

# Rank points of high leverage
mlb_mod %>%
  augment() %>%
  arrange(desc(.hat)) %>%
  head(n = 6)

# Rank influential points
mlb_mod %>% augment() %>% arrange(desc(.cooksd)) %>% head(n = 6)

# Create nontrivial_players
nontrivial_players <- mlbBat10 %>% filter(AB >= 10 & OBP < 0.500)


# Fit model to new data
mod_cleaner <- lm(SLG ~ OBP, data = nontrivial_players)

# View model summary
summary(mod_cleaner)

# Visualize new model
nontrivial_players %>% ggplot(mapping = aes(x = OBP, y = SLG)) + geom_point() + geom_smooth(method = "lm")

# Rank high leverage points
mlb_mod %>% augment() %>% arrange(desc(.hat), .cooksd) %>% head(n = 6)
