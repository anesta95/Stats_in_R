install.packages("Stat2Data")
install.packages("binr")
library(binr)
library(Stat2Data)
library(broom)
data("MedGPA")
# scatterplot with jitter
data_space <- ggplot(data = MedGPA, mapping = aes(x = GPA, y = Acceptance)) + 
  geom_jitter(width = 0, height = 0.05, alpha = 0.5)

# linear regression line
data_space + 
  geom_smooth(method = "lm", se = FALSE)

# filter
MedGPA_middle <- MedGPA %>% filter(GPA >= 3.375 & GPA <= 3.77)

# scatterplot with jitter
data_space <- ggplot(data = MedGPA_middle, mapping = aes(x = GPA, y = Acceptance)) + 
  geom_jitter(width = 0, height = 0.05, alpha = 0.5)

# linear regression line
data_space + 
  geom_smooth(method = "lm", se = FALSE)

# fit model
glm(Acceptance ~ GPA, data = MedGPA, family = binomial)

# scatterplot with jitter
data_space <- ggplot(data = MedGPA, mapping = aes(x = GPA, y = Acceptance)) + 
  geom_jitter(width = 0, height = 0.05, alpha = .5)

# add logistic curve
data_space +
  geom_smooth(method = "glm", se = FALSE, color = "red", method.args = list(family = "binomial"))

head(MedGPA)
MedGPA$GPA


MedGPA_binned <- tribble(
  ~bin,         ~mean_GPA, ~acceptance_rate,
  (2.72:3.3),      3.11,           0.2,  
  (3.3:3.44),      3.39,           0.2,  
  (3.44:3.58),     3.54,           0.75, 
  (3.58:3.7),      3.65,           0.333,
  (3.7:3.87),      3.79,           0.889,
  (3.87:3.97),     3.91,           1
  
)

MedGPA_binned[bin] <- lapply(MedGPA_binned[bin], factor)



# binned points and line
data_space <- ggplot(data = MedGPA_binned, mapping = aes(x = mean_GPA, y = acceptance_rate)) + geom_point() + geom_line()

# augmented model
MedGPA_plus <- augment(mod, type.predict = "response")
S
# logistic model on probability scale
data_space +
  geom_line(data = MedGPA_plus, aes(x = GPA, y = .fitted), color = "red")


# compute odds for bins
MedGPA_binned <- MedGPA_binned %>% 
  mutate(odds = acceptance_rate / (1 - acceptance_rate))

# plot binned odds
data_space <- ggplot(data = MedGPA_binned, aes(x = mean_GPA, y = odds)) + 
  geom_point() + geom_line()

# compute odds for observations
MedGPA_plus <- MedGPA_plus %>% 
  mutate(odds_hat = .fitted / (1 - .fitted))

# logistic model on odds scale
data_space +
  geom_line(data = MedGPA_plus, aes(x = GPA, y = odds_hat), color = "red")

# compute log odds for bins
MedGPA_binned <- MedGPA_binned %>% mutate(log_odds = log(acceptance_rate / (1 - acceptance_rate)))

# plot binned log odds
data_space <- ggplot(data = MedGPA_binned, aes(x = mean_GPA, y = log_odds)) + geom_point() + geom_line()

# compute log odds for observations
MedGPA_plus <- MedGPA_plus %>% mutate(log_odds_hat = log(.fitted / (1 - .fitted)))

# logistic model on log odds scale
data_space +
  geom_line(data = MedGPA_plus, aes(x = GPA, y = log_odds_hat), color = "red")

mod_GPA <- glm(formula = Acceptance ~ GPA, family = binomial, data = MedGPA)
exp(coef(mod_GPA))
# create new data frame

new_data <- data.frame(GPA = 3.51)

# make predictions

(augment(mod_GPA, type.predict = "response", newdata = new_data))
# data frame with binary predictions

tidy_GPA_mod <- augment(mod_GPA, type.predict = "response") %>% mutate(Acceptance_hat = round(.fitted))
tidy_GPA_mod
# confusion matrix
tidy_GPA_mod %>%
  select(Acceptance, Acceptance_hat) %>% 
  table()


