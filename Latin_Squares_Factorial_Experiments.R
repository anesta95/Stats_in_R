install.packages("naniar")
library(naniar)
library(tidyverse)
nyc_scores <- read_csv("nyc_sat.csv")
str(nyc_scores)
# Mean, var, and median of Math score
nyc_scores %>%
  group_by(Borough) %>% 
  summarize(mean = mean(Average_Score_SAT_Math, na.rm = TRUE),
            var = var(Average_Score_SAT_Math, na.rm = TRUE),
            median = median(Average_Score_SAT_Math, na.rm = TRUE))
T_Ed_lvl_combo <- c("CollegeStudent GradStudent MA PhD MA BA CollegeStudent CollegeStudent CollegeStudent BA GradStudent BA MA CollegeStudent CollegeStudent MA MA MA BA CollegeStudent BA BA GradStudent PhD MA GradStudent CollegeStudent MA CollegeStudent CollegeStudent GradStudent BA GradStudent MA CollegeStudent GradStudent CollegeStudent GradStudent BA MA GradStudent GradStudent GradStudent BA BA GradStudent GradStudent GradStudent CollegeStudent MA BA MA CollegeStudent BA GradStudent GradStudent PhD GradStudent GradStudent BA GradStudent MA CollegeStudent CollegeStudent PhD PhD MA PhD GradStudent MA BA GradStudent CollegeStudent BA PhD MA MA PhD BA CollegeStudent GradStudent PhD BA GradStudent PhD GradStudent PhD MA MA MA MA CollegeStudent MA MA GradStudent MA MA GradStudent MA MA BA CollegeStudent CollegeStudent MA GradStudent GradStudent CollegeStudent PhD CollegeStudent BA MA MA GradStudent MA BA CollegeStudent MA MA MA CollegeStudent PhD PhD BA CollegeStudent CollegeStudent BA MA PhD BA BA GradStudent BA CollegeStudent MA MA MA MA BA CollegeStudent CollegeStudent CollegeStudent PhD GradStudent GradStudent PhD BA BA MA GradStudent BA MA CollegeStudent BA PhD CollegeStudent BA PhD BA MA CollegeStudent BA BA MA CollegeStudent GradStudent BA GradStudent MA GradStudent CollegeStudent PhD MA MA MA MA CollegeStudent BA GradStudent PhD CollegeStudent MA BA GradStudent GradStudent MA CollegeStudent BA BA BA GradStudent MA CollegeStudent CollegeStudent MA CollegeStudent CollegeStudent GradStudent CollegeStudent MA CollegeStudent CollegeStudent PhD MA GradStudent MA MA MA BA MA CollegeStudent GradStudent MA MA GradStudent CollegeStudent GradStudent BA MA GradStudent GradStudent GradStudent PhD MA CollegeStudent BA MA PhD MA MA BA CollegeStudent CollegeStudent BA BA CollegeStudent MA CollegeStudent MA PhD GradStudent MA GradStudent CollegeStudent GradStudent GradStudent GradStudent MA MA MA MA BA MA CollegeStudent BA BA MA BA GradStudent BA CollegeStudent GradStudent BA BA GradStudent PhD MA GradStudent BA CollegeStudent MA GradStudent GradStudent CollegeStudent BA MA MA BA BA GradStudent MA PhD CollegeStudent GradStudent MA CollegeStudent GradStudent MA MA GradStudent MA GradStudent MA PhD BA PhD BA MA PhD BA GradStudent PhD GradStudent GradStudent BA CollegeStudent BA MA GradStudent MA BA CollegeStudent CollegeStudent GradStudent MA CollegeStudent CollegeStudent GradStudent GradStudent CollegeStudent MA GradStudent CollegeStudent MA MA GradStudent MA CollegeStudent MA CollegeStudent MA MA CollegeStudent GradStudent BA CollegeStudent PhD GradStudent GradStudent GradStudent MA GradStudent CollegeStudent GradStudent MA MA GradStudent BA MA BA MA MA GradStudent GradStudent MA MA PhD GradStudent CollegeStudent MA BA PhD MA MA GradStudent PhD MA PhD MA GradStudent MA BA BA MA MA CollegeStudent CollegeStudent PhD GradStudent BA BA CollegeStudent MA PhD BA MA CollegeStudent CollegeStudent MA MA GradStudent GradStudent GradStudent MA BA BA PhD CollegeStudent MA MA CollegeStudent PhD MA MA CollegeStudent BA MA PhD MA CollegeStudent BA CollegeStudent MA BA BA GradStudent GradStudent MA CollegeStudent BA PhD MA MA GradStudent MA GradStudent PhD CollegeStudent PhD BA CollegeStudent PhD GradStudent GradStudent MA CollegeStudent")
Teacher_Education_Level <- str_split(T_Ed_lvl_combo, " ")
nyc_scores <- cbind(nyc_scores, Teacher_Education_Level)
nyc_scores <- select(nyc_scores, -c(23))
colnames(nyc_scores)
names(nyc_scores)[23] <- "Teacher_Education_Level"
# Mean, var, and median of Math score by Teacher Education Level
nyc_scores %>%
  group_by(Teacher_Education_Level) %>% 
  summarize(mean = mean(Average_Score_SAT_Math, na.rm = TRUE),
            var = var(Average_Score_SAT_Math, na.rm = TRUE),
            median = median(Average_Score_SAT_Math, na.rm = TRUE))
# Mean, var, and median of Math score by both
nyc_scores %>%
  group_by(Borough, Teacher_Education_Level) %>% 
  summarize(mean = mean(Average_Score_SAT_Math, na.rm = TRUE),
            var = var(Average_Score_SAT_Math, na.rm = TRUE),
            median = median(Average_Score_SAT_Math, na.rm = TRUE))

# Examine missingness with miss_var_summary()
nyc_scores %>%  miss_var_summary()
library(simputation)
# Examine missingness with md.pattern()
md.pattern(nyc_scores)

# Impute the Math score by Borough
nyc_scores_2 <- impute_median(nyc_scores, Average_Score_SAT_Math ~ Borough)

# Convert Math score to numeric
nyc_scores_2$Average_Score_SAT_Math <- as.numeric(nyc_scores_2$Average_Score_SAT_Math)

# Examine scores by Borough in both datasets, before and after imputation
nyc_scores %>% group_by(Borough) %>% summarise(median = median(Average_Score_SAT_Math, na.rm = TRUE), mean = mean(Average_Score_SAT_Math, na.rm = TRUE))
nyc_scores_2 %>% group_by(Borough) %>% summarise(median = median(Average_Score_SAT_Math, na.rm = TRUE), mean = mean(Average_Score_SAT_Math, na.rm = TRUE))

# Load agricolae
library(agricolae)
?design.lsd

trt <- LETTERS[1:5]
# Design a LS with 5 treatments A:E then look at the sketch
my_design_lsd <- design.lsd(trt, seed = 42)
my_design_lsd$sketch

# Build nyc_scores_ls_lm
nyc_scores_ls_lm <- lm(Average_Score_SAT_Math ~ Tutoring_Program + Borough + Teacher_Education_Level,
                       data = nyc_scores_ls)

# Tidy the results with broom
tidy(nyc_scores_ls_lm)

# Examine the results with anova
anova(nyc_scores_ls_lm)

# Create a boxplot of Math scores by Borough, with a title and x/y axis labels
ggplot(nyc_scores, mapping = aes(x = Borough, y = Average_Score_SAT_Math)) +
  geom_boxplot() + 
  ggtitle("Average SAT Math Scores by Borough, NYC") +
  xlab("Borough (NYC)") +
  ylab("Average SAT Math Scores (2014-15")

# Create trt1 and trt2
trt1 <- LETTERS[1:5]
trt2 <- 1:5

# Create my_graeco_design
my_graeco_design <- design.graeco(trt1, trt2, serie = 0, seed = 42)

# Examine the parameters and sketch
my_graeco_design$parameters
my_graeco_design$sketch

# Build nyc_scores_gls_lm
nyc_scores_gls_lm <- lm(Average_Score_SAT_Math ~ Tutoring_Program + Borough + Teacher_Education_Level + Homework_Type,
                        data = nyc_scores_gls)

# Tidy the results with broom
tidy(nyc_scores_gls_lm)

# Examine the results with anova
anova(nyc_scores_gls_lm)

# Build the boxplot for the tutoring program vs. Math SAT score
ggplot(nyc_scores,
       aes(x = Tutoring_Program, y = Average_Score_SAT_Math)) + 
  geom_boxplot()

# Build the boxplot for the percent black vs. Math SAT score
ggplot(nyc_scores,
       aes(x = Percent_Black_HL, y = Average_Score_SAT_Math)) + 
  geom_boxplot()

# Build the boxplot for percent tested vs. Math SAT score
ggplot(nyc_scores,
       aes(x = Percent_Tested_HL, y = Average_Score_SAT_Math)) + 
  geom_boxplot()

# Create nyc_scores_factorial and examine the results
nyc_scores_factorial <- aov(Average_Score_SAT_Math ~ Percent_Tested_HL * Percent_Black_HL * Tutoring_Program, data = nyc_scores)
tidy(nyc_scores_factorial)

# Use shapiro.test() to test the outcome
shapiro.test(nyc_scores$Average_Score_SAT_Math)

# Plot nyc_scores_factorial to examine residuals
par(mfrow = c(2,2))
plot(nyc_scores_factorial)





