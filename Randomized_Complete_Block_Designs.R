install.packages("simputation")

library(tidyverse)
DEMO_file <- read_csv("demographics.csv")
MCQ_file <- read_csv("medical_conditions.csv")
BMX_file <- read_csv("body_measures.csv")


# Merge the 3 datasets you just created to create nhanes_combined
nhanes_combined <- list(DEMO_file, MCQ_file, BMX_file) %>%
  Reduce(function(df1, df2) inner_join(df1, df2, by = "seqn"), .)

#NHANES EDA

# Fill in the dplyr code
nhanes_combined %>% group_by(mcq365d) %>% summarise(mean = mean(bmxwt, na.rm = TRUE))

# Fill in the ggplot2 code
nhanes_combined %>% filter(ridageyr > 16) %>% 
  ggplot(aes(as.factor(mcq365d), bmxwt)) +
  geom_boxplot()


# Filter to keep only those 16+
nhanes_filter <- nhanes_combined %>% filter(ridageyr > 16)

# Load simputation & impute bmxwt by riagendr
install.packages("simputation")
library(simputation)
nhanes_final <- simputation::impute_median(nhanes_filter, bmxwt ~ riagendr)

# Recode mcq365d with recode() & examine with count()
nhanes_final$mcq365d <- recode(nhanes_final$mcq365d, 
                               `1` = 1,
                               `2` = 2,
                               `9` = 2)
nhanes_final %>% count(mcq365d)

# Use sample_n() to create nhanes_srs
nhanes_srs <- nhanes_final %>% sample_n(2500)

# Create nhanes_stratified with group_by() and sample_n()
nhanes_stratified <- nhanes_final %>%
  group_by(riagendr) %>% sample_n(2000)
nhanes_stratified %>% count(riagendr)

# Load sampling package and create nhanes_cluster with cluster()
install.packages("sampling")
library(sampling)
nhanes_cluster <- sampling::cluster(nhanes_final, c("indhhin2"), 6, method = "srswor")
install.packages("agricolae")
library(agricolae)
designs <- ls("package:agricolae", pattern = "design")
designs
#Use str() to view design.rcbd's criteria
str(design.rcbd)


# Build treats and rep
treats <- LETTERS[1:5]
blocks <- 4

# Build my_design_rcbd and view the sketch
my_design_rcbd <- design.rcbd(treats, r = blocks, seed = 42)
my_design_rcbd$sketch

# Use aov() to create nhanes_rcbd
nhanes_rcbd <- aov(bmxwt ~ mcq365d + riagendr, data = nhanes_final)

# Check the results of nhanes_rcbd with summary()
summary(nhanes_rcbd)

# Print the difference in weights by mcq365d and riagendr
nhanes_final %>% 
  group_by(mcq365d, riagendr) %>% 
  summarize(mean_wt = mean(bmxwt, na.rm = TRUE))

# Set up the 2x2 plotting grid and plot nhanes_rcbd
par(mfrow=c(2, 2))
plot(nhanes_rcbd)

# Run the code to view the interaction plots
with(nhanes_final, interaction.plot(mcq365d, riagendr, bmxwt))


# Run the code to view the interaction plots
with(nhanes_final, interaction.plot(riagendr, mcq365d, bmxwt))

# Create my_design_bibd_1
my_design_bibd_1 <- design.bib(LETTERS[1:3], k = 4, seed = 42)

#create my_design_bibd_2
my_design_bibd_2 <- design.bib(LETTERS[1:8], k = 3, seed = 42)

# Create my_design_bibd_3
my_design_bibd_3 <- design.bib(LETTERS[1:4], k = 4, seed = 42)
my_design_bibd_3$sketch

install.packages("lambda.r")
library(lambda.r)
lambda(t = 4, k = 3, r = 3)
?lambda
??<lambda>

# Calculate lambda
lambda(t = 4, k = 3, r = 3)

# Build the data.frame
creatinine <- c(1.98, 1.97, 2.35, 2.09, 1.87, 1.95, 2.08, 2.01, 1.84, 2.06, 1.97, 2.22)
food <- as.factor(c("A", "C", "D", "A", "B", "C", "B", "C", "D", "A", "B", "D"))
color <- as.factor(rep(c("Black", "White", "Orange", "Spotted"), each = 3))
cat_experiment <- as.data.frame(cbind(creatinine, food, color))

# Create cat_model and examine with summary()
cat_model <- aov(creatinine ~ food + color, data = cat_experiment)
summary(cat_model)  
  
# Create weightlift_model & examine results
weightlift_model <- aov(bmxarmc ~ weightlift_treat + ridreth1, data = nhanes_final)
summary(weightlift_model)


