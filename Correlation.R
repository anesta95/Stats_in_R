# Compute correlation
ncbirths %>%
  summarize(N = n(), r = cor(weight, mage))

# Compute correlation for all non-missing pairs
ncbirths %>%
  summarize(N = n(), r = cor(weight, weeks, use = "pairwise.complete.obs"))

#The Anscombe Dataset
data("anscombe")
View(anscombe)
x <- c(anscombe[1,1:4], anscombe[2,1:4], anscombe[3,1:4], anscombe[4,1:4], anscombe[5,1:4], anscombe[6,1:4], anscombe[7,1:4], anscombe[8,1:4], anscombe[9,1:4], anscombe[10,1:4], anscombe[11,1:4])
y <- c(anscombe[1,5:8], anscombe[2,5:8], anscombe[3,5:8], anscombe[4,5:8], anscombe[5,5:8], anscombe[6,5:8], anscombe[7,5:8], anscombe[8,5:8], anscombe[9,5:8], anscombe[10,5:8], anscombe[11,5:8])
anscombe <- do.call(rbind, Map(data.frame, x=x, y=y))
View(anscombe)
anscombe <- anscombe %>% mutate(id=rep(1:11, each = 4), set=rep(seq(1,4), times = 11))
View(anscombe)
anscombe %>%
  group_by(set) %>%
  summarize(N = n(), mean(x), sd(x), mean(y), sd(y), cor(x, y))

# Correlation for all baseball players
mlbBat10 %>%
  summarize(N = n(), r = cor(x=OBP, SLG))

# Correlation for all players with at least 200 ABs
mlbBat10 %>%
  filter(AB >= 200) %>%
  summarize(N = n(), r = cor(OBP, SLG))

# Correlation of body dimensions
bdims %>%
  group_by(sex) %>%
  summarize(N = n(), r = cor(hgt, wgt))

# Correlation among mammals, with and without log
mammals %>%
  summarize(N = n(), 
            r = cor(BodyWt, BrainWt), 
            r_log = cor(log(BodyWt), log(BrainWt)))

#Spurious correlation in random data
# Create faceted scatterplot
ggplot(noise, mapping = aes(x=x, y=y)) + geom_point() + facet_wrap(~z)


# Compute correlations for each dataset
noise_summary <- noise %>%
  group_by(z) %>%
  summarize(N = n(), spurious_cor = cor(x, y))

# Isolate sets with correlations above 0.2 in absolute strength
noise_summary %>%
  filter(abs(spurious_cor) > 0.2)
        