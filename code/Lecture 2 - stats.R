library(tidyverse)
library(ggplot2)

set.seed(2024)

# Load household data from CBS: 
# https://www.cbs.gov.il/he/publications/Pages/2023/%D7%9E%D7%A9%D7%A7%D7%99-%D7%91%D7%99%D7%AA-%D7%95%D7%9E%D7%A9%D7%A4%D7%97%D7%95%D7%AA-%D7%AA%D7%9B%D7%95%D7%A0%D7%95%D7%AA-%D7%93%D7%9E%D7%95%D7%92%D7%A8%D7%A4%D7%99%D7%95%D7%AA-2022-%D7%A2%D7%9C-%D7%A4%D7%99-%D7%A1%D7%A7%D7%A8-%D7%9B%D7%95%D7%97-%D7%90%D7%93%D7%9D.aspx
households <- readxl::read_excel("data/israel_households.xlsx",
                                 range = "D48:J48",
                                 col_names = c("7+", "6","5","4","3","2","1"))

households <- pivot_longer(households, cols = names(households))
households$nameInteger <- as.numeric(gsub("\\+", "", households$name))

# Create PDF (probability distribution function)
ggplot(households, aes(x = name, y=value)) + 
  geom_bar(stat="identity") + 
  theme_classic() +
  labs(x="Household size", y="Share")
ggsave("output/lecture2-stat/householdPdf.png", width = 10, units = "cm")

# Create a new column with CDF 
households <- households %>% arrange(name) %>% 
  mutate(upToValue = sapply(nameInteger, function(x) sum(households %>% filter(nameInteger<=x) %>% pull(value))))

# Adding zero value just to make the figure nicer
households = rbind(households, list(name=0, value = 0, nameInteger=0, upToValue = 0))

# Plot CDF
ggplot(households, aes(x = name, y=upToValue)) + 
  geom_step(group=1) +
  theme_classic() + 
  labs(x="Household size", y="Share <= x") +
  scale_y_continuous(limits = c(0, 100))
ggsave("output/lecture2-stat/householdCdf.png", width = 10, height = 6, units = "cm")


# *************************************************************************
#  NBA STATS ----
# *************************************************************************

# source: https://www.kaggle.com/datasets/vivovinco/20222023-nba-player-stats-regular?resource=download

NBA_regular <- read.delim("data/2022-2023 NBA/2022-2023 NBA Player Stats - Regular.csv",
                          sep = ";")

NBA_regular <- NBA_regular %>% mutate(ptsRank = ecdf(PTS)(PTS))

# PLOT PDF
ggplot(NBA_regular, aes(x=PTS)) +
  geom_density() + 
  theme_classic()
ggsave("output/lecture2-stat/pointsPDF.png", width = 15, units = "cm")


# PLOT CDF
ggplot(NBA_regular, aes(x = PTS)) + 
  stat_ecdf(geom = "point") +
  geom_point(data = NBA_regular %>%  filter(grepl("Avdija", Player)), aes(x=PTS, y=ptsRank, color="red")) +
  geom_text(data = NBA_regular %>%  filter(grepl("Avdija", Player)), aes(x=PTS+4, y=ptsRank, label = "Deni Avija", color="red")) + 
  theme_classic(base_size = 16) + 
  theme(legend.position = "none") + 
  labs(x="Points per game", y="") 
ggsave("output/lecture2-stat/pointsCDF.png", width = 15, units = "cm")

View(NBA_regular %>% select(Player, Tm, Age, AST, PTS) %>% sample_n(10))

# Summary stats
meanPts = mean(NBA_regular$PTS)
meanAsts = mean(NBA_regular$AST)

var(NBA_regular$PTS, na.rm=TRUE)
sd(NBA_regular$PTS, na.rm=TRUE)

# Scatter plots: points and assists
ggplot(NBA_regular, aes(x=PTS, y=AST)) + 
  geom_point(size=1) +
  theme_classic(base_size = 8) 
ggsave("output/lecture2-stat/pointsAssist.png", width = 9, height = 9, units = "cm")

NBA_regular <- NBA_regular %>% mutate(diffMeanPts = PTS - meanPts,
                                      diffMeanAst= AST - meanAsts,
                                      covPos = diffMeanPts*diffMeanAst>0)

ggplot(NBA_regular, aes(x=PTS, y=AST, color = covPos)) + 
  geom_point(size=1) +
  geom_vline(xintercept = meanPts) +
  geom_hline(yintercept = meanAsts) +
  theme_classic(base_size = 8) +
  theme(legend.position = "none")
ggsave("output/lecture2-stat/pointsAssistAdd.png", width = 9, height = 9, units = "cm")





# *************************************************************************
#  ESTIMATORS ----
# *************************************************************************


estimates = data.frame()

# Take sample of 100 points, repeat the exercise 50 times, each time different estimator for mean
for (i in 1:50) { 
  estimates[i,"mean_points_100"] = NBA_regular %>% slice_sample(n = 100, replace = TRUE) %>% 
    summarize(mean(PTS)) %>% pull()
}

# This time the sample size is 200, do this twice, once with the mean and once with a noisy mean
for (i in 1:50) { 
  estimates[i,"mean_points_200"] = NBA_regular %>% slice_sample(n = 200, replace = FALSE) %>% 
    summarize(mean(PTS)) %>% pull()
  
  estimates[i,"mean_points_200_noise"] = NBA_regular %>% slice_sample(n = 200, replace = FALSE) %>% 
    summarize(mean(PTS)) %>% pull() + 
    sample(c(1,-1),1)
}

# Repeast the exercise with a sample size of 300 and 500
for (i in 1:50) { 
  estimates[i,"mean_points_300"] = NBA_regular %>% slice_sample(n = 300, replace = FALSE) %>% 
    summarize(mean(PTS)) %>% pull()
}

for (i in 1:50) { 
  estimates[i,"mean_points_500"] = NBA_regular %>% slice_sample(n = 500, replace = FALSE) %>% 
    summarize(mean(PTS)) %>% pull()
}

estimates_long <- pivot_longer(estimates, cols = names(estimates))

# Show distribution
ggplot(estimates_long %>% filter(name=="mean_points_100"), aes(x=value)) + 
  geom_density() + 
  xlab("Estimate for mean points") +
  theme_classic()
ggsave("output/lecture2-stat/meanDistribution.png", width = 9, height = 6, units = "cm")

# Efficiency  
ggplot(estimates_long %>% filter(name %in% c("mean_points_200", "mean_points_200_noise")), 
       aes(x=value, color=name)) + 
  geom_density() + 
  xlab("Estimate for mean points") +
  theme_classic() 
ggsave("output/lecture2-stat/efficiency.png", width = 12, height = 6, units = "cm")


# Consistency  
ggplot(estimates_long %>% filter(name %in% c("mean_points_100", "mean_points_200", "mean_points_300", "mean_points_500")), 
       aes(x=value, color = name)) + 
  geom_density(geom="line") + 
  theme_classic()
ggsave("output/lecture2-stat/consistency.png", width = 12, height = 6, units = "cm")
