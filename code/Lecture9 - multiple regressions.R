library(stargazer)
library(dplyr)

library(ggpmisc)

NBA <- read.delim("data/2022-2023 NBA/2022-2023 NBA Player Stats - Regular.csv",
                          sep = ";")

# Create sq var
NBA <- NBA %>% mutate(AgeSq = Age^2)

# Run regressions
model1 <- lm(formula = PTS ~ Age + AgeSq, data=NBA)
model2 <- lm(formula = PTS ~ Age + AgeSq + G, data=NBA)

# Display regressions
stargazer(model1, model2, type = "text")

# Create plot
ggplot(NBA, aes(x = Age, y=PTS)) + 
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x,2,raw = TRUE), se = FALSE) +
  stat_poly_eq(formula = y ~ poly(x,2, raw = TRUE), use_label(c("eq")), size=8) + 
  theme_classic(base_size = 15)
ggsave("output/Lecture9-multipleRegressions/ageSqPts.png", width = 24, height = 20, units = "cm")
