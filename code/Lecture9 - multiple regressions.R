library(stargazer)
library(dplyr)

library(ggpmisc)

NBA <- read.delim("data/2022-2023 NBA/2022-2023 NBA Player Stats - Regular.csv",
                          sep = ";")
attributes(NBA$MP)
# Create sq var
NBA <- NBA %>% mutate(AgeSq = Age^2)

# Run regressions
model1 <- lm(formula = PTS ~ Age + AgeSq, data=NBA)
model2 <- lm(formula = PTS ~ Age + AgeSq + MP, data=NBA)

# Display regressions
stargazer(model1, model2, type = "text")

# Create plot
ggplot(NBA, aes(x = Age, y=PTS)) + 
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x,2,raw = TRUE), se = FALSE) +
  stat_poly_eq(formula = y ~ poly(x,2, raw = TRUE), use_label(c("eq")), size=8) + 
  theme_classic(base_size = 15)
ggsave("output/Lecture9-multipleRegressions/ageSqPts.png", width = 24, height = 20, units = "cm")




################

Airbnb <- readRDS("data/airbnb.rds")


Airbnb <- Airbnb %>% 
  mutate(sqMt = sqFt / 10.764,
         priceNIS100 = price * 3.3,
         numReviewSq = numReview^2) %>% 
  filter(20<sqMt & sqMt<300, Country %in% c("United States", "United Kingdom", "France", "Germany"))

model1 = lm(formula = price ~ bedrooms, data = Airbnb)
model2 = lm(formula = price ~ bedrooms + accommodates, data = Airbnb)
model3 = lm(formula = price ~ bedrooms + reviewRating, data = Airbnb)


stargazer(model1, model2, model3, type = "text")
cor(Airbnb$bedrooms, Airbnb$accommodates, use = "complete.obs")
cor(Airbnb$bedrooms, Airbnb$reviewRating, use = "complete.obs")
cor(Airbnb$priceNIS100, Airbnb$numReview, use = "complete.obs")
