library(dplyr)
library(modelr)

rm(list=ls())

men = heights %>% filter(sex=="male")
women = heights %>% filter(sex=="female")

heightIncomeMen = lm(data = men, formula = income ~ height)
summary(heightIncomeMen)

heightIncomeWomen = lm(data = women, formula = income ~ height)
summary(heightIncomeWomen)

library(ggplot2)
ggplot(women %>% sample_n(500), aes(x=height, y=income)) +
  geom_point() +
  theme_classic()


men <- men %>% mutate(heightCM = height * 2.54, 
                      incomeNIS = income / 12 * 3.8)

summary(lm(data = men, formula = incomeNIS ~ heightCM))


