library(ggplot2)
library(dplyr)
library(modelr) # modelr includes height

rm(list=ls())

heights <- heights %>% mutate(monthlyIncome = income / 12)

men = heights %>% filter(sex=="male")
women = heights %>% filter(sex=="female")

heightIncomeMen = lm(data = men, formula = monthlyIncome ~ height)
summary(heightIncomeMen)

heightIncomeWomen = lm(data = women, formula = monthlyIncome ~ height)
summary(heightIncomeWomen)


ggplot(women %>% sample_n(500), aes(x=height, y=income)) +
  geom_point() +
  theme_classic()

# Manually convert coefficents
inchToCm = 2.54
beta_1 <- heightIncomeMen$coefficients[["height"]]
beta_1 / inchToCm 

usdToNis = 3.3
beta_0 <- heightIncomeMen$coefficients[["(Intercept)"]]
beta_0 * usdToNis 
beta_1 / inchToCm  * usdToNis 

beta_0 * usdToNis + (beta_1 / inchToCm  * usdToNis ) * 174



men <- men %>% mutate(heightCM = height * 2.54, 
                      incomeNIS = monthlyIncome * 3.3)

model = lm(data = men, formula = incomeNIS ~ heightCM)
summary(model)
predict(model, data.frame(heightCM=174))


### Standard error
summary(lm(data = men, formula = monthlyIncome ~ heightCM))
33/2.54
215-33.196
215+33.196

85-13.196
85+13.196
