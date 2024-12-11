library(tidyverse)
library(ggplot2)
library(colorspace)
library(ggpmisc)

rm(list=ls())

set.seed(2024)


# *************************************************************************
#  GDP ----
# *************************************************************************


createGDPPlot <- function(data, color = "black", addLine = TRUE, addPoints=TRUE) {
  ggplot(data, aes(x=logGDP, y=hours)) +
    { if(addPoints) geom_point(color = color)} +
    scale_x_continuous(limits = c(5.5, 12)) +  scale_y_continuous(limits = c(0, 2)) +
    { if (addLine) geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = color) } +
    theme_classic(base_size = 12) 
}


# Source: https://databank.worldbank.org/reports.aspx?source=2&series=NY.GDP.PCAP.CD&country=#
globalGDP <- read.csv("Data/P_Data_Extract_From_World_Development_Indicators/gdp.csv")

globalGDP <- globalGDP %>% select(country = Country.Code, gdp=X2021..YR2021.) %>%
  filter(gdp!=".." & !is.na(gdp)) %>%
  mutate(gdp = as.numeric(gdp), sqrtGDP = sqrt(gdp), logGDP = log(gdp)) 
  
  
# *************************************************************************
#  LINEARITY ----
# *************************************************************************

dataLin = as.data.frame(list(x = runif(100, 1, 100), 
                  epsilon = rnorm(100, 0, 100)))
dataLin <- dataLin %>% mutate(y = x^2 + epsilon)

ggplot(dataLin, aes(x = x, y=y)) +
  geom_point() + 
  geom_smooth(method='lm', formula= y~x, se=FALSE) +
  theme_classic(base_size = 8)
ggsave("output/lecture5-olsAssumptions/linearAssumption.png", width=6, height = 8, units = "cm")

# *************************************************************************
# IID DAY ----
# *************************************************************************

dataIID = as.data.frame(list(minutes = runif(20, 1, 100), 
                             day = round(runif(20, 0, 1)*3 + 0.5)))
                        
dataIID <-  dataIID %>% mutate(
  epsilon = rnorm(20, -mean(dataIID$day), 10) + 20*day,
  mood = 5 + 0.5*minutes + epsilon, 
  dayF = factor(day, levels=1:3, labels = c("ראשון", "שישי", "שלישי")))

observedLine = lm(mood ~ minutes, data = dataIID)
obsIntercept = observedLine$coefficients[["(Intercept)"]]
obsSlope = observedLine$coefficients[["minutes"]]

ggplot(dataIID, aes(x = minutes, y=mood, color = dayF)) +
  geom_point(size = 3) + 
  geom_abline(intercept = obsIntercept, slope = obsSlope) + 
  theme_classic(base_size = 18) + 
  theme(legend.title = element_blank(), legend.position = "bottom")
ggsave("output/lecture5-olsAssumptions/iidAssumption.png", width=10, height = 12, units = "cm")



# *************************************************************************
# OMITTED  ----
# *************************************************************************

n = 30
dataOmitted = as.data.frame(list(education = runif(n, 10, 20))) 

dataOmitted <- dataOmitted %>% mutate(
  parentCollege = ifelse(education + rnorm(n,0,5)<15, 0, 1),
  wage =  5000 + 100*education + 1000*parentCollege + rnorm(n, 0, 100),
  parentCollegeF = factor(parentCollege, levels = 0:1, 
                          labels = c("Parent high school", "Parent college")))

observedLine = lm(wage ~ education, data = dataOmitted)
obsIntercept = observedLine$coefficients[["(Intercept)"]]
obsSlope = observedLine$coefficients[["education"]]

ggplot(dataOmitted, aes(x = education, y=wage)) +
  geom_point(size = 3) + 
  geom_abline(intercept = obsIntercept, slope = obsSlope) + 
  theme_classic(base_size = 18) 
ggsave("output/lecture5-olsAssumptions/omittedAssumption1.png", width=12, height = 12, units = "cm")



ggplot(dataOmitted, aes(x = education, y=wage, color = parentCollegeF)) +
  geom_point(size = 3) + 
  geom_abline(intercept = obsIntercept, slope = obsSlope) + 
  theme_classic(base_size = 18) + 
  geom_smooth(method='lm', formula= y~x, se=FALSE) +
  theme(legend.title = element_blank(), legend.position = "bottom")
ggsave("output/lecture5-olsAssumptions/omittedAssumption2.png", width=12, height = 12, units = "cm")



# *************************************************************************
#  HUMAN DAY ----
# *************************************************************************

humanDay <- read.csv("Data/humanDay/all_countries.csv")

unique(humanDay$Subcategory)
unique(humanDay$Category)

humanDay <- humanDay %>% 
  select(country = countryISO3, Subcategory, hours = hoursPerDayCombined) %>% 
  filter(Subcategory=="Food growth & collection")

mergedData <- left_join(humanDay, globalGDP, by = "country")


regResult = lm(hours ~ logGDP, mergedData)
summary(regResult)

ggplot(mergedData, aes(x=gdp, y=hours)) +
  geom_point() +
  theme_classic() 
ggsave("output/lecture5-olsAssumptions/gdpHoursBeforeLog.png")




createGDPPlot(mergedData, addLine = FALSE) 
ggsave("output/lecture5-olsAssumptions/gdpHoursPlotPopulation.png")

createGDPPlot(mergedData) +
  geom_vline(xintercept = 8, linetype="dashed") +
  geom_vline(xintercept = 10, linetype="dashed")
ggsave("output/lecture5-olsAssumptions/gdpHoursPlotPopulationWithBorder.png")


# *************************************************************************
#  SAMPE SIZE ----
# *************************************************************************


mergedData$index = 1:nrow(mergedData)
sizeOfDraw = 10
sizeOfLargeDraw = 40
numberOfDraws = 5

# Pick 40 random points from all, and only from x between 100 and 2
#indexAll <- mergedData %>% sample_n(sizeOfDraw * numberOfDraws) %>% pull(index)
#indexLargeDraw <- mergedData %>% sample_n(sizeOfLargeDraw * numberOfDraws) %>% pull(index)
#indexLimited <- mergedData %>% filter(8<logGDP & logGDP<10) %>% 
#  sample_n(sizeOfDraw * numberOfDraws) %>% pull(index)

allBetas = data.frame()
allBetasLarge = data.frame()
limBetas = data.frame()

firstIndex = 1

for (i in 1:5) {
  
  #firstIndex = (i-1)*sizeOfDraw+1
  #lastIndex = firstIndex+sizeOfDraw-1
  
  #firstIndexLarge = (i-1)*sizeOfLargeDraw+1
  #lastIndexLarge = firstIndexLarge+sizeOfLargeDraw-1
  
  #currentDataLarge = mergedData %>% filter(index %in% indexLargeDraw[firstIndexLarge:lastIndexLarge])
  
  currentDataAll = mergedData %>% sample_n(sizeOfDraw)
  currentRegAll = lm(hours~logGDP, currentDataAll)
  allBetas = rbind(allBetas, 
                   list(beta0=currentRegAll$coefficients[["(Intercept)"]],
                        beta1 = currentRegAll$coefficients[["logGDP"]]))
  
  
  currentDataLarge = mergedData %>% sample_n(sizeOfLargeDraw)
  currentRegLarge = lm(hours~logGDP, currentDataLarge)
  allBetasLarge = rbind(allBetasLarge, 
                   list(beta0=currentRegLarge$coefficients[["(Intercept)"]],
                        beta1 = currentRegLarge$coefficients[["logGDP"]]))
  
  
  #currentDataLim = mergedData %>% filter(index %in% indexLimited[firstIndex:lastIndex])
  currentDataLim = mergedData %>% filter(8<logGDP & logGDP<10) %>%
    sample_n(sizeOfDraw)
  currentRegLim = lm(hours~logGDP, currentDataLim)
  limBetas = rbind(limBetas, 
                   list(beta0=currentRegLim$coefficients[["(Intercept)"]],
                        beta1 = currentRegLim$coefficients[["logGDP"]]))
  
  createGDPPlot(currentDataAll, color = rainbow_hcl(5)[i])
  ggsave(paste0("output/lecture5-olsAssumptions/gdpHoursPlotAll", i, ".png"))
  
  createGDPPlot(currentDataLim, color = rainbow_hcl(5)[i])
  ggsave(paste0("output/lecture5-olsAssumptions/gdpHoursPlotLim", i, ".png"))
}


createGDPPlot(mergedData, addLine = FALSE) + 
  geom_abline(intercept = allBetasLarge$beta0[1], slope = allBetasLarge$beta1[1], color = rainbow_hcl(5)[1]) + 
  geom_abline(intercept = allBetasLarge$beta0[2], slope = allBetasLarge$beta1[2], color = rainbow_hcl(5)[2]) + 
  geom_abline(intercept = allBetasLarge$beta0[3], slope = allBetasLarge$beta1[3], color = rainbow_hcl(5)[3]) + 
  geom_abline(intercept = allBetasLarge$beta0[4], slope = allBetasLarge$beta1[4], color = rainbow_hcl(5)[4]) + 
  geom_abline(intercept = allBetasLarge$beta0[5], slope = allBetasLarge$beta1[5], color = rainbow_hcl(5)[5]) 
ggsave(paste0("output/lecture5-olsAssumptions/gdpHoursPlotAllLargeCombined.png"))


createGDPPlot(mergedData %>% filter(index %in% indexAll), addLine = FALSE)
ggsave(paste0("output/lecture5-olsAssumptions/gdpHoursPlotAllPoints.png"))

createGDPPlot(mergedData %>% filter(index %in% indexAll), addLine = FALSE, addPoints = FALSE) + 
  geom_abline(intercept = allBetas$beta0[1], slope = allBetas$beta1[1], color = rainbow_hcl(5)[1]) + 
  geom_abline(intercept = allBetas$beta0[2], slope = allBetas$beta1[2], color = rainbow_hcl(5)[2]) + 
  geom_abline(intercept = allBetas$beta0[3], slope = allBetas$beta1[3], color = rainbow_hcl(5)[3]) + 
  geom_abline(intercept = allBetas$beta0[4], slope = allBetas$beta1[4], color = rainbow_hcl(5)[4]) + 
  geom_abline(intercept = allBetas$beta0[5], slope = allBetas$beta1[5], color = rainbow_hcl(5)[5]) 
ggsave(paste0("output/lecture5-olsAssumptions/gdpHoursPlotAllCombined.png"))

createGDPPlot(mergedData %>% filter(index %in% indexLimited), addLine = FALSE)
ggsave(paste0("output/lecture5-olsAssumptions/gdpHoursPlotLimPoints.png"))

createGDPPlot(mergedData %>% filter(index %in% indexLimited), addLine = FALSE) + 
  geom_abline(intercept = limBetas$beta0[1], slope = limBetas$beta1[1], color = rainbow_hcl(5)[1]) + 
  geom_abline(intercept = limBetas$beta0[2], slope = limBetas$beta1[2], color = rainbow_hcl(5)[2]) + 
  geom_abline(intercept = limBetas$beta0[3], slope = limBetas$beta1[3], color = rainbow_hcl(5)[3]) + 
  geom_abline(intercept = limBetas$beta0[4], slope = limBetas$beta1[4], color = rainbow_hcl(5)[4]) + 
  geom_abline(intercept = limBetas$beta0[5], slope = limBetas$beta1[5], color = rainbow_hcl(5)[5]) 
ggsave(paste0("output/lecture5-olsAssumptions/gdpHoursPlotLimCombined.png"))




# *************************************************************************
#  Compare partent grandparent height  ----
# *************************************************************************


heights = data.frame(student = c(170, 172, 164, 176, 175, 160, 159, 157, 180),
                         father = c(174, 173, 177, 178, 180, 172, 169, 168, 181),
                         grandmother = c(160, 161, 150, 163, 170, 163, 155, 156, 166))

heights$index = 1:nrow(heights)

heights <-heights %>% mutate(index = 1:nrow(heights), 
                             even =index %% 2 ==0,
                             threeGroups = index %% 3)

heightsLong = pivot_longer(heights, cols = c(father, grandmother))

ggplot(heightsLong, aes(x = value, y=student)) + 
  geom_point() +
  scale_x_continuous(limits = c(150, 190)) +  
  scale_y_continuous(limits = c(150, 190)) +  
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  facet_wrap(~name) +
  theme_classic() + 
  theme(legend.position = "none")



ggplot(heightsLong, aes(x = value, y=student, color = as.factor(threeGroups))) + 
  geom_point() +
  scale_x_continuous(limits = c(150, 190)) +  
  scale_y_continuous(limits = c(150, 190)) +  
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  facet_wrap(~name) +
  theme_classic() + 
  stat_poly_eq(use_label(c("eq"))) +
  theme(legend.position = "none")
