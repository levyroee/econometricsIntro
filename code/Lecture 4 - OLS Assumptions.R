library(tidyverse)
library(ggplot2)
library(colorspace)

rm(list=ls())

set.seed(2024)


createGDPPlot <- function(data, color = "black", addLine = TRUE) {
  ggplot(data, aes(x=logGDP, y=hours)) +
    geom_point(color = color) +
    scale_x_continuous(limits = c(5.5, 12)) +  scale_y_continuous(limits = c(0, 2)) +
    { if (addLine) geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = color) } +
    theme_classic() 
}


# Source: https://databank.worldbank.org/reports.aspx?source=2&series=NY.GDP.PCAP.CD&country=#
globalGDP <- read.csv("Data/P_Data_Extract_From_World_Development_Indicators/gdp.csv")

globalGDP <- globalGDP %>% select(country = Country.Code, gdp=X2021..YR2021.) %>%
  filter(gdp!=".." & !is.na(gdp)) %>%
  mutate(gdp = as.numeric(gdp), sqrtGDP = sqrt(gdp), logGDP = log(gdp)) 
  
  

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
ggsave("output/lecture4-olsAssumptions/gdpHoursBeforeLog.png")




createGDPPlot(mergedData, addLine = FALSE) 
ggsave("output/lecture4-olsAssumptions/gdpHoursPlotPopulation.png")

createGDPPlot(mergedData) +
  geom_vline(xintercept = 8, linetype="dashed") +
  geom_vline(xintercept = 10, linetype="dashed")
ggsave("output/lecture4-olsAssumptions/gdpHoursPlotPopulationWithBorder.png")


mergedData$index = 1:nrow(mergedData)
sizeOfDraw = 8
sizeOfLargeDraw = 16
numberOfDraws = 5

# Pick 40 random points from all, and only from x between 100 and 2
indexAll <- mergedData %>% sample_n(sizeOfDraw * numberOfDraws) %>% pull(index)
indexLargeDraw <- mergedData %>% sample_n(sizeOfLargeDraw * numberOfDraws) %>% pull(index)
indexLimited <- mergedData %>% filter(8<logGDP & logGDP<10) %>% 
  sample_n(sizeOfDraw * numberOfDraws) %>% pull(index)

allBetas = data.frame()
allBetasLarge = data.frame()
limBetas = data.frame()

firstIndex = 1



for (i in 1:5) {
  
  firstIndex = (i-1)*sizeOfDraw+1
  lastIndex = firstIndex+sizeOfDraw-1
  
  firstIndexLarge = (i-1)*sizeOfLargeDraw+1
  lastIndexLarge = firstIndexLarge+sizeOfLargeDraw-1
  

  currentDataLarge = mergedData %>% filter(index %in% indexLargeDraw[firstIndexLarge:lastIndexLarge])
  currentRegLarge = lm(hours~logGDP, currentDataLarge)
  allBetasLarge = rbind(allBetasLarge, 
                   list(beta0=currentRegLarge$coefficients[["(Intercept)"]],
                        beta1 = currentRegLarge$coefficients[["logGDP"]]))
  
  currentDataAll = mergedData %>% filter(index %in% indexAll[firstIndex:lastIndex])
  currentRegAll = lm(hours~logGDP, currentDataAll)
  allBetas = rbind(allBetas, 
                   list(beta0=currentRegAll$coefficients[["(Intercept)"]],
                        beta1 = currentRegAll$coefficients[["logGDP"]]))
  
  currentDataLim = mergedData %>% filter(index %in% indexLimited[firstIndex:lastIndex])
  currentRegLim = lm(hours~logGDP, currentDataLim)
  limBetas = rbind(limBetas, 
                   list(beta0=currentRegLim$coefficients[["(Intercept)"]],
                        beta1 = currentRegLim$coefficients[["logGDP"]]))
  
  createGDPPlot(currentDataAll, color = rainbow_hcl(5)[i])
  ggsave(paste0("output/lecture4-olsAssumptions/gdpHoursPlotAll", i, ".png"))
  
  createGDPPlot(currentDataLim, color = rainbow_hcl(5)[i])
  ggsave(paste0("output/lecture4-olsAssumptions/gdpHoursPlotLim", i, ".png"))
}


createGDPPlot(mergedData %>% filter(index %in% indexLargeDraw), addLine = FALSE) + 
  geom_abline(intercept = allBetasLarge$beta0[1], slope = allBetasLarge$beta1[1], color = rainbow_hcl(5)[1]) + 
  geom_abline(intercept = allBetasLarge$beta0[2], slope = allBetasLarge$beta1[2], color = rainbow_hcl(5)[2]) + 
  geom_abline(intercept = allBetasLarge$beta0[3], slope = allBetasLarge$beta1[3], color = rainbow_hcl(5)[3]) + 
  geom_abline(intercept = allBetasLarge$beta0[4], slope = allBetasLarge$beta1[4], color = rainbow_hcl(5)[4]) + 
  geom_abline(intercept = allBetasLarge$beta0[5], slope = allBetasLarge$beta1[5], color = rainbow_hcl(5)[5]) 
ggsave(paste0("output/lecture4-olsAssumptions/gdpHoursPlotAllLargeCombined.png"))


createGDPPlot(mergedData %>% filter(index %in% indexAll), addLine = FALSE)
ggsave(paste0("output/lecture4-olsAssumptions/gdpHoursPlotAllPoints.png"))

createGDPPlot(mergedData %>% filter(index %in% indexAll), addLine = FALSE) + 
  geom_abline(intercept = allBetas$beta0[1], slope = allBetas$beta1[1], color = rainbow_hcl(5)[1]) + 
  geom_abline(intercept = allBetas$beta0[2], slope = allBetas$beta1[2], color = rainbow_hcl(5)[2]) + 
  geom_abline(intercept = allBetas$beta0[3], slope = allBetas$beta1[3], color = rainbow_hcl(5)[3]) + 
  geom_abline(intercept = allBetas$beta0[4], slope = allBetas$beta1[4], color = rainbow_hcl(5)[4]) + 
  geom_abline(intercept = allBetas$beta0[5], slope = allBetas$beta1[5], color = rainbow_hcl(5)[5]) 
ggsave(paste0("output/lecture4-olsAssumptions/gdpHoursPlotAllCombined.png"))

createGDPPlot(mergedData %>% filter(index %in% indexLimited), addLine = FALSE)
ggsave(paste0("output/lecture4-olsAssumptions/gdpHoursPlotLimPoints.png"))

createGDPPlot(mergedData %>% filter(index %in% indexLimited), addLine = FALSE) + 
  geom_abline(intercept = limBetas$beta0[1], slope = limBetas$beta1[1], color = rainbow_hcl(5)[1]) + 
  geom_abline(intercept = limBetas$beta0[2], slope = limBetas$beta1[2], color = rainbow_hcl(5)[2]) + 
  geom_abline(intercept = limBetas$beta0[3], slope = limBetas$beta1[3], color = rainbow_hcl(5)[3]) + 
  geom_abline(intercept = limBetas$beta0[4], slope = limBetas$beta1[4], color = rainbow_hcl(5)[4]) + 
  geom_abline(intercept = limBetas$beta0[5], slope = limBetas$beta1[5], color = rainbow_hcl(5)[5]) 
ggsave(paste0("output/lecture4-olsAssumptions/gdpHoursPlotLimCombined.png"))
