library(dplyr)
library(ggplot2)
library(ggpmisc)
library(stargazer)
library(raster)

rm(list=ls())

set.seed(2024)

Airbnb <- readRDS("data/airbnb.rds")

# Focus only on France in this exercise
Airbnb <- Airbnb %>% 
  mutate(sqMt = sqFt / 10.764,
         cleaningFeeZero = if_else(is.na(cleaningFee), 0, cleaningFee)) %>%
  filter(20<sqMt & sqMt<300 & Country=="France" &
         !is.na(price) & !is.na(reviewLocation))

# Find the lat long
Airbnb <- separate_wider_delim(Airbnb, location, delim = ", ", names = c("loc1", "loc2"))

# Find the distance from Paris
downtownParis = c(48.864716, 2.349014)
Airbnb$dist = mapply(function(x,y) pointDistance(p1 = downtownParis, p2 = c(as.numeric(x), as.numeric(y)), lonlat = TRUE),
                                                 Airbnb$loc1, Airbnb$loc2)/1000

# Only keep apt 10 km from Paris
Airbnb <- Airbnb %>% filter(dist<10)

# Run the regressions
naive1 <- lm(price~dist, data=Airbnb)

partial1 <- lm(dist~sqMt, data=Airbnb)
Airbnb <- Airbnb %>% mutate(residDist = partial1$residuals)
partial2 <- lm(price~residDist, data=Airbnb)

multi <- lm(price~sqMt + dist + price, data=Airbnb)

# Display the regressions
stargazer(naive1, partial1, partial2, multi, type="text", omit.stat = "F")
