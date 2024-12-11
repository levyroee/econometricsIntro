library(dplyr)
library(ggplot2)
library(ggpmisc)
library(stargazer)
library(raster)
library(tidyr)

rm(list=ls())

set.seed(2024)

AirbnbRaw <- readRDS("data/airbnb.rds")

# Focus only on France in this exercise
Airbnb <- AirbnbRaw %>% 
  mutate(sqMt = sqFt / 10.764) %>%
  filter(!is.na(sqMt) & 25<sqMt & sqMt<500 & 
           City=="London" &
         30<price & price<450)

# Find the lat long
Airbnb <- separate_wider_delim(Airbnb, location, delim = ", ", names = c("loc1", "loc2"))

# Find the distance from downtown
downtownParis = c(48.864716, 2.349014) 
downtownLondon = c(51.5072, -0.1276)
downtownBerlin = c(52.5200, 13.4050)
downtownNY = c(40.7128, 74.0060)
Airbnb$dist = mapply(function(x,y) pointDistance(p1 = downtownLondon, p2 = c(as.numeric(x), as.numeric(y)), lonlat = TRUE),
                                                 Airbnb$loc1, Airbnb$loc2)/1000

# Run the regressions

oneVar <- lm(price~dist, data=Airbnb)

multi <- lm(price~sqMt + dist + price, data=Airbnb)

partial1 <- lm(dist~sqMt, data=Airbnb)
Airbnb <- Airbnb %>% mutate(residDist = partial1$residuals)
partial2 <- lm(price~residDist, data=Airbnb)





# Display the regressions
stargazer(oneVar, multi, partial1, partial2, type="text", omit.stat = "F")

dataPlot = rbind(Airbnb %>% dplyr::select(x=dist, price) %>% mutate(type = "x = Original Distance"),
                 Airbnb %>% dplyr::select(x=residDist, price) %>% mutate(type = "x = Residualized Distance"))

# Draw regression line with points
ggplot(dataPlot, aes(x = x, y = price)) +
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  stat_poly_eq(use_label(c("eq")), color = "red") + 
  theme_classic() +
  facet_wrap(~type, scales = "free_x") 


Airbnb <- readRDS("data/airbnb.rds") %>% 
  mutate(sqMt = sqFt / 10.764) 

naive1 <- lm(price~ reviewRating + sqMt, data=Airbnb)
naive2 <- lm(price~ reviewRating + sqMt + roomType, data=Airbnb)
stargazer(naive1, naive2, type="text")
