library(dplyr)
library(ggplot2)
library(ggpmisc)


rm(list=ls())

set.seed(2024)

Airbnb <- readRDS("data/airbnb.rds")

Airbnb <- Airbnb %>% 
  mutate(sqMt = sqFt / 10.764,
         cleaningFeeZero = if_else(is.na(cleaningFee), 0, cleaningFee)) %>%
  filter(20<sqMt & sqMt<300, 
         Country %in% c("United States", "United Kingdom", "France", "Germany"))
  


createAirbnbPlot <- function(xVar, name) {
  ggplot(data = Airbnb, aes(x=get(xVar), y=price)) +
    geom_point(size = 0.5) + 
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
    labs(y="Price", x=name) +
    theme_classic() +
    stat_poly_eq(use_label(c("R2", "eq")), color = "red", size = 4)
  ggsave(paste0("output/lecture4-rsquared/airbnb", name, ".png"), 
         width = 8, height = 7, unit = "cm")
}

createAirbnbPlot("sqMt", "Square Meters")
createAirbnbPlot("numReview", "Number of Reviews")
createAirbnbPlot("bedrooms", "Number of Bedrooms")
createAirbnbPlot("reviewRating", "Total Rating")
createAirbnbPlot("reviewLocation", "Location Rating")
createAirbnbPlot("reviewComm", "Communication Rating")
createAirbnbPlot("reviewClean", "Cleanliness Rating")
createAirbnbPlot("responseRate", "Response Rate")
createAirbnbPlot("accommodates", "Accomodates")
createAirbnbPlot("cleaningFee", "Cleaning Fee")
