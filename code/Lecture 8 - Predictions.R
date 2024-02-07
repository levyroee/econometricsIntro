library(dplyr)
library(ggplot2)
library(ggpmisc)


rm(list=ls())

set.seed(2024)

Airbnb <- readRDS("data/airbnb.rds")

# Choose data
Airbnb <- Airbnb %>% 
  mutate(sqMt = sqFt / 10.764,
         cleaningFeeZero = if_else(is.na(cleaningFee), 0, cleaningFee)) %>%
  filter(20<sqMt & sqMt<300, 
         Country %in% c("United States", "United Kingdom", "France", "Germany"),
         !is.na(price))

model <- lm(price~sqMt, data=Airbnb)
summary(model)

# 34.071+1.602*100 - calculate manually
model$coefficients["(Intercept)"] + model$coefficients["sqMt"] * 100

# Comapre to R
predict(model, newdata = data.frame(sqMt=100))


# Calculate prediction confidence internval manually
meanX = mean(Airbnb$sqMt)

Airbnb <- Airbnb %>% mutate(xMinusMeanXSq = (sqMt-meanX)^2)

predictionSE = sigma(model) * sqrt(1 + 1/nrow(Airbnb) + 
                           (100-meanX)^2 / sum(Airbnb$xMinusMeanXSq) )


194.2825 + qt(0.975, df=model$df.residual)*predictionSE
194.2825 - qt(0.975, df=model$df.residual)*predictionSE

# Compare to command
predict(model, newdata = data.frame(sqMt=100), interval = "prediction")


# Calculate prediction confidence interval for mean manually
confidenceSE = sigma(model) * sqrt(1/nrow(Airbnb) + 
                                     (100-meanX)^2 / sum(Airbnb$xMinusMeanXSq) )

194.2825 + qt(0.975, df=model$df.residual)*confidenceSE
194.2825 - qt(0.975, df=model$df.residual)*confidenceSE


# compare to command
predict(model, newdata = data.frame(sqMt=100), interval = "confidence")


# *************************************************************************
#  PLOTS ----
# *************************************************************************


createAirbnbPlotWithSE <- function(xVar, name) {
  ggplot(data = Airbnb, aes(x=get(xVar), y=price)) +
    geom_point(size = 0.5) + 
    geom_smooth(method = "lm", formula = y ~ x) +
    labs(y="Price", x=name) +
    theme_classic() +
    stat_poly_eq(use_label(c("R2", "eq")), color = "red", size = 4, level=0.6)
  ggsave(paste0("output/lecture8-predictions/airbnb", name, ".png"), 
         width = 8, height = 7, unit = "cm")
}

createAirbnbPlotWithSE("sqMt", "Square Meters")
createAirbnbPlotWithSE("bedrooms", "Number of Bedrooms")
createAirbnbPlotWithSE("reviewRating", "Total Rating")
createAirbnbPlotWithSE("cleaningFee", "Cleaning Fee")



