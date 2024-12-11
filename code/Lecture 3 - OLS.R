library(dplyr)
library(ggplot2)
library(ggpmisc)
library(qualtRics)
library(ggrepel)


rm(list=ls())

set.seed(2024)

beta_0 = 2000
beta_1 = 0.4

# *************************************************************************
#  IN CLASS SIMULATION Two samples----
# *************************************************************************

qualtrics_api_credentials(install=TRUE, api_key = "5t18gdxt6Wht6AXZe43QY6gHacgzi1GklS19EGPr", base_url = "socialtau.fra1.qualtrics.com", overwrite = TRUE)
readRenviron("~/.Renviron")

# Get the qualtrics survey
survey <- fetch_survey(surveyID = "SV_cUqGTMGurYwfKJg")

# Save sample of 10 participants
sample <- survey %>% filter(group %in% c(1,2,3, "other")) %>% 
                               sample_n(10) 
  
# Show height and parent hegiht
sample %>% select(height, parentHeight)

# Calculate beta_1 and beta_0
observedLine = lm(height ~ parentHeight, data = sample)

obsIntercept = observedLine$coefficients[["(Intercept)"]]
obsSlope = observedLine$coefficients[["parentHeight"]]

# Draw regression line with points
ggplot(sample, aes(x = parentHeight, y=height, label=name)) +
  geom_point() + 
  geom_abline(intercept = obsIntercept, slope = obsSlope) + 
  #geom_text_repel() +
  theme_classic()

# Create random numbers
survey <- survey %>% 
  mutate(randomSample = round(runif(50)* nrow(survey)/10 + 0.5))

# Now draw two samples
sampleTwo <- survey %>% filter(group %in% c(1,2,3, "other") & randomSample %in% c(1,2)) 


# Draw regression line with points
ggplot(sampleTwo, aes(x = parentHeight, y=height, label=name)) +
  geom_point() + 
  geom_smooth(method='lm', formula= y~x, se=FALSE) +
  geom_text_repel() +
  facet_wrap(~randomSample) +
  theme_classic()






# *************************************************************************
#  SIMULATION Two samples----
# *************************************************************************

for (i in 1:2) {
  
  # Create data
  mpcData = data.frame( 
    x = rnorm(40, 6000, 2000),
    e = rnorm(40, 0, 1000)
  )
  
  mpcData <- mpcData %>% mutate(observedY = beta_0 + x*beta_1 + e)
  
  # Calculate line
  observedLine = lm(observedY~x, data = mpcData)
  obsIntercept = observedLine$coefficients[["(Intercept)"]]
  obsSlope = observedLine$coefficients[["x"]]
  
  # Create plot
  ggplot(mpcData, aes(x = x, y=observedY)) +
    geom_point() + 
    geom_abline(intercept = obsIntercept, slope = obsSlope) + 
    geom_abline(intercept = beta_0, slope = beta_1, color = "red") + 
    geom_text(aes(x=6500, y=11000, label = paste0(expression(hat(beta)),"[0]==", round(obsIntercept))), parse = TRUE) +
    geom_text(aes(x=6500, y=11500, label = paste0(expression(hat(beta)),"[1]==", round(obsSlope,digits = 4))), parse = TRUE) +
    scale_y_continuous(limits = c(0,12000)) +
    scale_x_continuous(limits = c(0,12000)) +
    theme_classic() 
  
  ggsave(paste0("output/lecture3-ols/mpcResult", i, ".png"), height = 15, width = 12, units = "cm")
}



# *************************************************************************
#  EXAMPLE calculate beta ----
# *************************************************************************


mpcSmall = data.frame( 
  x = c(10, 11, 9, 10, 12, 9),
  y = c(9.5, 9.2, 8.9, 8.8, 9.9, 8.8)
)

avgX = mean(mpcSmall$x)
avgY = mean(mpcSmall$y)

mpcSmall <- mpcSmall %>% mutate(u = y - beta_0 - x*beta_1,
                                xMinusAvgXTimesY = (x-avgX)*y,
                                xMinusAvgXSq = (x-avgX)^2)

beta1Numerator = sum(mpcSmall$xMinusAvgXTimesY)
beta1Denominator = sum(mpcSmall$xMinusAvgXSq)
cov(mpcSmall$x, mpcSmall$y)

beta_1_hat = beta1Numerator / beta1Denominator
beta_0_hat = avgY - beta_1_hat*avgX

beta_0_hat
beta_1_hat

# Verify that we got the correct coefficients 
lm(y~x, data = mpcSmall)


                              
mpcSmall <- mpcSmall %>% mutate(y_hat = beta_0_hat + x*beta_1_hat,
                                u_hat = y_hat-y)

# Checking algebric properties
round(sum(mpcSmall$u_hat))
round(sum(mpcSmall$u_hat*mpcSmall$x))
mean(mpcSmall$y_hat) - avgY

write.csv(mpcSmall, "output/lecture3-ols/mpcSmallTable.csv")


# *************************************************************************
#  EXAMPLE Airbnb data ----
# *************************************************************************


options(scipen=999)

# Data downloaded here: https://public.opendatasoft.com/explore/dataset/airbnb-listings/export/?disjunctive.host_verifications&disjunctive.amenities&disjunctive.features
Airbnb_Raw <- readr::read_delim("data/airbnb-listings.csv")
Airbnb <- Airbnb_Raw %>% dplyr::select(City, sqFt = `Square Feet`, Country, 
                                numReview = `Number of Reviews`,
                                price = Price,
                                guests = `Guests Included`,
                                totalListings = `Host Listings Count`,
                                reviewRating = `Review Scores Rating`,
                                reviewLocation = `Review Scores Location`,
                                reviewClean = `Review Scores Cleanliness`,
                                reviewComm = `Review Scores Communication`,
                                responseRate = `Host Response Rate`,
                                bedrooms = Bedrooms,
                                propertyType = `Property Type`,
                                roomType = `Room Type`,
                                accommodates = Accommodates,
                                cleaningFee = `Cleaning Fee`,
                                location = Geolocation) 
saveRDS(Airbnb, "data/airbnb.rds")


Airbnb_Raw %>% select(Geolocation, Latitude, Longitude) %>% slice(1:10)
Airbnb_Raw$Latitude[1]

Airbnb <- readRDS("data/airbnb.rds")

Airbnb <- Airbnb %>% mutate(sqMt = sqFt / 10.764) 


ggplot(Airbnb %>% filter(20<sqMt & sqMt<300, Country %in% c("United States", "United Kingdom", "France", "Germany")), aes(x = sqMt, y=Price)) +
  geom_point(size = 0.5) + 
  theme_classic() 
ggsave("output/lecture3-ols/airbnbPoints.png", width = 16, height = 14, unit = "cm")

ggplot(Airbnb %>% filter(20<sqMt & sqMt<300, Country %in% c("United States", "United Kingdom", "France", "Germany")), aes(x = sqMt, y=Price)) +
  geom_point(size = 0.5) + 
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  theme_classic() +
  stat_poly_eq(use_label("eq"), color = "red")
ggsave("output/lecture3-ols/airbnbAll.png", width = 16, height = 14, unit = "cm")


ggplot(Airbnb %>% filter(20<sqMt & sqMt<300, Country %in% c("United States", "United Kingdom", "France", "Germany")), 
       aes(x = sqMt, y=Price)) +
  geom_point(size = 0.5) + 
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  theme_classic() +
  facet_wrap(~Country) +
  stat_poly_eq(use_label(c("eq")), color = "red")
ggsave("output/lecture3-ols/airbnbByCity.png", width = 16, height = 14, unit = "cm")
