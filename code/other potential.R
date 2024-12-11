
# *************************************************************************
#  SECTION ----
# *************************************************************************

wvs <- readRDS("data/F00011421-WVS_Cross-National_Wave_7_rds_v5_0/WVS_Cross-National_Wave_7_Rds_v5_0.rds")

wvsSelected <- wvs %>% dplyr::select(leftRight = Q240, income = Q288, education = Q275R, numPeople = Q270, age = Q262, 
                                     sex = Q260, taxJust = Q180, demTaxTheRich = Q241, family=Q1, religOrg = Q94, attend = Q171, relig = Q173,
                                     divorce = Q185, B_COUNTRY_ALPHA, deathPenalty = Q195, abortion = Q184)

wvsSelected <- wvsSelected %>% 
  mutate(ageSq = age^2) %>% 
  mutate_all(function(x) if_else(x<0, NA, x)) %>%
  filter(B_COUNTRY_ALPHA=="BRA") %>%
  drop_na()

table(wvs$C_COW_ALPHA)

modelSingle = lm(data = wvsSelected, abortion ~ education)
modelMulti = lm(data = wvsSelected, abortion ~ relig + attend + income +  education + numPeople + sex  + age + ageSq + leftRight)


stargazer(modelSingle, modelMulti, type="text", omit.stat = "F")


table(wvs$Q275)


partial1 <- lm(data=wvsSelected, income~education + numPeople + age + sex)
wvsSelected <- wvsSelected %>% mutate(residIncome = partial1$residuals)

# *************************************************************************
#  SECTION ----
# *************************************************************************


israelPol <- read.csv("data/israel_polarization/IPP_wide_19_3_23.csv")
table(israelPol$w11_social_ladder)

single <- lm(data = israelPol, w11_left_right ~ w11_social_ladder)
multi <- lm(data = israelPol, w11_left_right ~ w11_social_ladder + w8_left_right)
stargazer(single, multi, type="text", omit.stat = "F")


israelPol = israelPol %>% mutate(income_corona = case_match(w8_income_coronavirus, 
                                                            "My income was greatly reduced as is less than half than it was prior to the crisis" ~ 1,
                                                            "My income was greatly reduced and is about half than it was prior to the crisis" ~ 2,
                                                            "My income was significantly reduced (around 20%-40%) in comparison to what it was prior to the crisis" ~ 3,
                                                            "My income was a little reduced (around 10%) in comparison to what it was before the crisis" ~ 4,
                                                            "My income has remained similar to what it was prior to the crisis" ~ 5, 
                                                            "My income has increased by a little (around 10%) in comparison to what it was before the crisis" ~ 6,
                                                            "My income has significantly increased (around 20%-40%) in comparison to what it was before the crisis" ~ 7,
                                                            "My income has greatly increased by half than it was before the crisis" ~ 8,
                                                            "My income has greatly increase by more than half than it was before the crisis" ~ 9), 
                                 gov_harm = case_match(w8_gov_policy_impact,
                                                       "I was not harmed at all" ~ 1,
                                                       "I was harmed much less than the average" ~ 2,
                                                       "I was harmed less than the average" ~ 3, 
                                                       "I was harmed a little less than the average" ~ 4,
                                                       "I was harmed to an average extent" ~ 5,
                                                       "I was harmed a little more than the average" ~ 6,
                                                       "I was harmed more than the average" ~ 7,
                                                       "I was harmed much more than the average" ~ 8),
                                 netanyahu_media = case_match(w8_netanyahu_media_self,
                                                              "Very much object" ~ 1,
                                                              "Object" ~ 2,
                                                              "Neither object nor support" ~ 3, 
                                                              "Support" ~4,
                                                              "Very much support" ~ 5))


single <- lm(data = israelPol, w8_thermometer_Likud ~ income_corona)
multi <- lm(data = israelPol, w8_thermometer_Likud ~ income_corona + w7_thermometer_Likud)
stargazer(single, multi, type="text", omit.stat = "F")

single <- lm(data = israelPol, w8_thermometer_Likud ~ income_corona)
multi <- lm(data = israelPol, w8_thermometer_Likud ~ income_corona + w7_thermometer_Likud)

table(israelPol$w8_gov_policy_impact)


