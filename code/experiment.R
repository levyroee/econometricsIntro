
library(qualtRics)
library(dplyr)
library(stargazer)

set.seed(2023)
allNumbers = sample(100:999, 32, replace = FALSE)
selectedNumbers = allNumbers[sample(1:32, 16, replace=FALSE)]

selectedNumbers = c(965, 835, 236, 655, 171, 984, 232, 985, 292, 660, 460, 306, 854, 481, 744, 581)

qualtrics_api_credentials(install=TRUE, api_key = "5t18gdxt6Wht6AXZe43QY6gHacgzi1GklS19EGPr", base_url = "socialtau.fra1.qualtrics.com", overwrite = TRUE)
readRenviron("~/.Renviron")

# Get the qualtrics survey
# a <- all_surveys()
 a %>% filter(name=="ניסוי אקונומטריקה")
EXP_RAW <- fetch_survey(surveyID = "SV_bQvJoIAoVRjfv6e")

#EXP_RAW <- qualtRics::read_survey("data/experiment/experimentData.csv")

correctQuestions = paste0("Q8_", which(allNumbers %in% selectedNumbers))
inCorrectQuestions = paste0("Q8_", which(!allNumbers %in% selectedNumbers))


EXP <- EXP_RAW %>% mutate(across(starts_with("Q8_"), ~ !is.na(.x)))  %>% 
  mutate(scoreCorrect = rowSums(across(correctQuestions)),
         scoreIncorrect = rowSums(across(inCorrectQuestions)),
         score = scoreCorrect - scoreIncorrect,
         ageSq = age^2) %>% 
  filter(StartDate >= as.Date("2023-1-1"))
EXP %>% dplyr::select(StartDate, scoreCorrect, scoreIncorrect, score) %>% slice (20:30)

hist(EXP$score)
hist(EXP$seconds)

modelMem = lm(data = EXP, score ~ memory)
modelSecond = lm(data = EXP, score ~ seconds)
modelSecondsMem = lm(data = EXP, score ~ seconds + memory)
modelSecondsMemAge = lm(data = EXP, score ~ seconds + memory + age + ageSq)
stargazer(modelSecond, modelSecondsMem, modelSecondsMemAge, type = "text", omit.stat = "F")

cor(EXP$seconds, EXP$memory, use = "complete.obs")

ggplot(data = EXP, aes(x=seconds, y=score)) + 
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  stat_poly_eq(use_label(c("eq"))) +
  theme_classic() 
