#### Descriptive Analysis ####

## Load relevant packages
library(psych)
library(car)
library(lavaan)
library(MBESS)

## Import cleaned and merged data set
thesis <- read.csv(file = "thesis.csv", 
                   header = TRUE, 
                   stringsAsFactors = FALSE)

#### General ####

describe(thesis[,c("percentOnTask",
                   "elaPerformance",
                   "mathPerformance",
                   "absences",
                   "SIBS.sum",
                   "SEBS.sum",
                   "AES.sum",
                   "SPS.sum",
                   "PES.sum"
                   )])

summary(thesis[,c("percentOnTask",
                  "elaPerformance",
                  "mathPerformance",
                  "absences",
                  "SIBS.sum",
                  "SEBS.sum",
                  "AES.sum",
                  "SPS.sum",
                  "PES.sum"
)])

#### Correlations ####

# Subset relevant variables
VarDF <- data.frame(thesis[,c("percentOnTask",
           "elaPerformance",
           "mathPerformance",
           "absences",
           "SIBS.sum",
           "SEBS.sum",
           "AES.sum",
           "SPS.sum",
           "PES.sum")])
# Print correlation matrix
cor(VarDF)
# Save as DF
VarCors <- data.frame(cor(VarDF))
# Save correlation matrix as csv file
write.csv(x = VarCors, file = "VarCors.csv")

#### Scale Internal Consistencies ####

## SEBS

# Subset SEBS items
SEBS <- data.frame(
  thesis[,c("sebs_bulliesOthers",
            "sebs_defiantOppositional",
            "sebs_fightsArgues",
            "sebs_easilyAngry",
            "sebs_lies",
            "sebs_disruptsClass",
            "sebs_sittingStillDifficult"
  )])

# Calculate categorical omega
# Result: .929[.904,.943], se = 0.0095
ci.reliability(data = SEBS, 
               type = "categorical", 
               interval.type = "bca", 
               B = 1000,
               conf.level = 0.95)

## SIBS

# Subset SIBS items
SIBS <- data.frame(
  thesis[,c("sibs_nervousWorriedFearful",
            "sibs_bullied",
            "sibs_spendsTimeAlone",
            "sibs_clingsToAdults",
            "sibs_withdrawn",
            "sibs_sadUnhappy",
            "sibs_complainsSickHurt"
  )])

# Calculate categorical omega
# Result: .847[.783,.888], se = 0.0243
ci.reliability(data = SIBS, 
               type = "categorical", 
               interval.type = "bca", 
               B = 1000,
               conf.level = 0.95)


## Academic Engagement Scale

# Subset AES items
AES <- data.frame(
  thesis[,c("swtrs_participatesMeaningfully",
            "swtrs_engagedInLearning",
            "swtrs_confidentNewChallengingMaterial",
            "swtrs_needsLittleSupervision",
            "swtrs_comfortableWorkingIndependently",
            "swtrs_inquisitiveInterestedLearningNewThings"
  )])

# Calculate categorical omega
# Result: .941[.913,.955], se = 0.0088
ci.reliability(data = AES, 
               type = "categorical", 
               interval.type = "bca", 
               B = 1000,
               conf.level = 0.95)

## School Prosociality Scale

# Subset SPS items
SPS <- data.frame(
  thesis[,c("swtrs_listensToTeachers",
            "swtrs_treatsClassmatesKindly",
            "swtrs_handlesFrustrationsWell",
            "swtrs_wellBehavedDuringClass",
            "swtrs_approachableEasyGetAlongWith",
            "swtrs_classmatesLikeWorkPlayWithThem"
  )])

# Calculate categorical omega
# Result: .940[.916,.954], se = 0.0090
ci.reliability(data = SPS, 
               type = "categorical", 
               interval.type = "bca", 
               B = 1000,
               conf.level = 0.95)

## Positive Emotionality Scale

# Subset PES items
PES <- data.frame(
  thesis[,c("swtrs_seemsRelaxedAtEase",
            "swtrs_enjoysGroupWorkWithOthers",
            "swtrs_sociableWithOthersDuringFreeTime",
            "swtrs_smilesAtSchool",
            "swtrs_seemsHappyInClass"
  )])

# Calculate categorical omega
# Result: .885[.840,.916], se = 0.0189
ci.reliability(data = PES, 
               type = "categorical", 
               interval.type = "bca", 
               B = 1000,
               conf.level = 0.95)