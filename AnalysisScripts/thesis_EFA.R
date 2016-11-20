#### SWTRS Exploratory Factor Analysis ####

#### Data Preparation ####

## Load relevant packages
library(psych)
library(nFactors)
library(dplyr)

## Import cleaned and merged data set
thesis <- read.csv(file = "thesis_merged.csv", 
                   header = TRUE, 
                   stringsAsFactors = FALSE)

## Get descriptives for all items
describe(thesis)
summary(thesis)
# Export item descriptives to .csv file in working directory
write.csv(x = thesis, file = "thesis_descriptives.csv")

## Subset only the SWTRS items in the data frame for EFA
swtrs.all <- data.frame(
  thesis[,c("swtrs_followsDirections",
            "swtrs_listensToTeachers",
            "swtrs_obeysClassRules",
            "swtrs_friendlyWithClassmates",
            "swtrs_handsFeetToSelf",
            "swtrs_getsAlongWithClassmates",
            "swtrs_treatsClassmatesKindly",
            "swtrs_playsWellWithOthers",
            "swtrs_respectfulToClassmates",
            "swtrs_handlesFrustrationsWell",
            "swtrs_peacefulDuringClass",
            "swtrs_respectfulToTeachers",
            "swtrs_participatesMeaningfully",
            "swtrs_wellBehavedDuringClass",
            "swtrs_staysOnTask",
            "swtrs_staysFocused",
            "swtrs_selfControlWhenFrustrated",
            "swtrs_engagedInLearning",
            "swtrs_confidentNewChallengingMaterial",
            "swtrs_remainsCalmDifficultSituation",
            "swtrs_seemsRelaxedAtEase",
            "swtrs_optimisticWillSucceedSchool",
            "swtrs_classmatesLikeWorkPlayWithThem",
            "swtrs_classmatesRespectfulToThem",
            "swtrs_approachableEasyGetAlongWith",
            "swtrs_enjoysGroupWorkWithOthers",
            "swtrs_sociableWithOthersDuringFreeTime",
            "swtrs_needsLittleSupervision",
            "swtrs_comfortableWorkingIndependently",
            "swtrs_willingTryNewActivities",
            "swtrs_inquisitiveInterestedLearningNewThings",
            "swtrs_laughsAppropriateTimes",
            "swtrs_smilesAtSchool",
            "swtrs_seemsHappyInClass",
            "swtrs_showsExcitementClassActivities",
            "swtrs_alertDuringLessons"
  )])


#### Inspect Hypothesized Feeling Good and Functioning Well Scales ####

### Feeling Good

## Inspect correlation matrix for Feeling Good items
polychoric(x = swtrs.all[,c("swtrs_confidentNewChallengingMaterial",
                            "swtrs_seemsRelaxedAtEase",
                            "swtrs_optimisticWillSucceedSchool",
                            "swtrs_enjoysGroupWorkWithOthers",
                            "swtrs_inquisitiveInterestedLearningNewThings",
                            "swtrs_laughsAppropriateTimes",
                            "swtrs_smilesAtSchool",
                            "swtrs_seemsHappyInClass",
                            "swtrs_showsExcitementClassActivities",
                            "swtrs_alertDuringLessons")])

## Calculate composite score
fgfw <- mutate(.data = swtrs.all, 
               FG = swtrs_confidentNewChallengingMaterial+
               swtrs_seemsRelaxedAtEase+
               swtrs_optimisticWillSucceedSchool+
               swtrs_enjoysGroupWorkWithOthers+
               swtrs_inquisitiveInterestedLearningNewThings+
               swtrs_laughsAppropriateTimes+
               swtrs_smilesAtSchool+
               swtrs_seemsHappyInClass+
               swtrs_showsExcitementClassActivities+
               swtrs_alertDuringLessons)

## Fit linear model to test for item redundancy
fit.fg <- lm(formula = FG ~ swtrs_confidentNewChallengingMaterial+
               swtrs_seemsRelaxedAtEase+
               swtrs_optimisticWillSucceedSchool+
               swtrs_enjoysGroupWorkWithOthers+
               swtrs_inquisitiveInterestedLearningNewThings+
               swtrs_laughsAppropriateTimes+
               swtrs_smilesAtSchool+
               swtrs_seemsHappyInClass+
               swtrs_showsExcitementClassActivities+
               swtrs_alertDuringLessons, 
             data = fgfw)
summary(fit.fg)

## Variance Inflation Factor/Tolerance Disgnostics for Redundancy
## Concern if: (a) Largest VIF is >= 10 (Myers, 1990), (b) Average VIF > 1
## (Bowerman & O'Connell, 1990), (c) 1/VIF (tolerance) < 0.1 = serious problem and
## < 0.2 concerning (Menard, 1995)
# VIF
vif(fit.fg)
mean(vif(fit.fg))
# Tolerance
1/vif(fit.fg)
mean(1/vif(fit.fg))
# Square root of VIF (indicates how much larger the standard error is compared
# to what it would be if uncorrelated with other variables in the model)
sqrt(vif(fit.fg))
mean(sqrt(vif(fit.fg)))

## Internal consistency
alpha(x = swtrs.all[,c("swtrs_confidentNewChallengingMaterial",
                       "swtrs_seemsRelaxedAtEase",
                       "swtrs_optimisticWillSucceedSchool",
                       "swtrs_enjoysGroupWorkWithOthers",
                       "swtrs_inquisitiveInterestedLearningNewThings",
                       "swtrs_laughsAppropriateTimes",
                       "swtrs_smilesAtSchool",
                       "swtrs_seemsHappyInClass",
                       "swtrs_showsExcitementClassActivities",
                       "swtrs_alertDuringLessons"
)])


### Functioning Well

## Inspect correlation matrix for Feeling Good items
polychoric(x = swtrs.all[,c("swtrs_peacefulDuringClass",
                            "swtrs_comfortableWorkingIndependently",
                            "swtrs_followsDirections",
                            "swtrs_listensToTeachers",
                            "swtrs_obeysClassRules",
                            "swtrs_friendlyWithClassmates",
                            "swtrs_handsFeetToSelf",
                            "swtrs_getsAlongWithClassmates",
                            "swtrs_treatsClassmatesKindly",
                            "swtrs_playsWellWithOthers",
                            "swtrs_respectfulToClassmates",
                            "swtrs_handlesFrustrationsWell",
                            "swtrs_respectfulToTeachers",
                            "swtrs_participatesMeaningfully",
                            "swtrs_wellBehavedDuringClass",
                            "swtrs_staysOnTask",
                            "swtrs_staysFocused",
                            "swtrs_selfControlWhenFrustrated",
                            "swtrs_engagedInLearning",
                            "swtrs_remainsCalmDifficultSituation",
                            "swtrs_classmatesLikeWorkPlayWithThem",
                            "swtrs_classmatesRespectfulToThem",
                            "swtrs_sociableWithOthersDuringFreeTime",
                            "swtrs_needsLittleSupervision",
                            "swtrs_willingTryNewActivities",
                            "swtrs_approachableEasyGetAlongWith"
                            
)])

## Calculate composite score
fgfw <- mutate(.data = swtrs.all, 
               FW = swtrs_peacefulDuringClass +
                 swtrs_comfortableWorkingIndependently +
                 swtrs_followsDirections +
                 swtrs_listensToTeachers+
                 swtrs_obeysClassRules+
                 swtrs_friendlyWithClassmates+
                 swtrs_handsFeetToSelf+
                 swtrs_getsAlongWithClassmates+
                 swtrs_treatsClassmatesKindly+
                 swtrs_playsWellWithOthers+
                 swtrs_respectfulToClassmates+
                 swtrs_handlesFrustrationsWell+
                 swtrs_respectfulToTeachers+
                 swtrs_participatesMeaningfully+
                 swtrs_wellBehavedDuringClass+
                 swtrs_staysOnTask+
                 swtrs_staysFocused+
                 swtrs_selfControlWhenFrustrated+
                 swtrs_engagedInLearning+
                 swtrs_remainsCalmDifficultSituation+
                 swtrs_classmatesLikeWorkPlayWithThem+
                 swtrs_classmatesRespectfulToThem+
                 swtrs_sociableWithOthersDuringFreeTime+
                 swtrs_needsLittleSupervision+
                 swtrs_willingTryNewActivities+
                 swtrs_approachableEasyGetAlongWith)

## Fit linear model to test for item redundancy
fit.fw <- lm(FW ~ swtrs_peacefulDuringClass+
               swtrs_comfortableWorkingIndependently+
               swtrs_followsDirections+
               swtrs_listensToTeachers+
               swtrs_obeysClassRules+
               swtrs_friendlyWithClassmates+
               swtrs_handsFeetToSelf+
               swtrs_getsAlongWithClassmates+
               swtrs_treatsClassmatesKindly+
               swtrs_playsWellWithOthers+
               swtrs_respectfulToClassmates+
               swtrs_handlesFrustrationsWell+
               swtrs_respectfulToTeachers+
               swtrs_participatesMeaningfully+
               swtrs_wellBehavedDuringClass+
               swtrs_staysOnTask+
               swtrs_staysFocused+
               swtrs_selfControlWhenFrustrated+
               swtrs_engagedInLearning+
               swtrs_remainsCalmDifficultSituation+
               swtrs_classmatesLikeWorkPlayWithThem+
               swtrs_classmatesRespectfulToThem+
               swtrs_sociableWithOthersDuringFreeTime+
               swtrs_needsLittleSupervision+
               swtrs_willingTryNewActivities+
               swtrs_approachableEasyGetAlongWith, 
             data = fgfw)
summary(fit.fw)

## Variance Inflation Factor/Tolerance Disgnostics for Redundancy
## Concern if: (a) Largest VIF is >= 10 (Myers, 1990), (b) Average VIF > 1
## (Bowerman & O'Connell, 1990), (c) 1/VIF (tolerance) < 0.1 = serious problem and
## < 0.2 concerning (Menard, 1995)
# VIF
vif(fit.fw)
mean(vif(fit.fw))
# Tolerance
1/vif(fit.fw)
mean(1/vif(fit.fw))
# Square root of VIF (indicates how much larger the standard error is compared
# to what it would be if uncorrelated with other variables in the model)
sqrt(vif(fit.fw))
mean(sqrt(vif(fit.fw)))

## Internal consistency
alpha(x = swtrs.all[,c("swtrs_peacefulDuringClass",
                       "swtrs_comfortableWorkingIndependently",
                       "swtrs_followsDirections",
                       "swtrs_listensToTeachers",
                       "swtrs_obeysClassRules",
                       "swtrs_friendlyWithClassmates",
                       "swtrs_handsFeetToSelf",
                       "swtrs_getsAlongWithClassmates",
                       "swtrs_treatsClassmatesKindly",
                       "swtrs_playsWellWithOthers",
                       "swtrs_respectfulToClassmates",
                       "swtrs_handlesFrustrationsWell",
                       "swtrs_respectfulToTeachers",
                       "swtrs_participatesMeaningfully",
                       "swtrs_wellBehavedDuringClass",
                       "swtrs_staysOnTask",
                       "swtrs_staysFocused",
                       "swtrs_selfControlWhenFrustrated",
                       "swtrs_engagedInLearning",
                       "swtrs_remainsCalmDifficultSituation",
                       "swtrs_classmatesLikeWorkPlayWithThem",
                       "swtrs_classmatesRespectfulToThem",
                       "swtrs_sociableWithOthersDuringFreeTime",
                       "swtrs_needsLittleSupervision",
                       "swtrs_willingTryNewActivities",
                       "swtrs_approachableEasyGetAlongWith"
                       
)])



## Calculate composite score
fgfw <- mutate(.data = swtrs.all, 
               FW.2 = swtrs_peacefulDuringClass +
                 swtrs_comfortableWorkingIndependently +
                 swtrs_followsDirections +
                 swtrs_listensToTeachers+
                 swtrs_obeysClassRules+
                 swtrs_friendlyWithClassmates+
                 swtrs_handsFeetToSelf+
                 swtrs_treatsClassmatesKindly+
                 swtrs_playsWellWithOthers+
                 swtrs_respectfulToClassmates+
                 swtrs_handlesFrustrationsWell+
                 swtrs_respectfulToTeachers+
                 swtrs_participatesMeaningfully+
                 swtrs_wellBehavedDuringClass+
                 swtrs_staysOnTask+
                 swtrs_staysFocused+
                 swtrs_selfControlWhenFrustrated+
                 swtrs_engagedInLearning+
                 swtrs_remainsCalmDifficultSituation+
                 swtrs_classmatesLikeWorkPlayWithThem+
                 swtrs_classmatesRespectfulToThem+
                 swtrs_sociableWithOthersDuringFreeTime+
                 swtrs_needsLittleSupervision+
                 swtrs_willingTryNewActivities+
                 swtrs_approachableEasyGetAlongWith)

## Fit linear model to test for item redundancy
fit.fw.2 <- lm(FW.2 ~ swtrs_peacefulDuringClass+
               swtrs_comfortableWorkingIndependently+
               swtrs_followsDirections+
               swtrs_listensToTeachers+
               swtrs_obeysClassRules+
               swtrs_friendlyWithClassmates+
               swtrs_handsFeetToSelf+
               swtrs_treatsClassmatesKindly+
               swtrs_playsWellWithOthers+
               swtrs_respectfulToClassmates+
               swtrs_handlesFrustrationsWell+
               swtrs_respectfulToTeachers+
               swtrs_participatesMeaningfully+
               swtrs_wellBehavedDuringClass+
               swtrs_staysOnTask+
               swtrs_staysFocused+
               swtrs_selfControlWhenFrustrated+
               swtrs_engagedInLearning+
               swtrs_remainsCalmDifficultSituation+
               swtrs_classmatesLikeWorkPlayWithThem+
               swtrs_classmatesRespectfulToThem+
               swtrs_sociableWithOthersDuringFreeTime+
               swtrs_needsLittleSupervision+
               swtrs_willingTryNewActivities+
               swtrs_approachableEasyGetAlongWith, 
             data = fgfw)
summary(fit.fw.2)

## Variance Inflation Factor/Tolerance Disgnostics for Redundancy
## Concern if: (a) Largest VIF is >= 10 (Myers, 1990), (b) Average VIF > 1
## (Bowerman & O'Connell, 1990), (c) 1/VIF (tolerance) < 0.1 = serious problem and
## < 0.2 concerning (Menard, 1995)
# VIF
vif(fit.fw.2)
mean(vif(fit.fw.2))
# Tolerance
1/vif(fit.fw.2)
mean(1/vif(fit.fw.2))
# Square root of VIF (indicates how much larger the standard error is compared
# to what it would be if uncorrelated with other variables in the model)
sqrt(vif(fit.fw.2))
mean(sqrt(vif(fit.fw.2)))




## Calculate composite score
fgfw <- mutate(.data = swtrs.all, 
               FW.3 = swtrs_peacefulDuringClass +
                 swtrs_comfortableWorkingIndependently +
                 swtrs_followsDirections +
                 swtrs_listensToTeachers+
                 swtrs_friendlyWithClassmates+
                 swtrs_handsFeetToSelf+
                 swtrs_treatsClassmatesKindly+
                 swtrs_playsWellWithOthers+
                 swtrs_respectfulToClassmates+
                 swtrs_handlesFrustrationsWell+
                 swtrs_respectfulToTeachers+
                 swtrs_participatesMeaningfully+
                 swtrs_wellBehavedDuringClass+
                 swtrs_staysOnTask+
                 swtrs_staysFocused+
                 swtrs_selfControlWhenFrustrated+
                 swtrs_engagedInLearning+
                 swtrs_remainsCalmDifficultSituation+
                 swtrs_classmatesLikeWorkPlayWithThem+
                 swtrs_classmatesRespectfulToThem+
                 swtrs_sociableWithOthersDuringFreeTime+
                 swtrs_needsLittleSupervision+
                 swtrs_willingTryNewActivities+
                 swtrs_approachableEasyGetAlongWith)

## Fit linear model to test for item redundancy
fit.fw.3 <- lm(FW.3 ~ swtrs_peacefulDuringClass+
                 swtrs_comfortableWorkingIndependently+
                 swtrs_followsDirections+
                 swtrs_listensToTeachers+
                 swtrs_friendlyWithClassmates+
                 swtrs_handsFeetToSelf+
                 swtrs_treatsClassmatesKindly+
                 swtrs_playsWellWithOthers+
                 swtrs_respectfulToClassmates+
                 swtrs_handlesFrustrationsWell+
                 swtrs_respectfulToTeachers+
                 swtrs_participatesMeaningfully+
                 swtrs_wellBehavedDuringClass+
                 swtrs_staysOnTask+
                 swtrs_staysFocused+
                 swtrs_selfControlWhenFrustrated+
                 swtrs_engagedInLearning+
                 swtrs_remainsCalmDifficultSituation+
                 swtrs_classmatesLikeWorkPlayWithThem+
                 swtrs_classmatesRespectfulToThem+
                 swtrs_sociableWithOthersDuringFreeTime+
                 swtrs_needsLittleSupervision+
                 swtrs_willingTryNewActivities+
                 swtrs_approachableEasyGetAlongWith, 
               data = fgfw)
summary(fit.fw.3)

## Variance Inflation Factor/Tolerance Disgnostics for Redundancy
## Concern if: (a) Largest VIF is >= 10 (Myers, 1990), (b) Average VIF > 1
## (Bowerman & O'Connell, 1990), (c) 1/VIF (tolerance) < 0.1 = serious problem and
## < 0.2 concerning (Menard, 1995)
# VIF
vif(fit.fw.3)
mean(vif(fit.fw.3))
# Tolerance
1/vif(fit.fw.3)
mean(1/vif(fit.fw.3))
# Square root of VIF (indicates how much larger the standard error is compared
# to what it would be if uncorrelated with other variables in the model)
sqrt(vif(fit.fw.3))
mean(sqrt(vif(fit.fw.3)))



## Calculate composite score
fgfw <- mutate(.data = swtrs.all, 
               FW.4 = swtrs_peacefulDuringClass +
                 swtrs_comfortableWorkingIndependently +
                 swtrs_followsDirections +
                 swtrs_listensToTeachers+
                 swtrs_handsFeetToSelf+
                 swtrs_treatsClassmatesKindly+
                 swtrs_playsWellWithOthers+
                 swtrs_respectfulToClassmates+
                 swtrs_handlesFrustrationsWell+
                 swtrs_respectfulToTeachers+
                 swtrs_participatesMeaningfully+
                 swtrs_wellBehavedDuringClass+
                 swtrs_staysOnTask+
                 swtrs_staysFocused+
                 swtrs_selfControlWhenFrustrated+
                 swtrs_engagedInLearning+
                 swtrs_remainsCalmDifficultSituation+
                 swtrs_classmatesLikeWorkPlayWithThem+
                 swtrs_classmatesRespectfulToThem+
                 swtrs_sociableWithOthersDuringFreeTime+
                 swtrs_needsLittleSupervision+
                 swtrs_willingTryNewActivities+
                 swtrs_approachableEasyGetAlongWith)

## Fit linear model to test for item redundancy
fit.fw.4 <- lm(FW.4 ~ swtrs_peacefulDuringClass+
                 swtrs_comfortableWorkingIndependently+
                 swtrs_followsDirections+
                 swtrs_listensToTeachers+
                 swtrs_handsFeetToSelf+
                 swtrs_treatsClassmatesKindly+
                 swtrs_playsWellWithOthers+
                 swtrs_respectfulToClassmates+
                 swtrs_handlesFrustrationsWell+
                 swtrs_respectfulToTeachers+
                 swtrs_participatesMeaningfully+
                 swtrs_wellBehavedDuringClass+
                 swtrs_staysOnTask+
                 swtrs_staysFocused+
                 swtrs_selfControlWhenFrustrated+
                 swtrs_engagedInLearning+
                 swtrs_remainsCalmDifficultSituation+
                 swtrs_classmatesLikeWorkPlayWithThem+
                 swtrs_classmatesRespectfulToThem+
                 swtrs_sociableWithOthersDuringFreeTime+
                 swtrs_needsLittleSupervision+
                 swtrs_willingTryNewActivities+
                 swtrs_approachableEasyGetAlongWith, 
               data = fgfw)
summary(fit.fw.4)

## Variance Inflation Factor/Tolerance Disgnostics for Redundancy
## Concern if: (a) Largest VIF is >= 10 (Myers, 1990), (b) Average VIF > 1
## (Bowerman & O'Connell, 1990), (c) 1/VIF (tolerance) < 0.1 = serious problem and
## < 0.2 concerning (Menard, 1995)
# VIF
vif(fit.fw.4)
mean(vif(fit.fw.4))
# Tolerance
1/vif(fit.fw.4)
mean(1/vif(fit.fw.4))
# Square root of VIF (indicates how much larger the standard error is compared
# to what it would be if uncorrelated with other variables in the model)
sqrt(vif(fit.fw.4))
mean(sqrt(vif(fit.fw.4)))



## Calculate composite score
fgfw <- mutate(.data = swtrs.all, 
               FW.5 = swtrs_peacefulDuringClass +
                 swtrs_comfortableWorkingIndependently +
                 swtrs_followsDirections +
                 swtrs_listensToTeachers+
                 swtrs_handsFeetToSelf+
                 swtrs_treatsClassmatesKindly+
                 swtrs_playsWellWithOthers+
                 swtrs_respectfulToClassmates+
                 swtrs_handlesFrustrationsWell+
                 swtrs_respectfulToTeachers+
                 swtrs_participatesMeaningfully+
                 swtrs_wellBehavedDuringClass+
                 swtrs_staysOnTask+
                 swtrs_staysFocused+
                 swtrs_engagedInLearning+
                 swtrs_remainsCalmDifficultSituation+
                 swtrs_classmatesLikeWorkPlayWithThem+
                 swtrs_classmatesRespectfulToThem+
                 swtrs_sociableWithOthersDuringFreeTime+
                 swtrs_needsLittleSupervision+
                 swtrs_willingTryNewActivities+
                 swtrs_approachableEasyGetAlongWith)

## Fit linear model to test for item redundancy
fit.fw.5 <- lm(FW.5 ~ swtrs_peacefulDuringClass+
                 swtrs_comfortableWorkingIndependently+
                 swtrs_followsDirections+
                 swtrs_listensToTeachers+
                 swtrs_handsFeetToSelf+
                 swtrs_treatsClassmatesKindly+
                 swtrs_playsWellWithOthers+
                 swtrs_respectfulToClassmates+
                 swtrs_handlesFrustrationsWell+
                 swtrs_respectfulToTeachers+
                 swtrs_participatesMeaningfully+
                 swtrs_wellBehavedDuringClass+
                 swtrs_staysOnTask+
                 swtrs_staysFocused+
                 swtrs_engagedInLearning+
                 swtrs_remainsCalmDifficultSituation+
                 swtrs_classmatesLikeWorkPlayWithThem+
                 swtrs_classmatesRespectfulToThem+
                 swtrs_sociableWithOthersDuringFreeTime+
                 swtrs_needsLittleSupervision+
                 swtrs_willingTryNewActivities+
                 swtrs_approachableEasyGetAlongWith, 
               data = fgfw)
summary(fit.fw.5)

## Variance Inflation Factor/Tolerance Disgnostics for Redundancy
## Concern if: (a) Largest VIF is >= 10 (Myers, 1990), (b) Average VIF > 1
## (Bowerman & O'Connell, 1990), (c) 1/VIF (tolerance) < 0.1 = serious problem and
## < 0.2 concerning (Menard, 1995)
# VIF
vif(fit.fw.5)
mean(vif(fit.fw.5))
# Tolerance
1/vif(fit.fw.5)
mean(1/vif(fit.fw.5))
# Square root of VIF (indicates how much larger the standard error is compared
# to what it would be if uncorrelated with other variables in the model)
sqrt(vif(fit.fw.5))
mean(sqrt(vif(fit.fw.5)))


## Calculate composite score
fgfw <- mutate(.data = swtrs.all, 
               FW.6 = swtrs_peacefulDuringClass +
                 swtrs_comfortableWorkingIndependently +
                 swtrs_followsDirections +
                 swtrs_listensToTeachers+
                 swtrs_handsFeetToSelf+
                 swtrs_treatsClassmatesKindly+
                 swtrs_playsWellWithOthers+
                 swtrs_handlesFrustrationsWell+
                 swtrs_respectfulToTeachers+
                 swtrs_participatesMeaningfully+
                 swtrs_wellBehavedDuringClass+
                 swtrs_staysOnTask+
                 swtrs_staysFocused+
                 swtrs_engagedInLearning+
                 swtrs_remainsCalmDifficultSituation+
                 swtrs_classmatesLikeWorkPlayWithThem+
                 swtrs_classmatesRespectfulToThem+
                 swtrs_sociableWithOthersDuringFreeTime+
                 swtrs_needsLittleSupervision+
                 swtrs_willingTryNewActivities+
                 swtrs_approachableEasyGetAlongWith)

## Fit linear model to test for item redundancy
fit.fw.6 <- lm(FW.6 ~ swtrs_peacefulDuringClass+
                 swtrs_comfortableWorkingIndependently+
                 swtrs_followsDirections+
                 swtrs_listensToTeachers+
                 swtrs_handsFeetToSelf+
                 swtrs_treatsClassmatesKindly+
                 swtrs_playsWellWithOthers+
                 swtrs_handlesFrustrationsWell+
                 swtrs_respectfulToTeachers+
                 swtrs_participatesMeaningfully+
                 swtrs_wellBehavedDuringClass+
                 swtrs_staysOnTask+
                 swtrs_staysFocused+
                 swtrs_engagedInLearning+
                 swtrs_remainsCalmDifficultSituation+
                 swtrs_classmatesLikeWorkPlayWithThem+
                 swtrs_classmatesRespectfulToThem+
                 swtrs_sociableWithOthersDuringFreeTime+
                 swtrs_needsLittleSupervision+
                 swtrs_willingTryNewActivities+
                 swtrs_approachableEasyGetAlongWith, 
               data = fgfw)
summary(fit.fw.6)

## Variance Inflation Factor/Tolerance Disgnostics for Redundancy
## Concern if: (a) Largest VIF is >= 10 (Myers, 1990), (b) Average VIF > 1
## (Bowerman & O'Connell, 1990), (c) 1/VIF (tolerance) < 0.1 = serious problem and
## < 0.2 concerning (Menard, 1995)
# VIF
vif(fit.fw.6)
mean(vif(fit.fw.6))
# Tolerance
1/vif(fit.fw.6)
mean(1/vif(fit.fw.6))
# Square root of VIF (indicates how much larger the standard error is compared
# to what it would be if uncorrelated with other variables in the model)
sqrt(vif(fit.fw.6))
mean(sqrt(vif(fit.fw.6)))






#### Initial EFA for ALL SWTRS Items ####

## Check results from unconstrained principal axis factoring analysis
## using the polychoric correlation matrix
efa.pa <- fa.poly(x = swtrs.all, 
                  fm = "pa", 
                  rotate = "promax")
# Kaiser-Meyer-Olkin Sampling Adequacy
KMO(swtrs.all)
# Sphericity
cortest.bartlett(swtrs.all)
# Eigenvalues
efa.pa$fa$values
# Scree plot and parallel analysis
plotnScree(nScree(efa.pa$fa$values), 
           main = "Scree Plot & Parallel Analysis")
# Inspect polychoric correlation matrix
efa.pa$rho


## Check 2 factor solution
efa.out.2f <- fa.poly(x = swtrs.all, 
                      fm = "pa", 
                      nfactors = 2, 
                      rotate = "promax", 
                      residual = TRUE)
# Inspect model residuals
resid.2f <- as.matrix(efa.out.2f$fa$residual)
describe(resid.2f)
boxplot(resid.2f)
# Factor plot
factor.plot(efa.out.2f$fa)
# Print summary
print.psych(x = efa.out.2f, 
            digits = 3, 
            sort = TRUE)


## Check 3 factor solution
## Scree plot and parallel analysis suggest this is best description
efa.out.3f <- fa.poly(x = swtrs.all, 
                      fm = "pa", 
                      nfactors = 3, 
                      rotate = "promax", 
                      residual = TRUE)
# Inspect model residuals
resid.3f <- as.matrix(efa.out.3f$fa$residual)
describe(resid.3f)
boxplot(resid.3f)
# Factor plot
factor.plot(efa.out.3f$fa)
# Print summary
print.psych(x = efa.out.3f, 
            digits = 3, 
            sort = TRUE)


## Check 4 factor solution
efa.out.4f <- fa.poly(x = swtrs.all, 
                      fm = "pa", 
                      nfactors = 4, 
                      rotate = "promax", 
                      residual = TRUE)
# Inspect model residuals
resid.4f <- as.matrix(efa.out.4f$fa$residual)
describe(resid.4f)
boxplot(resid.4f)
# Factor plot
factor.plot(efa.out.4f$fa)
# Print summary
print.psych(x = efa.out.4f, 
            digits = 3, 
            sort = TRUE)


#### EFA with 3 factor cross-loading items removed ####

## Remove high cross-loading items
swtrs.reduced.1 <- data.frame(
  thesis[,c("swtrs_listensToTeachers",
            "swtrs_obeysClassRules",
            "swtrs_friendlyWithClassmates",
            "swtrs_handsFeetToSelf",
            "swtrs_getsAlongWithClassmates",
            "swtrs_treatsClassmatesKindly",
            "swtrs_playsWellWithOthers",
            "swtrs_respectfulToClassmates",
            "swtrs_handlesFrustrationsWell",
            "swtrs_peacefulDuringClass",
            "swtrs_respectfulToTeachers",
            "swtrs_participatesMeaningfully",
            "swtrs_wellBehavedDuringClass",
            "swtrs_staysOnTask",
            "swtrs_selfControlWhenFrustrated",
            "swtrs_engagedInLearning",
            "swtrs_confidentNewChallengingMaterial",
            "swtrs_remainsCalmDifficultSituation",
            "swtrs_seemsRelaxedAtEase",
            "swtrs_classmatesLikeWorkPlayWithThem",
            "swtrs_classmatesRespectfulToThem",
            "swtrs_approachableEasyGetAlongWith",
            "swtrs_enjoysGroupWorkWithOthers",
            "swtrs_sociableWithOthersDuringFreeTime",
            "swtrs_needsLittleSupervision",
            "swtrs_comfortableWorkingIndependently",
            "swtrs_inquisitiveInterestedLearningNewThings",
            "swtrs_laughsAppropriateTimes",
            "swtrs_smilesAtSchool",
            "swtrs_seemsHappyInClass",
            "swtrs_alertDuringLessons"
  )])



## Check results from unconstrained principal axis factoring analysis
## using the polychoric correlation matrix
efa.pa.r1 <- fa.poly(x = swtrs.reduced.1, 
                  fm = "pa", 
                  rotate = "promax")
# Kaiser-Meyer-Olkin Sampling Adequacy
KMO(swtrs.reduced.1)
# Sphericity
cortest.bartlett(swtrs.reduced.1)
# Eigenvalues
efa.pa.r1$fa$values
# Scree plot and parallel analysis
plotnScree(nScree(efa.pa.r1$fa$values), 
           main = "Scree Plot & Parallel Analysis")


## Check 2 factor solution
efa.out.2f.b <- fa.poly(x = swtrs.reduced.1, 
                      fm = "pa", 
                      nfactors = 2, 
                      rotate = "promax", 
                      residual = TRUE)
# Inspect model residuals
resid.2f.b <- as.matrix(efa.out.2f.b$fa$residual)
describe(resid.2f.b)
boxplot(resid.2f.b)
# Factor plot
factor.plot(efa.out.2f.b$fa)
# Print summary
print.psych(x = efa.out.2f.b, 
            digits = 3, 
            sort = TRUE)


#### EFA for Reduced SWTRS item set ####





