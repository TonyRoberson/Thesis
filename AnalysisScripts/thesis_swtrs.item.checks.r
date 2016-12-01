#### Item Descriptives and Redundancy Checks ####

#### Data Preparation ####

## Load relevant packages
library(psych)
library(dplyr)
library(car)

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

### Functioning Well, "Gets along well with classmates" removed

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

## Internal consistency
alpha(x = swtrs.all[,c("swtrs_peacefulDuringClass",
                       "swtrs_comfortableWorkingIndependently",
                       "swtrs_followsDirections",
                       "swtrs_listensToTeachers",
                       "swtrs_obeysClassRules",
                       "swtrs_friendlyWithClassmates",
                       "swtrs_handsFeetToSelf",
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


### Functioning Well, "Obeys class rules" removed

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

## Internal consistency
alpha(x = swtrs.all[,c("swtrs_peacefulDuringClass",
                       "swtrs_comfortableWorkingIndependently",
                       "swtrs_followsDirections",
                       "swtrs_listensToTeachers",
                       "swtrs_friendlyWithClassmates",
                       "swtrs_handsFeetToSelf",
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


### Functioning Well, "Friendly with classmates" removed

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

## Internal consistency
alpha(x = swtrs.all[,c("swtrs_peacefulDuringClass",
                       "swtrs_comfortableWorkingIndependently",
                       "swtrs_followsDirections",
                       "swtrs_listensToTeachers",
                       "swtrs_handsFeetToSelf",
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


### Functioning Well, "Self control when frustrated" removed

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

## Internal consistency
alpha(x = swtrs.all[,c("swtrs_peacefulDuringClass",
                       "swtrs_comfortableWorkingIndependently",
                       "swtrs_followsDirections",
                       "swtrs_listensToTeachers",
                       "swtrs_handsFeetToSelf",
                       "swtrs_treatsClassmatesKindly",
                       "swtrs_playsWellWithOthers",
                       "swtrs_respectfulToClassmates",
                       "swtrs_handlesFrustrationsWell",
                       "swtrs_respectfulToTeachers",
                       "swtrs_participatesMeaningfully",
                       "swtrs_wellBehavedDuringClass",
                       "swtrs_staysOnTask",
                       "swtrs_staysFocused",
                       "swtrs_engagedInLearning",
                       "swtrs_remainsCalmDifficultSituation",
                       "swtrs_classmatesLikeWorkPlayWithThem",
                       "swtrs_classmatesRespectfulToThem",
                       "swtrs_sociableWithOthersDuringFreeTime",
                       "swtrs_needsLittleSupervision",
                       "swtrs_willingTryNewActivities",
                       "swtrs_approachableEasyGetAlongWith"
)])


### Functioning Well, "Respectful to classmates" removed

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

## Internal consistency
alpha(x = swtrs.all[,c("swtrs_peacefulDuringClass",
                       "swtrs_comfortableWorkingIndependently",
                       "swtrs_followsDirections",
                       "swtrs_listensToTeachers",
                       "swtrs_handsFeetToSelf",
                       "swtrs_treatsClassmatesKindly",
                       "swtrs_playsWellWithOthers",
                       "swtrs_handlesFrustrationsWell",
                       "swtrs_respectfulToTeachers",
                       "swtrs_participatesMeaningfully",
                       "swtrs_wellBehavedDuringClass",
                       "swtrs_staysOnTask",
                       "swtrs_staysFocused",
                       "swtrs_engagedInLearning",
                       "swtrs_remainsCalmDifficultSituation",
                       "swtrs_classmatesLikeWorkPlayWithThem",
                       "swtrs_classmatesRespectfulToThem",
                       "swtrs_sociableWithOthersDuringFreeTime",
                       "swtrs_needsLittleSupervision",
                       "swtrs_willingTryNewActivities",
                       "swtrs_approachableEasyGetAlongWith"
)])
