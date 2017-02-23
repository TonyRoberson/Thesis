#### SWTRS Exploratory Factor Analyses ####

## Load relevant packages
library(psych)
library(dplyr)
library(random.polychor.pa)
library(lavaan)
library(MBESS)

## Import cleaned and merged data set
thesis <- read.csv(file = "thesis.csv", 
                   header = TRUE, 
                   stringsAsFactors = FALSE)

## Subset SWTRS items selected for EFA
swtrs.efa <- data.frame(
  thesis[,c("swtrs_followsDirections",
            "swtrs_listensToTeachers",
            "swtrs_handsFeetToSelf",
            "swtrs_treatsClassmatesKindly",
            "swtrs_playsWellWithOthers",
            "swtrs_handlesFrustrationsWell",
            "swtrs_peacefulDuringClass",
            "swtrs_respectfulToTeachers",
            "swtrs_participatesMeaningfully",
            "swtrs_wellBehavedDuringClass",
            "swtrs_staysOnTask",
            "swtrs_staysFocused",
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


#### Initial EFA for ALL SWTRS Items ####

## Check results from unconstrained principal axis factoring analysis
## using the polychoric correlation matrix
efa.pa <- fa.poly(x = swtrs.efa, 
                  fa = "pa", 
                  rotate = "promax")
# Kaiser-Meyer-Olkin Sampling Adequacy
KMO(swtrs.efa)
# Sphericity
cortest.bartlett(swtrs.efa)
# Eigenvalues
efa.pa$fa$values
# Scree plot and parallel analysis
# Suggested factors = 3
random.polychor.pa(nvar = 31, 
                   n.ss = 184, 
                   nrep = 10, 
                   data.matrix = swtrs.efa, 
                   q.eigen = .95)
# Inspect polychoric correlation matrix
efa.pa$rho


## Check 3 factor solution
efa.out.3f <- fa.poly(x = swtrs.efa, 
                      fm = "pa", 
                      nfactors = 3, 
                      rotate = "promax", 
                      residual = TRUE)
# Inspect model residuals
resid.3f <- as.matrix(efa.out.3f$fa$residual)
describe(resid.3f)
boxplot(resid.3f)
# Print summary
print.psych(x = efa.out.3f, 
            digits = 3, 
            sort = TRUE)


## Check 2 factor solution
efa.out.2f <- fa.poly(x = swtrs.efa, 
                      fm = "pa", 
                      nfactors = 2, 
                      rotate = "promax", 
                      residual = TRUE)
# Inspect model residuals
resid.2f <- as.matrix(efa.out.2f$fa$residual)
describe(resid.2f)
boxplot(resid.2f)
# Print summary
print.psych(x = efa.out.2f, 
            digits = 3, 
            sort = TRUE)


## Check 4 factor solution
efa.out.4f <- fa.poly(x = swtrs.efa, 
                      fm = "pa", 
                      nfactors = 4, 
                      rotate = "promax", 
                      residual = TRUE)
# Inspect model residuals
resid.4f <- as.matrix(efa.out.4f$fa$residual)
describe(resid.4f)
boxplot(resid.4f)
# Print summary
print.psych(x = efa.out.4f, 
            digits = 3, 
            sort = TRUE)


#### Reduced EFA 1 ####

## Remove high cross-loading items:
# Follows Directions
# Alert during lessons
# Stays focused
# Willing to try new activities
# Optimistic will succeed in school

swtrs.reduced.1 <- data.frame(
  thesis[,c("swtrs_listensToTeachers",
            "swtrs_handsFeetToSelf",
            "swtrs_treatsClassmatesKindly",
            "swtrs_playsWellWithOthers",
            "swtrs_handlesFrustrationsWell",
            "swtrs_peacefulDuringClass",
            "swtrs_respectfulToTeachers",
            "swtrs_participatesMeaningfully",
            "swtrs_wellBehavedDuringClass",
            "swtrs_staysOnTask",
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
            "swtrs_seemsHappyInClass"
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
# Suggested factors = 3
random.polychor.pa(nvar = 25, 
                   n.ss = 184, 
                   nrep = 10, 
                   data.matrix = swtrs.reduced.1, 
                   q.eigen = .95)


## Check 3 factor solution
efa.out.3f.b <- fa.poly(x = swtrs.reduced.1,
                        fm = "pa",
                        nfactors = 3,
                        rotate = "promax",
                        residual = TRUE)
# Inspect model residuals
resid.3f.b <- as.matrix(efa.out.3f.b$fa$residual)
describe(resid.3f.b)
boxplot(resid.3f.b)
# Print summary
print.psych(x = efa.out.3f.b,
                digits = 3,
                sort = TRUE)


#### Reduced EFA 2: Preferred Structure! ####

## Items removed:
# Hands feet to self
# Respectful to teachers
# Laughs at appropriate times
# Peaceful during class
# Remains calm in dif. situations
# Classmates respectful to them
# Plays well with others
# Stays on task

swtrs.reduced.2 <- data.frame(
  thesis[,c("swtrs_listensToTeachers",
            "swtrs_treatsClassmatesKindly",
            "swtrs_handlesFrustrationsWell",
            "swtrs_participatesMeaningfully",
            "swtrs_wellBehavedDuringClass",
            "swtrs_engagedInLearning",
            "swtrs_confidentNewChallengingMaterial",
            "swtrs_seemsRelaxedAtEase",
            "swtrs_classmatesLikeWorkPlayWithThem",
            "swtrs_approachableEasyGetAlongWith",
            "swtrs_enjoysGroupWorkWithOthers",
            "swtrs_sociableWithOthersDuringFreeTime",
            "swtrs_needsLittleSupervision",
            "swtrs_comfortableWorkingIndependently",
            "swtrs_smilesAtSchool",
            "swtrs_seemsHappyInClass",
            "swtrs_inquisitiveInterestedLearningNewThings"
  )])

## Check results from unconstrained principal axis factoring analysis
## using the polychoric correlation matrix
efa.pa.r2 <- fa.poly(x = swtrs.reduced.2, 
                  fm = "pa", 
                  rotate = "promax")
# Kaiser-Meyer-Olkin Sampling Adequacy
KMO(swtrs.reduced.2)
# Sphericity
cortest.bartlett(swtrs.reduced.2)
# Eigenvalues
efa.pa.r2$fa$values
# Scree plot and parallel analysis
# Suggested factors = 3
random.polychor.pa(nvar = 17, 
                   n.ss = 184, 
                   nrep = 10,
                   data.matrix = swtrs.reduced.2, 
                   q.eigen = .95)

## Check 3 factor solution
efa.out.3f.d <- fa.poly(x = swtrs.reduced.2,
                        fm = "pa",
                        nfactors = 3,
                        rotate = "promax",
                        residual = TRUE)
# Inspect model residuals
resid.3f.d <- as.matrix(efa.out.3f.d$fa$residual)
describe(resid.3f.d)
boxplot(resid.3f.d)
# Print summary
print.psych(x = efa.out.3f.d,
                digits = 3,
                sort = TRUE)

#### EFA for SIBS/SEBS + SWTRS ####

pth.wb <- data.frame(
  thesis[,c("swtrs_listensToTeachers",
            "swtrs_treatsClassmatesKindly",
            "swtrs_handlesFrustrationsWell",
            "swtrs_participatesMeaningfully",
            "swtrs_wellBehavedDuringClass",
            "swtrs_engagedInLearning",
            "swtrs_confidentNewChallengingMaterial",
            "swtrs_seemsRelaxedAtEase",
            "swtrs_classmatesLikeWorkPlayWithThem",
            "swtrs_approachableEasyGetAlongWith",
            "swtrs_enjoysGroupWorkWithOthers",
            "swtrs_sociableWithOthersDuringFreeTime",
            "swtrs_needsLittleSupervision",
            "swtrs_comfortableWorkingIndependently",
            "swtrs_smilesAtSchool",
            "swtrs_seemsHappyInClass",
            "swtrs_inquisitiveInterestedLearningNewThings",
            "sibs_nervousWorriedFearful",
            "sibs_bullied",
            "sibs_spendsTimeAlone",
            "sibs_clingsToAdults",
            "sibs_withdrawn",
            "sibs_sadUnhappy",
            "sibs_complainsSickHurt",
            "sebs_bulliesOthers",
            "sebs_defiantOppositional",
            "sebs_fightsArgues",
            "sebs_easilyAngry",
            "sebs_lies",
            "sebs_disruptsClass",
            "sebs_sittingStillDifficult"
  )])

## Check results from unconstrained principal axis factoring analysis
## using the polychoric correlation matrix
efa.pth.wb <- fa.poly(x = pth.wb, 
                     fm = "pa", 
                     rotate = "promax")
# Kaiser-Meyer-Olkin Sampling Adequacy
KMO(pth.wb)
# Sphericity
cortest.bartlett(pth.wb)
# Eigenvalues
efa.pth.wb$fa$values
# Scree plot and parallel analysis
# Suggested factors = 3
random.polychor.pa(nvar = 31, 
                   n.ss = 184, 
                   nrep = 10, 
                   data.matrix = pth.wb, 
                   q.eigen = .95)
  

## Check 3 factor solution
efa.pthwb.out.3f <- fa.poly(x = pth.wb,
                        fm = "pa",
                        nfactors = 3,
                        rotate = "promax",
                        residual = TRUE)
# Inspect model residuals
resid.pthwb.3f <- as.matrix(efa.pthwb.out.3f$fa$residual)
describe(resid.pthwb.3f)
boxplot(resid.pthwb.3f)
# Print summary
print.psych(x = efa.pthwb.out.3f,
            digits = 3,
            sort = TRUE)


## Check 4 factor solution
efa.pthwb.out.4f <- fa.poly(x = pth.wb,
                            fm = "pa",
                            nfactors = 4,
                            rotate = "promax",
                            residual = TRUE)
# Inspect model residuals
resid.pthwb.4f <- as.matrix(efa.pthwb.out.4f$fa$residual)
describe(resid.pthwb.4f)
boxplot(resid.pthwb.4f)
# Print summary
print.psych(x = efa.pthwb.out.4f,
            digits = 3,
            sort = TRUE)

#### Reduced EFA for SIBS/SEBS + SWTRS ####

## Items Removed
# SEBS:  Bullies Others
# SIBS:  Withdrawn
#        Clings to adults
#        Nervous worried fearful
#        Bullied
# SWTRS: Enjoys group work with others,
#        Smiles at school
#        Sociable with others during free time
#        Seems Happy in class

      
pth.wb.2 <- data.frame(
  thesis[,c("swtrs_listensToTeachers",
            "swtrs_treatsClassmatesKindly",
            "swtrs_handlesFrustrationsWell",
            "swtrs_participatesMeaningfully",
            "swtrs_wellBehavedDuringClass",
            "swtrs_engagedInLearning",
            "swtrs_confidentNewChallengingMaterial",
            "swtrs_seemsRelaxedAtEase",
            "swtrs_classmatesLikeWorkPlayWithThem",
            "swtrs_approachableEasyGetAlongWith",
            "swtrs_needsLittleSupervision",
            "swtrs_comfortableWorkingIndependently",
            "swtrs_inquisitiveInterestedLearningNewThings",
            "sibs_spendsTimeAlone",
            "sibs_sadUnhappy",
            "sibs_complainsSickHurt",
            "sebs_defiantOppositional",
            "sebs_fightsArgues",
            "sebs_easilyAngry",
            "sebs_lies",
            "sebs_disruptsClass",
            "sebs_sittingStillDifficult"
  )])

## Check results from unconstrained principal axis factoring analysis
## using the polychoric correlation matrix
efa.pth.wb.2 <- fa.poly(x = pth.wb.2, 
                      fm = "pa", 
                      rotate = "promax")
# Kaiser-Meyer-Olkin Sampling Adequacy
KMO(pth.wb.2)
# Sphericity
cortest.bartlett(pth.wb.2)
# Eigenvalues
efa.pth.wb.2$fa$values
# Scree plot and parallel analysis
# Suggested factors = 3
random.polychor.pa(nvar = 22, 
                   n.ss = 184, 
                   nrep = 10, 
                   data.matrix = pth.wb.2, 
                   q.eigen = .95)


## Check 3 factor solution
efa.pthwb.out.3f.2 <- fa.poly(x = pth.wb.2,
                            fm = "pa",
                            nfactors = 3,
                            rotate = "promax",
                            residual = TRUE)
# Inspect model residuals
resid.pthwb.3f.2 <- as.matrix(efa.pthwb.out.3f.2$fa$residual)
describe(resid.pthwb.3f.2)
boxplot(resid.pthwb.3f.2)
# Print summary
print.psych(x = efa.pthwb.out.3f.2,
            digits = 3,
            sort = TRUE)

