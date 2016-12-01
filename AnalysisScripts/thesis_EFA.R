#### SWTRS Exploratory Factor Analyses ####

## Load relevant packages
library(psych)
library(dplyr)
library(nFactors)
library(random.polychor.pa)

## Import cleaned and merged data set
thesis <- read.csv(file = "thesis_merged.csv", 
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
                  fm = "pa", 
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

## Remove high cross-loading items
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


#### Reduced EFA 2 ####

## Items removed:
# Inquisitive Interested in new things

swtrs.reduced.2 <- data.frame(
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
            "swtrs_laughsAppropriateTimes",
            "swtrs_smilesAtSchool",
            "swtrs_seemsHappyInClass"
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
random.polychor.pa(nvar = 24, 
                   n.ss = 184, 
                   nrep = 10, 
                   data.matrix = swtrs.reduced.2, 
                   q.eigen = .95)

## Check 3 factor solution
efa.out.3f.c <- fa.poly(x = swtrs.reduced.2,
                        fm = "pa",
                        nfactors = 3,
                        rotate = "promax",
                        residual = TRUE)
# Inspect model residuals
resid.3f.c <- as.matrix(efa.out.3f.c$fa$residual)
describe(resid.3f.c)
boxplot(resid.3f.c)
# Print summary
print.psych(x = efa.out.3f.c,
                digits = 3,
                sort = TRUE)

#### Reduced EFA 3 ####

## Items removed:
# Hands feet to self
# Respectful to teachers
# Laughs at appropriate times
# Peaceful during class
# Remains calm in dif. situations
# Classmates respectful to them
# Plays well with others
# Stays on task

## Remove high cross-loading item
## Inquisitive Interested in new things

swtrs.reduced.3 <- data.frame(
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
            "swtrs_seemsHappyInClass"
  )])

## Check results from unconstrained principal axis factoring analysis
## using the polychoric correlation matrix
efa.pa.r3 <- fa.poly(x = swtrs.reduced.3, 
                  fm = "pa", 
                  rotate = "promax")
# Kaiser-Meyer-Olkin Sampling Adequacy
KMO(swtrs.reduced.3)
# Sphericity
cortest.bartlett(swtrs.reduced.3)
# Eigenvalues
efa.pa.r3$fa$values
# Scree plot and parallel analysis
# Suggested factors = 3
random.polychor.pa(nvar = 16, 
                   n.ss = 184, 
                   nrep = 10, 
                   data.matrix = swtrs.reduced.3, 
                   q.eigen = .95)

## Check 3 factor solution
efa.out.3f.d <- fa.poly(x = swtrs.reduced.3,
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


#### Reduced EFA 4 ####

## Items removed:
# Hands feet to self
# Respectful to teachers
# Laughs at appropriate times
# Peaceful during class
# Remains calm in dif. situations
# Classmates respectful to them
# Plays well with others
# Stays on task

swtrs.reduced.3 <- data.frame(
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
            "swtrs_seemsHappyInClass"
  )])

## Check results from unconstrained principal axis factoring analysis
## using the polychoric correlation matrix
efa.pa.r3 <- fa.poly(x = swtrs.reduced.3, 
                  fm = "pa", 
                  rotate = "promax")
# Kaiser-Meyer-Olkin Sampling Adequacy
KMO(swtrs.reduced.3)
# Sphericity
cortest.bartlett(swtrs.reduced.3)
# Eigenvalues
efa.pa.r3$fa$values
# Scree plot and parallel analysis
# Suggested factors = 3
random.polychor.pa(nvar = 16, 
                   n.ss = 184, 
                   nrep = 10, 
                   data.matrix = swtrs.reduced.3, 
                   q.eigen = .95)

## Check 3 factor solution
efa.out.3f.d <- fa.poly(x = swtrs.reduced.3,
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
