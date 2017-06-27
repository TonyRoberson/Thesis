#### SWTRS Exploratory Factor Analyses ####

## Load relevant packages
library(psych)
library(dplyr)
library(random.polychor.pa)
library(lavaan)

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
efa.pa <- fa(r = swtrs.efa,
             cor = "poly", 
             fm = "pa")
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
# Print Unidimensional solution
print.psych(x = efa.pa,
            digits = 3, 
            sort = TRUE)


## Check 3 factor solution
efa.out.3f <- fa(r = swtrs.efa, 
                      fm = "pa", 
                      nfactors = 3, 
                      rotate = "promax", 
                      residuals = TRUE, 
                      cor = "poly")
# Inspect model residuals
resid.3f <- as.matrix(efa.out.3f$residual)
describe(resid.3f)
boxplot(resid.3f)
# Print summary
print.psych(x = efa.out.3f, 
            digits = 3, 
            sort = TRUE)


## Check 2 factor solution
efa.out.2f <- fa(r = swtrs.efa, 
                  fm = "pa", 
                  nfactors = 2, 
                  rotate = "promax", 
                  residual = TRUE, 
                  cor = "poly")
# Inspect model residuals
resid.2f <- as.matrix(efa.out.2f$residual)
describe(resid.2f)
boxplot(resid.2f)
# Print summary
print.psych(x = efa.out.2f, 
            digits = 3, 
            sort = TRUE)


## Check 4 factor solution
efa.out.4f <- fa(r = swtrs.efa, 
                    fm = "pa", 
                    nfactors = 4, 
                    rotate = "promax", 
                    residual = TRUE,
                    cor = "poly")
# Inspect model residuals
resid.4f <- as.matrix(efa.out.4f$residual)
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
            "swtrs_inquisitiveInterestedLearningNewThings",
            "swtrs_laughsAppropriateTimes",
            "swtrs_smilesAtSchool",
            "swtrs_seemsHappyInClass",
            "swtrs_comfortableWorkingIndependently"
  )])


## Check results from unconstrained principal axis factoring analysis
## using the polychoric correlation matrix
efa.pa.r1 <- fa(r = swtrs.reduced.1, 
                fm = "pa", 
                rotate = "promax", 
                cor = "poly")
# Kaiser-Meyer-Olkin Sampling Adequacy
KMO(swtrs.reduced.1)
# Sphericity
cortest.bartlett(swtrs.reduced.1)
# Eigenvalues
efa.pa.r1$values
# Scree plot and parallel analysis
# Suggested factors = 3
random.polychor.pa(nvar = 25, 
                   n.ss = 184, 
                   nrep = 10, 
                   data.matrix = swtrs.reduced.1, 
                   q.eigen = .95)


## Check 3 factor solution
efa.out.3f.b <- fa(r = swtrs.reduced.1,
                   fm = "pa",
                   nfactors = 3,
                   rotate = "promax",
                   residual = TRUE, 
                   cor = "poly")
# Inspect model residuals
resid.3f.b <- as.matrix(efa.out.3f.b$residual)
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
            "swtrs_smilesAtSchool",
            "swtrs_seemsHappyInClass",
            "swtrs_inquisitiveInterestedLearningNewThings",
            "swtrs_comfortableWorkingIndependently"
  )])

## Check results from unconstrained principal axis factoring analysis
## using the polychoric correlation matrix
efa.pa.r2 <- fa(r = swtrs.reduced.2, 
                fm = "pa", 
                rotate = "promax",
                cor = "poly",
                residuals = TRUE)
# Kaiser-Meyer-Olkin Sampling Adequacy
KMO(swtrs.reduced.2)
# Sphericity
cortest.bartlett(swtrs.reduced.2)
# Eigenvalues
efa.pa.r2$values
# Scree plot and parallel analysis
# Suggested factors = 3
random.polychor.pa(nvar = 17, 
                   n.ss = 184, 
                   nrep = 1000,
                   data.matrix = swtrs.reduced.2, 
                   q.eigen = 0.95)
print(efa.pa.r2, digits = 3, sort = TRUE)

## Check 3 factor solution
efa.out.3f.d <- fa(r = swtrs.reduced.2,
                   fm = "pa",
                   nfactors = 3,
                   rotate = "promax",
                   residual = TRUE,
                   cor = "poly")
# Inspect model residuals
resid.3f.d <- as.matrix(efa.out.3f.d$residual)
describe(resid.3f.d)
boxplot(resid.3f.d)
# Print summary
print.psych(x = efa.out.3f.d,
            digits = 3,
            sort = TRUE)


#### Reduced EFA 1b: Heywood removed ####

## Remove high cross-loading items:
# Follows Directions
# Alert during lessons
# Stays focused
# Willing to try new activities
# Optimistic will succeed in school
## Remove Heywood case
# Comfortable working independently

swtrs.reduced.1b <- data.frame(
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
            "swtrs_inquisitiveInterestedLearningNewThings",
            "swtrs_laughsAppropriateTimes",
            "swtrs_smilesAtSchool",
            "swtrs_seemsHappyInClass"
  )])


## Check results from unconstrained principal axis factoring analysis
## using the polychoric correlation matrix
efa.pa.r1b <- fa(r = swtrs.reduced.1b, 
                  fm = "pa", 
                  rotate = "promax", 
                cor = "poly")
# Kaiser-Meyer-Olkin Sampling Adequacy
KMO(swtrs.reduced.1b)
# Sphericity
cortest.bartlett(swtrs.reduced.1b)
# Eigenvalues
efa.pa.r1b$values
# Scree plot and parallel analysis
# Suggested factors = 3
random.polychor.pa(nvar = 24, 
                   n.ss = 184, 
                   nrep = 10, 
                   data.matrix = swtrs.reduced.1b, 
                   q.eigen = .95)


## Check 3 factor solution
efa.out.3f.b2 <- fa(r = swtrs.reduced.1b,
                        fm = "pa",
                        nfactors = 3,
                        rotate = "promax",
                        residual = TRUE, 
                        cor = "poly")
# Inspect model residuals
resid.3f.b2 <- as.matrix(efa.out.3f.b2$residual)
describe(resid.3f.b2)
boxplot(resid.3f.b2)
# Print summary
print.psych(x = efa.out.3f.b2,
                digits = 3,
                sort = TRUE)


#### Reduced EFA 2b: Heywood removed ####

## Items removed:
# Respectful to teachers
# Laughs at appropriate times
# Peaceful during class
# Remains calm in dif. situations
# Classmates respectful to them
# Plays well with others
# Stays on task
## Heywood case:
# Hands feet to self

swtrs.reduced.2b <- data.frame(
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
            "swtrs_smilesAtSchool",
            "swtrs_seemsHappyInClass",
            "swtrs_inquisitiveInterestedLearningNewThings"
  )])

## Check results from unconstrained principal axis factoring analysis
## using the polychoric correlation matrix
efa.pa.r2b <- fa(r = swtrs.reduced.2b, 
                     fm = "pa", 
                     rotate = "promax",
                     cor = "poly",
                    residuals = TRUE)
# Kaiser-Meyer-Olkin Sampling Adequacy
KMO(swtrs.reduced.2b)
# Sphericity
cortest.bartlett(swtrs.reduced.2b)
# Eigenvalues
efa.pa.r2b$values
# Scree plot and parallel analysis
# Suggested factors = 3
random.polychor.pa(nvar = 16, 
                   n.ss = 184, 
                   nrep = 10,
                   data.matrix = swtrs.reduced.2b, 
                   q.eigen = 0.95)
print(efa.pa.r2b, 
      digits = 3, 
      sort = TRUE)

## Check 3 factor solution
efa.out.3f.d2 <- fa(r = swtrs.reduced.2b,
                        fm = "pa",
                        nfactors = 3,
                        rotate = "promax",
                        residual = TRUE,
                        cor = "poly")
# Inspect model residuals
resid.3f.d2 <- as.matrix(efa.out.3f.d2$residual)
describe(resid.3f.d2)
boxplot(resid.3f.d2)
# Print summary
print.psych(x = efa.out.3f.d2,
                digits = 3,
                sort = TRUE)

