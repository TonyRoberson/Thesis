##Mardia's Multivariate Normality Test for each DV with
##level-1 IVs (group-centered SIBS, SEBS, Engagment, and Social) and 
##level-2 IVs (SIBS, SEBS, Engagment, and Social group means)

library(MVN)


##PERCENT ON TASK
##All three tests were significantly skewed but not significantly kurtose
##Not multivariate normal

#All IVs included
mardiaTest(thesis[,c(56, 71:78)])

#Level-2 IVs only
mardiaTest(thesis[,c(56, 71,73,75,77)])

#Level-1 IVs only
mardiaTest(thesis[,c(56, 72,74,76,78)])




##MATH PERFORMANCE
##All three tests were significantly skewed but not significantly kurtose
##Not multivariate normal

#All IVs included
mardiaTest(thesis[,c(57, 71:78)])

#Level-2 IVs only
mardiaTest(thesis[,c(57, 71,73,75,77)])

#Level-1 IVs only
mardiaTest(thesis[,c(57, 72,74,76,78)])




##ELA PERFORMANCE
##All three tests were significantly skewed but not significantly kurtose
##Not multivariate normal

#All IVs included
mardiaTest(thesis[,c(58, 71:78)])

#Level-2 IVs only
mardiaTest(thesis[,c(58, 71,73,75,77)])

#Level-1 IVs only
mardiaTest(thesis[,c(58, 72,74,76,78)])




##ABSENCES
##All tests were significantly skewed and kurtose
##Not multivariate normal

#All IVs included
mardiaTest(thesis[,c(59, 71:78)])

#Level-2 IVs only
mardiaTest(thesis[,c(59, 71,73,75,77)])

#Level-1 IVs only
mardiaTest(thesis[,c(59, 72,74,76,78)])




