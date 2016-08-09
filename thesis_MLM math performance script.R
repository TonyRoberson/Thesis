###Multilevel modeling analyses predicting MATH PERFORMANCE


#Relevant packages to load
library('nlme')
library('car')



################### SIBS/SEBS AND SWTRS COMPARISON ##########################



### STEP 1: INTERCEPT ONLY ###



##PREFERRED INTERCEPT-ONLY MODEL
#Test fixed intercept-only model
#No clustering

mathPerformance1_fixedIntercept <- gls(mathPerformance ~ 1, 
                                data = thesis, 
                                method = "ML")
qqnorm(resid(mathPerformance1_fixedIntercept))
summary(mathPerformance1_fixedIntercept)


#Test intercept-only model
#Include clustering
#Non-significant

mathPerformance1b_randomIntercept <- lme(fixed = mathPerformance ~ 1, 
                                  data = thesis, 
                                  random = ~1|classID, 
                                  method = "ML")
qqnorm(resid(mathPerformance1b_randomIntercept))
summary(mathPerformance1b_randomIntercept)
anova(mathPerformance1_fixedIntercept, mathPerformance1b_randomIntercept)



### STEP 2: LEVEL-1 AND -2 FIXED VARIANCE COMPONENTS ###



#Add SIBS and SEBS predictors

mathPerformance2_addSIBS.SEBS <- gls(model = mathPerformance ~ sibs_total_GrpCentered + sebs_total_GrpCentered, 
                              data = thesis,
                              method = "ML")
qqnorm(resid(mathPerformance2_addSIBS.SEBS))
summary(mathPerformance2_addSIBS.SEBS)
anova(mathPerformance1_fixedIntercept, mathPerformance2_addSIBS.SEBS)


##PREFERRED FIXED EFFECTS MODEL
#Add ENGAGMENT and SOCIAL (SWTRS) predictors

mathPerformance2b_addSWTRS <- gls(model = mathPerformance ~ 
                             sibs_total_GrpCentered + sebs_total_GrpCentered + swtrs.social_total_GrpCentered + swtrs.engagement_total_GrpCentered, 
                           data = thesis, 
                           method = "ML")
qqnorm(resid(mathPerformance2b_addSWTRS))
summary(mathPerformance2b_addSWTRS)
anova(mathPerformance1_fixedIntercept, mathPerformance2_addSIBS.SEBS, mathPerformance2b_addSWTRS)


#Add level-2 group means for each level-1 predictor
#Non-significant

mathPerformance2c_addGroupMeans <- gls(model =  mathPerformance ~ 
                                  sibs_total_GrpCentered + sebs_total_GrpCentered + swtrs.social_total_GrpCentered + swtrs.engagement_total_GrpCentered +
                                  sibs_total_mean + sebs_total_mean + swtrs.social_total_mean + swtrs.engagement_total_mean, 
                                data = thesis, 
                                method = "ML")
qqnorm(resid(mathPerformance2c_addGroupMeans))
summary(mathPerformance2c_addGroupMeans)
anova(mathPerformance1_fixedIntercept, mathPerformance2_addSIBS.SEBS, mathPerformance2b_addSWTRS, mathPerformance2c_addGroupMeans)



### STEP 3: RANDOM COEFFICIENTS ###



#Test random slopes for SIBS
#Non-significant

mathPerformance3_addRandSIBS <- lme(fixed = mathPerformance ~ 
                               sibs_total_GrpCentered + sebs_total_GrpCentered + swtrs.social_total_GrpCentered + swtrs.engagement_total_GrpCentered, 
                             data = thesis,
                             random = ~0 + sibs_total_GrpCentered|classID,
                             method = "ML",
                             control = list(maxIter = 100, opt = "optim"))
qqnorm(resid(mathPerformance3_addRandSIBS))
summary(mathPerformance3_addRandSIBS)
anova(mathPerformance1_fixedIntercept, mathPerformance2_addSIBS.SEBS, mathPerformance2b_addSWTRS, mathPerformance3_addRandSIBS)


#Test random slopes for SEBS
#Non-significant

mathPerformance3b_addRandSEBS <- lme(fixed = mathPerformance ~ 
                                sibs_total_GrpCentered + sebs_total_GrpCentered + swtrs.social_total_GrpCentered + swtrs.engagement_total_GrpCentered,
                              data = thesis,
                              random = ~0 + sebs_total_GrpCentered|classID,
                              method = "ML", 
                              control = list(maxIter = 100, opt = "optim"))
qqnorm(resid(mathPerformance3b_addRandSEBS))
summary(mathPerformance3b_addRandSEBS)
anova(mathPerformance1_fixedIntercept, mathPerformance2_addSIBS.SEBS, mathPerformance2b_addSWTRS, mathPerformance3b_addRandSEBS)


##PREFERRED RANDOM COEFFICENT MODEL
#Test random slopes for ENGAGEMENT
#Significant

mathPerformance3c_addRandENGAGEMENT <- lme(fixed = mathPerformance ~ 
                                      sibs_total_GrpCentered + sebs_total_GrpCentered + swtrs.social_total_GrpCentered + swtrs.engagement_total_GrpCentered, 
                                    data = thesis,
                                    random = ~0 + swtrs.engagement_total_GrpCentered|classID,
                                    method = "ML", 
                                    control = list(maxIter = 100, opt = "optim"))
qqnorm(resid(mathPerformance3c_addRandENGAGEMENT))
summary(mathPerformance3c_addRandENGAGEMENT)
anova(mathPerformance1_fixedIntercept, mathPerformance2_addSIBS.SEBS, mathPerformance2b_addSWTRS, mathPerformance3c_addRandENGAGEMENT)


#Test random slopes for SOCIAL
#Non-significant

mathPerformance3d_addRandSOCIAL <- lme(fixed = mathPerformance ~ 
                                  sibs_total_GrpCentered + sebs_total_GrpCentered + swtrs.social_total_GrpCentered + swtrs.engagement_total_GrpCentered, 
                                data = thesis, 
                                random = ~0 + swtrs.social_total_GrpCentered|classID, 
                                method = "ML", 
                                control = list(maxIter = 100, opt = "optim"))
qqnorm(resid(mathPerformance3d_addRandSOCIAL))
summary(mathPerformance3d_addRandSOCIAL)
anova(mathPerformance1_fixedIntercept, mathPerformance2_addSIBS.SEBS, mathPerformance2b_addSWTRS, mathPerformance3d_addRandSOCIAL)



#### STEP 4: REDUCED MODEL ####



##PREFERRED REDUCED MODEL
#Reduced model with non-significant fixed effect predictors removed

mathPerformance4_reducedModel <- lme(fixed = mathPerformance ~ swtrs.engagement_total_GrpCentered, 
                                     data = thesis,
                                     random = ~0 + swtrs.engagement_total_GrpCentered|classID,
                                     method = "ML", 
                                     control = list(maxIter = 100, opt = "optim"))
qqnorm(resid(mathPerformance4_reducedModel))
summary(mathPerformance4_reducedModel)
anova(mathPerformance1_fixedIntercept, mathPerformance4_reducedModel)



#### STEP 5: TEST CONCURRENT VALIDITY WITH SWTRS ONLY MODEL ####



#Test model with ENGAGMENT and SOCIAL as only level-1 predictors
#Significant compared to random intercept model

mathPerformance5_SWTRSonly <- lme(fixed = mathPerformance ~ swtrs.engagement_total_GrpCentered + swtrs.social_total_GrpCentered, 
                                     data = thesis,
                                     random = ~0 + swtrs.engagement_total_GrpCentered|classID,
                                     method = "ML", 
                                     control = list(maxIter = 100, opt = "optim"))
qqnorm(resid(mathPerformance5_SWTRSonly))
summary(mathPerformance5_SWTRSonly)
anova(mathPerformance1_fixedIntercept, mathPerformance5_SWTRSonly)


#Test addition of level-2 means
#Non-significant improvement

mathPerformance5b_SWTRSonly <- lme(fixed = mathPerformance ~ swtrs.engagement_total_GrpCentered + swtrs.social_total_GrpCentered +
                                    swtrs.social_total_mean + swtrs.engagement_total_mean, 
                                  data = thesis,
                                  random = ~0 + swtrs.engagement_total_GrpCentered|classID,
                                  method = "ML", 
                                  control = list(maxIter = 100, opt = "optim"))
qqnorm(resid(mathPerformance5b_SWTRSonly))
summary(mathPerformance5b_SWTRSonly)
anova(mathPerformance1_fixedIntercept, mathPerformance5_SWTRSonly, mathPerformance5b_SWTRSonly)
