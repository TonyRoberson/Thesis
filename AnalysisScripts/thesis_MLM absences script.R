###Multilevel modeling analyses predicting ABSENCES

#Relevant packages to load
library('nlme')
library('car')



################### SIBS/SEBS AND SWTRS COMPARISON ##########################



### STEP 1: INTERCEPT ONLY ###



#Test fixed intercept-only model
#No clustering

absences1_fixedIntercept <- gls(absences ~ 1, 
                           data = thesis, 
                           method = "ML")
qqnorm(resid(absences1_fixedIntercept))
summary(absences1_fixedIntercept)


##PREFERRED INTERCEPT-ONLY MODEL
#Test intercept-only model
#Include clustering

absences1b_randomIntercept <- lme(fixed = absences ~ 1, 
                             data = thesis, 
                             random = ~1|classID, 
                             method = "ML")
qqnorm(resid(absences1b_randomIntercept))
summary(absences1b_randomIntercept)
anova(absences1_fixedIntercept, absences1b_randomIntercept)



### STEP 2: LEVEL-1 AND -2 FIXED VARIANCE COMPONENTS ###



#Add SIBS and SEBS predictors

absences2_addSIBS.SEBS <- lme(fixed = absences ~ sibs_total_GrpCentered + sebs_total_GrpCentered, 
                         data = thesis, 
                         random = ~1|classID, 
                         method = "ML")
qqnorm(resid(absences2_addSIBS.SEBS))
summary(absences2_addSIBS.SEBS)
anova(absences1_fixedIntercept, absences1b_randomIntercept, absences2_addSIBS.SEBS)


#Add ENGAGMENT and SOCIAL (SWTRS) predictors
#Non-significant model improvement but ENGAGEMENT only significant predictor

absences2b_addSWTRS <- lme(fixed = absences ~ 
                        sibs_total_GrpCentered + sebs_total_GrpCentered + swtrs.social_total_GrpCentered + swtrs.engagement_total_GrpCentered, 
                      data = thesis, 
                      random = ~1|classID, 
                      method = "ML")
qqnorm(resid(absences2b_addSWTRS))
summary(absences2b_addSWTRS)
anova(absences1_fixedIntercept, absences1b_randomIntercept, absences2_addSIBS.SEBS, absences2b_addSWTRS)


##PREFERRED FIXED EFFECTS MODEL
#Add level-2 group means for each level-1 predictor
#Marginally significant p = 0.0503

absences2c_addGroupMeans <- lme(fixed = absences ~ 
                             sibs_total_GrpCentered + sebs_total_GrpCentered + swtrs.social_total_GrpCentered + swtrs.engagement_total_GrpCentered +
                             sibs_total_mean + sebs_total_mean + swtrs.social_total_mean + swtrs.engagement_total_mean, 
                           data = thesis, 
                           random = ~1|classID, 
                           method = "ML")
qqnorm(resid(absences2c_addGroupMeans))
summary(absences2c_addGroupMeans)
anova(absences1_fixedIntercept, absences1b_randomIntercept, absences2_addSIBS.SEBS, absences2b_addSWTRS, absences2c_addGroupMeans)



### STEP 3: RANDOM COEFFICIENTS ###
##NO SIGNIFICANT RANDOM SLOPES/NO PREFERRED RC MODEL



#Test random slopes for SIBS
#Non-significant

absences3_addRandSIBS <- lme(fixed = absences ~ 
                          sibs_total_GrpCentered + sebs_total_GrpCentered + swtrs.social_total_GrpCentered + swtrs.engagement_total_GrpCentered +
                          sibs_total_mean + sebs_total_mean + swtrs.social_total_mean + swtrs.engagement_total_mean, 
                        data = thesis,
                        random = ~1 + sibs_total_GrpCentered|classID,
                        method = "ML",
                        control = list(maxIter = 100, opt = "optim"))
qqnorm(resid(absences3_addRandSIBS))
summary(absences3_addRandSIBS)
anova(absences1_fixedIntercept, absences1b_randomIntercept, absences2_addSIBS.SEBS, absences2b_addSWTRS, absences2c_addGroupMeans, absences3_addRandSIBS)


#Test random slopes for SEBS
#Non-significant

absences3b_addRandSEBS <- lme(fixed = absences ~ 
                           sibs_total_GrpCentered + sebs_total_GrpCentered + swtrs.social_total_GrpCentered + swtrs.engagement_total_GrpCentered +
                           sibs_total_mean + sebs_total_mean + swtrs.social_total_mean + swtrs.engagement_total_mean, 
                         data = thesis,
                         random = ~1 + sebs_total_GrpCentered|classID,
                         method = "ML", 
                         control = list(maxIter = 100, opt = "optim"))
qqnorm(resid(absences3b_addRandSEBS))
summary(absences3b_addRandSEBS)
anova(absences1_fixedIntercept, absences1b_randomIntercept, absences2_addSIBS.SEBS, absences2b_addSWTRS, absences2c_addGroupMeans, absences3b_addRandSEBS)


#Test random slopes for ENGAGEMENT
#Non-significant

absences3c_addRandENGAGEMENT <- lme(fixed = absences ~ 
                                 sibs_total_GrpCentered + sebs_total_GrpCentered + swtrs.social_total_GrpCentered + swtrs.engagement_total_GrpCentered +
                                 sibs_total_mean + sebs_total_mean + swtrs.social_total_mean + swtrs.engagement_total_mean, 
                               data = thesis,
                               random = ~1 + swtrs.engagement_total_GrpCentered|classID,
                               method = "ML", 
                               control = list(maxIter = 100, opt = "optim"))
qqnorm(resid(absences3c_addRandENGAGEMENT))
summary(absences3c_addRandENGAGEMENT)
anova(absences1_fixedIntercept, absences1b_randomIntercept, absences2_addSIBS.SEBS, absences2b_addSWTRS, absences2c_addGroupMeans, absences3c_addRandENGAGEMENT)


#Test random slopes for SOCIAL
#Non-significant

absences3d_addRandSOCIAL <- lme(fixed = absences ~ 
                             sibs_total_GrpCentered + sebs_total_GrpCentered + swtrs.social_total_GrpCentered + swtrs.engagement_total_GrpCentered +
                             sibs_total_mean + sebs_total_mean + swtrs.social_total_mean + swtrs.engagement_total_mean, 
                           data = thesis, 
                           random = ~1 + swtrs.social_total_GrpCentered|classID, 
                           method = "ML", 
                           control = list(maxIter = 100, opt = "optim"))
qqnorm(resid(absences3d_addRandSOCIAL))
summary(absences3d_addRandSOCIAL)
anova(absences1_fixedIntercept, absences1b_randomIntercept, absences2_addSIBS.SEBS, absences2b_addSWTRS, absences2c_addGroupMeans, absences3d_addRandSOCIAL)



#### STEP 4: REDUCED MODEL ####



##PREFERRED REDUCED MODEL
#Reduced model with non-significant fixed effect predictors removed

absences4_reducedModel <- lme(fixed = absences ~ swtrs.engagement_total_GrpCentered,
                         data = thesis, 
                         random = ~1|classID, 
                         method = "ML", 
                         control = list(maxIter = 100, opt = "optim"))
qqnorm(resid(absences4_reducedModel))
summary(absences4_reducedModel)
anova(absences1_fixedIntercept, absences1b_randomIntercept, absences4_reducedModel)



### STEP 5: TEST CONCURRENT VALIDITY WITH SWTRS ONLY ###



#Test model with ENGAGMENT and SOCIAL as only level-1 predictors
#Significant compared to random intercept model

absences5_SWTRSonly <- lme(fixed = absences ~ swtrs.engagement_total_GrpCentered + swtrs.social_total_GrpCentered,
                              data = thesis, 
                              random = ~1|classID, 
                              method = "ML", 
                              control = list(maxIter = 100, opt = "optim"))
qqnorm(resid(absences5_SWTRSonly))
summary(absences5_SWTRSonly)
anova(absences1_fixedIntercept, absences1b_randomIntercept, absences5_SWTRSonly)



#Test addition of level-2 means
#Non-significant improvement

absences5b_SWTRSonly <- lme(fixed = absences ~ swtrs.engagement_total_GrpCentered + swtrs.social_total_GrpCentered +
                                      swtrs.social_total_mean + swtrs.engagement_total_mean,
                           data = thesis, 
                           random = ~1|classID, 
                           method = "ML", 
                           control = list(maxIter = 100, opt = "optim"))
qqnorm(resid(absences5b_SWTRSonly))
summary(absences5b_SWTRSonly)
anova(absences1_fixedIntercept, absences1b_randomIntercept, absences5_SWTRSonly, absences5b_SWTRSonly)







