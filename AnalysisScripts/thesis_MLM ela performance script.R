### Multilevel modeling analyses predicting ELA PERFORMANCE


# Relevant packages to load
library('nlme')
library('car')



################### SIBS/SEBS AND SWTRS COMPARISON ##########################



### STEP 1: INTERCEPT ONLY ###



## PREFERRED INTERCEPT-ONLY MODEL
# Test fixed intercept-only model
# No clustering

elaPerformance1_fixedIntercept <- gls(elaPerformance ~ 1, 
                                       data = thesis, 
                                       method = "ML")
qqnorm(resid(elaPerformance1_fixedIntercept))
summary(elaPerformance1_fixedIntercept)


# Test intercept-only model
# Include clustering
# Non-significant

elaPerformance1b_randomIntercept <- lme(fixed = elaPerformance ~ 1, 
                                         data = thesis, 
                                         random = ~1|classID, 
                                         method = "ML")
qqnorm(resid(elaPerformance1b_randomIntercept))
summary(elaPerformance1b_randomIntercept)
anova(elaPerformance1_fixedIntercept, elaPerformance1b_randomIntercept)



### STEP 2: LEVEL-1 AND -2 FIXED VARIANCE COMPONENTS ###



# Add SIBS and SEBS predictors

elaPerformance2_addSIBS.SEBS <- gls(model = elaPerformance ~ sibs_total_GrpCentered + sebs_total_GrpCentered, 
                                     data = thesis,
                                     method = "ML")
qqnorm(resid(elaPerformance2_addSIBS.SEBS))
summary(elaPerformance2_addSIBS.SEBS)
anova(elaPerformance1_fixedIntercept, elaPerformance2_addSIBS.SEBS)


## PREFERRED FIXED EFFECTS MODEL
# Add ENGAGMENT and SOCIAL (SWTRS) predictors

elaPerformance2b_addSWTRS <- gls(model = elaPerformance ~ 
                                   sibs_total_GrpCentered + sebs_total_GrpCentered + 
                                   swtrs.social_total_GrpCentered + swtrs.engagement_total_GrpCentered, 
                                 data = thesis, 
                                 method = "ML")
qqnorm(resid(elaPerformance2b_addSWTRS))
summary(elaPerformance2b_addSWTRS)
anova(elaPerformance1_fixedIntercept, elaPerformance2_addSIBS.SEBS, elaPerformance2b_addSWTRS)


# Test alt fixed effect order

elaPerf2alt_addSWTRS <- gls(model = elaPerformance ~
                              swtrs.social_total_GrpCentered + swtrs.engagement_total_GrpCentered, 
                            data = thesis, 
                            method = "ML")
qqnorm(resid(elaPerf2alt_addSWTRS))
summary(elaPerf2alt_addSWTRS)
anova(elaPerformance1_fixedIntercept, elaPerf2alt_addSWTRS, elaPerformance2b_addSWTRS)

# Add level-2 group means for each level-1 predictor
# Non-significant

elaPerformance2c_addGroupMeans <- gls(model =  elaPerformance ~ 
                                        sibs_total_GrpCentered + sebs_total_GrpCentered +
                                        swtrs.social_total_GrpCentered + swtrs.engagement_total_GrpCentered +
                                        sibs_total_mean + sebs_total_mean + 
                                        swtrs.social_total_mean + swtrs.engagement_total_mean, 
                                       data = thesis, 
                                       method = "ML")
qqnorm(resid(elaPerformance2c_addGroupMeans))
summary(elaPerformance2c_addGroupMeans)
anova(elaPerformance1_fixedIntercept, elaPerformance2_addSIBS.SEBS, 
      elaPerformance2b_addSWTRS, elaPerformance2c_addGroupMeans)




### STEP 3: RANDOM COEFFICIENTS ###



# Test random slopes for SIBS
# Non-significant

elaPerformance3_addRandSIBS <- lme(fixed = elaPerformance ~ 
                                     sibs_total_GrpCentered + sebs_total_GrpCentered + 
                                     swtrs.social_total_GrpCentered + swtrs.engagement_total_GrpCentered, 
                                   data = thesis,
                                   random = ~0 + sibs_total_GrpCentered|classID,
                                   method = "ML",
                                   control = list(maxIter = 100, opt = "optim"))
qqnorm(resid(elaPerformance3_addRandSIBS))
summary(elaPerformance3_addRandSIBS)
anova(elaPerformance1_fixedIntercept, elaPerformance2_addSIBS.SEBS, 
      elaPerformance2b_addSWTRS, elaPerformance3_addRandSIBS)


# Test random slopes for SEBS
# Non-significant

elaPerformance3b_addRandSEBS <- lme(fixed = elaPerformance ~ 
                                      sibs_total_GrpCentered + sebs_total_GrpCentered + 
                                      swtrs.social_total_GrpCentered + swtrs.engagement_total_GrpCentered,
                                     data = thesis,
                                     random = ~0 + sebs_total_GrpCentered|classID,
                                     method = "ML", 
                                     control = list(maxIter = 100, opt = "optim"))
qqnorm(resid(elaPerformance3b_addRandSEBS))
summary(elaPerformance3b_addRandSEBS)
anova(elaPerformance1_fixedIntercept, elaPerformance2_addSIBS.SEBS,
      elaPerformance2b_addSWTRS, elaPerformance3b_addRandSEBS)


## PREFERRED RANDOM COEFFICENT MODEL
# Test random slopes for ENGAGEMENT
# Significant

elaPerformance3c_addRandENGAGEMENT <- lme(fixed = elaPerformance ~ 
                                            sibs_total_GrpCentered + sebs_total_GrpCentered + 
                                            swtrs.social_total_GrpCentered + swtrs.engagement_total_GrpCentered, 
                                           data = thesis,
                                           random = ~0 + swtrs.engagement_total_GrpCentered|classID,
                                           method = "ML", 
                                           control = list(maxIter = 100, opt = "optim"))
qqnorm(resid(elaPerformance3c_addRandENGAGEMENT))
summary(elaPerformance3c_addRandENGAGEMENT)
anova(elaPerformance1_fixedIntercept, elaPerformance2_addSIBS.SEBS, 
      elaPerformance2b_addSWTRS, elaPerformance3c_addRandENGAGEMENT)


# Test random slopes for SOCIAL
# Non-significant

elaPerformance3d_addRandSOCIAL <- lme(fixed = elaPerformance ~ 
                                        sibs_total_GrpCentered + sebs_total_GrpCentered + 
                                        swtrs.social_total_GrpCentered + swtrs.engagement_total_GrpCentered, 
                                       data = thesis, 
                                       random = ~0 + swtrs.social_total_GrpCentered|classID, 
                                       method = "ML", 
                                       control = list(maxIter = 100, opt = "optim"))
qqnorm(resid(elaPerformance3d_addRandSOCIAL))
summary(elaPerformance3d_addRandSOCIAL)
anova(elaPerformance1_fixedIntercept, elaPerformance2_addSIBS.SEBS, 
      elaPerformance2b_addSWTRS, elaPerformance3d_addRandSOCIAL)



#### STEP 4: REDUCED MODEL ####



## PREFERRED REDUCED MODEL
# Reduced model with non-significant fixed effect predictors removed

elaPerformance4_reducedModel <- lme(fixed = elaPerformance ~ swtrs.engagement_total_GrpCentered, 
                                     data = thesis,
                                     random = ~0 + swtrs.engagement_total_GrpCentered|classID,
                                     method = "ML", 
                                     control = list(maxIter = 100, opt = "optim"))
qqnorm(resid(elaPerformance4_reducedModel))
summary(elaPerformance4_reducedModel)
anova(elaPerformance1_fixedIntercept, elaPerformance4_reducedModel)




#### STEP 5: TEST CONCURRENT VALIDITY WITH SWTRS ONLY MODEL ####



# Test model with ENGAGMENT and SOCIAL as only level-1 predictors
# Significant compared to random intercept model

elaPerformance5_SWTRSonly <- lme(fixed = elaPerformance ~ 
                                   swtrs.engagement_total_GrpCentered + swtrs.social_total_GrpCentered, 
                                 data = thesis,
                                 random = ~0 + swtrs.engagement_total_GrpCentered|classID,
                                 method = "ML", 
                                 control = list(maxIter = 100, opt = "optim"))
qqnorm(resid(elaPerformance5_SWTRSonly))
summary(elaPerformance5_SWTRSonly)
anova(elaPerformance1_fixedIntercept, elaPerformance5_SWTRSonly)


# Test addition of level-2 means
# Non-significant improvement

elaPerformance5b_SWTRSonly <- lme(fixed = elaPerformance ~ 
                                    swtrs.engagement_total_GrpCentered + swtrs.social_total_GrpCentered +
                                    swtrs.social_total_mean + swtrs.engagement_total_mean, 
                                   data = thesis,
                                   random = ~0 + swtrs.engagement_total_GrpCentered|classID,
                                   method = "ML", 
                                   control = list(maxIter = 100, opt = "optim"))
qqnorm(resid(elaPerformance5b_SWTRSonly))
summary(elaPerformance5b_SWTRSonly)
anova(elaPerformance1_fixedIntercept, elaPerformance5_SWTRSonly, elaPerformance5b_SWTRSonly)


