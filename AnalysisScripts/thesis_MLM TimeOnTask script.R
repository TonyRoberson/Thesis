###Multilevel modeling analyses predicting TIME ON-TASK

#Relevant packages to load
library('nlme')
library('car')




################### SIBS/SEBS AND SWTRS COMPARISON ##########################



### STEP 1: INTERCEPT ONLY ###



#Test fixed intercept-only model
#No clustering

tot1_fixedIntercept <- gls(percentOnTask ~ 1, 
                           data = thesis, 
                           method = "ML")
qqnorm(resid(tot1_fixedIntercept))
summary(tot1_fixedIntercept)


## PREFERRED INTERCEPT-ONLY MODEL
# Test intercept-only model
# Include clustering

tot1b_randomIntercept <- lme(fixed = percentOnTask ~ 1, 
                             data = thesis, 
                             random = ~1|classID, 
                             method = "ML")
qqnorm(resid(tot1b_randomIntercept))
summary(tot1b_randomIntercept)
anova(tot1_fixedIntercept, tot1b_randomIntercept)



### STEP 2: LEVEL-1 AND -2 FIXED VARIANCE COMPONENTS ###



# Add SIBS and SEBS predictors

tot2_addSIBS.SEBS <- lme(fixed = percentOnTask ~ sibs_total_GrpCentered + sebs_total_GrpCentered, 
                         data = thesis, 
                         random = ~1|classID, 
                         method = "ML")
qqnorm(resid(tot2_addSIBS.SEBS))
summary(tot2_addSIBS.SEBS)
anova(tot1_fixedIntercept, tot1b_randomIntercept, tot2_addSIBS.SEBS)


# Add ENGAGMENT and SOCIAL (SWTRS) predictors

tot2b_addSWTRS <- lme(fixed = percentOnTask ~ 
                       sibs_total_GrpCentered + sebs_total_GrpCentered + 
                        swtrs.social_total_GrpCentered + swtrs.engagement_total_GrpCentered, 
                      data = thesis, 
                      random = ~1|classID, 
                      method = "ML")
qqnorm(resid(tot2b_addSWTRS))
summary(tot2b_addSWTRS)
anova(tot1_fixedIntercept, tot1b_randomIntercept, tot2_addSIBS.SEBS, tot2b_addSWTRS)


# Test alternate fixed predictor order
# Test SWTRS alone

tot2c_SWTRSalone <- lme(fixed = percentOnTask ~
                          swtrs.social_total_GrpCentered + swtrs.engagement_total_GrpCentered,
                        random = ~1|classID,
                        method = "ML",
                        data = thesis)
qqnorm(resid(tot2c_SWTRSalone))
summary(tot2c_SWTRSalone)
anova(tot1_fixedIntercept, tot1b_randomIntercept, tot2c_SWTRSalone)


# Test SIBS and SEBS added to SWTRS alone

tot2d_addPTHtoWB <- lme(fixed = percentOnTask ~
                          swtrs.social_total_GrpCentered + swtrs.engagement_total_GrpCentered +
                            sibs_total_GrpCentered + sebs_total_GrpCentered,
                        random = ~1|classID,
                        method = "ML",
                        data = thesis)
qqnorm(resid(tot2d_addPTHtoWB))
summary(tot2d_addPTHtoWB)
anova(tot1_fixedIntercept, tot1b_randomIntercept, tot2c_SWTRSalone, tot2d_addPTHtoWB)



## PREFERRED FIXED EFFECTS MODEL
# Add level-2 group means for each level-1 predictor

tot2e_addGroupMeans <- lme(fixed = percentOnTask ~ 
                            sibs_total_GrpCentered + sebs_total_GrpCentered + 
                             swtrs.social_total_GrpCentered + swtrs.engagement_total_GrpCentered +
                            sibs_total_mean + sebs_total_mean + 
                             swtrs.social_total_mean + swtrs.engagement_total_mean, 
                          data = thesis, 
                          random = ~1|classID, 
                          method = "ML")
qqnorm(resid(tot2e_addGroupMeans))
summary(tot2e_addGroupMeans)
anova(tot1_fixedIntercept, tot1b_randomIntercept, tot2_addSIBS.SEBS, 
      tot2b_addSWTRS, tot2e_addGroupMeans)



### STEP 3: RANDOM COEFFICIENTS ###



#Test random slopes for SIBS
#Non-significant

tot3_addRandSIBS <- lme(fixed = percentOnTask ~ 
                            sibs_total_GrpCentered + sebs_total_GrpCentered + 
                              swtrs.social_total_GrpCentered + swtrs.engagement_total_GrpCentered +
                            sibs_total_mean + sebs_total_mean + 
                              swtrs.social_total_mean + swtrs.engagement_total_mean, 
                        data = thesis,
                        random = ~1 + sibs_total_GrpCentered|classID,
                        method = "ML")
qqnorm(resid(tot3_addRandSIBS))
summary(tot3_addRandSIBS)
anova(tot1_fixedIntercept, tot1b_randomIntercept, tot2_addSIBS.SEBS, 
      tot2b_addSWTRS, tot2e_addGroupMeans, tot3_addRandSIBS)


#Test random slopes for SEBS
#Significant

tot3b_addRandSEBS <- lme(fixed = percentOnTask ~ 
                          sibs_total_GrpCentered + sebs_total_GrpCentered + 
                           swtrs.social_total_GrpCentered + swtrs.engagement_total_GrpCentered +
                          sibs_total_mean + sebs_total_mean + 
                           swtrs.social_total_mean + swtrs.engagement_total_mean, 
                         data = thesis,
                         random = ~1 + sebs_total_GrpCentered|classID,
                         method = "ML", 
                         control = list(maxIter = 100, opt = "optim"))
qqnorm(resid(tot3b_addRandSEBS))
summary(tot3b_addRandSEBS)
anova(tot1_fixedIntercept, tot1b_randomIntercept, tot2_addSIBS.SEBS, 
      tot2b_addSWTRS, tot2e_addGroupMeans, tot3b_addRandSEBS)


#Test random slopes for ENGAGEMENT
#Significant

tot3c_addRandENGAGEMENT <- lme(fixed = percentOnTask ~ 
                                 sibs_total_GrpCentered + sebs_total_GrpCentered + 
                                  swtrs.social_total_GrpCentered + swtrs.engagement_total_GrpCentered +
                                 sibs_total_mean + sebs_total_mean + 
                                  swtrs.social_total_mean + swtrs.engagement_total_mean, 
                               data = thesis,
                               random = ~1 + swtrs.engagement_total_GrpCentered|classID,
                               method = "ML", 
                               control = list(maxIter = 100, opt = "optim"))
qqnorm(resid(tot3c_addRandENGAGEMENT))
summary(tot3c_addRandENGAGEMENT)
anova(tot1_fixedIntercept, tot1b_randomIntercept, tot2_addSIBS.SEBS, 
      tot2b_addSWTRS, tot2e_addGroupMeans, tot3c_addRandENGAGEMENT)


#Test random slopes for SOCIAL
#Significant

tot3d_addRandSOCIAL <- lme(fixed = percentOnTask ~ 
                                 sibs_total_GrpCentered + sebs_total_GrpCentered + 
                                  swtrs.social_total_GrpCentered + swtrs.engagement_total_GrpCentered +
                                 sibs_total_mean + sebs_total_mean + 
                                  swtrs.social_total_mean + swtrs.engagement_total_mean, 
                               data = thesis, 
                               random = ~1 + swtrs.social_total_GrpCentered|classID, 
                               method = "ML", 
                               control = list(maxIter = 100, opt = "optim"))
qqnorm(resid(tot3d_addRandSOCIAL))
summary(tot3d_addRandSOCIAL)
anova(tot1_fixedIntercept, tot1b_randomIntercept, tot2_addSIBS.SEBS, 
      tot2b_addSWTRS, tot2e_addGroupMeans, tot3d_addRandSOCIAL)


#Final random model with all fixed effects and significant random slopes
#Non-significant

tot3e_RandomModel <- lme(fixed = percentOnTask ~ 
                             sibs_total_GrpCentered + sebs_total_GrpCentered + 
                              swtrs.social_total_GrpCentered + swtrs.engagement_total_GrpCentered +
                             sibs_total_mean + sebs_total_mean + 
                              swtrs.social_total_mean + swtrs.engagement_total_mean, 
                           data = thesis, 
                           random = ~1 + sebs_total_GrpCentered + swtrs.engagement_total_GrpCentered + 
                              swtrs.social_total_GrpCentered|classID, 
                           method = "ML", 
                           control = list(maxIter = 100, opt = "optim"))
qqnorm(resid(tot3e_RandomModel))
summary(tot3e_RandomModel)
anova(tot1_fixedIntercept, tot1b_randomIntercept, tot2_addSIBS.SEBS, 
      tot2b_addSWTRS, tot2e_addGroupMeans, tot3e_RandomModel)


##PREFERRED RANDOM MODEL
#final random model with all fixed effects and both significant SWTRS random slopes.
#SEBS random slopes were removed as it added the least improvement.
#Significant

tot3f_AltRandomModel <- lme(fixed = percentOnTask ~ 
                    sibs_total_GrpCentered + sebs_total_GrpCentered + 
                      swtrs.social_total_GrpCentered + swtrs.engagement_total_GrpCentered +
                    sibs_total_mean + sebs_total_mean + 
                      swtrs.social_total_mean + swtrs.engagement_total_mean, 
                  data = thesis, 
                  random = ~1 + swtrs.engagement_total_GrpCentered + 
                    swtrs.social_total_GrpCentered|classID, 
                  method = "ML", 
                  control = list(maxIter = 100, opt = "optim"))
qqnorm(resid(tot3f_AltRandomModel))
summary(tot3f_AltRandomModel)
anova(tot1_fixedIntercept, tot1b_randomIntercept, tot2_addSIBS.SEBS, 
      tot2b_addSWTRS, tot2e_addGroupMeans, tot3f_AltRandomModel)



#### STEP 4: REDUCED MODEL ####



#Reduced model with non-significant fixed effect predictors removed

tot4_reducedModel <- lme(fixed = percentOnTask ~ 
                           swtrs.engagement_total_GrpCentered + swtrs.social_total_GrpCentered,
                         data = thesis, 
                         random = ~1|classID, 
                         method = "ML", 
                         control = list(maxIter = 100, opt = "optim"))
qqnorm(resid(tot4_reducedModel))
summary(tot4_reducedModel)
anova(tot1_fixedIntercept, tot1b_randomIntercept, tot4_reducedModel)


##PREFERRED REDUCED MODEL
#Reduced model with non-significant fixed effect predictors removed
#and associated random effects if significant

tot4b_reducedModelRand <- lme(fixed = percentOnTask ~ 
                            swtrs.engagement_total_GrpCentered + swtrs.social_total_GrpCentered,
                         data = thesis, 
                         random = ~1 + swtrs.engagement_total_GrpCentered + 
                           swtrs.social_total_GrpCentered|classID, 
                         method = "ML", 
                         control = list(maxIter = 100, opt = "optim"))
qqnorm(resid(tot4b_reducedModel))
summary(tot4b_reducedModel)
anova(tot1_fixedIntercept, tot1b_randomIntercept, tot4_reducedModel, tot4b_reducedModelRand)
