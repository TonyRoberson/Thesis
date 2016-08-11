###Multilevel modeling analyses predicting PERCENT ON-TASK

#Relevant packages to load
library('nlme')
library('car')




################### SIBS/SEBS AND SWTRS COMPARISON ##########################



### STEP 1: INTERCEPT ONLY ###



#Test fixed intercept-only model
#No clustering

pot1_fixedIntercept <- gls(percentOnTask ~ 1, 
                           data = thesis, 
                           method = "ML")
qqnorm(resid(pot1_fixedIntercept))
summary(pot1_fixedIntercept)


##PREFERRED INTERCEPT-ONLY MODEL
#Test intercept-only model
#Include clustering

pot1b_randomIntercept <- lme(fixed = percentOnTask ~ 1, 
                             data = thesis, 
                             random = ~1|classID, 
                             method = "ML")
qqnorm(resid(pot1b_randomIntercept))
summary(pot1b_randomIntercept)
anova(pot1_fixedIntercept, pot1b_randomIntercept)



### STEP 2: LEVEL-1 AND -2 FIXED VARIANCE COMPONENTS ###



#Add SIBS and SEBS predictors

pot2_addSIBS.SEBS <- lme(fixed = percentOnTask ~ sibs_total_GrpCentered + sebs_total_GrpCentered, 
                         data = thesis, 
                         random = ~1|classID, 
                         method = "ML")
qqnorm(resid(pot2_addSIBS.SEBS))
summary(pot2_addSIBS.SEBS)
anova(pot1_fixedIntercept, pot1b_randomIntercept, pot2_addSIBS.SEBS)


#Add ENGAGMENT and SOCIAL (SWTRS) predictors

pot2b_addSWTRS <- lme(fixed = percentOnTask ~ 
                       sibs_total_GrpCentered + sebs_total_GrpCentered + swtrs.social_total_GrpCentered + swtrs.engagement_total_GrpCentered, 
                      data = thesis, 
                      random = ~1|classID, 
                      method = "ML")
qqnorm(resid(pot2b_addSWTRS))
summary(pot2b_addSWTRS)
anova(pot1_fixedIntercept, pot1b_randomIntercept, pot2_addSIBS.SEBS, pot2b_addSWTRS)


##PREFERRED FIXED EFFECTS MODEL
#Add level-2 group means for each level-1 predictor

pot2c_addGroupMeans <- lme(fixed = percentOnTask ~ 
                            sibs_total_GrpCentered + sebs_total_GrpCentered + swtrs.social_total_GrpCentered + swtrs.engagement_total_GrpCentered +
                            sibs_total_mean + sebs_total_mean + swtrs.social_total_mean + swtrs.engagement_total_mean, 
                          data = thesis, 
                          random = ~1|classID, 
                          method = "ML")
qqnorm(resid(pot2c_addGroupMeans))
summary(pot2c_addGroupMeans)
anova(pot1_fixedIntercept, pot1b_randomIntercept, pot2_addSIBS.SEBS, pot2b_addSWTRS, pot2c_addGroupMeans)



### STEP 3: RANDOM COEFFICIENTS ###



#Test random slopes for SIBS
#Non-significant

pot3_addRandSIBS <- lme(fixed = percentOnTask ~ 
                            sibs_total_GrpCentered + sebs_total_GrpCentered + swtrs.social_total_GrpCentered + swtrs.engagement_total_GrpCentered +
                            sibs_total_mean + sebs_total_mean + swtrs.social_total_mean + swtrs.engagement_total_mean, 
                        data = thesis,
                        random = ~1 + sibs_total_GrpCentered|classID,
                        method = "ML")
qqnorm(resid(pot3_addRandSIBS))
summary(pot3_addRandSIBS)
anova(pot1_fixedIntercept, pot1b_randomIntercept, pot2_addSIBS.SEBS, pot2b_addSWTRS, pot2c_addGroupMeans, pot3_addRandSIBS)


#Test random slopes for SEBS
#Significant

pot3b_addRandSEBS <- lme(fixed = percentOnTask ~ 
                          sibs_total_GrpCentered + sebs_total_GrpCentered + swtrs.social_total_GrpCentered + swtrs.engagement_total_GrpCentered +
                          sibs_total_mean + sebs_total_mean + swtrs.social_total_mean + swtrs.engagement_total_mean, 
                         data = thesis,
                         random = ~1 + sebs_total_GrpCentered|classID,
                         method = "ML", 
                         control = list(maxIter = 100, opt = "optim"))
qqnorm(resid(pot3b_addRandSEBS))
summary(pot3b_addRandSEBS)
anova(pot1_fixedIntercept, pot1b_randomIntercept, pot2_addSIBS.SEBS, pot2b_addSWTRS, pot2c_addGroupMeans, pot3b_addRandSEBS)


#Test random slopes for ENGAGEMENT
#Significant

pot3c_addRandENGAGEMENT <- lme(fixed = percentOnTask ~ 
                                 sibs_total_GrpCentered + sebs_total_GrpCentered + swtrs.social_total_GrpCentered + swtrs.engagement_total_GrpCentered +
                                 sibs_total_mean + sebs_total_mean + swtrs.social_total_mean + swtrs.engagement_total_mean, 
                               data = thesis,
                               random = ~1 + swtrs.engagement_total_GrpCentered|classID,
                               method = "ML", 
                               control = list(maxIter = 100, opt = "optim"))
qqnorm(resid(pot3c_addRandENGAGEMENT))
summary(pot3c_addRandENGAGEMENT)
anova(pot1_fixedIntercept, pot1b_randomIntercept, pot2_addSIBS.SEBS, pot2b_addSWTRS, pot2c_addGroupMeans, pot3c_addRandENGAGEMENT)


#Test random slopes for SOCIAL
#Significant

pot3d_addRandSOCIAL <- lme(fixed = percentOnTask ~ 
                                 sibs_total_GrpCentered + sebs_total_GrpCentered + swtrs.social_total_GrpCentered + swtrs.engagement_total_GrpCentered +
                                 sibs_total_mean + sebs_total_mean + swtrs.social_total_mean + swtrs.engagement_total_mean, 
                               data = thesis, 
                               random = ~1 + swtrs.social_total_GrpCentered|classID, 
                               method = "ML", 
                               control = list(maxIter = 100, opt = "optim"))
qqnorm(resid(pot3d_addRandSOCIAL))
summary(pot3d_addRandSOCIAL)
anova(pot1_fixedIntercept, pot1b_randomIntercept, pot2_addSIBS.SEBS, pot2b_addSWTRS, pot2c_addGroupMeans, pot3d_addRandSOCIAL)


#Final random model with all fixed effects and significant random slopes
#Non-significant

pot3e_Full <- lme(fixed = percentOnTask ~ 
                             sibs_total_GrpCentered + sebs_total_GrpCentered + swtrs.social_total_GrpCentered + swtrs.engagement_total_GrpCentered +
                             sibs_total_mean + sebs_total_mean + swtrs.social_total_mean + swtrs.engagement_total_mean, 
                           data = thesis, 
                           random = ~1 + sebs_total_GrpCentered + swtrs.engagement_total_GrpCentered + swtrs.social_total_GrpCentered|classID, 
                           method = "ML", 
                           control = list(maxIter = 100, opt = "optim"))
qqnorm(resid(pot3e_Full))
summary(pot3e_Full)
anova(pot1_fixedIntercept, pot1b_randomIntercept, pot2_addSIBS.SEBS, pot2b_addSWTRS, pot2c_addGroupMeans, pot3e_Full)


##PREFERRED RANDOM MODEL
#final random model with all fixed effects and both significant SWTRS random slopes.
#SEBS random slopes were removed as it added the least improvement.
#Significant

pot3f_Full <- lme(fixed = percentOnTask ~ 
                    sibs_total_GrpCentered + sebs_total_GrpCentered + swtrs.social_total_GrpCentered + swtrs.engagement_total_GrpCentered +
                    sibs_total_mean + sebs_total_mean + swtrs.social_total_mean + swtrs.engagement_total_mean, 
                  data = thesis, 
                  random = ~1 + swtrs.engagement_total_GrpCentered + swtrs.social_total_GrpCentered|classID, 
                  method = "ML", 
                  control = list(maxIter = 100, opt = "optim"))
qqnorm(resid(pot3f_Full))
summary(pot3f_Full)
anova(pot1_fixedIntercept, pot1b_randomIntercept, pot2_addSIBS.SEBS, pot2b_addSWTRS, pot2c_addGroupMeans, pot3f_Full)



#### STEP 4: REDUCED MODEL ####



#Reduced model with non-significant fixed effect predictors removed

pot4_reducedModel <- lme(fixed = percentOnTask ~ swtrs.engagement_total_GrpCentered + swtrs.social_total_GrpCentered,
                         data = thesis, 
                         random = ~1|classID, 
                         method = "ML", 
                         control = list(maxIter = 100, opt = "optim"))
qqnorm(resid(pot4_reducedModel))
summary(pot4_reducedModel)
anova(pot1_fixedIntercept, pot1b_randomIntercept, pot4_reducedModel)


##PREFERRED REDUCED MODEL
#Reduced model with non-significant fixed effect predictors removed
#and associated random effects if significant

pot4b_reducedModel <- lme(fixed = percentOnTask ~ swtrs.engagement_total_GrpCentered + swtrs.social_total_GrpCentered,
                         data = thesis, 
                         random = ~1 + swtrs.engagement_total_GrpCentered + swtrs.social_total_GrpCentered|classID, 
                         method = "ML", 
                         control = list(maxIter = 100, opt = "optim"))
qqnorm(resid(pot4b_reducedModel))
summary(pot4b_reducedModel)
anova(pot1_fixedIntercept, pot1b_randomIntercept, pot4_reducedModel, pot4b_reducedModel)
