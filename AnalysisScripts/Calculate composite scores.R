## Load relevant packages
library(plyr)
library(dplyr)
library(tidyr)
library(car)
library(psych)

## Calculate composite scores for SIBS and SEBS
thesis <- mutate(thesis,
                 sebs_total = sebs_defiantOppositional + sebs_fightsArgues + 
                              sebs_bulliesOthers + sebs_easilyAngry + 
                              sebs_lies + sebs_disruptsClass + sebs_sittingStillDifficult,
                 sibs_total = sibs_bullied + sibs_nervousWorriedFearful + 
                              sibs_spendsTimeAlone + sibs_clingsToAdults + 
                              sibs_withdrawn + sibs_sadUnhappy + sibs_complainsSickHurt)
