#Below is an outline of the procedures and syntax used to clean 
#and prepare the SWTRS pilot study data set for analysis.


#Load relevant packages
library(pacman)
p_load(plyr,dplyr,tidyr,car)

#Import raw thesis data
#NEED TO UPDATE FILE LOCATION DEPENDING ON WHERE IT IS!
thesis_raw <- read.csv("~/Desktop/Thesis/Data Analyses and Dcumentation/SWTRS_ Student Behaviors (Responses) - Form Responses 1.csv", stringsAsFactors=FALSE)

#Rename variables from raw data file and save as new data frame "thesis"
thesis <- rename(thesis_raw,
              ##Class/teacher letter code
                classID = Enter.Unique.Letter.Code,
              ##SEBS Items  
                sebs_defiantOppositional = For.each.item..please.indicate.how.frequently.the.student.showed.the.behavior.over.the.last.two.months..Never..Rarely..Occasionally..or.Frequently.Almost.Always...Defiant.or.oppositional.to.adults.,
                sebs_fightsArgues = For.each.item..please.indicate.how.frequently.the.student.showed.the.behavior.over.the.last.two.months..Never..Rarely..Occasionally..or.Frequently.Almost.Always...Fights.or.argues.with.peers.,
                sebs_bulliesOthers = For.each.item..please.indicate.how.frequently.the.student.showed.the.behavior.over.the.last.two.months..Never..Rarely..Occasionally..or.Frequently.Almost.Always...Bullies.others.,
                sebs_easilyAngry = For.each.item..please.indicate.how.frequently.the.student.showed.the.behavior.over.the.last.two.months..Never..Rarely..Occasionally..or.Frequently.Almost.Always...Gets.angry.easily.,
                sebs_lies = For.each.item..please.indicate.how.frequently.the.student.showed.the.behavior.over.the.last.two.months..Never..Rarely..Occasionally..or.Frequently.Almost.Always...Lies.to.get.out.of.trouble.,
                sebs_disruptsClass = For.each.item..please.indicate.how.frequently.the.student.showed.the.behavior.over.the.last.two.months..Never..Rarely..Occasionally..or.Frequently.Almost.Always...Disrupts.class.activities.,
                sebs_sittingStillDifficult = For.each.item..please.indicate.how.frequently.the.student.showed.the.behavior.over.the.last.two.months..Never..Rarely..Occasionally..or.Frequently.Almost.Always...Has.difficulty.sitting.still.,
              ##SIBS Items  
                sibs_nervousWorriedFearful = For.each.item..please.indicate.how.frequently.the.student.showed.the.behavior.over.the.last.two.months..Never..Rarely..Occasionally..or.Frequently.Almost.Always...Nervous..worried..or.fearful.,
                sibs_bullied = For.each.item..please.indicate.how.frequently.the.student.showed.the.behavior.over.the.last.two.months..Never..Rarely..Occasionally..or.Frequently.Almost.Always...Bullied.by.peers.,
                sibs_spendsTimeAlone = For.each.item..please.indicate.how.frequently.the.student.showed.the.behavior.over.the.last.two.months..Never..Rarely..Occasionally..or.Frequently.Almost.Always...Spends.time.alone.,
                sibs_clingsToAdults = For.each.item..please.indicate.how.frequently.the.student.showed.the.behavior.over.the.last.two.months..Never..Rarely..Occasionally..or.Frequently.Almost.Always...Clings.to.adults.,
                sibs_withdrawn = For.each.item..please.indicate.how.frequently.the.student.showed.the.behavior.over.the.last.two.months..Never..Rarely..Occasionally..or.Frequently.Almost.Always...Withdrawn.,
                sibs_sadUnhappy = For.each.item..please.indicate.how.frequently.the.student.showed.the.behavior.over.the.last.two.months..Never..Rarely..Occasionally..or.Frequently.Almost.Always...Seems.sad.or.unhappy.,
                sibs_complainsSickHurt = For.each.item..please.indicate.how.frequently.the.student.showed.the.behavior.over.the.last.two.months..Never..Rarely..Occasionally..or.Frequently.Almost.Always...Complains.about.being.sick.or.hurt.,
              ##SWTRS Items
                swtrs_followsDirections = For.each.item..please.indicate.how.frequently.the.student.showed.the.behavior.over.the.last.two.months..Almost.Never..Sometimes..Often..or.Almost.Always...Follows.directions.in.class., 
                swtrs_listensToTeachers = For.each.item..please.indicate.how.frequently.the.student.showed.the.behavior.over.the.last.two.months..Almost.Never..Sometimes..Often..or.Almost.Always...Listens.to.teachers.and.staff.at.school.,	
                swtrs_obeysClassRules = For.each.item..please.indicate.how.frequently.the.student.showed.the.behavior.over.the.last.two.months..Almost.Never..Sometimes..Often..or.Almost.Always...Obeys.classroom.rules.,	
                swtrs_friendlyWithClassmates = For.each.item..please.indicate.how.frequently.the.student.showed.the.behavior.over.the.last.two.months..Almost.Never..Sometimes..Often..or.Almost.Always...Is.friendly.with.classmates.,	
                swtrs_handsFeetToSelf = For.each.item..please.indicate.how.frequently.the.student.showed.the.behavior.over.the.last.two.months..Almost.Never..Sometimes..Often..or.Almost.Always...Keeps.hands.feet.to.self.during.class.time.,	
                swtrs_getsAlongWithClassmates = For.each.item..please.indicate.how.frequently.the.student.showed.the.behavior.over.the.last.two.months..Almost.Never..Sometimes..Often..or.Almost.Always...Gets.along.well.with.classmates.,
                swtrs_treatsClassmatesKindly = For.each.item..please.indicate.how.frequently.the.student.showed.the.behavior.over.the.last.two.months..Almost.Never..Sometimes..Often..or.Almost.Always...Treats.classmates.kindly.,
                swtrs_playsWellWithOthers = For.each.item..please.indicate.how.frequently.the.student.showed.the.behavior.over.the.last.two.months..Almost.Never..Sometimes..Often..or.Almost.Always...Plays.well.with.other.students.,	
                swtrs_respectfulToClassmates = For.each.item..please.indicate.how.frequently.the.student.showed.the.behavior.over.the.last.two.months..Almost.Never..Sometimes..Often..or.Almost.Always...Respectful.to.classmates.,
                swtrs_handlesFrustrationsWell = For.each.item..please.indicate.how.frequently.the.student.showed.the.behavior.over.the.last.two.months..Almost.Never..Sometimes..Often..or.Almost.Always...Handles.frustrations.at.school.well.,
                swtrs_peacefulDuringClass = For.each.item..please.indicate.how.frequently.the.student.showed.the.behavior.over.the.last.two.months..Almost.Never..Sometimes..Often..or.Almost.Always...Peaceful.during.class.time.,
                swtrs_respectfulToTeachers = For.each.item..please.indicate.how.frequently.the.student.showed.the.behavior.over.the.last.two.months..Almost.Never..Sometimes..Often..or.Almost.Always...Respectful.to.teachers.,
                swtrs_participatesMeaningfully = For.each.item..please.indicate.how.frequently.the.student.showed.the.behavior.over.the.last.two.months..Almost.Never..Sometimes..Often..or.Almost.Always...Participates.meaningfully.in.class.activities.,
                swtrs_wellBehavedDuringClass = For.each.item..please.indicate.how.frequently.the.student.showed.the.behavior.over.the.last.two.months..Almost.Never..Sometimes..Often..or.Almost.Always...Well.behaved.during.class.,
                swtrs_staysOnTask = For.each.item..please.indicate.how.frequently.the.student.showed.the.behavior.over.the.last.two.months..Almost.Never..Sometimes..Often..or.Almost.Always...Stays.on.task.when.given.an.assignment.,
                swtrs_staysFocused = For.each.item..please.indicate.how.frequently.the.student.showed.the.behavior.over.the.last.two.months..Almost.Never..Sometimes..Often..or.Almost.Always...Stays.focused.during.class.activities.,
                swtrs_selfControlWhenFrustrated = For.each.item..please.indicate.how.frequently.the.student.showed.the.behavior.over.the.last.two.months..Almost.Never..Sometimes..Often..or.Almost.Always...Shows.self.control.when.frustrated.at.school..,
                swtrs_engagedInLearning = For.each.item..please.indicate.how.frequently.the.student.showed.the.behavior.over.the.last.two.months..Almost.Never..Sometimes..Often..or.Almost.Always...Engaged.in.learning.,
                swtrs_confidentNewChallengingMaterial = For.each.item..please.indicate.how.frequently.the.student.showed.the.behavior.over.the.last.two.months..Almost.Never..Sometimes..Often..or.Almost.Always...Confident.when.faced.with.new.or.challenging.material..,
                swtrs_remainsCalmDifficultSituation = For.each.item..please.indicate.how.frequently.the.student.showed.the.behavior.over.the.last.two.months..Almost.Never..Sometimes..Often..or.Almost.Always...Remains.calm.when.facing.difficult.situations.at.school..,
                swtrs_seemsRelaxedAtEase = For.each.item..please.indicate.how.frequently.the.student.showed.the.behavior.over.the.last.two.months..Almost.Never..Sometimes..Often..or.Almost.Always...Seems.relaxed.and.at.ease.during.school.,
                swtrs_optimisticWillSucceedSchool = For.each.item..please.indicate.how.frequently.the.student.showed.the.behavior.over.the.last.two.months..Almost.Never..Sometimes..Often..or.Almost.Always...Optimistic.they.will.succeed.in.school.,
                swtrs_classmatesLikeWorkPlayWithThem = For.each.item..please.indicate.how.frequently.the.student.showed.the.behavior.over.the.last.two.months..Almost.Never..Sometimes..Often..or.Almost.Always...Classmates.like.to.work.or.play.with.him.her.,
                swtrs_classmatesRespectfulToThem = For.each.item..please.indicate.how.frequently.the.student.showed.the.behavior.over.the.last.two.months..Almost.Never..Sometimes..Often..or.Almost.Always...Classmates.are.respectful.toward.him.her.,
                swtrs_approachableEasyGetAlongWith = For.each.item..please.indicate.how.frequently.the.student.showed.the.behavior.over.the.last.two.months..Almost.Never..Sometimes..Often..or.Almost.Always...Approachable.and.easy.to.get.along.with.at.school.,
                swtrs_enjoysGroupWorkWithOthers = For.each.item..please.indicate.how.frequently.the.student.showed.the.behavior.over.the.last.two.months..Almost.Never..Sometimes..Often..or.Almost.Always...Enjoys.working.with.other.students.during.group.activities.,
                swtrs_sociableWithOthersDuringFreeTime = For.each.item..please.indicate.how.frequently.the.student.showed.the.behavior.over.the.last.two.months..Almost.Never..Sometimes..Often..or.Almost.Always...Sociable.with.other.students.during.free.time.,
                swtrs_needsLittleSupervision = For.each.item..please.indicate.how.frequently.the.student.showed.the.behavior.over.the.last.two.months..Almost.Never..Sometimes..Often..or.Almost.Always...Needs.little.supervision.when.given.an.assignment.,
                swtrs_comfortableWorkingIndependently = For.each.item..please.indicate.how.frequently.the.student.showed.the.behavior.over.the.last.two.months..Almost.Never..Sometimes..Often..or.Almost.Always...Comfortable.working.independently.on.school.assignments.,
                swtrs_willingTryNewActivities = For.each.item..please.indicate.how.frequently.the.student.showed.the.behavior.over.the.last.two.months..Almost.Never..Sometimes..Often..or.Almost.Always...Willing.to.try.new.activites.at.school.,
                swtrs_inquisitiveInterestedLearningNewThings = For.each.item..please.indicate.how.frequently.the.student.showed.the.behavior.over.the.last.two.months..Almost.Never..Sometimes..Often..or.Almost.Always...Inquisitive.or.interested.in.learning.new.things..,
                swtrs_laughsAppropriateTimes = For.each.item..please.indicate.how.frequently.the.student.showed.the.behavior.over.the.last.two.months..Almost.Never..Sometimes..Often..or.Almost.Always...Laughs.at.appropriate.times.in.school..,
                swtrs_smilesAtSchool = For.each.item..please.indicate.how.frequently.the.student.showed.the.behavior.over.the.last.two.months..Almost.Never..Sometimes..Often..or.Almost.Always...Smiles.at.school.,
                swtrs_seemsHappyInClass = For.each.item..please.indicate.how.frequently.the.student.showed.the.behavior.over.the.last.two.months..Almost.Never..Sometimes..Often..or.Almost.Always...Seems.happy.during.class.,
                swtrs_showsExcitementClassActivities = For.each.item..please.indicate.how.frequently.the.student.showed.the.behavior.over.the.last.two.months..Almost.Never..Sometimes..Often..or.Almost.Always...Shows.excitement.for.class.activities.,
                swtrs_alertDuringLessons = For.each.item..please.indicate.how.frequently.the.student.showed.the.behavior.over.the.last.two.months..Almost.Never..Sometimes..Often..or.Almost.Always...Alert.during.lessons.and.when.listening.to.instructions.,
              ##Concurrent outcome measures  
                percentOnTask = In.the.past.two.months..about.what.percent.of.time.was.the.student.on.task.during.class.,
                elaPerformance = In.the.past.two.months..how.well.has.the.student.performed.in.English.Language.Arts.,
                mathPerformance = In.the.past.two.months..how.well.has.the.student.performed.in.Math.,
                absences = In.the.past.two.months..about.how.many.full.days.of.school.has.the.student.missed.)

#Change class ID codes that are not "D" but student Age is "11" to "F"
thesis$classID <- as.character(thesis$classID)
thesis$classID[thesis$classID != "D" & thesis$Student.Age == "11"] <- "F"

#Standardize teacher codes
#Capitalize all lowercase letter codes and set to factor
thesis$classID <- toupper(x = thesis$classID)

#Change age "6 1/2" to "6"
thesis[173,"Student.Age"] = 6

#Convert Class ID, Gender, Race/Ethnicity, and Age to factor
thesis$classID  <- factor(x = thesis$classID)
thesis$Student.Gender <- factor(x = thesis$Student.Gender)
thesis$Student.Race.Ethnicity <- factor(x = thesis$Student.Race.Ethnicity)
thesis$Student.Age <- factor(x = thesis$Student.Age)

#Recode SIBS and SEBS to numeric
thesis <- within(thesis, {
  sibs_withdrawn <- Recode(sibs_withdrawn, 
                           '"Never" = 1; "Rarely" = 2; "Occasionally" = 3; "Frequently/Almost Always" = 4',
                           as.factor.result=FALSE)
  sibs_spendsTimeAlone <- Recode(sibs_spendsTimeAlone, 
                                 '"Never" = 1; "Rarely" = 2; "Occasionally" = 3; "Frequently/Almost Always" = 4',
                                 as.factor.result=FALSE)
  sibs_sadUnhappy <- Recode(sibs_sadUnhappy, 
                            '"Never" = 1; "Rarely" = 2; "Occasionally" = 3; "Frequently/Almost Always" = 4',
                            as.factor.result=FALSE)
  sibs_nervousWorriedFearful <- Recode(sibs_nervousWorriedFearful, 
                                       '"Never" = 1; "Rarely" = 2; "Occasionally" = 3; "Frequently/Almost Always" = 4',
                                       as.factor.result=FALSE)
  sibs_complainsSickHurt <- Recode(sibs_complainsSickHurt, 
                                   '"Never" = 1; "Rarely" = 2; "Occasionally" = 3; "Frequently/Almost Always" = 4',
                                   as.factor.result=FALSE)
  sibs_clingsToAdults <- Recode(sibs_clingsToAdults, 
                                '"Never" = 1; "Rarely" = 2; "Occasionally" = 3; "Frequently/Almost Always" = 4',
                                as.factor.result=FALSE)
  sibs_bullied <- Recode(sibs_bullied, 
                         '"Never" = 1; "Rarely" = 2; "Occasionally" = 3; "Frequently/Almost Always" = 4',
                         as.factor.result=FALSE)
  sebs_sittingStillDifficult <- Recode(sebs_sittingStillDifficult, 
                                       '"Never" = 1; "Rarely" = 2; "Occasionally" = 3; "Frequently/Almost Always" = 4',
                                       as.factor.result=FALSE)
  sebs_lies <- Recode(sebs_lies, 
                      '"Never" = 1; "Rarely" = 2; "Occasionally" = 3; "Frequently/Almost Always" = 4',
                      as.factor.result=FALSE)
  sebs_fightsArgues <- Recode(sebs_fightsArgues, 
                              '"Never" = 1; "Rarely" = 2; "Occasionally" = 3; "Frequently/Almost Always" = 4',
                              as.factor.result=FALSE)
  sebs_easilyAngry <- Recode(sebs_easilyAngry, 
                             '"Never" = 1; "Rarely" = 2; "Occasionally" = 3; "Frequently/Almost Always" = 4',
                             as.factor.result=FALSE)
  sebs_disruptsClass <- Recode(sebs_disruptsClass, 
                               '"Never" = 1; "Rarely" = 2; "Occasionally" = 3; "Frequently/Almost Always" = 4',
                               as.factor.result=FALSE)
  sebs_defiantOppositional <- Recode(sebs_defiantOppositional, 
                                     '"Never" = 1; "Rarely" = 2; "Occasionally" = 3; "Frequently/Almost Always" = 4',
                                     as.factor.result=FALSE)
  sebs_bulliesOthers <- Recode(sebs_bulliesOthers, 
                               '"Never" = 1; "Rarely" = 2; "Occasionally" = 3; "Frequently/Almost Always" = 4',
                               as.factor.result=FALSE)
})


#Recode SWTRS items to numeric
thesis <- within(thesis, {
  swtrs_willingTryNewActivities <- Recode(swtrs_willingTryNewActivities, 
                                          '"Almost Never" = 1; "Sometimes" = 2; "Often" = 3; "Almost Always" = 4;', 
                                          as.factor.result=FALSE)
  swtrs_wellBehavedDuringClass <- Recode(swtrs_wellBehavedDuringClass, 
                                         '"Almost Never" = 1; "Sometimes" = 2; "Often" = 3; "Almost Always" = 4;', 
                                         as.factor.result=FALSE)
  swtrs_treatsClassmatesKindly <- Recode(swtrs_treatsClassmatesKindly, 
                                         '"Almost Never" = 1; "Sometimes" = 2; "Often" = 3; "Almost Always" = 4;', 
                                         as.factor.result=FALSE)
  swtrs_staysOnTask <- Recode(swtrs_staysOnTask, 
                              '"Almost Never" = 1; "Sometimes" = 2; "Often" = 3; "Almost Always" = 4;', 
                              as.factor.result=FALSE)
  swtrs_staysFocused <- Recode(swtrs_staysFocused, 
                               '"Almost Never" = 1; "Sometimes" = 2; "Often" = 3; "Almost Always" = 4;', 
                               as.factor.result=FALSE)
  swtrs_sociableWithOthersDuringFreeTime <- 
    Recode(swtrs_sociableWithOthersDuringFreeTime, 
           '"Almost Never" = 1; "Sometimes" = 2; "Often" = 3; "Almost Always" = 4;', 
           as.factor.result=FALSE)
  swtrs_smilesAtSchool <- Recode(swtrs_smilesAtSchool, 
                                 '"Almost Never" = 1; "Sometimes" = 2; "Often" = 3; "Almost Always" = 4;', 
                                 as.factor.result=FALSE)
  swtrs_showsExcitementClassActivities <- 
    Recode(swtrs_showsExcitementClassActivities, 
           '"Almost Never" = 1; "Sometimes" = 2; "Often" = 3; "Almost Always" = 4;', 
           as.factor.result=FALSE)
  swtrs_selfControlWhenFrustrated <- Recode(swtrs_selfControlWhenFrustrated,
                                            '"Almost Never" = 1; "Sometimes" = 2; "Often" = 3; "Almost Always" = 4;', 
                                            as.factor.result=FALSE)
  swtrs_seemsRelaxedAtEase <- Recode(swtrs_seemsRelaxedAtEase, 
                                     '"Almost Never" = 1; "Sometimes" = 2; "Often" = 3; "Almost Always" = 4;', 
                                     as.factor.result=FALSE)
  swtrs_seemsHappyInClass <- Recode(swtrs_seemsHappyInClass, 
                                    '"Almost Never" = 1; "Sometimes" = 2; "Often" = 3; "Almost Always" = 4;', 
                                    as.factor.result=FALSE)
  swtrs_respectfulToTeachers <- Recode(swtrs_respectfulToTeachers, 
                                       '"Almost Never" = 1; "Sometimes" = 2; "Often" = 3; "Almost Always" = 4;', 
                                       as.factor.result=FALSE)
  swtrs_respectfulToClassmates <- Recode(swtrs_respectfulToClassmates, 
                                         '"Almost Never" = 1; "Sometimes" = 2; "Often" = 3; "Almost Always" = 4;', 
                                         as.factor.result=FALSE)
  swtrs_remainsCalmDifficultSituation <- 
    Recode(swtrs_remainsCalmDifficultSituation, 
           '"Almost Never" = 1; "Sometimes" = 2; "Often" = 3; "Almost Always" = 4;', 
           as.factor.result=FALSE)
  swtrs_playsWellWithOthers <- Recode(swtrs_playsWellWithOthers, 
                                      '"Almost Never" = 1; "Sometimes" = 2; "Often" = 3; "Almost Always" = 4;', 
                                      as.factor.result=FALSE)
  swtrs_peacefulDuringClass <- Recode(swtrs_peacefulDuringClass, 
                                      '"Almost Never" = 1; "Sometimes" = 2; "Often" = 3; "Almost Always" = 4;', 
                                      as.factor.result=FALSE)
  swtrs_participatesMeaningfully <- Recode(swtrs_participatesMeaningfully, 
                                           '"Almost Never" = 1; "Sometimes" = 2; "Often" = 3; "Almost Always" = 4;', 
                                           as.factor.result=FALSE)
  swtrs_optimisticWillSucceedSchool <- 
    Recode(swtrs_optimisticWillSucceedSchool, 
           '"Almost Never" = 1; "Sometimes" = 2; "Often" = 3; "Almost Always" = 4;', 
           as.factor.result=FALSE)
  swtrs_obeysClassRules <- Recode(swtrs_obeysClassRules, 
                                  '"Almost Never" = 1; "Sometimes" = 2; "Often" = 3; "Almost Always" = 4;', 
                                  as.factor.result=FALSE)
  swtrs_needsLittleSupervision <- Recode(swtrs_needsLittleSupervision, 
                                         '"Almost Never" = 1; "Sometimes" = 2; "Often" = 3; "Almost Always" = 4;', 
                                         as.factor.result=FALSE)
  swtrs_listensToTeachers <- Recode(swtrs_listensToTeachers, 
                                    '"Almost Never" = 1; "Sometimes" = 2; "Often" = 3; "Almost Always" = 4;', 
                                    as.factor.result=FALSE)
  swtrs_laughsAppropriateTimes <- Recode(swtrs_laughsAppropriateTimes, 
                                         '"Almost Never" = 1; "Sometimes" = 2; "Often" = 3; "Almost Always" = 4;', 
                                         as.factor.result=FALSE)
  swtrs_inquisitiveInterestedLearningNewThings <- 
    Recode(swtrs_inquisitiveInterestedLearningNewThings, 
           '"Almost Never" = 1; "Sometimes" = 2; "Often" = 3; "Almost Always" = 4;', 
           as.factor.result=FALSE)
  swtrs_handsFeetToSelf <- Recode(swtrs_handsFeetToSelf, 
                                  '"Almost Never" = 1; "Sometimes" = 2; "Often" = 3; "Almost Always" = 4;', 
                                  as.factor.result=FALSE)
  swtrs_handlesFrustrationsWell <- Recode(swtrs_handlesFrustrationsWell, 
                                          '"Almost Never" = 1; "Sometimes" = 2; "Often" = 3; "Almost Always" = 4;', 
                                          as.factor.result=FALSE)
  swtrs_getsAlongWithClassmates <- Recode(swtrs_getsAlongWithClassmates, 
                                          '"Almost Never" = 1; "Sometimes" = 2; "Often" = 3; "Almost Always" = 4;', 
                                          as.factor.result=FALSE)
  swtrs_friendlyWithClassmates <- Recode(swtrs_friendlyWithClassmates, 
                                         '"Almost Never" = 1; "Sometimes" = 2; "Often" = 3; "Almost Always" = 4;', 
                                         as.factor.result=FALSE)
  swtrs_followsDirections <- Recode(swtrs_followsDirections, 
                                    '"Almost Never" = 1; "Sometimes" = 2; "Often" = 3; "Almost Always" = 4;', 
                                    as.factor.result=FALSE)
  swtrs_enjoysGroupWorkWithOthers <- Recode(swtrs_enjoysGroupWorkWithOthers,
                                            '"Almost Never" = 1; "Sometimes" = 2; "Often" = 3; "Almost Always" = 4;', 
                                            as.factor.result=FALSE)
  swtrs_engagedInLearning <- Recode(swtrs_engagedInLearning, 
                                    '"Almost Never" = 1; "Sometimes" = 2; "Often" = 3; "Almost Always" = 4;', 
                                    as.factor.result=FALSE)
  swtrs_confidentNewChallengingMaterial <- 
    Recode(swtrs_confidentNewChallengingMaterial, 
           '"Almost Never" = 1; "Sometimes" = 2; "Often" = 3; "Almost Always" = 4;', 
           as.factor.result=FALSE)
  swtrs_comfortableWorkingIndependently <- 
    Recode(swtrs_comfortableWorkingIndependently, 
           '"Almost Never" = 1; "Sometimes" = 2; "Often" = 3; "Almost Always" = 4;', 
           as.factor.result=FALSE)
  swtrs_classmatesRespectfulToThem <- 
    Recode(swtrs_classmatesRespectfulToThem, 
           '"Almost Never" = 1; "Sometimes" = 2; "Often" = 3; "Almost Always" = 4;', 
           as.factor.result=FALSE)
  swtrs_classmatesLikeWorkPlayWithThem <- 
    Recode(swtrs_classmatesLikeWorkPlayWithThem, 
           '"Almost Never" = 1; "Sometimes" = 2; "Often" = 3; "Almost Always" = 4;', 
           as.factor.result=FALSE)
  swtrs_approachableEasyGetAlongWith <- 
    Recode(swtrs_approachableEasyGetAlongWith, 
           '"Almost Never" = 1; "Sometimes" = 2; "Often" = 3; "Almost Always" = 4;', 
           as.factor.result=FALSE)
  swtrs_alertDuringLessons <- Recode(swtrs_alertDuringLessons, 
                                     '"Almost Never" = 1; "Sometimes" = 2; "Often" = 3; "Almost Always" = 4;', 
                                     as.factor.result=FALSE)
})

#Recode percentOnTask to numeric
thesis <- within(thesis, {
  percentOnTask <- Recode(percentOnTask, 
                          '"0–10%" = 1; "11–20%" = 2; "21–30%" = 3; "31–40%" = 4; "41–50%" = 5; 
                          "51–60%" = 6; "61–70%" = 7; "71–80%" = 8; "81–90%" = 9; "91–100%" = 10; ;',
                          as.factor.result=FALSE)
})

#Recode mathPerformance and elaPerformance to numeric
thesis <- within(thesis, {
  mathPerformance <- Recode(mathPerformance, 
                            '"Far below grade level" = 1; "Below grade level" = 2; "At grade level" = 3; 
                            "Above grade level" = 4; "Far above grade level" = 5',
                            as.factor.result=FALSE)
  elaPerformance <- Recode(elaPerformance, 
                           '"Far below grade level" = 1; "Below grade level" = 2; "At grade level" = 3; 
                           "Above grade level" = 4; "Far above grade level" = 5',
                           as.factor.result=FALSE)
})

#Change two character entries in absence to numeric
thesis$absences[thesis$absences == "3- Mostly tardy"] <- 3
thesis$absences[thesis$absences == "I1"] <- 1

#Convert absences to numeric vector
thesis$absences <- as.numeric(thesis$absences)

#Standardize teacher codes
#Capitalize all lowercase letter codes and set as factor
thesis$classID <- toupper(x = thesis$classID) %>% factor()

##TEACHER DEMOGRAPHICS

#Import raw teacher demographic file
#NEED TO UPDATE FILE LOCATION DEPENDING ON WHERE IT IS!
teacherDemographics_raw <- read.csv("~/Desktop/Thesis/Data Analyses and Dcumentation/SWTRS_ Teacher Demographics (Responses) - Form Responses 1.csv", 
                                    na.strings = c(""," ","NA"),
                                    header = TRUE,   
                                    stringsAsFactors=FALSE)

#Copy to new data fame to be cleaned
teacherDemographics <- teacherDemographics_raw

#Remove row that was prematurely submitted by teacher before completing
teacherDemographics <- teacherDemographics[-13,]

#Recode Highest.Degree.Earned
teacherDemographics$Highest.Degree.Earned[teacherDemographics$Highest.Degree.Earned == "Masters" | 
                                              teacherDemographics$Highest.Degree.Earned == "Master of Arts in Teaching" | 
                                              teacherDemographics$Highest.Degree.Earned == "Masters " |
                                              teacherDemographics$Highest.Degree.Earned == "Graduate "] <- "Graduate"
teacherDemographics$Highest.Degree.Earned[teacherDemographics$Highest.Degree.Earned == "Bachelor " | 
                                              teacherDemographics$Highest.Degree.Earned == "Bachelors " |
                                              teacherDemographics$Highest.Degree.Earned == "B.A."] <- "Bachelors"

#Set as factor
teacherDemographics$Enter.Unique.Letter.Code  <- as.factor(teacherDemographics$Enter.Unique.Letter.Code)
teacherDemographics$Grade.You.Teach  <- as.factor(teacherDemographics$Grade.You.Teach)
teacherDemographics$Gender  <- as.factor(teacherDemographics$Gender)
teacherDemographics$Highest.Degree.Earned  <- as.factor(teacherDemographics$Highest.Degree.Earned)
teacherDemographics$Race.Ethnicity  <- as.factor(teacherDemographics$Race.Ethnicity)

teacherDemographics <- rename(teacherDemographics, 
                                classID = Enter.Unique.Letter.Code,
                                gradeLevel = Grade.You.Teach,
                                teacher_sex = Gender,
                                teacher_age = Age,
                                teacher_yearsTeaching = Number.of.Years.Teaching,
                                teacher_degree = Highest.Degree.Earned,
                                teacher_race = Race.Ethnicity)
                               

##MERGE DATA SETS: ‘thesis’ AND ‘teacherDemographics’
#Use full_join to merge by class ID
thesis_merged <- full_join(x = thesis, y = teacherDemographics, by = "classID")
