require(dplyr)
require(tidyverse)
require(lme4)
require(purrr)
require(lmerTest)
require(quickpsy)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source("SimulateDataFunction.r")
set.seed(4)

SimulateDataframe_Twolevel = function(nParticipants,
                                    ConditionOfInterest,
                                    StandardValues,
                                    reps,
                                    PSE_Difference,
                                    JND_Difference,
                                    Multiplicator_PSE_Standard,
                                    Multiplicator_SD_Standard,
                                    SD_ResponseFunction,
                                    Mean_Variability_Between,
                                    SD_Variability_Between){
  
  Psychometric = SimulatePsychometricData(nParticipants,
                                        ConditionOfInterest,
                                        StandardValues,
                                        reps,
                                        PSE_Difference,
                                        JND_Difference,
                                        Multiplicator_PSE_Standard,
                                        Multiplicator_SD_Standard,
                                        Type_ResponseFunction,
                                        SD_ResponseFunction,
                                        Mean_Variability_Between,
                                        SD_Variability_Between)
  
  (Parameters = quickpsy(Psychometric,Difference,Answer,
                         grouping = .(ID,ConditionOfInterest,StandardValues), 
                         bootstrap = "none")$par)
  Parameters2 = Parameters %>%
    filter(parn == "p1") %>%
    select(ID,ConditionOfInterest,Mean=par, StandardValues)
  Parameters2$SD = Parameters$par[Parameters$parn == "p2"]
  FittedPsychometricFunctions = Parameters2
  
  GLMM = glmer(cbind(Yes, Total - Yes) ~ Difference*ConditionOfInterest + 
                 (1 + Difference + ConditionOfInterest |ID) + 
                 (1 + Difference + ConditionOfInterest |StandardValues),
               family = binomial(link = "logit"),
               data = Psychometric,
               nAGQ = 1,
               glmerControl(optimizer = "nloptwrap"))
  
  TwoLevelMean = lmer(Mean ~ ConditionOfInterest + (1|ID) + (1|StandardValues),
                      data = FittedPsychometricFunctions)
  
  TwoLevelSD = lmer(SD ~ ConditionOfInterest + (1|ID) + (1|StandardValues),
                    data = FittedPsychometricFunctions)
  
  c(summary(GLMM)$coef[15],
    summary(GLMM)$coef[16],
    summary(TwoLevelMean)$coef[10],
    summary(TwoLevelSD)$coef[10])
}

#######Comparison of Two Level approach and GLMM approach
Range_Participants = c(10,12,14,16,18,20)
ConditionOfInterest = c(0,1)
StandardValues = c(5,6,7,8)
Range_reps = c(40,70,100)
Multiplicator_PSE_Standard = 0
Multiplicator_SD_Standard = 0.15
Type_ResponseFunction = "Cauchy"
SD_ResponseFunction = 0.1
Mean_Variability_Between = 0.2
SD_Variability_Between = 0.2

Range_PSE_Difference = c(-0.025,-0.0125,0,0.0125,0.025)
Range_JND_Difference = c(-0.1,-0.05,0,0.05,0.1)

nIterations = 100

TotalNumber = length(Range_reps)*length(Range_PSE_Difference)*length(Range_JND_Difference)*length(Range_Participants)
CurrentRunthrough = 0
rightnow = Sys.time()

for (reps in Range_reps){
  for (PSE_Difference in Range_PSE_Difference){
    for (JND_Difference in Range_JND_Difference){
      for (n in Range_Participants){
        
        TimeStartTrial = Sys.time()
        
        Pvalues_Accuracy = c()
        Pvalues_Precision = c()
        Pvalues_Accuracy_TwoLevel = c()
        Pvalues_Precision_TwoLevel = c()
        Pvalues_Accuracy_ANOVA = c()
        Pvalues_Precision_ANOVA = c()
        
        for (j in 1:nIterations){
        Pvalues = SimulateDataframe_Twolevel(n, 
                                               ConditionOfInterest, 
                                               StandardValues, 
                                               reps, 
                                               PSE_Difference, 
                                               JND_Difference, 
                                               Multiplicator_PSE_Standard, 
                                               Multiplicator_SD_Standard, 
                                               SD_ResponseFunction, 
                                               Mean_Variability_Between, 
                                               SD_Variability_Between)
          
                Pvalues_Accuracy = c(Pvalues_Accuracy, Pvalues[1])
                Pvalues_Precision = c(Pvalues_Precision, Pvalues[2])
                Pvalues_Accuracy_TwoLevel = c(Pvalues_Accuracy_TwoLevel, Pvalues[3])
                Pvalues_Precision_TwoLevel = c(Pvalues_Precision_TwoLevel, Pvalues[4])
              }
              
          CurrentRunthrough = CurrentRunthrough + 1
              
          PowerfulDataframe = data.frame(n=n, 
                                          ConditionsOfInterest=length(ConditionOfInterest), 
                                          StandardValue1=StandardValues[1],
                                          StandardValue2=StandardValues[2], 
                                          reps=reps, 
                                          PSE_Difference=PSE_Difference, 
                                          JND_Difference=JND_Difference, 
                                          Multiplicator_PSE_Standard=Multiplicator_PSE_Standard, 
                                          Multiplicator_SD_Standard=Multiplicator_SD_Standard, 
                                          SD_ResponseFunction=SD_ResponseFunction, 
                                          Mean_Variability_Between=Mean_Variability_Between, 
                                          SD_Variability_Between=SD_Variability_Between, 
                                          power_Accuracy = mean(Pvalues_Accuracy < 0.05),  
                                          power_Precision = mean(Pvalues_Precision < 0.05),
                                          power_Accuracy_Twolevel = mean(Pvalues_Accuracy_TwoLevel < 0.05),  
                                          power_Precision_Twolevel = mean(Pvalues_Precision_TwoLevel < 0.05),
                                          Duration = Sys.time() - TimeStartTrial)
          if (CurrentRunthrough == 0){
              
            write.table(PowerfulDataframe, file = "PowerTwoLevelComparison.csv", sep = ",", row.names = FALSE)} 
          
          if (CurrentRunthrough > 0){
            write.table(PowerfulDataframe, file = "PowerTwoLevelComparison.csv", 
                        sep = ",", append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)}
          
          print(paste0("RUNTHROUGH ", 
                       CurrentRunthrough, 
                       "out of ", 
                       TotalNumber,
                       ": ", 
                       n, 
                       ", reps, ", 
                       PSE_Difference, 
                       ", ", 
                       JND_Difference, 
                       ", " , 
                       mean(Pvalues_Accuracy < 0.05), 
                       ", " , 
                       mean(Pvalues_Precision < 0.05),
                       " ", 
                       Sys.time() - TimeStartTrial, 
                       " END. "))
          
      }
    }
  }
}

OverallDuration = rightnow - Sys.time()
