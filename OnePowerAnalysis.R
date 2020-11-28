source(paste0(dirname(rstudioapi::getSourceEditorContext()$path),
              "/SimulateDataFunction.r"))
require(dplyr)
require(purrr)
require(lme4)
require(ggplot2)
set.seed(65)

RangeNs = c(10,12,14,16,18,20)
RangeRepetitions = c(40,70,100)

ConditionOfInterest = c(0,1)
StandardValues = c(5,6,7,8)
PSE_Difference = 0.025
JND_Difference = 0.25
Multiplicator_PSE_Standard = 0
Multiplicator_SD_Standard = 0.15
Type_ResponseFunction = "normal"
SD_ResponseFunction = 0.1
Mean_Variability_Between = 0.2
SD_Variability_Between = 0.2

nIterations = 200
TimeStartSimulations = Sys.time()

PowerfulDataframe = data.frame()

for (nParticipants in RangeNs){
  for (reps in RangeRepetitions){
    
    TimeStartTrial = Sys.time() #get time at beginning of trial
    
    for(i in 1:nIterations){
      
      print(nParticipants)
      print(reps)
      print(nIterations)
      
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
      
      
      GLMM = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + 
                     (ConditionOfInterest+Difference| ID) + 
                     (Difference| StandardValues), 
                   family = binomial(link = "logit"), 
                   data = Psychometric,
                   nAGQ = 1,
                   glmerControl(optimizer = "nloptwrap"))
      
      PowerfulDataframe = rbind(PowerfulDataframe,
                                c(nParticipants=nParticipants,
                                  reps=reps, 
                                  pvalue_PSE = summary(GLMM)$coefficients[14],
                                  pvalue_JND = summary(GLMM)$coefficients[16],
                                  iteration = i))
    }
    print(paste0("200 iterations took ", round(Sys.time() - TimeStartTrial), " seconds. The power for the current run through (",nParticipants," Participants, ", reps, " Repetitions) is ",mean(PowerfulDataframe$pvalue_PSE[PowerfulDataframe$nParticipants == nParticipants & PowerfulDataframe$reps == reps] < 0.05)))
  }
}

colnames(PowerfulDataframe) = c("nParticipants","reps","pvalue_PSE","pvalue_JND","iteration")

alpha = 0.05

PowerfulDataframe = PowerfulDataframe %>% group_by(nParticipants,reps) %>% 
  mutate(Power_PSE = mean(pvalue_PSE < alpha),
         Power_JND = mean(pvalue_JND < alpha))

PowerfulDataframe2 = (PowerfulDataframe %>% group_by(nParticipants,reps) %>% 
  slice(1))

PowerfulDataframe3 = data.frame(nParticipants = rep(PowerfulDataframe2$nParticipants,2),
                                reps = rep(PowerfulDataframe2$reps,2),
                                Power = c(PowerfulDataframe2$Power_PSE,PowerfulDataframe2$Power_JND),
                                WhichValue = c(rep("PSE",18),rep("JND",18)))


ggplot(PowerfulDataframe3, aes(nParticipants,Power, color = as.factor(reps))) +
  geom_line(size = 1) +
  xlab("Number of Participants") +
  ylab("Power") +
  scale_x_continuous(breaks = c(10,12,14,16,18,20)) +
  scale_color_manual(name = "Repetitions\nper Staircase", 
                     values = c(Red,BlauUB,Yellow)) +
  geom_hline(yintercept = 0.9, linetype=2) +
  geom_hline(yintercept = 0.95, linetype=3) +
  ylim(c(0,1)) +
  facet_wrap(WhichValue~.) +
  theme(legend.position = c(0.4,0.2))
ggsave(paste0(dirname(rstudioapi::getSourceEditorContext()$path),
              "/Figures/(Figure 8) OnePowerAnalysis.jpg"), w = 5, h = 5)
