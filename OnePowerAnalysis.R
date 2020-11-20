SimulatePsychometricData = function(nParticipants,
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
                                    SD_Variability_Between){
  
  
  ID = paste0("S0",1:nParticipants)
  
  Psychometric = expand.grid(ID=ID, ConditionOfInterest=ConditionOfInterest, StandardValues=StandardValues, reps = 1:reps)
  
  Psychometric = Psychometric %>%
    group_by(ID) %>%#
    mutate(PSE_Factor_ID = rnorm(1,1,Mean_Variability_Between), #how much variability is in the means of the psychometric functions between subjects?
           SD_Factor_ID = rnorm(1,1,SD_Variability_Between)) #how much variability is in the standard deviations of the psychometric functions between subjects?
  
  Psychometric = Psychometric %>%
    mutate(
      Mean_Standard = StandardValues+StandardValues*Multiplicator_PSE_Standard,
      SD_Standard = StandardValues*Multiplicator_SD_Standard,
      Mean = (Mean_Standard + (ConditionOfInterest==1)*Mean_Standard*PSE_Difference),
      SD = abs(SD_Standard + (ConditionOfInterest==1)*SD_Standard*JND_Difference))
  
  Psychometric = Psychometric %>%
    mutate(
      Mean = Mean*PSE_Factor_ID,
      SD = SD*SD_Factor_ID)
  
  if (Type_ResponseFunction == "normal"){
    
    Psychometric = Psychometric %>%
      mutate(
        staircase_factor = rnorm(length(reps),1,SD_ResponseFunction*(1+ConditionOfInterest*JND_Difference)))
    
  } else if (Type_ResponseFunction == "Cauchy"){
    
    Psychometric = Psychometric %>%
      mutate(
        staircase_factor = rcauchy(length(reps),1,SD_ResponseFunction*(1+ConditionOfInterest*JND_Difference)))
    
  } else{
    
    print("distribution not valid")
    
  }
  
  Psychometric = Psychometric %>%
    mutate(Presented_TestStimulusStrength = Mean*staircase_factor,
           Difference = Presented_TestStimulusStrength - StandardValues)
  
  Psychometric = Psychometric %>%
    mutate(
      AnswerProbability = pnorm(Presented_TestStimulusStrength,Mean,SD),
      
      Answer = as.numeric(rbernoulli(length(AnswerProbability),AnswerProbability))
    )
  
  
  Psychometric = Psychometric %>%
    filter(abs(staircase_factor-1) < 0.75) %>%
    group_by(ID,ConditionOfInterest,StandardValues,Difference) %>%
    mutate(Yes = sum(Answer==1),
           Total = length(ConditionOfInterest))
  
  Psychometric
}

RangeNs = c(10,12,14,16,18,20)
RangeRepetitions = c(40,70,100)

ConditionOfInterest = c(0,1)
StandardValues = c(5,6,7,8)
PSE_Difference = 0.025
JND_Difference = 0.1
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
      
      print(paste0("Number of Participants: ", nParticipants))
      print(paste0("Number of Repetitions per Staircase: ", reps))
      print(paste0("Iteration: ", i))
      
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
      
      
      GLMM = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + (ConditionOfInterest+Difference| ID) + (Difference| StandardValues), 
                   family = binomial(link = "logit"), 
                   data = Psychometric,
                   nAGQ = 1,
                   glmerControl(optimizer = "nloptwrap"))
      
      PowerfulDataframe = rbind(PowerfulDataframe,data.frame(nParticipants = rep(nParticipants,2),
                                                    rep = rep(reps,2), 
                                                    WhichValue = c("PSE", "JND"),
                                                    pvalue = c(summary(GLMM)$coefficients[14],summary(GLMM)$coefficients[16]),
                                                    iteration = rep(i,2)))
    }
    print(paste0("200 iterations took ", round(Sys.time() - TimeStartTrial), " seconds. The power for the current run through (",nParticipants," Participants, ", reps, " Repetitions) is ",mean(PowerfulDataframe$pvalue_PSE[PowerfulDataframe$nParticipants == nParticipants & PowerfulDataframe$reps == reps] < 0.05)))
  }
}

colnames(PowerfulDataframe) = c("nParticipants","reps","WhichValue","pvalue","iteration")


alpha = 0.05

PowerfulDataframe = PowerfulDataframe %>% group_by(nParticipants,reps, WhichValue) %>% 
  mutate(Power = mean(pvalue < alpha))

PowerfulDataframe %>% group_by(nParticipants,reps) %>% 
  slice(1)

DurationSimulations = Sys.time() - TimeStartSimulations

ggplot(PowerfulDataframe, aes(nParticipants,Power, color = as.factor(reps))) +
  geom_line(size = 1) +
  xlab("Number of Participants") +
  ylab("Power") +
  scale_x_continuous(breaks = c(10,12,14,16,18,20)) +
  scale_color_manual(name = "Repetitions\nper Staircase", 
                       values = c(Red,BlauUB,Yellow)) +
  geom_hline(yintercept = 0.8, linetype=1) +
  geom_hline(yintercept = 0.9, linetype=2) +
  geom_hline(yintercept = 0.95, linetype=3) +
  ylim(c(0,1)) +
  facet_wrap(WhichValue~.) +
  theme(legend.position = c(0.4,0.2))
ggsave("Figures/FigureOnePowerAnalysis.jpg",w = 7, h = 7)

