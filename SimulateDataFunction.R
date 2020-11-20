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
      Mean = (Mean_Standard + (ConditionOfInterest==1)*Mean_Standard*PSE_Difference)*PSE_Factor_ID,
      SD = abs(SD_Standard + (ConditionOfInterest==1)*SD_Standard*JND_Difference)*SD_Factor_ID)

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
           Difference = Presented_TestStimulusStrength - StandardValues,
           AnswerProbability = pnorm(Presented_TestStimulusStrength,Mean,SD),
           ##get binary answers ("Test was stronger" yes/no) from probabilities for each trial
           Answer = as.numeric(rbernoulli(length(AnswerProbability),AnswerProbability))
    )
  
  ###prepare for glmer() - needs sum of YES/Total per stimulus strength and condition
  Psychometric = Psychometric %>%
    filter(abs(staircase_factor-1) < 0.75) %>%
    group_by(ID,ConditionOfInterest,StandardValues,Difference) %>%
    mutate(Yes = sum(Answer==1),
           Total = length(ConditionOfInterest))
  
  Psychometric
}