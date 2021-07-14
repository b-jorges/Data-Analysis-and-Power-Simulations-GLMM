require(purrr)

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
      Mean_Standard = StandardValues+StandardValues*Multiplicator_PSE_Standard, #get the mean of the psychometric function for the baseline condition
      SD_Standard = StandardValues*Multiplicator_SD_Standard, #get the standard deviation of the psychometric function for the baseline condition
      Mean = (Mean_Standard + (ConditionOfInterest==1)*Mean_Standard*PSE_Difference)*PSE_Factor_ID, #same but for condition of interest
      SD = abs(SD_Standard + (ConditionOfInterest==1)*SD_Standard*JND_Difference)*SD_Factor_ID) #same but for condition of interest
  
  if (Type_ResponseFunction == "normal"){
    
    Psychometric = Psychometric %>%
      mutate(
        #draw stimulus strengths from a normal distribution to approximate the values drawn by the a staircase procedure
        #this is standardized to a mean of 1
        staircase_factor = rnorm(length(reps),1,SD_ResponseFunction*(1+ConditionOfInterest*JND_Difference)))     
    
  } else if (Type_ResponseFunction == "Cauchy"){
    
    Psychometric = Psychometric %>%
      mutate(
        #same but values drawn from a Cauchy function
        staircase_factor = rcauchy(length(reps),1,SD_ResponseFunction*(1+ConditionOfInterest*JND_Difference)))
    
  } else{
    
    print("distribution not valid")
  }
  
  
  Psychometric = Psychometric %>%
    mutate(Presented_TestStimulusStrength = Mean*staircase_factor, #which stimulus strengths are shown? transform values from above (standardized to 1) to the stimulus strengths in condition of interest
           Difference = Presented_TestStimulusStrength - StandardValues, #difference in stimulus strength between reference stimulus and test stimulus strength (chosen by staircase) 
           AnswerProbability = pnorm(Presented_TestStimulusStrength,Mean,SD), #choose for each difference how likely the participant is to choose one or the other as more intense
           ##get binary answers ("Test was stronger" yes/no) from probabilities for each trial
           Answer = as.numeric(rbernoulli(length(AnswerProbability),AnswerProbability)) #draw answers based on probability
    )
  
  ###prepare for glmer() - needs sum of YES/Total per stimulus strength and condition
  Psychometric = Psychometric %>%
    filter(abs(staircase_factor-1) < 0.75) %>%
    group_by(ID,ConditionOfInterest,StandardValues,Difference) %>%
    mutate(Yes = sum(Answer==1),
           Total = length(ConditionOfInterest))
  
  Psychometric
}