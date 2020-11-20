###Pull the whole repository
require(dplyr)
require(ggplot2)
require(quickpsy)
set.seed(45)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#see paper for a description of each of these parameters
nParticipants = 5 #number of participants in the study
ID = paste0("S0",1:nParticipants) #make a vector with an ID for each participant
ConditionOfInterest = c(0,1) #just a vector to decide whether a trial is baseline (0) or test condition (1)
StandardValues = c(5,8) #values for the comparison stimulus (e.g., 5 m/s and 8 m/s)
reps = 100 #how many trials does a staircase have?
PSE_Difference = 0.1 #the mean difference in PSEs between baseline and test (0.1 corresponds to 10%)
JND_Difference = 0.25 #the mean difference in PSEs between baseline and test (0.25 corresponds to 25%)
Multiplicator_PSE_Standard = 0 #what is the PSE of the comparison stimulus? when 0, it is just the StandardValue
Multiplicator_SD_Standard = 0.15 #what is the standard deviation of the Cummulative Gaussian for the comparison stimulus, in units of the standard values?
Type_ResponseFunction = "Cauchy" #which response distribution best describes the distribution of responses presented with the staircase?
SD_ResponseFunction = 0.1 #what is the standard deviation (for Type_ResponseFunction = "normal") or the scale (for Type_ResponseFunction = "Cauchy") of this distribution?
Mean_Variability_Between = 0.2 #what is the between-participant variability for PSEs?
SD_Variability_Between = 0.2 #what is the between-participant variability for JNDs?

#make dataframe with one row per trial, participant, condition and standard value
Psychometric = expand.grid(ID=ID, ConditionOfInterest=ConditionOfInterest, StandardValues=StandardValues, reps = 1:reps)

Psychometric = Psychometric %>%
  group_by(ID) %>%#
  mutate(PSE_Factor_ID = rnorm(1,1,Mean_Variability_Between), #how much variability is in the means of the psychometric functions between subjects?
         SD_Factor_ID = rnorm(1,1,SD_Variability_Between)) #how much variability is in the standard deviations of the psychometric functions between subjects?

#setup means and standard deviations of Cummulative Gaussian distributions (used as psychometric functions)
Psychometric = Psychometric %>%
  mutate(
    Mean_Standard = StandardValues+StandardValues*Multiplicator_PSE_Standard,
    SD_Standard = StandardValues*Multiplicator_SD_Standard,
    Mean = (Mean_Standard + (ConditionOfInterest==1)*Mean_Standard*PSE_Difference)*PSE_Factor_ID,
    SD = abs(SD_Standard + (ConditionOfInterest==1)*SD_Standard*JND_Difference)*SD_Factor_ID)

#pick a response function and create multipliers corresponding to the values presented during the staircase
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

#set everything up:
Psychometric = Psychometric %>%
  mutate(#compute presented stimulus strengths based on means of psychometric functions and the staircase factor
    Presented_TestStimulusStrength = Mean*staircase_factor, 
    #compute difference between test and comparison
    Difference = Presented_TestStimulusStrength - StandardValues, 
    #compute how likely a "test is bigger" response is
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

#draw psychometric functions
PsychometricFunctions = quickpsy(Psychometric,Difference,Answer,grouping = .(ConditionOfInterest,ID,StandardValues), bootstrap = "none")
plot(PsychometricFunctions) +
  scale_color_manual(name = "",
                     values = c(Red,BlauUB),
                     labels = c("Control","Experimental")) +
  xlab("Difference between Comparison and Test") +
  ylab("Probability to choose Test") +
  geom_vline(linetype = 2, xintercept = 0, color = "grey") +
  geom_hline(linetype = 2, yintercept = 0.5, color = "grey")
ggsave(paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/Figures/(Figure 3) PsychometricFunctions.jpg"), w = 12, h = 5)