require(dplyr)
require(tidyverse)
require(lme4)
require(purrr)
require(lmerTest)
require(quickpsy)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source("SimulateDataFunction.r")

#Running this entire script can take very long, about ~48 hours

set.seed(4)
Dataframe = c()

#Simple function that computed the RMSE between data points and a cumulative Gaussian function. 
#We use this later to fit a Cumulative Gaussian to the curves recovered by the GLMMs
FitCumGaussian = function(par,Mean,Difference,Prediction){
  (mean((pnorm(Difference,Mean,par)-Prediction)^2))
}

#how many datasets are we simulating to calculate power etc. over?
Repetitions = 100

nParticipants = 20 #how many participants are simulated
ConditionOfInterest = c(0,1) #0 = baseline condition, 1 = test condition
StandardValues = c(5,6,7,8) #strengths of the reference stimulus
reps = 70 #how many trials per staircase
Multiplicator_PSE_Standard = 0 #expected percentage difference between the mean of the psychometric function in the baseline condition and the strength of the reference stimulus
Multiplicator_SD_Standard = 0.15 #expected standard deviation of fitted psychometric functions (Cum Gaussians) in percent of the reference stimulus
Type_ResponseFunction = "Cauchy" #expected distribution that best describes the test stimulus strengths chosen by the staircase
SD_ResponseFunction = 0.1 #standard deviation of this function (if Cum Gaussian) or scale (if Cauchy)
Mean_Variability_Between = 0.2 #expected variability in PSE between participants as fraction of the stengths of the reference stimulus
SD_Variability_Between = 0.2 #expected variability in JNDs between participants  as fraction of the stengths of the reference stimulus


ID = paste0("s",1:nParticipants) #vector with participant IDs

for (j in 1:6){
  
  #we simulate datasets for different combinations of differneces in PSEs and JNDs between baseline and test condition  
  if (j == 1){
    PSE_Difference = -0.0125
    JND_Difference = 0.08
  }
  
  else if (j == 2){
    PSE_Difference = 0
    JND_Difference = 0.08
  }
  
  else if (j == 3){
    PSE_Difference = 0.0125
    JND_Difference = 0
  }
  
  else if (j == 4){
    PSE_Difference = -0.0125
    JND_Difference = -0.08
  }
  
  else if (j == 5){
    PSE_Difference = 0.0125
    JND_Difference = -0.08
  }
  
  else if (j == 6){
    PSE_Difference = 0.0125
    JND_Difference = 0.08
  }
  
  
  
  for (i in 1:Repetitions){
    
    print(i)
    print(PSE_Difference)
    print(JND_Difference)
    
    Beginning = Sys.time()
    
    #simulate the psychometric data (see "SimulateDataFunction.r")
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

    ##########fit psychometric functions and get values    
    Parameters = quickpsy(Psychometric,Difference,Answer,
                          grouping = .(ID,ConditionOfInterest,StandardValues), 
                          bootstrap = "none")$par
    
    Parameters2 = Parameters %>%
      filter(parn == "p1") %>%
      select(ID,ConditionOfInterest,Mean=par, StandardValues)
    Parameters2$SD = Parameters$par[Parameters$parn == "p2"]
    FittedPsychometricFunctions = Parameters2
    ##########/end
    
    Dataframe1 = Psychometric
    
    
    #Fit all models of interest (with different random effect condigurations, inspired my Moscatelli et al. 2012, JoV)
    #Also predict values from fitted parameters and save them in a dataframe for later use
    
    print("Model14")
    Model14 = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + 
                      (1 + ConditionOfInterest| ID) +
                      (1 + Difference |StandardValues), 
                    family = binomial(link = "logit"),
                    data = Dataframe1,
                    nAGQ = 1,
                    glmerControl(optimizer = "nloptwrap"))
    Dataframe1$Prediction_Model14 = predict(Model14, type = "response", newdata = Dataframe1)
    ?glmerControl
    print("Model15")
    Model15 = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + 
                      (1 + Difference + ConditionOfInterest| ID) +
                      (1 + Difference |StandardValues), 
                    family = binomial(link = "logit"),
                    data = Dataframe1,
                    nAGQ = 1,
                    glmerControl(optimizer = "nloptwrap"))
    Dataframe1$Prediction_Model15 = predict(Model15, type = "response", newdata = Dataframe1)
    
    
    print("Model24")
    Model24 = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + 
                      (1 + ConditionOfInterest| ID) +
                      (1 + Difference + ConditionOfInterest|StandardValues), 
                    family = binomial(link = "logit"),
                    data = Dataframe1,
                    nAGQ = 1,
                    glmerControl(optimizer = "nloptwrap"))
    Dataframe1$Prediction_Model24 = predict(Model24, type = "response", newdata = Dataframe1)
    
    print("Model25")
    Model25 = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + 
                      (1 + Difference + ConditionOfInterest| ID) +
                      (1 + Difference + ConditionOfInterest|StandardValues), 
                    family = binomial(link = "logit"),
                    data = Dataframe1,
                    nAGQ = 1,
                    glmerControl(optimizer = "nloptwrap"))
    Dataframe1$Prediction_Model25 = predict(Model25, type = "response", newdata = Dataframe1)
    
    print("Model_LMM_Mean")    
    Model_LMM_Mean = lmer(Mean ~ ConditionOfInterest + (1|ID) + (1|StandardValues),
                          data = FittedPsychometricFunctions)
    summary(Model_LMM_Mean)
    Dataframe1$Prediction_LMM_Mean = predict(Model_LMM_Mean, type = "response",newdata = Dataframe1)
    
    print("Model_LMM_SD")    
    Model_LMM_SD = lmer(SD ~ ConditionOfInterest + (1|ID) + (1|StandardValues),
                        data = FittedPsychometricFunctions)
    summary(Model_LMM_SD)
    Dataframe1$Prediction_LMM_SD = predict(Model_LMM_SD, type = "response",newdata = Dataframe1)

    #get the AIC for each of the fitted GLMMs    
    Dataframe1$AIC14 = summary(Model14)$AICtab[1]
    Dataframe1$AIC15 = summary(Model15)$AICtab[1]
    Dataframe1$AIC24 = summary(Model24)$AICtab[1]
    Dataframe1$AIC25 = summary(Model25)$AICtab[1]
    Dataframe1$AIC_LMM_Mean = summary(Model_LMM_Mean)$AICtab[1]
    Dataframe1$AIC_LMM_SD = summary(Model_LMM_SD)$AIC[1]

    #get the regression coefficient corresponding to the PSEs    
    Dataframe1$CoefCond14 = summary(Model14)$coefficients[2]
    Dataframe1$CoefCond15 = summary(Model15)$coefficients[2]
    Dataframe1$CoefCond24 = summary(Model24)$coefficients[2]
    Dataframe1$CoefCond25 = summary(Model25)$coefficients[2]
    Dataframe1$CoefCond_LMM_Mean = summary(Model_LMM_Mean)$coefficients[2]
  
    #get the regression coefficient corresponding to the JNDs  
    Dataframe1$CoefInteraction14 = summary(Model14)$coefficients[4]
    Dataframe1$CoefInteraction15 = summary(Model15)$coefficients[4]
    Dataframe1$CoefInteraction24 = summary(Model24)$coefficients[4]
    Dataframe1$CoefInteraction25 = summary(Model25)$coefficients[4]
    Dataframe1$CoefInteraction_LMM_SD = summary(Model_LMM_SD)$coefficients[1]

    #get the estimated p value (via lmerTest) for the regression coefficient corresponding to PSEs    
    Dataframe1$PvaluesCoI14 = summary(Model14)$coefficients[14]
    Dataframe1$PvaluesCoI15 = summary(Model15)$coefficients[14]
    Dataframe1$PvaluesCoI24 = summary(Model24)$coefficients[14]
    Dataframe1$PvaluesCoI25 = summary(Model25)$coefficients[14]
    Dataframe1$Pvalues_LMM_Mean = summary(Model_LMM_Mean)$coefficients[10]

    #get the estimated p value (via lmerTest) for the regression coefficient corresponding to JNDs    
    Dataframe1$PvaluesInterac14 = summary(Model14)$coefficients[16]
    Dataframe1$PvaluesInterac15 = summary(Model15)$coefficients[16]
    Dataframe1$PvaluesInterac24 = summary(Model24)$coefficients[16]
    Dataframe1$PvaluesInterac25 = summary(Model25)$coefficients[16]
    Dataframe1$Pvalues_LMM_SD = summary(Model_LMM_SD)$coefficients[10]

    #get the estimated standard error for the regression coefficient corresponding to PSEs    
    Dataframe1$SECoI14 = summary(Model14)$coefficients[6]
    Dataframe1$SECoI15 = summary(Model15)$coefficients[6]
    Dataframe1$SECoI24 = summary(Model24)$coefficients[6]
    Dataframe1$SECoI25 = summary(Model25)$coefficients[6]
    Dataframe1$SECoI_LMM_Mean = summary(Model_LMM_Mean)$coefficients[4]
    
    #get the estimated standard error for the regression coefficient corresponding to JNDs
    Dataframe1$SEInterac14 = summary(Model14)$coefficients[8]
    Dataframe1$SEInterac15 = summary(Model15)$coefficients[8]
    Dataframe1$SEInterac24 = summary(Model24)$coefficients[6]
    Dataframe1$SEInterac25 = summary(Model25)$coefficients[6]
    Dataframe1$SEInterac_LMM_SD = summary(Model_LMM_SD)$coefficients[4]

    #get the PSEs for the responses predicted from the fitted parameters for each model 
    #by recovering the stimulus strength where the predicted responses are closest to 0.5     
    Dataframe1 = Dataframe1 %>%
      group_by(ConditionOfInterest,ID,StandardValues) %>%
      mutate(Mean_Model14 = Difference[which.min(abs(Prediction_Model14-0.5))],
             Mean_Model15 = Difference[which.min(abs(Prediction_Model15-0.5))],
             Mean_Model24 = Difference[which.min(abs(Prediction_Model24-0.5))],
             Mean_Model25 = Difference[which.min(abs(Prediction_Model25-0.5))],
             Mean_Model_LMM = mean(Prediction_LMM_Mean),
             SD_Model_LMM = mean(Prediction_LMM_SD))

    #get the standard deviations of fitted psychometric functions for the responses 
    #predicted from the fitted parameters for each model     
    Dataframe1 = Dataframe1 %>%
      group_by(ConditionOfInterest,ID,StandardValues) %>%
      mutate(SD_Model14 = optimize(FitCumGaussian,c(0,5),Mean = Mean_Model14,Difference = Difference,Prediction = Prediction_Model14)$minimum,
             SD_Model15 = optimize(FitCumGaussian,c(0,5),Mean = Mean_Model15,Difference = Difference,Prediction = Prediction_Model15)$minimum,
             SD_Model24 = optimize(FitCumGaussian,c(0,5),Mean = Mean_Model24,Difference = Difference,Prediction = Prediction_Model24)$minimum,
             SD_Model25 = optimize(FitCumGaussian,c(0,5),Mean = Mean_Model25,Difference = Difference,Prediction = Prediction_Model25)$minimum)

    #save for each simulated dataset... :    
    Dataframe1$Repetition = i #... an index for which simulated dataset it is
    Dataframe1$PSE_Difference = PSE_Difference #...the difference in PSE between baseline and condition of interest
    Dataframe1$SD_Difference = JND_Difference  #...the difference in JND between baseline and condition of interest
    Dataframe1$CombinationStimuli = j #... the combination of PSE and JND (redundancy)
    
    Dataframe = rbind(Dataframe,Dataframe1 %>% 
                        group_by(ConditionOfInterest,ID,StandardValues) %>%
                        slice(1))
    
    print(Sys.time() - Beginning)
  }
}

#Mostly transform the dataframe constructed above from wide to long:
Dataframe2 = data.frame(Model = rep(c("M14","M15","M24","M25","M_LMM"),
                                    each=nParticipants*length(ConditionOfInterest)*length(StandardValues)*Repetitions*6),
                        StandardValues = rep(Dataframe$StandardValues,5),
                        ConditionOfInterest = rep(Dataframe$ConditionOfInterest,5),
                        ID = rep(Dataframe$ID,5),
                        Repetition = rep(Dataframe$Repetition,5),
                        Mean_Modeled = c(Dataframe$Mean_Model14,Dataframe$Mean_Model15,
                                         Dataframe$Mean_Model24,Dataframe$Mean_Model25,
                                         Dataframe$Mean_Model_LMM),
                        SD_Modeled = c(Dataframe$SD_Model14,Dataframe$SD_Model15,
                                       Dataframe$SD_Model24,Dataframe$SD_Model25,
                                       Dataframe$SD_Model_LMM),
                        AIC = c(Dataframe$AIC14,Dataframe$AIC15,Dataframe$AIC24,Dataframe$AIC25,
                                Dataframe$AIC_LMM_Mean),
                        CoefCond = c(Dataframe$CoefCond14,Dataframe$CoefCond15,
                                     Dataframe$CoefCond24,Dataframe$CoefCond25,Dataframe$CoefCond_LMM_Mean),
                        CoefInteract = c(Dataframe$CoefInteraction14,Dataframe$CoefInteraction15,
                                         Dataframe$CoefInteraction24,Dataframe$CoefInteraction25,
                                         Dataframe$CoefInteraction_LMM_SD),
                        PvaluesCoI = c(Dataframe$PvaluesCoI14,Dataframe$PvaluesCoI15,
                                       Dataframe$PvaluesCoI24,Dataframe$PvaluesCoI25,Dataframe$Pvalues_LMM_Mean),
                        PvaluesInterac = c(Dataframe$PvaluesInterac14,Dataframe$PvaluesInterac15,
                                           Dataframe$PvaluesInterac24,Dataframe$PvaluesInterac25,
                                           Dataframe$Pvalues_LMM_SD),
                        SECoI = c(Dataframe$SECoI14,Dataframe$SECoI15,Dataframe$SECoI24,
                                  Dataframe$SECoI25,Dataframe$SECoI_LMM_Mean),
                        SEInterac = c(Dataframe$SEInterac14,Dataframe$SEInterac15,
                                      Dataframe$SEInterac24,Dataframe$SEInterac25,Dataframe$SEInterac_LMM_SD),
                        Mean_Actual = rep(Dataframe$Mean, 5)-rep(Dataframe$StandardValues,5),
                        SD_Actual = rep(Dataframe$SD, 5),
                        PSE_Difference = rep(Dataframe$PSE_Difference, 5),
                        JND_Difference = rep(Dataframe$SD_Difference, 5))
Dataframe2$Condition_PSEJND = paste0(Dataframe2$JND_Difference,Dataframe2$PSE_Difference)

#compute the difference between means and standard deviations fitted through GLMMs 
#and means and standard deviations that were underlying the simulations of these datasets
Dataframe2 = Dataframe2 %>% 
  mutate(ActualPSEs = case_when(
    ConditionOfInterest == 1 ~ -0.1*StandardValues,
    ConditionOfInterest == 0 ~ 0),
    ActualSDs = case_when(
      ConditionOfInterest == 0 ~ 0.15*StandardValues,
      ConditionOfInterest == 1 ~ 1.25*0.15*StandardValues
    ),
    Condition = case_when(
      ConditionOfInterest == 0 ~ "Baseline",
      ConditionOfInterest == 1 ~ "Condition of Interest"
    )) %>% 
  group_by(Condition_PSEJND) %>% 
  #compute difference in AIC between most complete model (M25, see paper) and each other model.
  mutate(AIC_Norm = AIC-median(AIC[Model == "M25"]))

write.csv(Dataframe2,"DifferentConfigurationsLMMGLMM.csv")