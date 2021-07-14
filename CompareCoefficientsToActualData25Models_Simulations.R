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
    
    ##########fit psychometric functions and get means and standard deviations
    (Parameters = quickpsy(Psychometric,Difference,Answer,
                           grouping = .(ID,ConditionOfInterest,StandardValues), 
                           bootstrap = "none")$par)
    Parameters2 = Parameters %>%
      filter(parn == "p1") %>%
      select(ID,ConditionOfInterest,Mean=par, StandardValues)
    Parameters2$SD = Parameters$par[Parameters$parn == "p2"]
    FittedPsychometricFunctions = Parameters2
    ##########/end
    
    Dataframe1 = Psychometric
    
    
    #Fit all models of interest (with different random effect condigurations, inspired my Moscatelli et al. 2012, JoV)
    #Also predict values from fitted parameters and save them in a dataframe for later use
    
    print("Model1")
    Model1 = glm(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference, 
                 family = binomial(link = "logit"),
                 data = Dataframe1,
    )
    Dataframe1$Prediction_Model1 = predict(Model1, type = "response", newdata = Dataframe1)
    
    print("Model2")
    Model2 = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + 
                     (1| ID), 
                   family = binomial(link = "logit"),
                   data = Dataframe1,
                   nAGQ = 1,
                   glmerControl(optimizer = "nloptwrap"))
    Dataframe1$Prediction_Model2 = predict(Model2, type = "response", newdata = Dataframe1)
    
    print("Model3")
    Model3 = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + 
                     (1 + Difference| ID), 
                   family = binomial(link = "logit"),
                   data = Dataframe1,
                   nAGQ = 1,
                   glmerControl(optimizer = "nloptwrap"))
    Dataframe1$Prediction_Model3 = predict(Model3, type = "response", newdata = Dataframe1)
    
    print("Model4")
    Model4 = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + 
                     (1 + ConditionOfInterest| ID), 
                   family = binomial(link = "logit"),
                   data = Dataframe1,
                   nAGQ = 1,
                   glmerControl(optimizer = "nloptwrap"))
    Dataframe1$Prediction_Model4 = predict(Model4, type = "response", newdata = Dataframe1)
    
    print("Model5")
    Model5 = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + 
                     (1 + Difference + ConditionOfInterest| ID), 
                   family = binomial(link = "logit"),
                   data = Dataframe1,
                   nAGQ = 1,
                   glmerControl(optimizer = "nloptwrap"))
    Dataframe1$Prediction_Model5 = predict(Model5, type = "response", newdata = Dataframe1)
    
    print("Model6")
    Model6 = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + 
                     (1|StandardValues), 
                   family = binomial(link = "logit"),
                   data = Dataframe1,
                   nAGQ = 1,
                   glmerControl(optimizer = "nloptwrap"))
    Dataframe1$Prediction_Model6 = predict(Model6, type = "response", newdata = Dataframe1)
    
    print("Model7")
    Model7 = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + 
                     (1| ID) +
                     (1|StandardValues),
                   family = binomial(link = "logit"),
                   data = Dataframe1,
                   nAGQ = 1,
                   glmerControl(optimizer = "nloptwrap"))
    Dataframe1$Prediction_Model7 = predict(Model7, type = "response", newdata = Dataframe1)
    
    print("Model8")
    Model8 = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + 
                     (1 + Difference| ID) +
                     (1|StandardValues), 
                   family = binomial(link = "logit"),
                   data = Dataframe1,
                   nAGQ = 1,
                   glmerControl(optimizer = "nloptwrap"))
    Dataframe1$Prediction_Model8 = predict(Model8, type = "response", newdata = Dataframe1)
    
    print("Model9")
    Model9 = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + 
                     (1 + ConditionOfInterest| ID) +
                     (1|StandardValues), 
                   family = binomial(link = "logit"),
                   data = Dataframe1,
                   nAGQ = 1,
                   glmerControl(optimizer = "nloptwrap"))
    Dataframe1$Prediction_Model9 = predict(Model9, type = "response", newdata = Dataframe1)
    
    print("Model10")
    Model10 = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + 
                      (1 + Difference + ConditionOfInterest| ID) +
                      (1|StandardValues), 
                    family = binomial(link = "logit"),
                    data = Dataframe1,
                    nAGQ = 1,
                    glmerControl(optimizer = "nloptwrap"))
    Dataframe1$Prediction_Model10 = predict(Model10, type = "response", newdata = Dataframe1)
    
    print("Model11")
    Model11 = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference +
                      (1 + Difference |StandardValues), 
                    family = binomial(link = "logit"),
                    data = Dataframe1,
                    nAGQ = 1,
                    glmerControl(optimizer = "nloptwrap"))
    Dataframe1$Prediction_Model11 = predict(Model11, type = "response", newdata = Dataframe1)
    
    print("Model12")
    Model12 = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + 
                      (1| ID) +
                      (1 + Difference |StandardValues), 
                    family = binomial(link = "logit"),
                    data = Dataframe1,
                    nAGQ = 1,
                    glmerControl(optimizer = "nloptwrap"))
    Dataframe1$Prediction_Model12 = predict(Model12, type = "response", newdata = Dataframe1)
    
    print("Model13")
    Model13 = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + 
                      (1 + Difference| ID) +
                      (1 + Difference |StandardValues), 
                    family = binomial(link = "logit"),
                    data = Dataframe1,
                    nAGQ = 1,
                    glmerControl(optimizer = "nloptwrap"))
    Dataframe1$Prediction_Model13 = predict(Model13, type = "response", newdata = Dataframe1)
    
    print("Model14")
    Model14 = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + 
                      (1 + ConditionOfInterest| ID) +
                      (1 + Difference |StandardValues), 
                    family = binomial(link = "logit"),
                    data = Dataframe1,
                    nAGQ = 1,
                    glmerControl(optimizer = "nloptwrap"))
    Dataframe1$Prediction_Model14 = predict(Model14, type = "response", newdata = Dataframe1)
    
    print("Model15")
    Model15 = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + 
                      (1 + Difference + ConditionOfInterest| ID) +
                      (1 + Difference |StandardValues), 
                    family = binomial(link = "logit"),
                    data = Dataframe1,
                    nAGQ = 1,
                    glmerControl(optimizer = "nloptwrap"))
    Dataframe1$Prediction_Model15 = predict(Model15, type = "response", newdata = Dataframe1)
    
    print("Model16")
    Model16 = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + 
                      (1 + ConditionOfInterest|StandardValues), 
                    family = binomial(link = "logit"),
                    data = Dataframe1,
                    nAGQ = 1,
                    glmerControl(optimizer = "nloptwrap"))
    Dataframe1$Prediction_Model16 = predict(Model16, type = "response", newdata = Dataframe1)
    
    print("Model17")
    Model17 = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + 
                      (1| ID) +
                      (1 + ConditionOfInterest|StandardValues), 
                    family = binomial(link = "logit"),
                    data = Dataframe1,
                    nAGQ = 1,
                    glmerControl(optimizer = "nloptwrap"))
    Dataframe1$Prediction_Model17 = predict(Model17, type = "response", newdata = Dataframe1)
    
    print("Model18")
    Model18 = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + 
                      (1 + Difference| ID) +
                      (1 + ConditionOfInterest|StandardValues), 
                    family = binomial(link = "logit"),
                    data = Dataframe1,
                    nAGQ = 1,
                    glmerControl(optimizer = "nloptwrap"))
    Dataframe1$Prediction_Model18 = predict(Model18, type = "response", newdata = Dataframe1)
    
    print("Model19")
    Model19 = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + 
                      (1 + ConditionOfInterest| ID) +
                      (1 + ConditionOfInterest|StandardValues), 
                    family = binomial(link = "logit"),
                    data = Dataframe1,
                    nAGQ = 1,
                    glmerControl(optimizer = "nloptwrap"))
    Dataframe1$Prediction_Model19 = predict(Model19, type = "response", newdata = Dataframe1)
    
    print("Model20")
    Model20 = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + 
                      (1 + Difference + ConditionOfInterest| ID) +
                      (1 + ConditionOfInterest|StandardValues), 
                    family = binomial(link = "logit"),
                    data = Dataframe1,
                    nAGQ = 1,
                    glmerControl(optimizer = "nloptwrap"))
    Dataframe1$Prediction_Model20 = predict(Model20, type = "response", newdata = Dataframe1)
    
    print("Model21")
    Model21 = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference +
                      (1 + Difference + ConditionOfInterest|StandardValues), 
                    family = binomial(link = "logit"),
                    data = Dataframe1,
                    nAGQ = 1,
                    glmerControl(optimizer = "nloptwrap"))
    Dataframe1$Prediction_Model21 = predict(Model21, type = "response", newdata = Dataframe1)
    
    print("Model22")
    Model22 = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + 
                      (1| ID) +
                      (1 + Difference + ConditionOfInterest|StandardValues), 
                    family = binomial(link = "logit"),
                    data = Dataframe1,
                    nAGQ = 1,
                    glmerControl(optimizer = "nloptwrap"))
    Dataframe1$Prediction_Model22 = predict(Model22, type = "response", newdata = Dataframe1)
    
    print("Model23")
    Model23 = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + 
                      (1 + Difference| ID) +
                      (1 + Difference + ConditionOfInterest|StandardValues), 
                    family = binomial(link = "logit"),
                    data = Dataframe1,
                    nAGQ = 1,
                    glmerControl(optimizer = "nloptwrap"))
    Dataframe1$Prediction_Model23 = predict(Model23, type = "response", newdata = Dataframe1)
    
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
    
    
    #get the AIC for each of the fitted GLMMs
    Dataframe1$AIC1 = Model1$aic
    Dataframe1$AIC2 = summary(Model2)$AICtab[1]
    Dataframe1$AIC3 = summary(Model3)$AICtab[1]
    Dataframe1$AIC4 = summary(Model4)$AICtab[1]
    Dataframe1$AIC5 = summary(Model5)$AICtab[1]
    Dataframe1$AIC6 = summary(Model6)$AICtab[1]
    Dataframe1$AIC7 = summary(Model7)$AICtab[1]
    Dataframe1$AIC8 = summary(Model8)$AICtab[1]
    Dataframe1$AIC9 = summary(Model9)$AICtab[1]
    Dataframe1$AIC10 = summary(Model10)$AICtab[1]
    Dataframe1$AIC11 = summary(Model11)$AICtab[1]
    Dataframe1$AIC12 = summary(Model12)$AICtab[1]
    Dataframe1$AIC13 = summary(Model13)$AICtab[1]
    Dataframe1$AIC14 = summary(Model14)$AICtab[1]
    Dataframe1$AIC15 = summary(Model15)$AICtab[1]
    Dataframe1$AIC16 = summary(Model16)$AICtab[1]
    Dataframe1$AIC17 = summary(Model17)$AICtab[1]
    Dataframe1$AIC18 = summary(Model18)$AICtab[1]
    Dataframe1$AIC19 = summary(Model19)$AICtab[1]
    Dataframe1$AIC20 = summary(Model20)$AICtab[1]
    Dataframe1$AIC21 = summary(Model21)$AICtab[1]
    Dataframe1$AIC22 = summary(Model22)$AICtab[1]
    Dataframe1$AIC23 = summary(Model23)$AICtab[1]
    Dataframe1$AIC24 = summary(Model24)$AICtab[1]
    Dataframe1$AIC25 = summary(Model25)$AICtab[1]
    
    #get the regression coefficient corresponding to the PSEs
    Dataframe1$CoefCond1 =   summary(Model1)$coefficients[2]
    Dataframe1$CoefCond2 = summary(Model2)$coefficients[2]
    Dataframe1$CoefCond3 = summary(Model3)$coefficients[2]
    Dataframe1$CoefCond4 = summary(Model4)$coefficients[2]
    Dataframe1$CoefCond5 = summary(Model5)$coefficients[2]
    Dataframe1$CoefCond6 = summary(Model6)$coefficients[2]
    Dataframe1$CoefCond7 = summary(Model7)$coefficients[2]
    Dataframe1$CoefCond8 = summary(Model8)$coefficients[2]
    Dataframe1$CoefCond9 = summary(Model9)$coefficients[2]
    Dataframe1$CoefCond10 = summary(Model10)$coefficients[2]
    Dataframe1$CoefCond11 = summary(Model11)$coefficients[2]
    Dataframe1$CoefCond12 = summary(Model12)$coefficients[2]
    Dataframe1$CoefCond13 = summary(Model13)$coefficients[2]
    Dataframe1$CoefCond14 = summary(Model14)$coefficients[2]
    Dataframe1$CoefCond15 = summary(Model15)$coefficients[2]
    Dataframe1$CoefCond16 = summary(Model16)$coefficients[2]
    Dataframe1$CoefCond17 = summary(Model17)$coefficients[2]
    Dataframe1$CoefCond18 = summary(Model18)$coefficients[2]
    Dataframe1$CoefCond19 = summary(Model19)$coefficients[2]
    Dataframe1$CoefCond20 = summary(Model20)$coefficients[2]
    Dataframe1$CoefCond21 = summary(Model21)$coefficients[2]
    Dataframe1$CoefCond22 = summary(Model22)$coefficients[2]
    Dataframe1$CoefCond23 = summary(Model23)$coefficients[2]
    Dataframe1$CoefCond24 = summary(Model24)$coefficients[2]
    Dataframe1$CoefCond25 = summary(Model25)$coefficients[2]
    
    #get the regression coefficient corresponding to the JNDs
    Dataframe1$CoefInteraction1 =   summary(Model1)$coefficients[4]
    Dataframe1$CoefInteraction2 = summary(Model2)$coefficients[4]
    Dataframe1$CoefInteraction3 = summary(Model3)$coefficients[4]
    Dataframe1$CoefInteraction4 = summary(Model4)$coefficients[4]
    Dataframe1$CoefInteraction5 = summary(Model5)$coefficients[4]
    Dataframe1$CoefInteraction6 = summary(Model6)$coefficients[4]
    Dataframe1$CoefInteraction7 = summary(Model7)$coefficients[4]
    Dataframe1$CoefInteraction8 = summary(Model8)$coefficients[4]
    Dataframe1$CoefInteraction9 = summary(Model9)$coefficients[4]
    Dataframe1$CoefInteraction10 = summary(Model10)$coefficients[4]
    Dataframe1$CoefInteraction11 = summary(Model11)$coefficients[4]
    Dataframe1$CoefInteraction12 = summary(Model12)$coefficients[4]
    Dataframe1$CoefInteraction13 = summary(Model13)$coefficients[4]
    Dataframe1$CoefInteraction14 = summary(Model14)$coefficients[4]
    Dataframe1$CoefInteraction15 = summary(Model15)$coefficients[4]
    Dataframe1$CoefInteraction16 = summary(Model16)$coefficients[4]
    Dataframe1$CoefInteraction17 = summary(Model17)$coefficients[4]
    Dataframe1$CoefInteraction18 = summary(Model18)$coefficients[4]
    Dataframe1$CoefInteraction19 = summary(Model19)$coefficients[4]
    Dataframe1$CoefInteraction20 = summary(Model20)$coefficients[4]
    Dataframe1$CoefInteraction21 = summary(Model21)$coefficients[4]
    Dataframe1$CoefInteraction22 = summary(Model22)$coefficients[4]
    Dataframe1$CoefInteraction23 = summary(Model23)$coefficients[4]
    Dataframe1$CoefInteraction24 = summary(Model24)$coefficients[4]
    Dataframe1$CoefInteraction25 = summary(Model25)$coefficients[4]
    
    #get the estimated p value (via lmerTest) for the regression coefficient corresponding to PSEs
    Dataframe1$PvaluesCoI1 =   summary(Model1)$coefficients[14]
    Dataframe1$PvaluesCoI2 = summary(Model2)$coefficients[14]
    Dataframe1$PvaluesCoI3 = summary(Model3)$coefficients[14]
    Dataframe1$PvaluesCoI4 = summary(Model4)$coefficients[14]
    Dataframe1$PvaluesCoI5 = summary(Model5)$coefficients[14]
    Dataframe1$PvaluesCoI6 = summary(Model6)$coefficients[14]
    Dataframe1$PvaluesCoI7 = summary(Model7)$coefficients[14]
    Dataframe1$PvaluesCoI8 = summary(Model8)$coefficients[14]
    Dataframe1$PvaluesCoI9 = summary(Model9)$coefficients[14]
    Dataframe1$PvaluesCoI10 = summary(Model10)$coefficients[14]
    Dataframe1$PvaluesCoI11 = summary(Model11)$coefficients[14]
    Dataframe1$PvaluesCoI12 = summary(Model12)$coefficients[14]
    Dataframe1$PvaluesCoI13 = summary(Model13)$coefficients[14]
    Dataframe1$PvaluesCoI14 = summary(Model14)$coefficients[14]
    Dataframe1$PvaluesCoI15 = summary(Model15)$coefficients[14]
    Dataframe1$PvaluesCoI16 = summary(Model16)$coefficients[14]
    Dataframe1$PvaluesCoI17 = summary(Model17)$coefficients[14]
    Dataframe1$PvaluesCoI18 = summary(Model18)$coefficients[14]
    Dataframe1$PvaluesCoI19 = summary(Model19)$coefficients[14]
    Dataframe1$PvaluesCoI20 = summary(Model20)$coefficients[14]
    Dataframe1$PvaluesCoI21 = summary(Model21)$coefficients[14]
    Dataframe1$PvaluesCoI22 = summary(Model22)$coefficients[14]
    Dataframe1$PvaluesCoI23 = summary(Model23)$coefficients[14]
    Dataframe1$PvaluesCoI24 = summary(Model24)$coefficients[14]
    Dataframe1$PvaluesCoI25 = summary(Model25)$coefficients[14]
    
    #get the estimated p value (via lmerTest) for the regression coefficient corresponding to JNDs
    Dataframe1$PvaluesInterac1 =   summary(Model1)$coefficients[16]
    Dataframe1$PvaluesInterac2 = summary(Model2)$coefficients[16]
    Dataframe1$PvaluesInterac3 = summary(Model3)$coefficients[16]
    Dataframe1$PvaluesInterac4 = summary(Model4)$coefficients[16]
    Dataframe1$PvaluesInterac5 = summary(Model5)$coefficients[16]
    Dataframe1$PvaluesInterac6 = summary(Model6)$coefficients[16]
    Dataframe1$PvaluesInterac7 = summary(Model7)$coefficients[16]
    Dataframe1$PvaluesInterac8 = summary(Model8)$coefficients[16]
    Dataframe1$PvaluesInterac9 = summary(Model9)$coefficients[16]
    Dataframe1$PvaluesInterac10 = summary(Model10)$coefficients[16]
    Dataframe1$PvaluesInterac11 = summary(Model11)$coefficients[16]
    Dataframe1$PvaluesInterac12 = summary(Model12)$coefficients[16]
    Dataframe1$PvaluesInterac13 = summary(Model13)$coefficients[16]
    Dataframe1$PvaluesInterac14 = summary(Model14)$coefficients[16]
    Dataframe1$PvaluesInterac15 = summary(Model15)$coefficients[16]
    Dataframe1$PvaluesInterac16 = summary(Model16)$coefficients[16]
    Dataframe1$PvaluesInterac17 = summary(Model17)$coefficients[16]
    Dataframe1$PvaluesInterac18 = summary(Model18)$coefficients[16]
    Dataframe1$PvaluesInterac19 = summary(Model19)$coefficients[16]
    Dataframe1$PvaluesInterac20 = summary(Model20)$coefficients[16]
    Dataframe1$PvaluesInterac21 = summary(Model21)$coefficients[16]
    Dataframe1$PvaluesInterac22 = summary(Model22)$coefficients[16]
    Dataframe1$PvaluesInterac23 = summary(Model23)$coefficients[16]
    Dataframe1$PvaluesInterac24 = summary(Model24)$coefficients[16]
    Dataframe1$PvaluesInterac25 = summary(Model25)$coefficients[16]
    
    #get the estimated standard error for the regression coefficient corresponding to PSEs
    Dataframe1$SECoI1 =   summary(Model1)$coefficients[6]
    Dataframe1$SECoI2 = summary(Model2)$coefficients[6]
    Dataframe1$SECoI3 = summary(Model3)$coefficients[6]
    Dataframe1$SECoI4 = summary(Model4)$coefficients[6]
    Dataframe1$SECoI5 = summary(Model5)$coefficients[6]
    Dataframe1$SECoI6 = summary(Model6)$coefficients[6]
    Dataframe1$SECoI7 = summary(Model7)$coefficients[6]
    Dataframe1$SECoI8 = summary(Model8)$coefficients[6]
    Dataframe1$SECoI9 = summary(Model9)$coefficients[6]
    Dataframe1$SECoI10 = summary(Model10)$coefficients[6]
    Dataframe1$SECoI11 = summary(Model11)$coefficients[6]
    Dataframe1$SECoI12 = summary(Model12)$coefficients[6]
    Dataframe1$SECoI13 = summary(Model13)$coefficients[6]
    Dataframe1$SECoI14 = summary(Model14)$coefficients[6]
    Dataframe1$SECoI15 = summary(Model15)$coefficients[6]
    Dataframe1$SECoI16 = summary(Model16)$coefficients[6]
    Dataframe1$SECoI17 = summary(Model17)$coefficients[6]
    Dataframe1$SECoI18 = summary(Model18)$coefficients[6]
    Dataframe1$SECoI19 = summary(Model19)$coefficients[6]
    Dataframe1$SECoI20 = summary(Model20)$coefficients[6]
    Dataframe1$SECoI21 = summary(Model21)$coefficients[6]
    Dataframe1$SECoI22 = summary(Model22)$coefficients[6]
    Dataframe1$SECoI23 = summary(Model23)$coefficients[6]
    Dataframe1$SECoI24 = summary(Model24)$coefficients[6]
    Dataframe1$SECoI25 = summary(Model25)$coefficients[6]
    
    #get the estimated standard error for the regression coefficient corresponding to JNDs
    Dataframe1$SEInterac1 =   summary(Model1)$coefficients[8]
    Dataframe1$SEInterac2 = summary(Model2)$coefficients[8]
    Dataframe1$SEInterac3 = summary(Model3)$coefficients[8]
    Dataframe1$SEInterac4 = summary(Model4)$coefficients[8]
    Dataframe1$SEInterac5 = summary(Model5)$coefficients[8]
    Dataframe1$SEInterac6 = summary(Model6)$coefficients[8]
    Dataframe1$SEInterac7 = summary(Model7)$coefficients[8]
    Dataframe1$SEInterac8 = summary(Model8)$coefficients[8]
    Dataframe1$SEInterac9 = summary(Model9)$coefficients[8]
    Dataframe1$SEInterac10 = summary(Model10)$coefficients[8]
    Dataframe1$SEInterac11 = summary(Model11)$coefficients[8]
    Dataframe1$SEInterac12 = summary(Model12)$coefficients[8]
    Dataframe1$SEInterac13 = summary(Model13)$coefficients[8]
    Dataframe1$SEInterac14 = summary(Model14)$coefficients[8]
    Dataframe1$SEInterac15 = summary(Model15)$coefficients[8]
    Dataframe1$SEInterac16 = summary(Model16)$coefficients[6]
    Dataframe1$SEInterac17 = summary(Model17)$coefficients[6]
    Dataframe1$SEInterac18 = summary(Model18)$coefficients[6]
    Dataframe1$SEInterac19 = summary(Model19)$coefficients[6]
    Dataframe1$SEInterac20 = summary(Model20)$coefficients[6]
    Dataframe1$SEInterac21 = summary(Model21)$coefficients[6]
    Dataframe1$SEInterac22 = summary(Model22)$coefficients[6]
    Dataframe1$SEInterac23 = summary(Model23)$coefficients[6]
    Dataframe1$SEInterac24 = summary(Model24)$coefficients[6]
    Dataframe1$SEInterac25 = summary(Model25)$coefficients[6]
    
    #get the PSEs for the responses predicted from the fitted parameters for each model 
    #by recovering the stimulus strength where the predicted responses are closest to 0.5 
    Dataframe1 = Dataframe1 %>%
      group_by(ConditionOfInterest,ID,StandardValues) %>%
      mutate(Mean_Model1 = Difference[which.min(abs(Prediction_Model1-0.5))],
             Mean_Model2 = Difference[which.min(abs(Prediction_Model2-0.5))],
             Mean_Model3 = Difference[which.min(abs(Prediction_Model3-0.5))],
             Mean_Model4 = Difference[which.min(abs(Prediction_Model4-0.5))],
             Mean_Model5 = Difference[which.min(abs(Prediction_Model5-0.5))],
             Mean_Model6 = Difference[which.min(abs(Prediction_Model6-0.5))],
             Mean_Model7 = Difference[which.min(abs(Prediction_Model7-0.5))],
             Mean_Model8 = Difference[which.min(abs(Prediction_Model8-0.5))],
             Mean_Model9 = Difference[which.min(abs(Prediction_Model9-0.5))],
             Mean_Model10 = Difference[which.min(abs(Prediction_Model10-0.5))],
             Mean_Model11 = Difference[which.min(abs(Prediction_Model11-0.5))],
             Mean_Model12 = Difference[which.min(abs(Prediction_Model12-0.5))],
             Mean_Model13 = Difference[which.min(abs(Prediction_Model13-0.5))],
             Mean_Model14 = Difference[which.min(abs(Prediction_Model14-0.5))],
             Mean_Model15 = Difference[which.min(abs(Prediction_Model15-0.5))],
             Mean_Model16 = Difference[which.min(abs(Prediction_Model16-0.5))],
             Mean_Model17 = Difference[which.min(abs(Prediction_Model17-0.5))],
             Mean_Model18 = Difference[which.min(abs(Prediction_Model18-0.5))],
             Mean_Model19 = Difference[which.min(abs(Prediction_Model19-0.5))],
             Mean_Model20 = Difference[which.min(abs(Prediction_Model20-0.5))],
             Mean_Model21 = Difference[which.min(abs(Prediction_Model21-0.5))],
             Mean_Model22 = Difference[which.min(abs(Prediction_Model22-0.5))],
             Mean_Model23 = Difference[which.min(abs(Prediction_Model23-0.5))],
             Mean_Model24 = Difference[which.min(abs(Prediction_Model24-0.5))],
             Mean_Model25 = Difference[which.min(abs(Prediction_Model25-0.5))])
    
    #get the standard deviations of fitted psychometric functions for the responses 
    #predicted from the fitted parameters for each model 
    Dataframe1 = Dataframe1 %>%
      group_by(ConditionOfInterest,ID,StandardValues) %>%
      mutate(SD_Model1 = optimize(FitCumGaussian,c(0,5),Mean = Mean_Model1,Difference = Difference,Prediction = Prediction_Model1)$minimum,
             SD_Model2 = optimize(FitCumGaussian,c(0,5),Mean = Mean_Model2,Difference = Difference,Prediction = Prediction_Model2)$minimum,
             SD_Model3 = optimize(FitCumGaussian,c(0,5),Mean = Mean_Model3,Difference = Difference,Prediction = Prediction_Model3)$minimum,
             SD_Model4 = optimize(FitCumGaussian,c(0,5),Mean = Mean_Model4,Difference = Difference,Prediction = Prediction_Model4)$minimum,
             SD_Model5 = optimize(FitCumGaussian,c(0,5),Mean = Mean_Model5,Difference = Difference,Prediction = Prediction_Model5)$minimum,
             SD_Model6 = optimize(FitCumGaussian,c(0,5),Mean = Mean_Model6,Difference = Difference,Prediction = Prediction_Model6)$minimum,
             SD_Model7 = optimize(FitCumGaussian,c(0,5),Mean = Mean_Model7,Difference = Difference,Prediction = Prediction_Model7)$minimum,
             SD_Model8 = optimize(FitCumGaussian,c(0,5),Mean = Mean_Model8,Difference = Difference,Prediction = Prediction_Model8)$minimum,
             SD_Model9 = optimize(FitCumGaussian,c(0,5),Mean = Mean_Model9,Difference = Difference,Prediction = Prediction_Model9)$minimum,
             SD_Model10 = optimize(FitCumGaussian,c(0,5),Mean = Mean_Model10,Difference = Difference,Prediction = Prediction_Model10)$minimum,
             SD_Model11 = optimize(FitCumGaussian,c(0,5),Mean = Mean_Model11,Difference = Difference,Prediction = Prediction_Model11)$minimum,
             SD_Model12 = optimize(FitCumGaussian,c(0,5),Mean = Mean_Model12,Difference = Difference,Prediction = Prediction_Model12)$minimum,
             SD_Model13 = optimize(FitCumGaussian,c(0,5),Mean = Mean_Model13,Difference = Difference,Prediction = Prediction_Model13)$minimum,
             SD_Model14 = optimize(FitCumGaussian,c(0,5),Mean = Mean_Model14,Difference = Difference,Prediction = Prediction_Model14)$minimum,
             SD_Model15 = optimize(FitCumGaussian,c(0,5),Mean = Mean_Model15,Difference = Difference,Prediction = Prediction_Model15)$minimum,
             SD_Model16 = optimize(FitCumGaussian,c(0,5),Mean = Mean_Model16,Difference = Difference,Prediction = Prediction_Model16)$minimum,
             SD_Model17 = optimize(FitCumGaussian,c(0,5),Mean = Mean_Model17,Difference = Difference,Prediction = Prediction_Model17)$minimum,
             SD_Model18 = optimize(FitCumGaussian,c(0,5),Mean = Mean_Model18,Difference = Difference,Prediction = Prediction_Model18)$minimum,
             SD_Model19 = optimize(FitCumGaussian,c(0,5),Mean = Mean_Model19,Difference = Difference,Prediction = Prediction_Model19)$minimum,
             SD_Model20 = optimize(FitCumGaussian,c(0,5),Mean = Mean_Model20,Difference = Difference,Prediction = Prediction_Model20)$minimum,
             SD_Model21 = optimize(FitCumGaussian,c(0,5),Mean = Mean_Model21,Difference = Difference,Prediction = Prediction_Model21)$minimum,
             SD_Model22 = optimize(FitCumGaussian,c(0,5),Mean = Mean_Model22,Difference = Difference,Prediction = Prediction_Model22)$minimum,
             SD_Model23 = optimize(FitCumGaussian,c(0,5),Mean = Mean_Model23,Difference = Difference,Prediction = Prediction_Model23)$minimum,
             SD_Model24 = optimize(FitCumGaussian,c(0,5),Mean = Mean_Model24,Difference = Difference,Prediction = Prediction_Model24)$minimum,
             SD_Model25 = optimize(FitCumGaussian,c(0,5),Mean = Mean_Model25,Difference = Difference,Prediction = Prediction_Model25)$minimum)
    
    #save for each simulated dataset... :
    Dataframe1$Repetition = i #... an index for which simulated dataset it is
    Dataframe1$PSE_Difference = PSE_Difference #...the difference in PSE between baseline and condition of interest
    Dataframe1$SD_Difference = JND_Difference  #...the difference in JND between baseline and condition of interest
    Dataframe1$CombinationStimuli = j #... the combination of PSE and JND (redundancy)
    
    #... save one line per baseline/condition of interset, participant and strength of reference stimulus
    Dataframe = rbind(Dataframe,Dataframe1 %>% 
                        group_by(ConditionOfInterest,ID,StandardValues) %>%
                        slice(1))
    
    print(Sys.time() - Beginning)
  }
}

#Mostly transform the dataframe constructed above from wide to long:
Dataframe2 = data.frame(Model = rep(c("M01", "M02", "M03", "M04", "M05", "M06", "M07", 
                                      "M08", "M09", "M10", "M11", "M12", "M13", "M14",
                                      "M15", "M16", "M17", "M18", "M19", "M20", "M21",
                                      "M22", "M23", "M24", "M25"),
                                    each=nParticipants*length(ConditionOfInterest)*length(StandardValues)*Repetitions*6),
                        StandardValues = rep(Dataframe$StandardValues,25),
                        ConditionOfInterest = rep(Dataframe$ConditionOfInterest,25),
                        ID = rep(Dataframe$ID,25),
                        Repetition = rep(Dataframe$Repetition,25),
                        Mean_Modeled = c(Dataframe$Mean_Model1,Dataframe$Mean_Model2,Dataframe$Mean_Model3,Dataframe$Mean_Model4,
                                         Dataframe$Mean_Model5,Dataframe$Mean_Model6,Dataframe$Mean_Model7,Dataframe$Mean_Model8,
                                         Dataframe$Mean_Model9,Dataframe$Mean_Model10,Dataframe$Mean_Model11,Dataframe$Mean_Model12,
                                         Dataframe$Mean_Model13,Dataframe$Mean_Model14,Dataframe$Mean_Model15,Dataframe$Mean_Model16,
                                         Dataframe$Mean_Model17,Dataframe$Mean_Model18,Dataframe$Mean_Model19,Dataframe$Mean_Model20,
                                         Dataframe$Mean_Model21,Dataframe$Mean_Model22,Dataframe$Mean_Model23,Dataframe$Mean_Model24,
                                         Dataframe$Mean_Model25),
                        SD_Modeled = c(Dataframe$SD_Model1,Dataframe$SD_Model2,Dataframe$SD_Model3,Dataframe$SD_Model4,
                                       Dataframe$SD_Model5,Dataframe$SD_Model6,Dataframe$SD_Model7,Dataframe$SD_Model8,
                                       Dataframe$SD_Model9,Dataframe$SD_Model10,Dataframe$SD_Model11,Dataframe$SD_Model12,
                                       Dataframe$SD_Model13,Dataframe$SD_Model14,Dataframe$SD_Model15,Dataframe$SD_Model16,
                                       Dataframe$SD_Model17,Dataframe$SD_Model18,Dataframe$SD_Model19,Dataframe$SD_Model20,
                                       Dataframe$SD_Model21,Dataframe$SD_Model22,Dataframe$SD_Model23,Dataframe$SD_Model24,
                                       Dataframe$SD_Model25),
                        AIC = c(Dataframe$AIC1,Dataframe$AIC2,Dataframe$AIC3,Dataframe$AIC4,
                                Dataframe$AIC5,Dataframe$AIC6,Dataframe$AIC7,Dataframe$AIC8,
                                Dataframe$AIC9,Dataframe$AIC10,Dataframe$AIC11,Dataframe$AIC12,
                                Dataframe$AIC13,Dataframe$AIC14,Dataframe$AIC15,Dataframe$AIC16,
                                Dataframe$AIC17,Dataframe$AIC18,Dataframe$AIC19,Dataframe$AIC20,
                                Dataframe$AIC21,Dataframe$AIC22,Dataframe$AIC23,Dataframe$AIC24,
                                Dataframe$AIC25),
                        CoefCond = c(Dataframe$CoefCond1,Dataframe$CoefCond2,Dataframe$CoefCond3,Dataframe$CoefCond4,
                                     Dataframe$CoefCond5,Dataframe$CoefCond6,Dataframe$CoefCond7,Dataframe$CoefCond8,
                                     Dataframe$CoefCond9,Dataframe$CoefCond10,Dataframe$CoefCond11,Dataframe$CoefCond12,
                                     Dataframe$CoefCond13,Dataframe$CoefCond14,Dataframe$CoefCond15,Dataframe$CoefCond16,
                                     Dataframe$CoefCond17,Dataframe$CoefCond18,Dataframe$CoefCond19,Dataframe$CoefCond20,
                                     Dataframe$CoefCond21,Dataframe$CoefCond22,Dataframe$CoefCond23,Dataframe$CoefCond24,
                                     Dataframe$CoefCond25),
                        CoefInteract = c(Dataframe$CoefInteraction1,Dataframe$CoefInteraction2,Dataframe$CoefInteraction3,Dataframe$CoefInteraction4,
                                         Dataframe$CoefInteraction5,Dataframe$CoefInteraction6,Dataframe$CoefInteraction7,Dataframe$CoefInteraction8,
                                         Dataframe$CoefInteraction9,Dataframe$CoefInteraction10,Dataframe$CoefInteraction11,Dataframe$CoefInteraction12,
                                         Dataframe$CoefInteraction13,Dataframe$CoefInteraction14,Dataframe$CoefInteraction15,Dataframe$CoefInteraction16,
                                         Dataframe$CoefInteraction17,Dataframe$CoefInteraction18,Dataframe$CoefInteraction19,Dataframe$CoefInteraction20,
                                         Dataframe$CoefInteraction21,Dataframe$CoefInteraction22,Dataframe$CoefInteraction23,Dataframe$CoefInteraction24,
                                         Dataframe$CoefInteraction25),
                        PvaluesCoI = c(Dataframe$PvaluesCoI1,Dataframe$PvaluesCoI2,Dataframe$PvaluesCoI3,Dataframe$PvaluesCoI4,
                                       Dataframe$PvaluesCoI5,Dataframe$PvaluesCoI6,Dataframe$PvaluesCoI7,Dataframe$PvaluesCoI8,
                                       Dataframe$PvaluesCoI9,Dataframe$PvaluesCoI10,Dataframe$PvaluesCoI11,Dataframe$PvaluesCoI12,
                                       Dataframe$PvaluesCoI13,Dataframe$PvaluesCoI14,Dataframe$PvaluesCoI15,Dataframe$PvaluesCoI16,
                                       Dataframe$PvaluesCoI17,Dataframe$PvaluesCoI18,Dataframe$PvaluesCoI19,Dataframe$PvaluesCoI20,
                                       Dataframe$PvaluesCoI21,Dataframe$PvaluesCoI22,Dataframe$PvaluesCoI23,Dataframe$PvaluesCoI24,
                                       Dataframe$PvaluesCoI25),
                        PvaluesInterac = c(Dataframe$PvaluesInterac1,Dataframe$PvaluesInterac2,Dataframe$PvaluesInterac3,Dataframe$PvaluesInterac4,
                                           Dataframe$PvaluesInterac5,Dataframe$PvaluesInterac6,Dataframe$PvaluesInterac7,Dataframe$PvaluesInterac8,
                                           Dataframe$PvaluesInterac9,Dataframe$PvaluesInterac10,Dataframe$PvaluesInterac11,Dataframe$PvaluesInterac12,
                                           Dataframe$PvaluesInterac13,Dataframe$PvaluesInterac14,Dataframe$PvaluesInterac15,Dataframe$PvaluesInterac16,
                                           Dataframe$PvaluesInterac17,Dataframe$PvaluesInterac18,Dataframe$PvaluesInterac19,Dataframe$PvaluesInterac20,
                                           Dataframe$PvaluesInterac21,Dataframe$PvaluesInterac22,Dataframe$PvaluesInterac23,Dataframe$PvaluesInterac24,
                                           Dataframe$PvaluesInterac25),
                        SECoI = c(Dataframe$SECoI1,Dataframe$SECoI2,Dataframe$SECoI3,Dataframe$SECoI4,
                                  Dataframe$SECoI5,Dataframe$SECoI6,Dataframe$SECoI7,Dataframe$SECoI8,
                                  Dataframe$SECoI9,Dataframe$SECoI10,Dataframe$SECoI11,Dataframe$SECoI12,
                                  Dataframe$SECoI13,Dataframe$SECoI14,Dataframe$SECoI15,Dataframe$SECoI16,
                                  Dataframe$SECoI17,Dataframe$SECoI18,Dataframe$SECoI19,Dataframe$SECoI20,
                                  Dataframe$SECoI21,Dataframe$SECoI22,Dataframe$SECoI23,Dataframe$SECoI24,
                                  Dataframe$SECoI25),
                        SEInterac = c(Dataframe$SEInterac1,Dataframe$SEInterac2,Dataframe$SEInterac3,Dataframe$SEInterac4,
                                      Dataframe$SEInterac5,Dataframe$SEInterac6,Dataframe$SEInterac7,Dataframe$SEInterac8,
                                      Dataframe$SEInterac9,Dataframe$SEInterac10,Dataframe$SEInterac11,Dataframe$SEInterac12,
                                      Dataframe$SEInterac13,Dataframe$SEInterac14,Dataframe$SEInterac15,Dataframe$SEInterac16,
                                      Dataframe$SEInterac17,Dataframe$SEInterac18,Dataframe$SEInterac19,Dataframe$SEInterac20,
                                      Dataframe$SEInterac21,Dataframe$SEInterac22,Dataframe$SEInterac23,Dataframe$SEInterac24,
                                      Dataframe$SEInterac25),
                        Mean_Actual = rep(Dataframe$Mean, 25)-rep(Dataframe$StandardValues,25),
                        SD_Actual = rep(Dataframe$SD, 25),
                        PSE_Difference = rep(Dataframe$PSE_Difference, 25),
                        JND_Difference = rep(Dataframe$SD_Difference, 25))
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
      ConditionOfInterest == 1 ~ ""
    )) %>% 
  group_by(Condition_PSEJND) %>% 
  #compute difference in AIC between most complete model (M25, see paper) and each other model.
  mutate(AIC_Norm = AIC-median(AIC[Model == "M25"]))

write.csv(Dataframe2,"Data/DifferentConfigurations25Models.csv")