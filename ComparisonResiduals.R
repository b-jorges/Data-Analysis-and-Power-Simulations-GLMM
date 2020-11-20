require(dplyr)
require(tidyverse)
require(lme4)
require(purrr)
require(performance)

set.seed(1)

nParticipants = 10
ID = paste0("S0",1:5)
ConditionOfInterest = c(0,1)
StandardValues = c(5,6,7,8)
reps = 1:100
PSE_Difference = -0.1
JND_Difference = 0.25
Multiplicator_PSE_Standard = 0
Multiplicator_SD_Standard = 0.15
Type_ResponseFunction = "normal"
SD_ResponseFunction = 0.1
Mean_Variability_Between = 0.15
SD_Variability_Between = 0.15

for(i in 1:1){
  
  start_time <- Sys.time()

Dataframe1 = SimulatePsychometricData(nParticipants,
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

Model1 = glm(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference, 
               family = binomial(link = "probit"),
               data = Dataframe1)
Residuals1 = simulateResiduals(Model1)

Model2 = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + 
                 (1| ID), 
               family = binomial(link = "probit"),
               data = Dataframe1)
Residuals2 = simulateResiduals(Model2)

Model3 = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + 
                 (1 + Difference| ID), 
               family = binomial(link = "probit"),
               data = Dataframe1)
Residuals3 = simulateResiduals(Model3)

Model4 = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + 
                 (1 + ConditionOfInterest| ID), 
               family = binomial(link = "probit"),
               data = Dataframe1)
Residuals4 = simulateResiduals(Model4)

Model5 = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + 
                 (1 + Difference + ConditionOfInterest| ID), 
               family = binomial(link = "probit"),
               data = Dataframe1)
Residuals5 = simulateResiduals(Model5)

Model6 = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + 
                 (1|StandardValues), 
               family = binomial(link = "probit"),
               data = Dataframe1)
summary(Model6)
Residuals6 = simulateResiduals(Model6)

Model7 = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + 
                 (1| ID) +
                 (1|StandardValues), 
               family = binomial(link = "probit"),
               data = Dataframe1)
Residuals7 = simulateResiduals(Model7)

Model8 = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + 
                 (1 + Difference| ID) +
                 (1|StandardValues), 
               family = binomial(link = "probit"),
               data = Dataframe1)
Residuals8 = simulateResiduals(Model8)

Model9 = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + 
                 (1 + ConditionOfInterest| ID) +
                 (1|StandardValues), 
               family = binomial(link = "probit"),
               data = Dataframe1)
Residuals9 = simulateResiduals(Model9)

Model10 = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + 
                 (1 + Difference + ConditionOfInterest| ID) +
                 (1|StandardValues), 
               family = binomial(link = "probit"),
               data = Dataframe1)
Residuals10 = simulateResiduals(Model10)

Model11 = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference +
                 (1 + Difference |StandardValues), 
               family = binomial(link = "probit"),
               data = Dataframe1)
Residuals11 = simulateResiduals(Model11)

Model12 = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + 
                 (1| ID) +
                 (1 + Difference |StandardValues), 
               family = binomial(link = "probit"),
               data = Dataframe1)
Residuals12 = simulateResiduals(Model12)

Model13 = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + 
                 (1 + Difference| ID) +
                 (1 + Difference |StandardValues), 
               family = binomial(link = "probit"),
               data = Dataframe1)
Residuals13 = simulateResiduals(Model13)

Model14 = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + 
                 (1 + ConditionOfInterest| ID) +
                 (1 + Difference |StandardValues), 
               family = binomial(link = "probit"),
               data = Dataframe1)
Residuals14 = simulateResiduals(Model14)

Model15 = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + 
                 (1 + Difference + ConditionOfInterest| ID) +
                 (1 + Difference |StandardValues), 
               family = binomial(link = "probit"),
               data = Dataframe1)
Residuals15 = simulateResiduals(Model15)

Model16 = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + 
                 (1 + ConditionOfInterest|StandardValues), 
               family = binomial(link = "probit"),
               data = Dataframe1)
Residuals16 = simulateResiduals(Model16)

Model17 = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + 
                 (1| ID) +
                 (1 + ConditionOfInterest|StandardValues), 
               family = binomial(link = "probit"),
               data = Dataframe1)
Residuals17 = simulateResiduals(Model17)

Model18 = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + 
                 (1 + Difference| ID) +
                 (1 + ConditionOfInterest|StandardValues), 
               family = binomial(link = "probit"),
               data = Dataframe1)
Residuals18 = simulateResiduals(Model18)

Model19 = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + 
                 (1 + ConditionOfInterest| ID) +
                 (1 + ConditionOfInterest|StandardValues), 
               family = binomial(link = "probit"),
               data = Dataframe1)
Residuals19 = simulateResiduals(Model19)

Model20 = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + 
                 (1 + Difference + ConditionOfInterest| ID) +
                 (1 + ConditionOfInterest|StandardValues), 
               family = binomial(link = "probit"),
               data = Dataframe1)
Residuals20 = simulateResiduals(Model20)

Model21 = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference +
                 (1 + Difference + ConditionOfInterest|StandardValues), 
               family = binomial(link = "probit"),
               data = Dataframe1)
Residuals21 = simulateResiduals(Model21)

Model22 = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + 
                 (1| ID) +
                 (1 + Difference + ConditionOfInterest|StandardValues), 
               family = binomial(link = "probit"),
               data = Dataframe1)
Residuals22 = simulateResiduals(Model22)

Model23 = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + 
                 (1 + Difference| ID) +
                 (1 + Difference + ConditionOfInterest|StandardValues), 
               family = binomial(link = "probit"),
               data = Dataframe1)
Residuals23 = simulateResiduals(Model23)

Model24 = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + 
                 (1 + ConditionOfInterest| ID) +
                 (1 + Difference + ConditionOfInterest|StandardValues), 
               family = binomial(link = "probit"),
               data = Dataframe1)
Residuals24 = simulateResiduals(Model24)

Model25 = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + 
                 (1 + Difference + ConditionOfInterest| ID) +
                 (1 + Difference + ConditionOfInterest|StandardValues), 
               family = binomial(link = "probit"),
               data = Dataframe1)
Residuals25 = simulateResiduals(Model25)

Values = data.frame(
           SD_ID_Intercept = c(2,sd(ranef(Model2)$ID[[1]]),sd(ranef(Model3)$ID[[1]]),sd(ranef(Model4)$ID[[1]]),
                               sd(ranef(Model5)$ID[[1]]),2,sd(ranef(Model7)$ID[[1]]),sd(ranef(Model8)$ID[[1]]),
                               sd(ranef(Model9)$ID[[1]]), sd(ranef(Model10)$ID[[1]]),2,sd(ranef(Model12)$ID[[1]]),
                               sd(ranef(Model13)$ID[[1]]),sd(ranef(Model14)$ID[[1]]),sd(ranef(Model15)$ID[[1]]),2,
                               sd(ranef(Model17)$ID[[1]]),sd(ranef(Model18)$ID[[1]]),sd(ranef(Model19)$ID[[1]]),sd(ranef(Model20)$ID[[1]]),
                               2,sd(ranef(Model22)$ID[[1]]),sd(ranef(Model23)$ID[[1]]),sd(ranef(Model24)$ID[[1]]),
                               sd(ranef(Model25)$ID[[1]])),
           SD_ID_Difference = c(2,2,sd(ranef(Model3)$ID[[2]]),2,
                               sd(ranef(Model5)$ID[[2]]),2,2,sd(ranef(Model8)$ID[[2]]),
                               sd(ranef(Model9)$ID[[2]]),sd(ranef(Model10)$ID[[2]]),2,2,
                               sd(ranef(Model13)$ID[[2]]),sd(ranef(Model14)$ID[[2]]),sd(ranef(Model15)$ID[[2]]),2,
                               2,sd(ranef(Model18)$ID[[2]]),sd(ranef(Model19)$ID[[2]]),sd(ranef(Model20)$ID[[2]]),
                               2,2,sd(ranef(Model23)$ID[[2]]),sd(ranef(Model24)$ID[[2]]),
                               sd(ranef(Model25)$ID[[2]])),
           SD_ID_CoI = c(2,2,2,sd(ranef(Model4)$ID[[2]]),
                               sd(ranef(Model5)$ID[[3]]),2,2,2,
                               sd(ranef(Model9)$ID[[2]]),sd(ranef(Model10)$ID[[3]]),2,2,
                               2,sd(ranef(Model14)$ID[[2]]),sd(ranef(Model15)$ID[[3]]),2,
                               2,2,sd(ranef(Model19)$ID[[2]]),sd(ranef(Model20)$ID[[3]]),
                               2,2,2,sd(ranef(Model24)$ID[[2]]),
                               sd(ranef(Model25)$ID[[3]])),
           SD_SV_Intercept = c(2,2,2,2,
                               2,sd(ranef(Model6)$StandardValues[[1]]),sd(ranef(Model7)$StandardValues[[1]]),sd(ranef(Model8)$StandardValues[[1]]),
                               sd(ranef(Model9)$StandardValues[[1]]),sd(ranef(Model10)$StandardValues[[1]]),sd(ranef(Model11)$StandardValues[[1]]),sd(ranef(Model12)$StandardValues[[1]]),
                               sd(ranef(Model13)$StandardValues[[1]]),sd(ranef(Model14)$StandardValues[[1]]),sd(ranef(Model15)$StandardValues[[1]]),sd(ranef(Model16)$StandardValues[[1]]),
                               sd(ranef(Model17)$StandardValues[[1]]),sd(ranef(Model18)$StandardValues[[1]]),sd(ranef(Model19)$StandardValues[[1]]),sd(ranef(Model20)$StandardValues[[1]]),
                               sd(ranef(Model21)$StandardValues[[1]]),sd(ranef(Model22)$StandardValues[[1]]),sd(ranef(Model23)$StandardValues[[1]]),sd(ranef(Model24)$StandardValues[[1]]),
                               sd(ranef(Model25)$StandardValues[[1]])),
           Label = c("0/0",	"1/0",	"Diff/0",	"Cond/0",	
                     "Diff+Cond/0", "0/1",	"1/1",	"Diff/1",	
                     "Cond/1",	"Diff+Cond/1", "0/Diff",	"1/Diff",	
                     "Diff/Diff",	"Cond/Diff","Diff+Cond/Diff","0/Cond",	
                     "1/Cond",	"Diff/Cond",	"Cond/Cond",	"Diff+Cond/Cond",
                     "0/Diff+Cond",	"1/Diff+Cond",	"Diff/Diff+Cond",	"Cond/Diff+Cond",	
                     "Diff+Cond/Diff+Cond"),
           SD_SV_Difference = c(2,2,2,2,
                               2,2,2,2,
                               2,2,sd(ranef(Model11)$StandardValues[[2]]),sd(ranef(Model12)$StandardValues[[2]]),
                               sd(ranef(Model13)$StandardValues[[2]]),sd(ranef(Model14)$StandardValues[[2]]),sd(ranef(Model15)$StandardValues[[2]]),2,
                               2,2,2,2,
                               sd(ranef(Model21)$StandardValues[[2]]),sd(ranef(Model22)$StandardValues[[2]]),sd(ranef(Model23)$StandardValues[[2]]),sd(ranef(Model24)$StandardValues[[2]]),
                               sd(ranef(Model25)$StandardValues[[2]])),
           SD_SV_CoI = c(2,2,2,2,
                               2,2,2,2,
                               2,2,2,2,
                               2,2,2,sd(ranef(Model16)$StandardValues[[2]]),
                               sd(ranef(Model17)$StandardValues[[2]]),sd(ranef(Model18)$StandardValues[[2]]),sd(ranef(Model19)$StandardValues[[2]]),sd(ranef(Model20)$StandardValues[[2]]),
                               sd(ranef(Model21)$StandardValues[[3]]),sd(ranef(Model22)$StandardValues[[3]]),sd(ranef(Model23)$StandardValues[[3]]),sd(ranef(Model24)$StandardValues[[3]]),
                               sd(ranef(Model25)$StandardValues[[3]])),
           Fixed_Effect_CoI = c(coef(Model1)[[2]],summary(Model2)$coefficients[2],summary(Model3)$coefficients[2],summary(Model4)$coefficients[2],
                                summary(Model5)$coefficients[2],summary(Model6)$coefficients[2],summary(Model7)$coefficients[2],summary(Model8)$coefficients[2],
                                summary(Model9)$coefficients[2],summary(Model10)$coefficients[2],summary(Model11)$coefficients[2],summary(Model12)$coefficients[2],
                                summary(Model13)$coefficients[2],summary(Model14)$coefficients[2],summary(Model15)$coefficients[2],summary(Model16)$coefficients[2],
                                summary(Model17)$coefficients[2],summary(Model18)$coefficients[2],summary(Model19)$coefficients[2],summary(Model20)$coefficients[2],
                                summary(Model21)$coefficients[2],summary(Model22)$coefficients[2],summary(Model23)$coefficients[2],summary(Model24)$coefficients[2],
                                summary(Model25)$coefficients[2]),
           Fixed_Effect_Difference = c(coef(Model1)[[3]],summary(Model2)$coefficients[3],summary(Model3)$coefficients[3],summary(Model4)$coefficients[3],
                                       summary(Model5)$coefficients[3],summary(Model6)$coefficients[3],summary(Model7)$coefficients[3],summary(Model8)$coefficients[3],
                                       summary(Model9)$coefficients[3],summary(Model10)$coefficients[3],summary(Model11)$coefficients[3],summary(Model12)$coefficients[3],
                                       summary(Model13)$coefficients[3],summary(Model14)$coefficients[3],summary(Model15)$coefficients[3],summary(Model16)$coefficients[3],
                                       summary(Model17)$coefficients[3],summary(Model18)$coefficients[3],summary(Model19)$coefficients[3],summary(Model20)$coefficients[3],
                                       summary(Model21)$coefficients[3],summary(Model22)$coefficients[3],summary(Model23)$coefficients[3],summary(Model24)$coefficients[3],
                                       summary(Model25)$coefficients[3]),
           Fixed_Effect_Interaction = c(coef(Model1)[[4]],summary(Model2)$coefficients[4],summary(Model3)$coefficients[4],summary(Model4)$coefficients[4],
                                        summary(Model5)$coefficients[4],summary(Model6)$coefficients[4],summary(Model7)$coefficients[4],summary(Model8)$coefficients[4],
                                        summary(Model9)$coefficients[4],summary(Model10)$coefficients[4],summary(Model11)$coefficients[4],summary(Model12)$coefficients[4],
                                        summary(Model13)$coefficients[4],summary(Model14)$coefficients[4],summary(Model15)$coefficients[4],summary(Model16)$coefficients[4],
                                        summary(Model17)$coefficients[4],summary(Model18)$coefficients[4],summary(Model19)$coefficients[4],summary(Model20)$coefficients[4],
                                        summary(Model21)$coefficients[4],summary(Model22)$coefficients[4],summary(Model23)$coefficients[4],summary(Model24)$coefficients[4],
                                        summary(Model25)$coefficients[4]),
           AIC = c(Model1$aic,summary(Model2)$AICtab[1],summary(Model2)$AICtab[1],summary(Model2)$AICtab[1],
                   summary(Model5)$AICtab[1],summary(Model6)$AICtab[1],summary(Model7)$AICtab[1],summary(Model8)$AICtab[1],
                   summary(Model9)$AICtab[1],summary(Model10)$AICtab[1],summary(Model11)$AICtab[1],summary(Model12)$AICtab[1],
                   summary(Model13)$AICtab[1],summary(Model14)$AICtab[1],summary(Model15)$AICtab[1],summary(Model16)$AICtab[1],
                   summary(Model17)$AICtab[1],summary(Model18)$AICtab[1],summary(Model19)$AICtab[1],summary(Model20)$AICtab[1],
                   summary(Model21)$AICtab[1],summary(Model22)$AICtab[1],summary(Model23)$AICtab[1],summary(Model24)$AICtab[1],
                   summary(Model25)$AICtab[1]),
           SE_CoI = c(summary(Model1)$coefficients[6],summary(Model2)$coefficients[6],summary(Model3)$coefficients[6],summary(Model4)$coefficients[6],
                      summary(Model5)$coefficients[6],summary(Model4)$coefficients[6],summary(Model7)$coefficients[6],summary(Model8)$coefficients[6],
                      summary(Model9)$coefficients[6],summary(Model10)$coefficients[6],summary(Model11)$coefficients[6],summary(Model12)$coefficients[6],
                      summary(Model13)$coefficients[6],summary(Model14)$coefficients[6],summary(Model15)$coefficients[6],summary(Model16)$coefficients[6],
                      summary(Model17)$coefficients[6],summary(Model18)$coefficients[6],summary(Model19)$coefficients[6],summary(Model20)$coefficients[6],
                      summary(Model21)$coefficients[6],summary(Model22)$coefficients[6],summary(Model23)$coefficients[6],summary(Model24)$coefficients[6],
                      summary(Model25)$coefficients[6]),
           SE_Difference = c(summary(Model1)$coefficients[7],summary(Model2)$coefficients[7],summary(Model3)$coefficients[7],summary(Model4)$coefficients[7],
                      summary(Model5)$coefficients[7],summary(Model6)$coefficients[7],summary(Model7)$coefficients[7],summary(Model8)$coefficients[7],
                      summary(Model9)$coefficients[7],summary(Model10)$coefficients[7],summary(Model11)$coefficients[7],summary(Model12)$coefficients[7],
                      summary(Model13)$coefficients[7],summary(Model14)$coefficients[7],summary(Model15)$coefficients[7],summary(Model16)$coefficients[7],
                      summary(Model17)$coefficients[7],summary(Model18)$coefficients[7],summary(Model19)$coefficients[7],summary(Model20)$coefficients[7],
                      summary(Model21)$coefficients[7],summary(Model22)$coefficients[7],summary(Model23)$coefficients[7],summary(Model24)$coefficients[7],
                      summary(Model25)$coefficients[7]),
           SE_Interaction = c(summary(Model1)$coefficients[8],summary(Model2)$coefficients[8],summary(Model3)$coefficients[8],summary(Model4)$coefficients[8],
                      summary(Model5)$coefficients[8],summary(Model4)$coefficients[8],summary(Model7)$coefficients[8],summary(Model8)$coefficients[8],
                      summary(Model9)$coefficients[8],summary(Model10)$coefficients[8],summary(Model11)$coefficients[8],summary(Model12)$coefficients[8],
                      summary(Model13)$coefficients[8],summary(Model14)$coefficients[8],summary(Model15)$coefficients[8],summary(Model16)$coefficients[8],
                      summary(Model17)$coefficients[8],summary(Model18)$coefficients[8],summary(Model19)$coefficients[8],summary(Model20)$coefficients[8],
                      summary(Model21)$coefficients[8],summary(Model22)$coefficients[8],summary(Model23)$coefficients[8],summary(Model24)$coefficients[8],
                      summary(Model25)$coefficients[8]),
           Convergence = c(Model1$converged,check_convergence(Model2)[1],check_convergence(Model3)[1],check_convergence(Model4)[1],
                           check_convergence(Model5)[1],check_convergence(Model6)[1],check_convergence(Model7)[1],check_convergence(Model8)[1],
                           check_convergence(Model9)[1],check_convergence(Model10)[1],check_convergence(Model11)[1],check_convergence(Model12)[1],
                           check_convergence(Model13)[1],check_convergence(Model14)[1],check_convergence(Model15)[1],check_convergence(Model16)[1],
                           check_convergence(Model17)[1],check_convergence(Model18)[1],check_convergence(Model19)[1],check_convergence(Model20)[1],
                           check_convergence(Model21)[1],check_convergence(Model22)[1],check_convergence(Model23)[1],check_convergence(Model24)[1],
                           check_convergence(Model25)[1]))


write.csv(Values, paste0("Hello",i,".csv"))

jpeg(paste0("PlotResiduals",i,".jpeg"), w = 2000, h = 2000)
par(mfrow=c(5,5))
plotResiduals(Residuals1, quantreg = T)
plotResiduals(Residuals2, quantreg = T)
plotResiduals(Residuals3, quantreg = T)
plotResiduals(Residuals4, quantreg = T)
plotResiduals(Residuals5, quantreg = T)
plotResiduals(Residuals6, quantreg = T)
plotResiduals(Residuals7, quantreg = T)
plotResiduals(Residuals8, quantreg = T)
plotResiduals(Residuals9, quantreg = T)
plotResiduals(Residuals10, quantreg = T)
plotResiduals(Residuals11, quantreg = T)
plotResiduals(Residuals12, quantreg = T)
plotResiduals(Residuals13, quantreg = T)
plotResiduals(Residuals14, quantreg = T)
plotResiduals(Residuals15, quantreg = T)
plotResiduals(Residuals16, quantreg = T)
plotResiduals(Residuals17, quantreg = T)
plotResiduals(Residuals18, quantreg = T)
plotResiduals(Residuals19, quantreg = T)
plotResiduals(Residuals20, quantreg = T)
plotResiduals(Residuals21, quantreg = T)
plotResiduals(Residuals22, quantreg = T)
plotResiduals(Residuals23, quantreg = T)
plotResiduals(Residuals24, quantreg = T)
plotResiduals(Residuals25, quantreg = T)
dev.off()

Sys.time()-start_time
}
