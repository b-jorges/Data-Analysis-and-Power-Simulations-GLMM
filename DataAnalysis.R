set.seed(101)

Psychometric = SimulatePsychometricData(nParticipants = 5,
                                        ConditionOfInterest = c(0,1),
                                        StandardValues = c(5,8),
                                        reps = 100,
                                        PSE_Difference = 0.1,
                                        JND_Difference = 0.25,
                                        Multiplicator_PSE_Standard = 0,
                                        Multiplicator_SD_Standard = 0.15,
                                        Type_ResponseFunction = "Cauchy",
                                        SD_ResponseFunction = 0.1,
                                        Mean_Variability_Between = 0.2,
                                        SD_Variability_Between = 0.2)

###########################################
####Two-Level approach######
###########################################
require(quickpsy)

###Fitting psychometric functions and extracting means and standard deviations
PsychometricFunctions = quickpsy(Psychometric,Difference,Answer,grouping = .(ConditionOfInterest,ID,StandardValues), bootstrap = "none")

Parameters = PsychometricFunctions$par
Parameters2 = Parameters %>%
  filter(parn == "p1") %>%
  select(ID,ConditionOfInterest,Mean=par, StandardValues)
Parameters2$SD = Parameters$par[Parameters$parn == "p2"]
Parameters = Parameters2

###performing ANOVA
###computes pvalues for ANOVA
require(lmerTest)

ANOVA_Mean = lm(Mean ~ as.factor(ConditionOfInterest)*StandardValues,Parameters)
ANOVA_SD = lm(SD ~ as.factor(ConditionOfInterest)*StandardValues,Parameters)
LMM_Mean = lmer(Mean ~ as.factor(ConditionOfInterest)*StandardValues + (1 | ID),
         data = Parameters)
LMM_SD = lmer(SD ~ as.factor(ConditionOfInterest)*StandardValues + (1 | ID),
                data = Parameters)

summary(ANOVA_Mean)
summary(ANOVA_SD)
summary(LMM_Mean)
summary(LMM_SD)


Parameters$ConditionOfInterest[Parameters$ConditionOfInterest == 1] = "Condition of Interest"
Parameters$ConditionOfInterest[Parameters$ConditionOfInterest == 0] = "Baseline"

Plot_LMM_Mean = ggplot(Parameters,aes(StandardValues,Mean/StandardValues,color = ID)) +
  geom_point(size = 4) +
  facet_grid(.~ConditionOfInterest) +
  scale_color_manual(name = "",
                     values = colorRampPalette(c(BlauUB,Yellow, Red))(5)) +
  geom_smooth(method='lm', se = FALSE) +
  xlab("Standard Values (m/s)") +
  ylab("Normalized Mean (m/s)")

Plot_LM_Mean = ggplot(Parameters,aes(StandardValues,Mean/StandardValues)) +
  geom_point(size = 4) +
  facet_grid(.~ConditionOfInterest) +
  geom_smooth(method='lm',color = "black", se = FALSE) +
  xlab("Standard Values (m/s)") +
  ylab("Normalized Mean (m/s)")
plot_grid(Plot_LM_Mean,Plot_LMM_Mean, labels = "AUTO")
ggsave("(Figure 4) Means.jpg", w = 12, h = 5)

LMM_SD = lmer(SD ~ ConditionOfInterest*StandardValues + (1 | ID),
           data = Parameters)
summary(LMM_SD)

LMM_Mean = lmer(Mean ~ ConditionOfInterest + (1 | ID) + (1 | StandardValues),
                data = Parameters)
LMM_SD = lmer(SD ~ ConditionOfInterest + (1 | ID) + (1 | StandardValues),
              data = Parameters)
summary(LMM_Mean)
summary(LMM_SD)


GLMM_RandomIntercepts_JND = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + (1| ID), 
             family = binomial(link = "probit"),
             data = Psychometric)
GLMM2_RandomInterceptsAndSlopes_JND = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + (1 + Difference| ID), 
             family = binomial(link = "probit"),
             data = Psychometric)
summary(GLMM_RandomIntercepts_JND)
summary(GLMM2_RandomInterceptsAndSlopes_JND)
Sim1 = simulateResiduals(GLMM_RandomIntercepts_JND)
Sim2 = simulateResiduals(GLMM2_RandomInterceptsAndSlopes_JND)

GLMM_RandomIntercepts_Null_JND = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest + Difference + (1| ID), 
                                       family = binomial(link = "probit"),
                                       data = Psychometric)
GLMM2_RandomInterceptsAndSlopes_Null_JND = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest + Difference + (1 + Difference| ID), 
                                                 family = binomial(link = "probit"),
                                                 data = Psychometric)
anova(GLMM_RandomIntercepts_JND,GLMM_RandomIntercepts_Null_JND)
anova(GLMM2_RandomInterceptsAndSlopes_JND,GLMM2_RandomInterceptsAndSlopes_Null_JND)

GLMM_RandomIntercepts_PSE = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + (1| ID), 
                                  family = binomial(link = "probit"),
                                  data = Psychometric)
GLMM2_RandomInterceptsAndSlopes_PSE = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest + Difference + (1 + Difference| ID), 
                                            family = binomial(link = "probit"),
                                            data = Psychometric)

GLMM_RandomIntercepts_Null_PSE = glmer(cbind(Yes, Total - Yes) ~ Difference + (1| ID), 
                                       family = binomial(link = "probit"),
                                       data = Psychometric)
GLMM2_RandomInterceptsAndSlopes_Null_PSE = glmer(cbind(Yes, Total - Yes) ~ Difference + (1 + Difference| ID), 
                                                 family = binomial(link = "probit"),
                                                 data = Psychometric)

anova(GLMM_RandomIntercepts_PSE,GLMM_RandomIntercepts_Null_PSE)
anova(GLMM2_RandomInterceptsAndSlopes_PSE,GLMM2_RandomInterceptsAndSlopes_Null_PSE)

####Testing assumptions is hard. The DHARMa package helps a lot with that:
#install.packages(DHARMa)
require(DHARMa)
Sim_Simple = simulateResiduals(GLMM_RandomIntercepts_JND)


GLMM3 = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + 
                                                         (1 + Difference| ID) +
                                                         (1 + Difference| StandardValues), 
                                                       family = binomial(link = "probit"),
                                                       data = Psychometric)
Sim3 = simulateResiduals(GLMM3)

GLMM4 = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + 
                                                       (1 + Difference| ID) +
                                                       (1|StandardValues), 
                                                     family = binomial(link = "probit"),
                                                     data = Psychometric)
Sim4 = simulateResiduals(GLMM4)

GLMM5 = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + 
                                                       (1| ID) +
                                                       (1 + Difference|StandardValues), 
                                                     family = binomial(link = "probit"),
                                                     data = Psychometric)
Sim5 = simulateResiduals(GLMM5)

GLMM6 = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + 
                (1 + ConditionOfInterest| ID) +
                (1|StandardValues), 
              family = binomial(link = "probit"),
              data = Psychometric)
Sim6 = simulateResiduals(GLMM6)

GLMM7 = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + 
                (1| ID) +
                (1 + ConditionOfInterest|StandardValues), 
              family = binomial(link = "probit"),
              data = Psychometric)
Sim7 = simulateResiduals(GLMM7)

GLMM2_ThreeRandomEffectsPerIDAndStandardValues = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + 
                                                   (1 + Difference + ConditionOfInterest| ID) +
                                                   (1 + Difference + ConditionOfInterest| StandardValues), 
                                                 family = binomial(link = "probit"),
                                                 data = Psychometric)
Sim_Complex = simulateResiduals(GLMM2_ThreeRandomEffectsPerIDAndStandardValues)





Panel1_Simple = ggplot(Psychometric, aes(x=factor(StandardValues), y=residuals(Sim_Simple))) +
  geom_jitter(alpha = 0.1, width = 0.1, color = Red) +
  ylab("Std. Residuals") +
  xlab("Standard Values (m/s)")
Panel2_Simple = ggplot(Psychometric, aes(x=factor(ID), y=residuals(Sim_Simple))) + 
  geom_jitter(alpha = 0.1, width = 0.1, color = BlauUB)  +
  ylab("Std. Residuals") +
  xlab("ID")
Panel3_Simple = ggplot(Psychometric, aes(x=factor(ConditionOfInterest), y=residuals(Sim_Simple))) + 
  geom_jitter(alpha = 0.1, width = 0.1, color = Yellow) +
  ylab("Std. Residuals") +
  xlab("Condition Of Interest")
Panel4_Simple = ggplot(Psychometric, aes(x=Difference, y=residuals(Sim_Simple))) + 
  geom_jitter(alpha = 0.1, width = 0.1, color = Turquoise) +
  ylab("Std. Residuals") +
  xlab("Difference (m/s)")


Panel1_Complex = ggplot(Psychometric, aes(x=factor(StandardValues), y=residuals(Sim_Complex))) +
  geom_jitter(alpha = 0.1, width = 0.1, color = Red) +
  ylab("Std. Residuals") +
  xlab("Standard Values (m/s)")
Panel2_Complex = ggplot(Psychometric, aes(x=factor(ID), y=residuals(Sim_Complex))) +
  geom_jitter(alpha = 0.1, width = 0.1, color = BlauUB) +
  ylab("Std. Residuals") +
  xlab("ID")
Panel3_Complex = ggplot(Psychometric, aes(x=factor(ConditionOfInterest), y=residuals(Sim_Complex))) + 
  geom_jitter(alpha = 0.1, width = 0.1, color = Yellow) +
  ylab("Std. Residuals") +
  xlab("Condition Of Interest")
Panel4_Complex = ggplot(Psychometric, aes(x=Difference, y=residuals(Sim_Complex))) + 
  geom_jitter(alpha = 0.1, width = 0.1, color = Turquoise) +
  ylab("Std. Residuals") +
  xlab("Difference (m/s)")

plot_grid(Panel1_Simple,Panel2_Simple,Panel3_Simple,Panel4_Simple,Panel1_Complex,Panel2_Complex,Panel3_Complex,Panel4_Complex, 
          nrow = 2,
          labels = "AUTO")
ggsave("Figure Residuals.jpg", w = 12.5, h = 5)

plotResiduals(Sim_Simple, Psychometric$Difference, quantreg = T)
plotResiduals(Sim1, Psychometric$Difference, quantreg = T)
plotResiduals(Sim2, Psychometric$Difference, quantreg = T)
plotResiduals(Sim3, Psychometric$Difference, quantreg = T)
plotResiduals(Sim_Medium, Psychometric$Difference, quantreg = T)
plotResiduals(Sim4, Psychometric$Difference, quantreg = T)
plotResiduals(Sim5, Psychometric$Difference, quantreg = T)
plotResiduals(Sim6, Psychometric$Difference, quantreg = T)
plotResiduals(Sim7, Psychometric$Difference, quantreg = T)
plotResiduals(Sim_Complex, Psychometric$Difference, quantreg = T)



plotQQunif(Sim_Medium, testOutliers = F, testDispersion = F)
plotQQunif(Sim_Complex, testOutliers = F, testDispersion = F)

GLMM3_RandomInterceptsAndTwoSlopes_JND = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + (Difference+ConditionOfInterest| ID), 
                                            family = binomial(link = "probit"),
                                            data = Psychometric)
summary(GLMM_RandomIntercepts_JND)
summary(GLMM2_RandomInterceptsAndSlopes_JND)
summary(GLMM3_RandomInterceptsAndTwoSlopes_JND)

GLMM_RandomIntercepts_Null_JND = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest + Difference + (1| ID), 
                              family = binomial(link = "probit"),
                              data = Psychometric)
GLMM2_RandomInterceptsAndSlopes_Null_JND = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest + Difference + (Difference| ID), 
                                        family = binomial(link = "probit"),
                                        data = Psychometric)
GLMM3_RandomInterceptsTwoAndSlopes_Null_JND = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest + Difference + (Difference + ConditionOfInterest| ID), 
                                                 family = binomial(link = "probit"),
                                                 data = Psychometric)

anova(GLMM_RandomIntercepts_JND,GLMM_RandomIntercepts_Null_JND)
anova(GLMM2_RandomInterceptsAndSlopes_JND,GLMM2_RandomInterceptsAndSlopes_Null_JND)
anova(GLMM3_RandomInterceptsAndTwoSlopes_JND,GLMM3_RandomInterceptsTwoAndSlopes_Null_JND)





GLMM_RandomIntercepts_PSE = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + (1| ID), 
                              family = binomial(link = "probit"),
                              data = Psychometric)
GLMM2_RandomInterceptsAndSlopes_PSE = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest + Difference + (Difference| ID), 
                                        family = binomial(link = "probit"),
                                        data = Psychometric)
GLMM3_RandomInterceptsTwoAndSlopes_PSE = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest + Difference + (Difference + ConditionOfInterest| ID), 
                                            family = binomial(link = "probit"),
                                            data = Psychometric)

GLMM_RandomIntercepts_Null_PSE = glmer(cbind(Yes, Total - Yes) ~ Difference + (1| ID), 
                                   family = binomial(link = "probit"),
                                   data = Psychometric)
GLMM2_RandomInterceptsAndSlopes_Null_PSE = glmer(cbind(Yes, Total - Yes) ~ Difference + (Difference| ID), 
                                             family = binomial(link = "probit"),
                                             data = Psychometric)
GLMM3_RandomInterceptsAndTwoSlopes_Null_PSE = glmer(cbind(Yes, Total - Yes) ~ Difference + (Difference + ConditionOfInterest| ID), 
                                                 family = binomial(link = "probit"),
                                                 data = Psychometric)

anova(GLMM_RandomIntercepts_PSE,GLMM_RandomIntercepts_Null_PSE)
anova(GLMM2_RandomInterceptsAndSlopes_PSE,GLMM2_RandomInterceptsAndSlopes_Null_PSE)
anova(GLMM3_RandomInterceptsTwoAndSlopes_PSE,GLMM3_RandomInterceptsAndTwoSlopes_Null_PSE)


####Testing assumptions is hard. The DHARMa package helps a lot with that: 
require(DHARMa)


Sim1 = simulateResiduals(GLMM_RandomIntercepts_JND)
plot(Sim1)

Sim2 = simulateResiduals(GLMM2_RandomInterceptsAndSlopes_JND)
plot(Sim2)
plot(recalculateResiduals(Sim2, group = Psychometric$ID))


Sim3 = simulateResiduals(GLMM3_RandomInterceptsAndTwoSlopes_JND)
plot(Sim3)
plot(recalculateResiduals(Sim3, group = Psychometric$ID))

Sim4 = simulateResiduals(GLMM_RandomIntercepts_PSE)
plot(Sim4)
plot(recalculateResiduals(Sim4, group = Psychometric$ID))

Sim5 = simulateResiduals(GLMM2_RandomInterceptsAndSlopes_PSE)
plot(Sim5)

Sim6 = simulateResiduals(GLMM3_RandomInterceptsTwoAndSlopes_PSE)
plot(Sim6)


GLMM4_RandomInterceptsTimesTwo_JND = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + 
                                            (1| ID) + 
                                            (1| StandardValues), 
                                  family = binomial(link = "probit"),
                                  data = Psychometric)
GLMM5_RandomInterceptsAndSlopesTimesTwo_JND = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + 
                                                      (Difference| ID) + 
                                                      (Difference| StandardValues), 
                                            family = binomial(link = "probit"),
                                            data = Psychometric)
GLMM6_RandomInterceptsAndTwoSlopesTimesTwo_JND = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + 
                                                         (Difference+ConditionOfInterest| ID) +
                                                         (Difference+ConditionOfInterest| StandardValues), 
                                               family = binomial(link = "probit"),
                                               data = Psychometric)




Sim7 = simulateResiduals(GLMM4_RandomInterceptsTimesTwo_JND)
plot(Sim7)
testDispersion(Sim7)
testUniformity(Sim7)

Sim8 = simulateResiduals(GLMM5_RandomInterceptsAndSlopesTimesTwo_JND)
plot(Sim8)
plot(recalculateResiduals(Sim8, group = Psychometric$ID))

Sim9 = simulateResiduals(GLMM6_RandomInterceptsAndTwoSlopesTimesTwo_JND)
plot(Sim9)
plot(recalculateResiduals(Sim9, group = Psychometric$ID))

hello = testResiduals(Sim9)
hello$uniformity$p.value
hello$dispersion$p.value

testDispersion(Sim9)


overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

overdisp_fun(GLMM6_RandomInterceptsAndTwoSlopesTimesTwo_JND)


ggplot(Psychometric, aes(x=factor(StandardValues), y=residuals(Sim1)))+geom_jitter(alpha = 0.1)
ggplot(Psychometric, aes(x=factor(ID), y=residuals(Sim1)))+geom_jitter(alpha = 0.1, width = 0.1)
ggplot(Psychometric, aes(x=factor(ConditionOfInterest), y=residuals(Sim1)))+geom_jitter(alpha = 0.1, width = 0.1)

ggplot(Psychometric, aes(x=factor(StandardValues), y=residuals(Sim2)))+geom_jitter(alpha = 0.1)
ggplot(Psychometric, aes(x=factor(ID), y=residuals(Sim2)))+geom_jitter(alpha = 0.1, width = 0.1)
ggplot(Psychometric, aes(x=factor(ConditionOfInterest), y=residuals(Sim2)))+geom_jitter(alpha = 0.1, width = 0.1)

ggplot(Psychometric, aes(x=factor(StandardValues), y=residuals(Sim3)))+geom_jitter(alpha = 0.1)
ggplot(Psychometric, aes(x=factor(ID), y=residuals(Sim3)))+geom_jitter(alpha = 0.1, width = 0.1)
ggplot(Psychometric, aes(x=factor(ConditionOfInterest), y=residuals(Sim3)))+geom_jitter(alpha = 0.1, width = 0.1)

ggplot(Psychometric, aes(x=factor(StandardValues), y=residuals(Sim7)))+geom_jitter(alpha = 0.1)
ggplot(Psychometric, aes(x=factor(ID), y=residuals(Sim7)))+geom_jitter(alpha = 0.1, width = 0.1)
ggplot(Psychometric, aes(x=factor(ConditionOfInterest), y=residuals(Sim7)))+geom_jitter(alpha = 0.1, width = 0.1)

ggplot(Psychometric, aes(x=factor(StandardValues), y=residuals(Sim8)))+geom_jitter(alpha = 0.1)
ggplot(Psychometric, aes(x=factor(ID), y=residuals(Sim8)))+geom_jitter(alpha = 0.1, width = 0.1)
ggplot(Psychometric, aes(x=factor(ConditionOfInterest), y=residuals(Sim8)))+geom_jitter(alpha = 0.1, width = 0.1)

ggplot(Psychometric, aes(x=factor(StandardValues), y=residuals(Sim9)))+geom_jitter(alpha = 0.1)
ggplot(Psychometric, aes(x=factor(ID), y=residuals(Sim9)))+geom_jitter(alpha = 0.1, width = 0.1)
ggplot(Psychometric, aes(x=factor(ConditionOfInterest), y=residuals(Sim9)))+geom_jitter(alpha = 0.1, width = 0.1)

ggplot(Psychometric, aes(x=Difference, y=residuals(GLMM6_RandomInterceptsAndTwoSlopesTimesTwo_JND)))+geom_point(alpha = 0.1)
ggplot(Psychometric, aes(x=factor(StandardValues), y=residuals(GLMM6_RandomInterceptsAndTwoSlopesTimesTwo_JND)))+geom_jitter(alpha = 0.1)
ggplot(Psychometric, aes(x=factor(ID), y=residuals(GLMM6_RandomInterceptsAndTwoSlopesTimesTwo_JND)))+geom_jitter(alpha = 0.1, width = 0.1)
ggplot(Psychometric, aes(x=factor(ConditionOfInterest), y=residuals(GLMM6_RandomInterceptsAndTwoSlopesTimesTwo_JND)))+geom_jitter(alpha = 0.1, width = 0.1)

ggplot(Psychometric, aes(x=Difference, y=residuals(GLMM_RandomIntercepts_Null_PSE)))+geom_point(alpha = 0.1)
ggplot(Psychometric, aes(x=factor(StandardValues), y=residuals(GLMM_RandomIntercepts_Null_PSE)))+geom_jitter(alpha = 0.1)
ggplot(Psychometric, aes(x=factor(ID), y=residuals(GLMM_RandomIntercepts_Null_PSE)))+geom_jitter(alpha = 0.1, width = 0.1)
ggplot(Psychometric, aes(x=factor(ConditionOfInterest), y=residuals(GLMM_RandomIntercepts_Null_PSE)))+geom_jitter(alpha = 0.1, width = 0.1)

plot(residuals(GLMM_RandomIntercepts_Null_PSE))

ggplot(Psychometric, aes(x=Difference, y=residuals(GLMM_RandomIntercepts_JND)))+geom_point(alpha = 0.1)

