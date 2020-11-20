set.seed(34)

Psychometric = SimulatePsychometricData(nParticipants = 5,
                                        ConditionOfInterest = c(0,1),
                                        StandardValues = c(5,6,7,8),
                                        reps = 50,
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
PsychometricFunctions = quickpsy(Psychometric,
                                 Difference,
                                 Answer,
                                 grouping = .(ConditionOfInterest,ID,StandardValues), 
                                 bootstrap = "none")
plot(PsychometricFunctions)

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
round(ranef(LMM_Mean)$ID$"(Intercept)",2)

LMM_Mean_2 = lmer(Mean ~ ConditionOfInterest + (1 | ID) + (1 | StandardValues),
                  data = Parameters)
LMM_SD_2 = lmer(SD ~ ConditionOfInterest + (1 | ID) + (1 | StandardValues),
                data = Parameters)
summary(LMM_Mean_2)
summary(LMM_SD_2)

#make dataframe with all of the above to paste into paper
ResultsDifferentAnalyses = data.frame(Analysis = c("ANOVA PSE", 
                                                  "LMM PSE",
                                                  "LMM 2 PSE",
                                                  "ANOVA SD", 
                                                  "LMM SD",
                                                  "LMM 2 SD"),
                                     Coefficient = c(round(summary(ANOVA_Mean)$coef[2],2),
                                                     round(summary(LMM_Mean)$coef[2],2),
                                                     round(summary(LMM_Mean_2)$coef[2],2),
                                                     round(summary(ANOVA_SD)$coef[2],2),
                                                     round(summary(LMM_SD)$coef[2],2),
                                                     round(summary(LMM_SD_2)$coef[2],2)),
                                     SE = c(round(summary(ANOVA_Mean)$coef[6],2),
                                            round(summary(LMM_Mean)$coef[6],2),
                                            round(summary(LMM_Mean_2)$coef[4],2),
                                            round(summary(ANOVA_SD)$coef[6],2),
                                            round(summary(LMM_SD)$coef[6],2),
                                            round(summary(LMM_SD_2)$coef[4],2)),
                                     Pvalue = c(round(summary(ANOVA_Mean)$coef[14],2),
                                                round(summary(LMM_Mean)$coef[18],2),
                                                round(summary(LMM_Mean_2)$coef[10],2),
                                                round(summary(ANOVA_SD)$coef[14],2),
                                                round(summary(LMM_SD)$coef[18],2),
                                                round(summary(LMM_SD_2)$coef[10],2)))
##make plots: Figure 4
Parameters$ConditionOfInterest[Parameters$ConditionOfInterest == 1] = "Condition of Interest"
Parameters$ConditionOfInterest[Parameters$ConditionOfInterest == 0] = "Baseline"

Plot_LMM_Mean = ggplot(Parameters,aes(StandardValues,Mean,color = ID)) +
  geom_point(size = 4) +
  facet_grid(.~ConditionOfInterest) +
  scale_color_manual(name = "",
                     values = colorRampPalette(c(BlauUB,Yellow, Red))(5)) +
  geom_smooth(method='lm', se = FALSE) +
  xlab("Standard Values (m/s)") +
  ylab("Normalized Mean (m/s)")

Plot_LM_Mean = ggplot(Parameters,aes(StandardValues,Mean)) +
  geom_point(size = 4) +
  facet_grid(.~ConditionOfInterest) +
  geom_smooth(method='lm',color = "black", se = FALSE) +
  xlab("Standard Values (m/s)") +
  ylab("Normalized Mean (m/s)")
plot_grid(Plot_LM_Mean,Plot_LMM_Mean, labels = "AUTO")
ggsave(paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/Figures/(Figure4) SDs and Means.jpg"), w = 12, h = 5)

GLMM_RandomIntercepts_JND = glmer(Answer ~ ConditionOfInterest*Difference + (1| ID), 
             family = binomial(link = "probit"),
             data = Psychometric)
GLMM2_RandomInterceptsAndSlopes_JND = glmer(Answer ~ ConditionOfInterest*Difference + (1 + Difference| ID), 
             family = binomial(link = "probit"),
             data = Psychometric)
summary(GLMM_RandomIntercepts_JND)
summary(GLMM2_RandomInterceptsAndSlopes_JND)

GLMM_RandomIntercepts_Null_JND = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest + Difference + (1| ID), 
                                       family = binomial(link = "probit"),
                                       data = Psychometric)
GLMM2_RandomInterceptsAndSlopes_Null_JND = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest + Difference + (1 + Difference| ID), 
                                                 family = binomial(link = "probit"),
                                                 data = Psychometric)
anova(GLMM_RandomIntercepts_JND,GLMM_RandomIntercepts_Null_JND)
anova(GLMM2_RandomInterceptsAndSlopes_JND,GLMM2_RandomInterceptsAndSlopes_Null_JND)

GLMM_RandomIntercepts_PSE = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest + Difference + (1| ID), 
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

anova(GLMM_RandomIntercepts_JND,GLMM2_RandomInterceptsAndSlopes_JND)
anova(GLMM_RandomIntercepts_PSE,GLMM2_RandomInterceptsAndSlopes_PSE)
summary(GLMM_RandomIntercepts_JND)
summary(GLMM2_RandomInterceptsAndSlopes_JND)
summary(GLMM_RandomIntercepts_PSE)
summary(GLMM2_RandomInterceptsAndSlopes_PSE)



####Testing assumptions is hard. The DHARMa package helps a lot with that:
#install.packages(DHARMa)
require(DHARMa)

Sim_Simple = simulateResiduals(GLMM_RandomIntercepts_JND)

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
ggsave(paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/(Figure 5) Figure Residuals.jpg"), w = 12.5, h = 5)
