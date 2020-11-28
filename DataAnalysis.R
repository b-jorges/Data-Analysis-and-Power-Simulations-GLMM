set.seed(34)

setwd(paste0(dirname(rstudioapi::getSourceEditorContext()$path)))
source("SimulateDataFunction.r")
source("functions.r")

require(ggplot2)
require(cowplot)
theme_set(theme_cowplot())

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

###performing t test
###computes pvalues for t test
Baseline_PSE = (Parameters %>% filter(ConditionOfInterest == 0))$Mean
CoI_PSE = (Parameters %>% filter(ConditionOfInterest == 1))$Mean
t.test(Baseline_PSE, CoI_PSE)

Baseline_SD = (Parameters %>% filter(ConditionOfInterest == 0))$SD
CoI_SD = (Parameters %>% filter(ConditionOfInterest == 1))$SD
t.test(Baseline_SD, CoI_SD)

#this is equivalent to a Linear Model with ConditionOfInterest as fixed effect
LM_Mean = lm(Mean ~ ConditionOfInterest, Parameters)
LM_SD = lm(SD ~ ConditionOfInterest, Parameters)
summary(LM_Mean)
summary(LM_SD)

LMM_Mean_ID = lmer(Mean ~ ConditionOfInterest + (1 | ID),
                  data = Parameters)
LMM_SD_ID = lmer(SD ~ ConditionOfInterest + (1 | ID),
                data = Parameters)
summary(LMM_Mean_ID)
summary(LMM_SD_ID)
round(ranef(LMM_Mean_ID)$ID$"(Intercept)",2)
round(ranef(LMM_SD_ID)$ID$"(Intercept)",2)


LMM_Mean_ID_SV = lmer(Mean ~ ConditionOfInterest + (1 | ID) + (1 | StandardValues),
                   data = Parameters)
LMM_SD_ID_SV = lmer(SD ~ ConditionOfInterest + (1 | ID) + (1 | StandardValues),
                 data = Parameters)
summary(LMM_Mean_ID_SV)
summary(LMM_SD_ID_SV)


#make dataframe with all of the above to paste into paper
ResultsDifferentAnalyses = data.frame(Analysis = c("t test PSE", 
                                                  "LMM 1PSE",
                                                  "LMM 2 PSE",
                                                  "t test SD", 
                                                  "LMM 1 SD",
                                                  "LMM 2 SD"),
                                     Coefficient = c(round(summary(LM_Mean)$coef[2],2),
                                                     round(summary(LMM_Mean_ID)$coef[2],2),
                                                     round(summary(LMM_Mean_ID_SV)$coef[2],2),
                                                     round(summary(LM_SD)$coef[2],2),
                                                     round(summary(LMM_SD_ID)$coef[2],2),
                                                     round(summary(LMM_SD_ID_SV)$coef[2],2)),
                                     SE = c(round(summary(LM_Mean)$coef[4],2),
                                            round(summary(LMM_Mean_ID)$coef[4],2),
                                            round(summary(LMM_Mean_ID_SV)$coef[4],2),
                                            round(summary(LM_SD)$coef[4],2),
                                            round(summary(LMM_SD_ID)$coef[4],2),
                                            round(summary(LMM_SD_ID_SV)$coef[4],2)),
                                     Pvalue = c(round(summary(LM_Mean)$coef[8],3),
                                                round(summary(LMM_Mean_ID)$coef[10],3),
                                                round(summary(LMM_Mean_ID_SV)$coef[10],3),
                                                round(summary(LM_SD)$coef[8],3),
                                                round(summary(LMM_SD_ID)$coef[10],3),
                                                round(summary(LMM_SD_ID_SV)$coef[10],3)))

##make plots: Figure 4
Parameters$ConditionOfInterest[Parameters$ConditionOfInterest == 1] = "Condition of Interest"
Parameters$ConditionOfInterest[Parameters$ConditionOfInterest == 0] = "Baseline"

Parameters$Mean_ID = c()
for (i in 1:length(rownames(ranef(LMM_Mean_ID)$ID))){
  Participant = rownames(ranef(LMM_Mean_ID)$ID)[i]
  Parameters$Mean_ID[Parameters$ID == Participant] = Parameters$Mean[Parameters$ID == Participant]-ranef(LMM_Mean_ID)$ID$"(Intercept)"[i]
}

Parameters$Mean_ID_StandardValues = c()
for (i in 1:length(rownames(ranef(LMM_Mean_ID_SV)$ID))){
  for (j in 1:length(rownames(ranef(LMM_Mean_ID_SV)$StandardValues))){
    Participant = rownames(ranef(LMM_Mean_ID_SV)$ID)[i]
    StandardValue = rownames(ranef(LMM_Mean_ID_SV)$StandardValues)[j]
    Parameters$Mean_ID_StandardValues[Parameters$ID == Participant & Parameters$StandardValues == StandardValue] = 
      Parameters$Mean[Parameters$ID == Participant & Parameters$StandardValues == StandardValue] - 
      ranef(LMM_Mean_ID_SV)$ID$"(Intercept)"[i] - ranef(LMM_Mean_ID_SV)$StandardValue$"(Intercept)"[j]
  }
}

Plot_Mean = ggplot(Parameters) +
  geom_flat_violin(position = position_nudge(x = .05, y = 0), 
          fill = "grey",
          aes(x=ConditionOfInterest,y=Mean),
          color = "grey",
          width = 0.6) +
  geom_point(position = position_jitter(width = 0.05), 
             size = 3,
             aes(x=ConditionOfInterest, y=Mean, fill = ID, colour = ID)) +
  xlab("") +
  ylab("Mean (m/s)") +
  coord_cartesian(ylim = c(-1.7,2.7)) +
  scale_color_manual(name = "",
                     values = colorRampPalette(c(BlauUB,Yellow, Red))(5)) +
  theme(legend.position = "none") +
  ggtitle("A. No random effects") +
  geom_hline(yintercept = mean(Parameters$Mean[Parameters$ConditionOfInterest == "Baseline"]),
             linetype = 2)

Plot_Mean_ID = ggplot(Parameters) +
  geom_flat_violin(position = position_nudge(x = .05, y = 0), 
                   fill = "grey",
                   aes(x=ConditionOfInterest,y=Mean_ID),
                   color = "grey",
                   width = 0.6) +
  geom_point(position = position_jitter(width = 0.05), 
             size = 3,
             aes(x=ConditionOfInterest, y=Mean_ID, fill = ID, colour = ID)) +
  xlab("") +
  ylab(expression("Mean - Intercept"[ID]*" (m/s)")) +
  coord_cartesian(ylim = c(-1.7,2.7)) +
  scale_color_manual(name = "",
                     values = colorRampPalette(c(BlauUB,Yellow, Red))(5)) +
  theme(legend.position = "none") +
  ggtitle("B. One Random Intercept") +
  geom_hline(yintercept = mean(Parameters$Mean_ID[Parameters$ConditionOfInterest == "Baseline"]),
             linetype = 2)

Plot_Mean_ID_StandardValues = ggplot(Parameters) +
  geom_flat_violin(position = position_nudge(x = .05, y = 0), 
                   fill = "grey",
                   aes(x=ConditionOfInterest,y=Mean_ID_StandardValues),
                   color = "grey",
                   width = 0.6) +
  geom_point(position = position_jitter(width = 0.05), 
             size = 3,
             aes(x=ConditionOfInterest, y=Mean_ID_StandardValues, fill = ID, colour = ID)) +
  xlab("") +
  ylab(expression("Mean - Intercept"[ID]*" - Intercept"[StandardValues]*" (m/s)")) +
  coord_cartesian(ylim = c(-1.7,2.7)) +
  scale_color_manual(name = "",
                     values = colorRampPalette(c(BlauUB,Yellow, Red))(5)) +
  theme(legend.position = "none") +
  ggtitle("C. Two Random Intercepts")+
  geom_hline(yintercept = mean(Parameters$Mean_ID_StandardValues[Parameters$ConditionOfInterest == "Baseline"]),
             linetype = 2)

plot_grid(Plot_Mean,Plot_Mean_ID,Plot_Mean_ID_StandardValues, nrow = 1)
ggsave("Figures/(Figure 4) Random Effects.jpg", w = 11, h = 6)


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
