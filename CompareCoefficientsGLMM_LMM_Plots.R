require(tidyverse)
require(lme4)
require(purrr)
require(ggplot2)
require(cowplot)
theme_set(theme_cowplot())

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

Dataframe2 = read.csv("Data/DifferentConfigurationsLMMGLMM.csv")

#####MAIN PLOTS
#model predictions 0.1 0.08
#_0.1_0.08
FigurePSEsFromGLMM_0.1_0.08 = ggplot(Dataframe2 %>% filter(PSE_Difference == 0.025 & JND_Difference == 0.08),aes(Model,Mean_Modeled-Mean_Actual)) +
  geom_boxplot() + 
  ylab("PSE Output of GLMM - Actual PSE") +
  geom_hline(aes(yintercept = 0), linetype = 2, size = 1) + 
  coord_cartesian(ylim = c(-3,3)) +
  xlab("") +
  ggtitle("PSE coefficient (PSE Difference = 0.025/JND Difference = 0.08)")

FigureSDsFromGLMM_0.1_0.08 = ggplot(Dataframe2 %>% filter(PSE_Difference == 0.025 & JND_Difference == 0.08),aes(Model,SD_Modeled-SD_Actual)) +
  geom_boxplot() + 
  coord_cartesian(ylim = c(-1,2.5)) +
  geom_hline(aes(yintercept = 0), linetype = 2, size = 1) +
  ylab("SD Output of GLMM - Actual SD") +
  xlab("") +
  ggtitle("JND coefficient (PSE Difference = 0.025/JND Difference = 0.08)")
FigureValuesFromGLMMs_0.1_0.08 = plot_grid(FigurePSEsFromGLMM_0.1_0.08,FigureSDsFromGLMM_0.1_0.08, nrow = 2, labels = "AUTO")
ggsave("Figures/FigureValuesFromGLMMs_0.025_0.08_LMMGLMM.jpeg", w = 12, h = 7.2)
#####

###SE
FigureSEsPSEsFromGLMM_0.1_0.08 = ggplot(Dataframe2 %>% filter(PSE_Difference == 0.025 & JND_Difference == 0.08),
                               aes(Model,SECoI)) +
  geom_boxplot() + 
  ylab("SE for PSE estimate") +
  xlab("") +
  coord_cartesian(ylim = c(0,0.2)) +
  ggtitle("SE of PSE coefficient (PSE Difference = 0.025/JND Difference = 0.08)")
FigureSEsSDsFromGLMM_0.1_0.08 = ggplot(Dataframe2 %>% filter(PSE_Difference == 0.025 & JND_Difference == 0.08),
                              aes(Model,SEInterac)) +
  geom_boxplot() + 
  ylab("SE for SD estimate") +
  xlab("") +
  coord_cartesian(ylim = c(0,0.2)) +
  ggtitle("SE of JND coefficient (PSE Difference = 0.025/JND Difference = 0.08)")
FigureSEsFromGLMMs_0.1_0.08 = plot_grid(FigureSEsPSEsFromGLMM_0.1_0.08,FigureSEsSDsFromGLMM_0.1_0.08, nrow = 2, labels = "AUTO")
ggsave(paste0("Figures/FigureSEsFromGLMMs_0.025_0.08_LMMGLMM.jpeg"), w = 12, h = 7.2)


#####
#p values
Dataframe3 = Dataframe2 %>%
  filter(PSE_Difference == 0.025 & JND_Difference == 0.08) %>% 
  group_by(Model,Repetition) %>% 
  slice(1) %>% 
  group_by(Model) %>% 
  mutate(Power_CoI = sum(PvaluesCoI < 0.05)/length(PvaluesCoI),
         Power_Interac = sum(PvaluesInterac < 0.05)/length(PvaluesInterac))

FigurePvaluesFromGLMM_CoI_0.1_0.08 = ggplot(Dataframe3,aes(Model,Power_CoI)) +
  geom_point(size = 5) + 
  ylab("Power") +
  xlab("") +
  geom_hline(yintercept = 0.05, linetype = 3) +
  geom_hline(yintercept = 0.9, linetype = 2) +
  coord_cartesian(ylim = c(0,1)) +
  ggtitle("Power PSE coefficient (PSE Difference = 0.025/JND Difference = 0.08)")
FigurePvaluesFromGLMM_Interac_0.1_0.08 = ggplot(Dataframe3,aes(Model,Power_Interac)) +
  geom_point(size = 5) + 
  ylab("Power") +
  xlab("") +
  geom_hline(yintercept = 0.05, linetype = 3) +
  geom_hline(yintercept = 0.9, linetype = 2) +
  coord_cartesian(ylim = c(0,1)) +
  ggtitle("Power JND coefficient (PSE Difference = 0.025/JND Difference = 0.08)")
FigureValuesFromGLMMs_0.1_0.08 = plot_grid(FigurePvaluesFromGLMM_CoI_0.1_0.08,FigurePvaluesFromGLMM_Interac_0.1_0.08, nrow = 2, labels = "AUTO")
ggsave(paste0("Figures/FigurePValuesFromGLMMs_0.025_0.08_LMMGLMM.jpeg"), w = 12, h = 7.2)
#####

#####
#AICs
AICsGLMMs_0.1_0.08 = ggplot(Dataframe2 %>% filter(PSE_Difference == 0.025 & JND_Difference == 0.08 & Model != "M_LMM"),aes(Model,AIC_Norm)) +
  geom_boxplot() + 
  xlab("") +
  ylab("AIC - AIC(Model25)") +
  geom_hline(aes(yintercept = 0), linetype = 2, size = 1) +
  ggtitle("AIC (PSE Difference = 0.025/JND Difference = 0.08)")
ggsave(paste0("Figures/Figure AICs_0.025_0.08_Models_LMMGLMM.jpeg"), w = 12, h = 3.6)
#####



###################################################################################################################
###################################SUPPLEMENTARY PLOTS#############################################################
###################################################################################################################
###################################PSE = -0.025, JND = -0.08 _M0.025_M0.08
#model predictions
FigurePSEsFromGLMM_M0.025_M0.08 = ggplot(Dataframe2 %>% filter(PSE_Difference == -0.025 & JND_Difference == -0.08),aes(Model,Mean_Modeled-Mean_Actual)) +
  geom_boxplot() + 
  ylab("PSE Output of GLMM - Actual PSE") +
  geom_hline(aes(yintercept = 0), linetype = 2, size = 1) + 
  coord_cartesian(ylim = c(-3,3)) +
  xlab("") +
  ggtitle("PSE coefficient (PSE Difference = -0.025/JND Difference = -0.08)")

FigureSDsFromGLMM_M0.025_M0.08 = ggplot(Dataframe2 %>% filter(PSE_Difference == -0.025 & JND_Difference == -0.08),aes(Model,SD_Modeled-SD_Actual)) +
  geom_boxplot() + 
  coord_cartesian(ylim = c(-1,2.5)) +
  geom_hline(aes(yintercept = 0), linetype = 2, size = 1) +
  ylab("SD Output of GLMM - Actual SD") +
  xlab("") +
  ggtitle("JND coefficient (PSE Difference = -0.025/JND Difference = -0.08)")
FigureValuesFromGLMMs_M0.025_M0.08 = plot_grid(FigurePSEsFromGLMM_M0.025_M0.08,FigureSDsFromGLMM_M0.025_M0.08, nrow = 2, labels = "AUTO")
ggsave(paste0("Figures/FigureValuesFromGLMMs_-0.025_-0.08_LMMGLMM.jpeg"), w = 12, h = 7.2)
#####

###SE
FigureSEsPSEsFromGLMM_M0.025_M0.08 = ggplot(Dataframe2 %>% filter(PSE_Difference == -0.025 & JND_Difference == -0.08),
                                            aes(Model,SECoI)) +
  geom_boxplot() + 
  ylab("SE for PSE estimate") +
  xlab("") +
  coord_cartesian(ylim = c(0,0.2)) +
  ggtitle("SE of PSE coefficient (PSE Difference = -0.025/JND Difference = -0.08)")
FigureSEsSDsFromGLMM_M0.025_M0.08 = ggplot(Dataframe2 %>% filter(PSE_Difference == -0.025 & JND_Difference == -0.08),
                                           aes(Model,SEInterac)) +
  geom_boxplot() + 
  ylab("SE for SD estimate") +
  xlab("") +
  coord_cartesian(ylim = c(0,0.2)) +
  ggtitle("SE of JND coefficient (PSE Difference = -0.025/JND Difference = -0.08)")
FigureSEsFromGLMMs_M0.025_M0.08 = plot_grid(FigureSEsPSEsFromGLMM_M0.025_M0.08,FigureSEsSDsFromGLMM_M0.025_M0.08, nrow = 2, labels = "AUTO")
ggsave(paste0("Figures/FigureSEsFromGLMMs_-0.025_-0.08_LMMGLMM.jpeg"), w = 12, h = 7.2)


#####
#p values
Dataframe3_M0.025_M0.08 = Dataframe2 %>%
  filter(PSE_Difference == -0.025 & JND_Difference == -0.08) %>% 
  group_by(Model,Repetition) %>% 
  slice(1) %>% 
  group_by(Model) %>% 
  mutate(Power_CoI = sum(PvaluesCoI < 0.05)/length(PvaluesCoI),
         Power_Interac = sum(PvaluesInterac < 0.05)/length(PvaluesInterac))

FigurePvaluesFromGLMM_CoI_M0.025_M0.08 = ggplot(Dataframe3_M0.025_M0.08,aes(Model,Power_CoI)) +
  geom_point(size = 5) + 
  ylab("Power") +
  xlab("") +
  geom_hline(yintercept = 0.05, linetype = 3) +
  geom_hline(yintercept = 0.9, linetype = 2) +
  coord_cartesian(ylim = c(0,1)) +
  ggtitle("Power PSE coefficient (PSE Difference = -0.025/JND Difference = -0.08)")
FigurePvaluesFromGLMM_Interac_M0.025_M0.08 = ggplot(Dataframe3_M0.025_M0.08,aes(Model,Power_Interac)) +
  geom_point(size = 5) + 
  ylab("Power") +
  xlab("") +
  geom_hline(yintercept = 0.05, linetype = 3) +
  geom_hline(yintercept = 0.9, linetype = 2) +
  coord_cartesian(ylim = c(0,1)) +
  ggtitle("Power JND coefficient (PSE Difference = -0.025/JND Difference = -0.08)")
FigureValuesFromGLMMs_M0.025_M0.08 = plot_grid(FigurePvaluesFromGLMM_CoI_M0.025_M0.08,FigurePvaluesFromGLMM_Interac_M0.025_M0.08, nrow = 2, labels = "AUTO")
ggsave(paste0("Figures/FigurePValuesFromGLMMs_-0.025_-0.08_LMMGLMM.jpeg"), w = 12, h = 7.2)
#####

#####
#AICs
AICsGLMMs_M0.025_M0.08 = ggplot(Dataframe2 %>% filter(PSE_Difference == -0.025 & JND_Difference == -0.08 & Model != "M_LMM"),aes(Model,AIC_Norm)) +
  geom_boxplot() + 
  xlab("") +
  ylab("AIC - AIC(Model25)") +
  geom_hline(aes(yintercept = 0), linetype = 2, size = 1) +
  ggtitle("AIC (PSE Difference = -0.025/JND Difference = -0.08)")
ggsave(paste0("Figures/Figure AICs_-0.025_-0.08_Models_LMMGLMM.jpeg"), w = 12, h = 3.6)
#####


###################################PSE = 0, JND = 0.08
FigurePSEsFromGLMM_0_0.08_ = ggplot(Dataframe2 %>% filter(PSE_Difference == 0 & JND_Difference == 0.08),aes(Model,Mean_Modeled-Mean_Actual)) +
  geom_boxplot() + 
  ylab("PSE Output of GLMM - Actual PSE") +
  geom_hline(aes(yintercept = 0), linetype = 2, size = 1) + 
  coord_cartesian(ylim = c(-3,3)) +
  xlab("") +
  ggtitle("PSE coefficient (PSE Difference = 0/JND Difference = 0.08)")

FigureSDsFromGLMM_0_0.08_ = ggplot(Dataframe2 %>% filter(PSE_Difference == 0 & JND_Difference == 0.08),aes(Model,SD_Modeled-SD_Actual)) +
  geom_boxplot() + 
  coord_cartesian(ylim = c(-1,2.5)) +
  geom_hline(aes(yintercept = 0), linetype = 2, size = 1) +
  ylab("SD Output of GLMM - Actual SD") +
  xlab("") +
  ggtitle("JND coefficient (PSE Difference = 0/JND Difference = 0.08)")
plot_grid(FigurePSEsFromGLMM_0_0.08_,FigureSDsFromGLMM_0_0.08_, nrow = 2, labels = "AUTO")
ggsave(paste0("Figures/FigureValuesFromGLMMs_0_0.08_LMMGLMM.jpeg"), w = 12, h = 7.2)
#####

###SE
FigureSEsPSEsFromGLMM_0_0.08_ = ggplot(Dataframe2 %>% filter(PSE_Difference == 0 & JND_Difference == 0.08),
                                        aes(Model,SECoI)) +
  geom_boxplot() + 
  ylab("SE for PSE estimate") +
  xlab("") +
  coord_cartesian(ylim = c(0,0.2)) +
  ggtitle("SE of PSE coefficient (PSE Difference = 0/JND Difference = 0.08)")
FigureSEsSDsFromGLMM_0_0.08_ = ggplot(Dataframe2 %>% filter(PSE_Difference == 0 & JND_Difference == 0.08),
                                       aes(Model,SEInterac)) +
  geom_boxplot() + 
  ylab("SE for SD estimate") +
  xlab("") +
  coord_cartesian(ylim = c(0,0.2)) +
  ggtitle("SE of JND coefficient (PSE Difference = 0/JND Difference = 0.08)")
FigureSEsFromGLMMs_0_0.08_ = plot_grid(FigureSEsPSEsFromGLMM_0_0.08_,FigureSEsSDsFromGLMM_0_0.08_, nrow = 2, labels = "AUTO")
ggsave(paste0("Figures/FigureSEsFromGLMMs_0_0.08_LMMGLMM.jpeg"), w = 12, h = 7.2)

#####
#p values
Dataframe4 = Dataframe2 %>%
  filter(PSE_Difference == 0 & JND_Difference == 0.08) %>% 
  group_by(Model,Repetition) %>% 
  slice(1) %>% 
  group_by(Model) %>% 
  mutate(Power_CoI = sum(PvaluesCoI < 0.05)/length(PvaluesCoI),
         Power_Interac = sum(PvaluesInterac < 0.05)/length(PvaluesInterac))

FigurePvaluesFromGLMM_CoI_0_0.08_ = ggplot(Dataframe4,aes(Model,Power_CoI)) +
  geom_point(size = 5) + 
  ylab("Power") +
  xlab("") +
  geom_hline(yintercept = 0.05, linetype = 3) +
  geom_hline(yintercept = 0.9, linetype = 2) +
  coord_cartesian(ylim = c(0,1)) +
  ggtitle("Power PSE coefficient (PSE Difference = 0/JND Difference = 0.08)")
FigurePvaluesFromGLMM_Interac_0_0.08_ = ggplot(Dataframe4,aes(Model,Power_Interac)) +
  geom_point(size = 5) + 
  ylab("Power") +
  xlab("") +
  geom_hline(yintercept = 0.05, linetype = 3) +
  geom_hline(yintercept = 0.9, linetype = 2) +
  coord_cartesian(ylim = c(0,1)) +
  ggtitle("Power JND coefficient (PSE Difference = 0/JND Difference = 0.08)")
plot_grid(FigurePvaluesFromGLMM_CoI_0_0.08_,FigurePvaluesFromGLMM_Interac_0_0.08_, nrow = 2, labels = "AUTO")
ggsave(paste0("Figures/FigurePValuesFromGLMMs_0_0.08_LMMGLMM.jpeg"), w = 12, h = 7.2)
#####

#####
#AICs
AICsGLMMs_0_0.08 = ggplot(Dataframe2 %>% filter(PSE_Difference == 0 & JND_Difference == 0.08 & Model != "M_LMM"),aes(Model,AIC_Norm)) +
  geom_boxplot() + 
  xlab("") +
  ylab("AIC - AIC(Model25)") +
  geom_hline(aes(yintercept = 0), linetype = 2, size = 1) +
  ggtitle("AIC (PSE Difference = 0/JND Difference = 0.08)")
ggsave(paste0("Figures/Figure AICs_0-0.08_Models_LMMGLMM.jpeg"), w = 12, h = 3.6)
#####


###################################################################################################################
###################################PSE = -0.025, JND = 0.08 _M0.025_0.08
FigurePSEsFromGLMM_M0.025_0.08 = ggplot(Dataframe2 %>% filter(PSE_Difference == -0.025 & JND_Difference == 0.08),aes(Model,Mean_Modeled-Mean_Actual)) +
  geom_boxplot() + 
  ylab("PSE Output of GLMM - Actual PSE") +
  geom_hline(aes(yintercept = 0), linetype = 2, size = 1) + 
  coord_cartesian(ylim = c(-3,3)) +
  xlab("") +
  ggtitle("PSE coefficient (PSE Difference = -0.025/JND Difference = 0.08)")
FigureSDsFromGLMM_M0.025_0.08 = ggplot(Dataframe2 %>% filter(PSE_Difference == -0.025 & JND_Difference == 0.08),aes(Model,SD_Modeled-SD_Actual)) +
  geom_boxplot() + 
  coord_cartesian(ylim = c(-1,2.5)) +
  geom_hline(aes(yintercept = 0), linetype = 2, size = 1) +
  ylab("SD Output of GLMM - Actual SD") +
  xlab("") +
  ggtitle("JND coefficient (PSE Difference = -0.025/JND Difference = 0.08)")
plot_grid(FigurePSEsFromGLMM_M0.025_0.08,FigureSDsFromGLMM_M0.025_0.08, nrow = 2, labels = "AUTO")
ggsave(paste0("Figures/FigureValuesFromGLMMs_-0.025_0.08_LMMGLMM.jpeg"), w = 12, h = 7.2)
#####


###SE
FigureSEsPSEsFromGLMM_M0.025_0.08 = ggplot(Dataframe2 %>% filter(PSE_Difference == -0.025 & JND_Difference == 0.08),
                                           aes(Model,SECoI)) +
  geom_boxplot() + 
  ylab("SE for PSE estimate") +
  xlab("") +
  coord_cartesian(ylim = c(0,0.2)) +
  ggtitle("SE of PSE coefficient (PSE Difference = -0.025/JND Difference = 0.08)")
FigureSEsSDsFromGLMM_M0.025_0.08 = ggplot(Dataframe2 %>% filter(PSE_Difference == -0.025 & JND_Difference == 0.08),
                                          aes(Model,SEInterac)) +
  geom_boxplot() + 
  ylab("SE for SD estimate") +
  xlab("") +
  coord_cartesian(ylim = c(0,0.2)) +
  ggtitle("SE of JND coefficient (PSE Difference = -0.025/JND Difference = 0.08)")
FigureSEsFromGLMMs_M0.025_0.08 = plot_grid(FigureSEsPSEsFromGLMM_M0.025_0.08,FigureSEsSDsFromGLMM_M0.025_0.08, nrow = 2, labels = "AUTO")
ggsave(paste0("Figures/FigureSEsFromGLMMs_-0.025_0.08_LMMGLMM.jpeg"), w = 12, h = 7.2)



#####
#p values
Dataframe5 = Dataframe2 %>%
  filter(PSE_Difference == -0.025 & JND_Difference == 0.08) %>% 
  group_by(Model,Repetition) %>% 
  slice(1) %>% 
  group_by(Model) %>% 
  mutate(Power_CoI = sum(PvaluesCoI < 0.05)/length(PvaluesCoI),
         Power_Interac = sum(PvaluesInterac < 0.05)/length(PvaluesInterac))

FigurePvaluesFromGLMM_CoI_M0.025_0.08 = ggplot(Dataframe5,aes(Model,Power_CoI)) +
  geom_point(size = 5) + 
  ylab("Power") +
  xlab("") +
  geom_hline(yintercept = 0.05, linetype = 3) +
  geom_hline(yintercept = 0.9, linetype = 2) +
  coord_cartesian(ylim = c(0,1)) +
  ggtitle("Power PSE coefficient (PSE Difference = -0.025/JND Difference = 0.08)")
FigurePvaluesFromGLMM_Interac_M0.025_0.08 = ggplot(Dataframe5,aes(Model,Power_Interac)) +
  geom_point(size = 5) + 
  ylab("Power") +
  xlab("") +
  geom_hline(yintercept = 0.05, linetype = 3) +
  geom_hline(yintercept = 0.9, linetype = 2) +
  coord_cartesian(ylim = c(0,1)) +
  ggtitle("Power JND coefficient (PSE Difference = -0.025/JND Difference = 0.08)")
plot_grid(FigurePvaluesFromGLMM_CoI_M0.025_0.08,FigurePvaluesFromGLMM_Interac_M0.025_0.08, nrow = 2, labels = "AUTO")
ggsave(paste0("Figures/FigurePValuesFromGLMMs_-0.025_0.08_LMMGLMM.jpeg"), w = 12, h = 7.2)
#####

#####
#AICs
AICsGLMMs_M0.025_0.08 = ggplot(Dataframe2 %>% filter(PSE_Difference == -0.025 & JND_Difference == 0.08 & Model != "M_LMM"),aes(Model,AIC_Norm)) +
  geom_boxplot() + 
  xlab("") +
  ylab("AIC - AIC(Model25)") +
  geom_hline(aes(yintercept = 0), linetype = 2, size = 1) +
  ggtitle("AIC (PSE Difference = -0.025/JND Difference = 0.08)")
ggsave(paste0("Figures/Figure AICs_-0.025_0.08_Models_LMMGLMM.jpeg"), w = 12, h = 3.6)
#####


###################################################################################################################
###################################PSE = 0.025, JND = 0 _0.05_0
FigurePSEsFromGLMM_0.025_0 = ggplot(Dataframe2 %>% filter(PSE_Difference == 0.025 & JND_Difference == 0),aes(Model,Mean_Modeled-Mean_Actual)) +
  geom_boxplot() + 
  ylab("PSE Output of GLMM - Actual PSE") +
  geom_hline(aes(yintercept = 0), linetype = 2, size = 1) + 
  coord_cartesian(ylim = c(-3,3)) +
  xlab("") +
  ggtitle("PSE coefficient (PSE Difference = 0.025/JND Difference = 0)")
FigureSDsFromGLMM_0.025_0 = ggplot(Dataframe2 %>% filter(PSE_Difference == 0.025 & JND_Difference == 0),aes(Model,SD_Modeled-SD_Actual)) +
  geom_boxplot() + 
  coord_cartesian(ylim = c(-1,2.5)) +
  geom_hline(aes(yintercept = 0), linetype = 2, size = 1) +
  ylab("SD Output of GLMM - Actual SD") +
  xlab("") +
  ggtitle("JND coefficient (PSE Difference = 0.025/JND Difference = 0)")
plot_grid(FigurePSEsFromGLMM_0.025_0,FigureSDsFromGLMM_0.025_0, nrow = 2, labels = "AUTO")
ggsave(paste0("Figures/FigureValuesFromGLMMs_0.025_0_LMMGLMM.jpeg"), w = 12, h = 7.2)
#####

###SE
FigureSEsPSEsFromGLMM_0.025_0 = ggplot(Dataframe2 %>% filter(PSE_Difference == 0.025 & JND_Difference == 0),
                                      aes(Model,SECoI)) +
  geom_boxplot() + 
  ylab("SE for PSE estimate") +
  xlab("") +
  coord_cartesian(ylim = c(0,0.2)) +
  ggtitle("SE of PSE coefficient (PSE Difference = 0.025/JND Difference = 0)")
FigureSEsSDsFromGLMM_0.025_0 = ggplot(Dataframe2 %>% filter(PSE_Difference == 0.025 & JND_Difference == 0),
                                     aes(Model,SEInterac)) +
  geom_boxplot() + 
  ylab("SE for SD estimate") +
  xlab("") +
  coord_cartesian(ylim = c(0,0.2)) +
  ggtitle("SE of JND coefficient (PSE Difference = 0.025/JND Difference = 0)")
FigureSEsFromGLMMs_0.025_0 = plot_grid(FigureSEsPSEsFromGLMM_0.025_0,FigureSEsSDsFromGLMM_0.025_0, nrow = 2, labels = "AUTO")
ggsave(paste0("Figures/FigureSEsFromGLMMs_0.025_0_LMMGLMM.jpeg"), w = 12, h = 7.2)


#####
#p values
Dataframe6 = Dataframe2 %>%
  filter(PSE_Difference == 0.025 & JND_Difference == 0) %>% 
  group_by(Model,Repetition) %>% 
  slice(1) %>% 
  group_by(Model) %>% 
  mutate(Power_CoI = sum(PvaluesCoI < 0.05)/length(PvaluesCoI),
         Power_Interac = sum(PvaluesInterac < 0.05)/length(PvaluesInterac))

FigurePvaluesFromGLMM_CoI_0.025_0 = ggplot(Dataframe6,aes(Model,Power_CoI)) +
  geom_point(size = 5) + 
  ylab("Power") +
  xlab("") +
  geom_hline(yintercept = 0.05, linetype = 3) +
  geom_hline(yintercept = 0.9, linetype = 2) +
  coord_cartesian(ylim = c(0,1)) +
  ggtitle("Power PSE coefficient (PSE Difference = 0.025/JND Difference = 0)")
FigurePvaluesFromGLMM_Interac_0.025_0 = ggplot(Dataframe6,aes(Model,Power_Interac)) +
  geom_point(size = 5) + 
  ylab("Power") +
  xlab("") +
  geom_hline(yintercept = 0.05, linetype = 3) +
  geom_hline(yintercept = 0.9, linetype = 2) +
  ggtitle("Power JND coefficient (PSE Difference = 0.025/JND Difference = 0)")
plot_grid(FigurePvaluesFromGLMM_CoI_0.025_0,FigurePvaluesFromGLMM_Interac_0.025_0, nrow = 2, labels = "AUTO")
ggsave(paste0("Figures/FigurePValuesFromGLMMs_0.025_0_LMMGLMM.jpeg"), w = 12, h = 7.2)
#####

#####
#AICs
AICsGLMMs_0.025_0 = ggplot(Dataframe2 %>% filter(PSE_Difference == 0.025 & JND_Difference == 0 & Model != "M_LMM"),aes(Model,AIC_Norm)) +
  geom_boxplot() + 
  xlab("") +
  ylab("AIC - AIC(Model25)") +
  geom_hline(aes(yintercept = 0), linetype = 2, size = 1) +
  ggtitle("AIC (PSE Difference = 0.025/JND Difference = 0)")
ggsave(paste0("Figures/Figure AICs_0.025_0_Models_LMMGLMM.jpeg"), w = 12, h = 3.6)
#####


###################################################################################################################
###################################PSE = 0.025, JND = -0.08##########################################################
#_0.025_M0.08
FigurePSEsFromGLMM_0.025_M0.08 = ggplot(Dataframe2 %>% filter(PSE_Difference == 0.025 & JND_Difference == -0.08),aes(Model,Mean_Modeled-Mean_Actual)) +
  geom_boxplot() + 
  ylab("PSE Output of GLMM - Actual PSE") +
  geom_hline(aes(yintercept = 0), linetype = 2, size = 1) + 
  coord_cartesian(ylim = c(-3,3)) +
  xlab("") +
  ggtitle("PSE coefficient (PSE Difference = 0.025/JND Difference = -0.08)")
FigureSDsFromGLMM_0.025_M0.08 = ggplot(Dataframe2 %>% filter(PSE_Difference == 0.025 & JND_Difference == -0.08),aes(Model,SD_Modeled-SD_Actual)) +
  geom_boxplot() + 
  coord_cartesian(ylim = c(-1,2.5)) +
  geom_hline(aes(yintercept = 0), linetype = 2, size = 1) +
  ylab("SD Output of GLMM - Actual SD") +
  xlab("") +
  ggtitle("JND coefficient (PSE Difference = 0.025/JND Difference = -0.08)")
plot_grid(FigurePSEsFromGLMM_0.025_M0.08,FigureSDsFromGLMM_0.025_M0.08, nrow = 2, labels = "AUTO")
ggsave(paste0("Figures/FigureValuesFromGLMMs_0.025_-0.08_LMMGLMM.jpeg"), w = 12, h = 7.2)
#####

###SE
FigureSEsPSEsFromGLMM_0.025_M0.08 = ggplot(Dataframe2 %>% filter(PSE_Difference == 0.025 & JND_Difference == -0.08),
                                           aes(Model,SECoI)) +
  geom_boxplot() + 
  ylab("SE for PSE estimate") +
  xlab("") +
  coord_cartesian(ylim = c(0,0.2)) +
  ggtitle("SE of PSE coefficient (PSE Difference = 0.025/JND Difference = -0.08)")
FigureSEsSDsFromGLMM_0.025_M0.08 = ggplot(Dataframe2 %>% filter(PSE_Difference == 0.025 & JND_Difference == -0.08),
                                          aes(Model,SEInterac)) +
  geom_boxplot() + 
  ylab("SE for SD estimate") +
  xlab("") +
  coord_cartesian(ylim = c(0,0.2)) +
  ggtitle("SE of JND coefficient (PSE Difference = 0.025/JND Difference = -0.08)")
FigureSEsFromGLMMs_0.025_M0.08 = plot_grid(FigureSEsPSEsFromGLMM_0.025_M0.08,FigureSEsSDsFromGLMM_0.025_M0.08, nrow = 2, labels = "AUTO")
ggsave(paste0("Figures/FigureSEsFromGLMMs_0.025_-0.08_LMMGLMM.jpeg"), w = 12, h = 7.2)


#####
#p values
Dataframe7 = Dataframe2 %>%
  filter(PSE_Difference == 0.025 & JND_Difference == -0.08) %>% 
  group_by(Model,Repetition) %>% 
  slice(1) %>% 
  group_by(Model) %>% 
  mutate(Power_CoI = sum(PvaluesCoI < 0.05)/length(PvaluesCoI),
         Power_Interac = sum(PvaluesInterac < 0.05)/length(PvaluesInterac))

FigurePvaluesFromGLMM_CoI_0.025_M0.08 = ggplot(Dataframe7,aes(Model,Power_CoI)) +
  geom_point(size = 5) + 
  ylab("Power") +
  xlab("") +
  geom_hline(yintercept = 0.05, linetype = 3) +
  geom_hline(yintercept = 0.9, linetype = 2) +
  coord_cartesian(ylim = c(0,1)) +
  ggtitle("Power PSE coefficient (PSE Difference = 0.025/JND Difference = -0.08)")
FigurePvaluesFromGLMM_Interac_0.025_M0.08 = ggplot(Dataframe7,aes(Model,Power_Interac)) +
  geom_point(size = 5) + 
  ylab("Power") +
  xlab("") +
  geom_hline(yintercept = 0.05, linetype = 3) +
  geom_hline(yintercept = 0.9, linetype = 2) +
  coord_cartesian(ylim = c(0,1)) +
  ggtitle("Power JND coefficient (PSE Difference = 0.025/JND Difference = -0.08)")
plot_grid(FigurePvaluesFromGLMM_CoI_0.025_M0.08,FigurePvaluesFromGLMM_Interac_0.025_M0.08, nrow = 2, labels = "AUTO")
ggsave(paste0("Figures/FigurePValuesFromGLMMs_0.025_-0.08_LMMGLMM.jpeg"), w = 12, h = 7.2)
#####

#####
#AICs
AICsGLMMs_0.025_M0.08 = ggplot(Dataframe2 %>% filter(PSE_Difference == 0.025 & JND_Difference == -0.08 & Model != "M_LMM"),aes(Model,AIC_Norm)) +
  geom_boxplot() + 
  xlab("") +
  ylab("AIC - AIC(Model25)") +
  geom_hline(aes(yintercept = 0), linetype = 2, size = 1) +
  ggtitle("AIC (PSE Difference = 0.025/JND Difference = -0.08)")
ggsave(paste0("Figures/Figure AICs_0.025_-0.08_Models_LMMGLMM.jpeg"), w = 12, h = 3.6)
#####


####Power together
####Everything
Dataframe2 = read.csv("Data/DifferentConfigurationsLMMGLMM.csv")

Dataframe2 = Dataframe2 %>% 
  mutate(Condition_PSEJND2 = 
           case_when(Condition_PSEJND == "-0.125-0.025" ~ "PSE: -0.025, JND: -0.08",
                     Condition_PSEJND == "-0.01250.025" ~ "PSE: 0.025, JND: -0.08",
                     Condition_PSEJND == "0.125-0.025" ~ "PSE: -0.025, JND: 0.08",
                     Condition_PSEJND == "0.1250" ~ "PSE: 0, JND: 0.08",
                     Condition_PSEJND == "0.1250.025" ~ "PSE: 0.025, JND: 0.08",
                     Condition_PSEJND == "00.025" ~ "PSE: 0.025, JND: 0"),
         PSE_Formatted = 
           case_when(PSE_Difference == -0.025 ~ "PSE: -0.025",
                     PSE_Difference == 0 ~ "PSE: 0",
                     PSE_Difference == 0.025 ~ "PSE: 0.025"),
         SD_Formatted = 
           case_when(JND_Difference == -0.125 ~ "JND: -0.125",
                     JND_Difference == 0 ~ "JND: 0",
                     JND_Difference == 0.125 ~ "JND: 0.125"))

Dataframe8 = Dataframe2 %>%
  group_by(Condition_PSEJND,Model,Repetition) %>% 
  slice(1) %>% 
  group_by(Condition_PSEJND,Model) %>% 
  mutate(Power_CoI = sum(PvaluesCoI < 0.05)/length(PvaluesCoI),
         Power_Interac = sum(PvaluesInterac < 0.05)/length(PvaluesInterac),
         Condition_PSEJND2 = 
           case_when(Condition_PSEJND == "-0.08-0.025" ~ "PSE: -0.025, JND: -0.08",
                     Condition_PSEJND == "-0.080.025" ~ "PSE: 0.025, JND: -0.08",
                     Condition_PSEJND == "0.08-0.025" ~ "PSE: -0.025, JND: 0.08",
                     Condition_PSEJND == "0.080" ~ "PSE: 0, JND: 0.08",
                     Condition_PSEJND == "0.080.025" ~ "PSE: 0.025, JND: 0.08",
                     Condition_PSEJND == "00.025" ~ "PSE: 0.025, JND: 0"))

PowerPSEs = ggplot(Dataframe8,aes(Model,Power_CoI)) +
  geom_point(size = 5) + 
  ylab("Power") +
  xlab("") +
  geom_hline(yintercept = 0.05, linetype = 3) +
  geom_hline(yintercept = 0.9, linetype = 2) +
  ggtitle("Power PSE difference (LMM vs. top four GLMMs)") +
  facet_grid(PSE_Formatted+SD_Formatted~.) +
  scale_x_discrete(labels = c("LMM", "M14", "M15","M24","M25"))
PowerJNDs = ggplot(Dataframe8,aes(Model,Power_Interac)) +
  geom_point(size = 5) + 
  ylab("Power") +
  xlab("") +
  geom_hline(yintercept = 0.05, linetype = 3) +
  geom_hline(yintercept = 0.9, linetype = 2) +
  ggtitle("Power JND difference (LMM vs. top four GLMMs)") +
  facet_grid(PSE_Formatted+SD_Formatted~.) +
  scale_x_discrete(labels = c("LMM", "M14", "M15","M24","M25"))
plot_grid(PowerPSEs,PowerJNDs, labels = "AUTO")
ggsave(paste0("Figures/GLMMLMM Models Power.jpeg"), w = 14, h = 8)

CoefficientPSEs = ggplot(Dataframe2,aes(Model,Mean_Modeled-Mean_Actual)) +
  geom_boxplot() + 
  ylab("Recovered PSE difference - Actual PSE difference") +
  xlab("") +
  geom_hline(yintercept = 0, linetype = 3) +
  ggtitle("Recovered PSE Differences (LMM vs. top four GLMMs)") +
  facet_grid(PSE_Formatted+SD_Formatted~.) +
  coord_cartesian(ylim = c(-0.3,0.3)) +
  scale_x_discrete(labels = c("LMM", "M14", "M15","M24","M25"))
CoefficientJNDs = ggplot(Dataframe2,aes(Model,SD_Modeled-SD_Actual)) +
  geom_boxplot() + 
  ylab("Recovered JND difference - Actual JND difference") +
  xlab("") +
  geom_hline(yintercept = 0, linetype = 3) +
  ggtitle("Recovered JND Differences (LMM vs. top four GLMMs)") +
  facet_grid(PSE_Formatted+SD_Formatted~.) +
  coord_cartesian(ylim = c(-0.3,0.3)) +
  scale_x_discrete(labels = c("LMM", "M14", "M15","M24","M25"))
plot_grid(CoefficientPSEs,CoefficientJNDs,labels = "AUTO")
ggsave(paste0("Figures/GLMMLMM Models Coefficient.jpeg"), w = 14, h = 12)