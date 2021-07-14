require(tidyverse)
require(ggplot2)
require(cowplot)
theme_set(theme_cowplot())

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#####download the following data file from OSF: https://osf.io/y2p8t/ and place it in a folder called "Data", 
######which is located in the same directory as this script
Dataframe2 = read.csv("Data/DifferentConfigurationsLMMGLMM.csv")

Dataframe2 = Dataframe2 %>% 
  mutate(PSE_Formatted = 
           case_when(PSE_Difference == -0.0125 ~ "PSE: -0.0125",
                     PSE_Difference == 0 ~ "PSE: 0",
                     PSE_Difference == 0.0125 ~ "PSE: 0.0125"),
         SD_Formatted = 
           case_when(JND_Difference == -0.08 ~ "JND: -0.08",
                     JND_Difference == 0 ~ "JND: 0",
                     JND_Difference == 0.08 ~ "JND: 0.08"))

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
ggsave(paste0("Figures/(Supplementary Figure 11) GLMMLMM Models Coefficient.jpeg"), w = 14, h = 12)

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
ggsave(paste0("Figures/(Supplementary Figure 12) GLMMLMM Models Power.jpeg"), w = 14, h = 8)

