require(tidyverse)
require(ggplot2)
require(cowplot)
theme_set(theme_cowplot())

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#####download the following data file from OSF: https://osf.io/usjhx/ and place it in a folder called "Data", 
######which is located in the same directory as this script


####Everything
Dataframe2 = rbind(read.csv(header = T, file = paste0("Data/DifferentConfigurations25Models.csv")))

Dataframe2 = Dataframe2 %>% 
  mutate(Condition_PSEJND2 = 
           case_when(Condition_PSEJND == "-0.08-0.0125" ~ "PSE: -0.0125, JND: -0.08",
                     Condition_PSEJND == "-0.080.0125" ~ "PSE: 0.0125, JND: -0.08",
                     Condition_PSEJND == "0.08-0.0125" ~ "PSE: -0.0125, JND: 0.08",
                     Condition_PSEJND == "0.080" ~ "PSE: 0, JND: 0.08",
                     Condition_PSEJND == "0.080.0125" ~ "PSE: 0.0125, JND: 0.08",
                     Condition_PSEJND == "00.0125" ~ "PSE: 0.0125, JND: 0"),
         PSE_Formatted = 
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
           case_when(Condition_PSEJND == "-0.08-0.0125" ~ "PSE: -0.0125, JND: -0.08",
                     Condition_PSEJND == "-0.080.0125" ~ "PSE: 0.0125, JND: -0.08",
                     Condition_PSEJND == "0.08-0.0125" ~ "PSE: -0.0125, JND: 0.08",
                     Condition_PSEJND == "0.080" ~ "PSE: 0, JND: 0.08",
                     Condition_PSEJND == "0.080.0125" ~ "PSE: 0.0125, JND: 0.08",
                     Condition_PSEJND == "00.0125" ~ "PSE: 0.0125, JND: 0"))

ggplot(Dataframe8,aes(Model,Power_CoI)) +
  geom_point(size = 5) + 
  ylab("Power") +
  xlab("") +
  geom_hline(yintercept = 0.05, linetype = 3) +
  geom_hline(yintercept = 0.9, linetype = 2) +
  ggtitle("Power PSE difference (25 GLMMs)") +
  facet_grid(PSE_Formatted+SD_Formatted~.)
ggsave(paste0("Figures/(Supplementary Figure 4) 25 Models Power PSE.jpeg"), w = 12, h = 8)

ggplot(Dataframe8,aes(Model,Power_Interac)) +
  geom_point(size = 5) + 
  ylab("Power") +
  xlab("") +
  geom_hline(yintercept = 0.05, linetype = 3) +
  geom_hline(yintercept = 0.9, linetype = 2) +
  ggtitle("Power JND difference (25 GLMMs)") +
  facet_grid(PSE_Formatted+SD_Formatted~.)
ggsave(paste0("Figures/(Supplementary Figure 5) 25 Models Power JND.jpeg"), w = 12, h = 8)

ggplot(Dataframe2,aes(Model,Mean_Modeled-Mean_Actual)) +
  geom_boxplot() + 
  ylab("Recovered PSE difference - Actual PSE difference") +
  xlab("") +
  geom_hline(yintercept = 0, linetype = 3) +
  ggtitle("Recovered PSE Differences (25 GLMMs)") +
  facet_grid(PSE_Formatted+SD_Formatted~.) +
  coord_cartesian(ylim = c(-1,1))
ggsave(paste0("Figures/(Supplementary Figure 6) 25 Models Parameters PSEs.jpeg"), w = 12, h = 12)

ggplot(Dataframe2,aes(Model,SD_Modeled-SD_Actual)) +
  geom_boxplot() + 
  ylab("Recovered JND difference - Actual JND difference") +
  xlab("") +
  geom_hline(yintercept = 0, linetype = 3) +
  ggtitle("Recovered JND Differences (25 GLMMs)") +
  facet_grid(PSE_Formatted+SD_Formatted~.) +
  coord_cartesian(ylim = c(-0.5,1.75))
ggsave(paste0("Figures/(Supplementary Figure 7) 25 Models Parameters SDs.jpeg"), w = 12, h = 12)

ggplot(Dataframe2,aes(Model,SECoI)) +
  geom_boxplot() + 
  ylab("Standard Error") +
  xlab("") +
  ggtitle("Standard Errors for PSE coefficients (25 GLMMs)") +
  facet_grid(PSE_Formatted+SD_Formatted~.)
ggsave(paste0("Figures/(Supplementary Figure 8) 25 Models SEs PSEs.jpeg"), w = 12, h = 8)

ggplot(Dataframe2,aes(Model,SEInterac)) +
  geom_boxplot() + 
  ylab("Standard Error") +
  xlab("") +
  ggtitle("Standard Errors for JND coefficients (25 GLMMs)") +
  facet_grid(PSE_Formatted+SD_Formatted~.)
ggsave(paste0("Figures/(Supplementary Figure 9) 25 Models SEs SDs.jpeg"), w = 12, h = 8)

ggplot(Dataframe2,aes(Model,AIC_Norm)) +
  geom_boxplot() + 
  ylab(bquote("AIC - AIC"[M25])) +
  xlab("") +
  ggtitle("Model fits (25 GLMMs)") +
  facet_grid(PSE_Formatted+SD_Formatted~.)
ggsave(paste0("Figures/(Supplementary Figure 10) 25 Models AICs.jpeg"), w = 12, h = 8)
