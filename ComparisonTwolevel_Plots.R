###Pull the whole repository
require(dplyr)
require(ggplot2)
require(cowplot)
theme_set(theme_cowplot())


########################################################################
##############compare power for GLMM and Two-Level approach#############
########################################################################
Dataframe_wide_Big = rbind(read.csv(header = T, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/Data/PowerTwoLevelComparison.csv"))) %>%
  select(power_Accuracy,power_Precision,power_Accuracy_Twolevel,power_Precision_Twolevel,n, PSE_Difference, JND_Difference, reps)
Dataframe_Powers_Big = data.frame(PSE_Difference = rep(Dataframe_wide_Big$PSE_Difference),
                                  JND_Difference = rep(Dataframe_wide_Big$JND_Difference),
                                  #                                  PSE_Difference = rep(Dataframe_wide_Big$PSE_Difference,6),
                                  #                                  JND_Difference = rep(Dataframe_wide_Big$JND_Difference,6),
                                  n = rep(Dataframe_wide_Big$n,4),
                                  reps = rep(Dataframe_wide_Big$reps,4),
                                  #                                 n = rep(Dataframe_wide_Big$n,6),
                                  #                                  reps = rep(Dataframe_wide_Big$reps,6),
                                  power = c(Dataframe_wide_Big$power_Accuracy,
                                            Dataframe_wide_Big$power_Precision,
                                            Dataframe_wide_Big$power_Accuracy_Twolevel,
                                            Dataframe_wide_Big$power_Precision_Twolevel),
                                  #                                            Dataframe_wide_Big$power_Accuracy_ANOVA,
                                  #                                            Dataframe_wide_Big$power_Precision_ANOVA),
                                  label = c(rep("Accuracy (GLMM)",length(Dataframe_wide_Big$reps)),
                                            rep("Precision (GLMM)",length(Dataframe_wide_Big$reps)),
                                            rep("Accuracy (Two level)",length(Dataframe_wide_Big$reps)),
                                            rep("Precision (Two level)",length(Dataframe_wide_Big$reps))))
#                                            rep("Accuracy_Twolevel_ANOVA",length(Dataframe_wide_Big$reps)),
#                                            rep("Precision_Twolevel_ANOVA",length(Dataframe_wide_Big$reps))))

PSE_Names <- c(
  '-0.025'="PSE -2.5%",
  "-0.0125"="PSE -1.25%",
  "0"="PSE +0%",
  "0.0125"="PSE +1.25%",
  "0.025"="PSE +2.5%"
)

JND_Names <- c(
  "-0.1"="JND -10%",
  "-0.05"="JND -5%",
  "0"="JND +0%",
  "0.05"="JND +5%",
  "0.1"="JND +10%"
)


Powers1 = ggplot(Dataframe_Powers_Big %>% filter(reps == 40), aes(n,power,color = label)) +
  geom_line(size = 2) +
  facet_grid(JND_Difference~PSE_Difference,
             labeller = labeller(JND_Difference = as_labeller(JND_Names),
                                 PSE_Difference = as_labeller(PSE_Names))) +
  xlab("N° of Participants") +
  ylab("Power") +
  geom_hline(linetype = 2, yintercept = 0.8) +
  geom_hline(linetype = 1, yintercept = 0.05) +
  geom_hline(linetype = 3, yintercept = 0.95) +
  scale_x_continuous(breaks=c(10,15,20)) +
  scale_y_continuous(breaks=c(0.25,0.75)) +
  scale_color_manual(values = c(BlauUB,LightBlauUB,Red,LightRed), 
                     name = "") +
  ggtitle("A. 40 Trials per Staircase")

Powers2 = ggplot(Dataframe_Powers_Big %>% filter(reps == 70), aes(n,power,color = label)) +
  geom_line(size = 2) +
  facet_grid(JND_Difference~PSE_Difference,
             labeller = labeller(JND_Difference = as_labeller(JND_Names),
                                 PSE_Difference = as_labeller(PSE_Names))) +
  xlab("N° of Participants") +
  ylab("Power") +
  geom_hline(linetype = 2, yintercept = 0.8) +
  geom_hline(linetype = 1, yintercept = 0.05) +
  geom_hline(linetype = 3, yintercept = 0.95) +
  scale_x_continuous(breaks=c(10,15,20)) +
  scale_y_continuous(breaks=c(0.25,0.75)) +
  scale_color_manual(values = c(BlauUB,LightBlauUB,Red,LightRed), 
                     name = "") +
  ggtitle("B. 70 Trials per Staircase")

Powers3 = ggplot(Dataframe_Powers_Big %>% filter(reps == 100), aes(n,power,color = label)) +
  geom_line(size = 2) +
  facet_grid(JND_Difference~PSE_Difference,
             labeller = labeller(JND_Difference = as_labeller(JND_Names),
                                 PSE_Difference = as_labeller(PSE_Names))) +
  xlab("N° of Participants") +
  ylab("Power") +
  geom_hline(linetype = 2, yintercept = 0.8) +
  geom_hline(linetype = 1, yintercept = 0.05) +
  geom_hline(linetype = 3, yintercept = 0.95) +
  scale_x_continuous(breaks=c(10,15,20)) +
  scale_y_continuous(breaks=c(0.25,0.75)) +
  scale_color_manual(values = c(BlauUB,LightBlauUB,Red,LightRed), 
                     name = "") +
  ggtitle("C. 100 Trials per Staircase")

plot_shared_legend(Powers1,Powers2, Powers3)
ggsave("Figures/PowerComparison.jpg", w = 18, h = 12)
