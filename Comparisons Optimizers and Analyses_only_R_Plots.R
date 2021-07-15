###Pull the whole repository
require(dplyr)
require(tidyverse)
require(lme4)
require(ggplot2)
require(cowplot)
theme_set(theme_cowplot())
require(quickpsy)
require(brms)
require(rstan)
#require(lmerTest)
require(DHARMa)

Where_Am_I <- function(path=T){
  if (path == T){
    dirname(rstudioapi::getSourceEditorContext()$path)
  }
  else {
    rstudioapi::getSourceEditorContext()$path
  }
}

binomial_smooth <- function(...) {
  geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)}

setwd(Where_Am_I())

source("Utilities/parabolic.r")
source("Utilities/functions.r")
source("Utilities/colourschemes.r")
source("Utilities/PowerFunctions.r")


Dataframe_pvalues1 = rbind(read.csv(header = T, file = paste0(Where_Am_I(),"/Data/ComparisonMethodsSmallEffect.csv")))
Dataframe_pvalues1$Effect = "Small Effect"
Dataframe_pvalues2 = rbind(read.csv(header = T, file = paste0(Where_Am_I(),"/Data/ComparisonMethodsNoEffect.csv")))
Dataframe_pvalues2$Effect = "No Effect"

Dataframe_pvalues = rbind(Dataframe_pvalues1,Dataframe_pvalues2)

Dataframe_pvalues = Dataframe_pvalues %>%
  mutate(Optimizer = case_when(
    label == "JuliaAIC_NeldMeader_AGP0" ~ "Julia: Nelder-Mead, fast",
    label == "JuliaAIC_bobyqa_AGP0" ~ "Julia: BOBYQA, fast",
    label == "JuliaAIC_NeldMeader_AGP1" ~ "Julia: Nelder-Mead, slow",
    label == "JuliaAIC_bobyqa_AGP1" ~ "Julia: BOBYQA, slow",
    label == "NelderMead_nAGQ0" ~ "R: Nelder-Mead, fast",
    label == "NelderMead_nAGQ1" ~ "R: Nelder-Mead, slow",
    label == "Bobyqa_nAGQ0" ~ "R: BOBYQA, fast",
    label == "Bobyqa_nAGQ1" ~ "R: BOBYQA, slow",
    label == "nloptwrap_nAGQ0" ~ "R: nloptwrap, fast",
    label == "nloptwrap_nAGQ1" ~ "R: nloptwrap, slow",
    label == "JuliaLRT_Fast" ~ "Julia: LRT, fast",
    label == "JuliaLRT_Slow" ~ "Julia: LRT, slow",
    label == "JuliaLRT" ~ "Julia: LRT",
    label == "R: LRT" ~ "R: LRT")
  )%>%
  mutate(
    nTrials = case_when(
      reps == 30 ~ "30 repetitions",
      reps == 40 ~ "40 repetitions",
      reps == 50 ~ "50 repetitions",
      reps == 60 ~ "60 repetitions")) %>% 
  filter(Optimizer %in% c("R: Nelder-Mead, fast",
                          "R: Nelder-Mead, slow",
                          "R: BOBYQA, fast",
                          "R: BOBYQA, slow",
                          "R: nloptwrap, fast",
                          "R: LRT",
                          "R: nloptwrap, slow"))

Dataframe_pvalues$label[2]
Dataframe_pvalues = Dataframe_pvalues %>%
  #  filter(Effect == "No Effect") %>% 
  group_by(reps,n,iteration,Effect) %>% 
  mutate(AIC_Ratio = AIC/AIC[2],
         Duration_LRT_R = Duration[Optimizer == "R: nloptwrap, fast"] + Duration[Optimizer == "R: LRT"]) %>% 
  group_by(reps,n,Optimizer) %>% 
  mutate(Median_AIC_Ratio = median(AIC_Ratio))

Dataframe_pvalues$Duration[Dataframe_pvalues$Optimizer == "R: LRT"] = 
  Dataframe_pvalues$Duration_LRT_R[Dataframe_pvalues$Optimizer == "R: LRT"]

Dataframe_pvalues = Dataframe_pvalues %>%         
  group_by(reps,n,label) %>% 
  mutate(MeanDuration = mean(Duration),
         SEDuration = SE(Duration),
         SE_Duration_n_reps_label = SE(Duration))
Dataframe_pvalues$Pvalues_Interaction[is.nan(Dataframe_pvalues$Pvalues_Interaction)] = 1

#######Timing
TimingPlot1 = ggplot(Dataframe_pvalues,
                     aes(n,MeanDuration, color = Optimizer)) +
  geom_point(size=2) +
  geom_line(size=1) +
  facet_grid(.~nTrials) +
  ylab("Mean Duration (s)") +
  scale_color_manual(values = c(rainbow(7)), name = "Method") +
  scale_x_continuous(breaks = c(10,15,20)) +
  ggtitle("A. All Configurations")

ggsave("Figures/(Figure 9) Different Durations only R.jpg",w=9,h=6)


#######False Positives
Dataframe_pvalues$Bin_Accuracy = 0
Dataframe_pvalues$Bin_Interaction = 0
for (i in (1:length(Dataframe_pvalues$iteration))){
  print(i)  
  Bins = seq(0,0.95,0.05)
  Dataframe_pvalues$Bin_Accuracy[i] = Bins[which.min(abs(Bins-Dataframe_pvalues$Pvalues_Accuracy[i]))]+0.025
  Dataframe_pvalues$Bin_Interaction[i] = Bins[which.min(abs(Bins-Dataframe_pvalues$Pvalues_Interaction[i]))]+0.025
}


Dataframe_pvalues = Dataframe_pvalues %>%
  group_by(Bin_Accuracy,Optimizer,PSE_Difference) %>%
  mutate(BinCountAccuracy = length(Bin_Accuracy)) %>%
  group_by(Bin_Interaction,Optimizer,PSE_Difference) %>%
  mutate(BinCountInteraction = length(Bin_Interaction)) %>% 
  group_by(Optimizer,PSE_Difference) %>% 
  mutate(PercentageAccuracy = BinCountAccuracy/length(Bin_Accuracy),
         PercentageInteraction = BinCountInteraction/length(Bin_Accuracy))


FalsePositiveRate_Interaction = Dataframe_pvalues %>% 
  filter(Bin_Interaction %in% c(Bins[1]+0.025) & Effect == "No Effect") %>%
  group_by(Optimizer,PSE_Difference, Effect,Bin_Interaction) %>% 
  slice(1) %>% 
  select(Optimizer,Effect,PSE_Difference,PercentageInteraction,BinCountInteraction,Bin_Interaction) %>% 
  group_by(Optimizer) %>% 
  mutate(FalsePositiveRate = sum(PercentageInteraction)) %>% 
  slice(1) %>% 
  arrange(desc(Optimizer))

FalsePositiveRate_Accuracy = Dataframe_pvalues %>% 
  filter(Bin_Accuracy %in% c(Bins[1]+0.025) & Effect == "No Effect") %>%
  group_by(Optimizer,PSE_Difference, Effect,Bin_Accuracy) %>% 
  slice(1) %>% 
  select(Optimizer,Effect,PSE_Difference,PercentageAccuracy,BinCountAccuracy,Bin_Accuracy) %>% 
  filter(!(Optimizer %in% c("R: LRT"))) %>% 
  group_by(Optimizer) %>% 
  mutate(FalsePositiveRate = sum(PercentageAccuracy)) %>% 
  slice(1) %>% 
  arrange(desc(Optimizer))

FalsePositiveRate_Interaction2 = Dataframe_pvalues %>% 
  filter(Bin_Interaction %in% c(Bins[1]+0.025) & Effect == "Small Effect") %>%
  group_by(Optimizer,PSE_Difference, Effect,Bin_Interaction) %>% 
  slice(1) %>% 
  select(Optimizer,Effect,PSE_Difference,PercentageInteraction,BinCountInteraction,Bin_Interaction) %>% 
  group_by(Optimizer) %>% 
  mutate(FalsePositiveRate = sum(PercentageInteraction)) %>% 
  slice(1) %>% 
  arrange(desc(Optimizer))

FalsePositiveRate_Accuracy2 = Dataframe_pvalues %>% 
  filter(Bin_Accuracy %in% c(Bins[1]+0.025) & Effect == "Small Effect") %>%
  group_by(Optimizer,PSE_Difference, Effect,Bin_Accuracy) %>% 
  slice(1) %>% 
  select(Optimizer,Effect,PSE_Difference,PercentageAccuracy,BinCountAccuracy,Bin_Accuracy) %>% 
  filter(!(Optimizer %in% c("R: LRT"))) %>% 
  group_by(Optimizer) %>% 
  mutate(FalsePositiveRate = sum(PercentageAccuracy)) %>% 
  slice(1) %>% 
  arrange(desc(Optimizer))


NumberBins_Accuracy1 = length(unique((Dataframe_pvalues %>% 
                                        filter(Optimizer != "R: LRT" &
                                               Effect == "No Effect"))$BinCountAccuracy))

PlotAccuracy = ggplot(Dataframe_pvalues %>% 
                        filter(Optimizer != "R: LRT" &
                                 Effect == "No Effect"),
                      aes(Bin_Accuracy,Optimizer, fill = as.factor(BinCountAccuracy))) +
  geom_tile() +
  xlab("p value") +
  scale_fill_manual(values=colorRampPalette(c(BlauUB,Red,Yellow))(NumberBins_Accuracy1)) +
  theme(legend.position = "") +
  geom_vline(aes(xintercept = 0.05), linetype = 2,color = "white", size = 1) +
  ylab("") +
  ggtitle("A. Accuracy, No Effect") +
  annotate(geom = "text", x = 1.1, y = 6,
           label = round(FalsePositiveRate_Accuracy$FalsePositiveRate[1],2)) +
  annotate(geom = "text", x = 1.1, y = 5,
           label = round(FalsePositiveRate_Accuracy$FalsePositiveRate[2],2)) +
  annotate(geom = "text", x = 1.1, y = 4,
           label = round(FalsePositiveRate_Accuracy$FalsePositiveRate[3],2)) +
  annotate(geom = "text", x = 1.1, y = 3,
           label = round(FalsePositiveRate_Accuracy$FalsePositiveRate[4],2)) +
  annotate(geom = "text", x = 1.1, y = 2,
           label = round(FalsePositiveRate_Accuracy$FalsePositiveRate[5],2)) +
  annotate(geom = "text", x = 1.1, y = 1,
           label = round(FalsePositiveRate_Accuracy$FalsePositiveRate[6],2))
PlotAccuracy

NumberBins_Interaction1 = length(unique((Dataframe_pvalues %>% 
                                           filter(Effect == "No Effect"))$BinCountInteraction))
PlotInteraction = ggplot(Dataframe_pvalues %>% filter(Effect == "No Effect"),
                         aes(Bin_Interaction,Optimizer, fill = as.factor(BinCountInteraction))) +
  geom_tile() +
  xlab("p value") +
  scale_fill_manual(values=colorRampPalette(c(BlauUB,Red, Yellow))(NumberBins_Interaction1)) +
  theme(legend.position = "") +
  ylab("") +
  geom_vline(aes(xintercept = 0.05), linetype = 2,color = "white", size = 1) +
  ggtitle("B. Interaction, No Effect") +
  annotate(geom = "text", x = 1.1, y = 7,
           label = round(FalsePositiveRate_Interaction$FalsePositiveRate[1],2)) +  
  annotate(geom = "text", x = 1.1, y = 6,
           label = round(FalsePositiveRate_Interaction$FalsePositiveRate[2],2)) +
  annotate(geom = "text", x = 1.1, y = 5,
           label = round(FalsePositiveRate_Interaction$FalsePositiveRate[3],2)) +
  annotate(geom = "text", x = 1.1, y = 4,
           label = round(FalsePositiveRate_Interaction$FalsePositiveRate[4],2)) +
  annotate(geom = "text", x = 1.1, y = 3,
           label = round(FalsePositiveRate_Interaction$FalsePositiveRate[5],2)) +
  annotate(geom = "text", x = 1.1, y = 2,
           label = round(FalsePositiveRate_Interaction$FalsePositiveRate[6],2)) +
  annotate(geom = "text", x = 1.1, y = 1,
           label = round(FalsePositiveRate_Interaction$FalsePositiveRate[7],2))
PlotInteraction

NumberBins_Accuracy2 = length(unique((Dataframe_pvalues %>% 
                                        filter(Optimizer != "R: LRT" &
                                                 Effect == "Small Effect"))$BinCountAccuracy))
PlotAccuracy2 = ggplot(Dataframe_pvalues %>% 
                         filter(Effect == "Small Effect" &
                                  Optimizer != "R: LRT"),
                       aes(Bin_Accuracy,Optimizer, fill = as.factor(BinCountAccuracy))) +
  geom_tile() +
  geom_vline(aes(xintercept = 0.05), linetype = 2,color = "white", size = 1) +
  xlab("p value") +
  ylab("") +
  scale_fill_manual(values=colorRampPalette(c(BlauUB,Red,Yellow))(NumberBins_Accuracy2)) +
  theme(legend.position = "") +
  ggtitle("C. Accuracy, Small Effect") +
  annotate(geom = "text", x = 1.1, y = 6,
           label = round(FalsePositiveRate_Accuracy2$FalsePositiveRate[1],2)) +
  annotate(geom = "text", x = 1.1, y = 5,
           label = round(FalsePositiveRate_Accuracy2$FalsePositiveRate[2],2)) +
  annotate(geom = "text", x = 1.1, y = 4,
           label = round(FalsePositiveRate_Accuracy2$FalsePositiveRate[3],2)) +
  annotate(geom = "text", x = 1.1, y = 3,
           label = round(FalsePositiveRate_Accuracy2$FalsePositiveRate[4],2)) +
  annotate(geom = "text", x = 1.1, y = 2,
           label = round(FalsePositiveRate_Accuracy2$FalsePositiveRate[5],2)) +
  annotate(geom = "text", x = 1.1, y = 1,
           label = round(FalsePositiveRate_Accuracy2$FalsePositiveRate[6],2))


NumberBins_Interaction2 = length(unique((Dataframe_pvalues %>% 
                                           filter(Effect == "Small Effect"))$BinCountInteraction))
PlotInteraction2 = ggplot(Dataframe_pvalues %>% 
                            filter(Effect == "Small Effect"),
                          aes(Bin_Interaction,Optimizer, fill = as.factor(BinCountInteraction))) +
  geom_tile() +
  geom_vline(aes(xintercept = 0.05), linetype = 2,color = "white", size = 1) +
  xlab("p value") +
  ylab("") +
  scale_fill_manual(values=colorRampPalette(c(BlauUB,Red,Yellow))(NumberBins_Interaction2)) +
  theme(legend.position = "") +
  ggtitle("D. Interaction, Small Effect") +
  annotate(geom = "text", x = 1.1, y = 7,
           label = round(FalsePositiveRate_Interaction2$FalsePositiveRate[1],2)) +  
  annotate(geom = "text", x = 1.1, y = 6,
           label = round(FalsePositiveRate_Interaction2$FalsePositiveRate[2],2)) +
  annotate(geom = "text", x = 1.1, y = 5,
           label = round(FalsePositiveRate_Interaction2$FalsePositiveRate[3],2)) +
  annotate(geom = "text", x = 1.1, y = 4,
           label = round(FalsePositiveRate_Interaction2$FalsePositiveRate[4],2)) +
  annotate(geom = "text", x = 1.1, y = 3,
           label = round(FalsePositiveRate_Interaction2$FalsePositiveRate[5],2)) +
  annotate(geom = "text", x = 1.1, y = 2,
           label = round(FalsePositiveRate_Interaction2$FalsePositiveRate[6],2)) +
  annotate(geom = "text", x = 1.1, y = 1,
           label = round(FalsePositiveRate_Interaction2$FalsePositiveRate[7],2))
plot_grid(PlotAccuracy,PlotInteraction,PlotAccuracy2,PlotInteraction2, nrow = 2)
ggsave("Figures/(Figure 11) False Positives only R.jpg",w=12,h=6)

############AICs
ggplot(Dataframe_pvalues %>%
         filter(Optimizer != "R: LRT"),
       aes(n,Median_AIC_Ratio, color = Optimizer)) +
  geom_point(size=2) +
  geom_line(size=1) +
  facet_grid(.~nTrials) +
  ylab("AIC DIfference") +
  scale_color_manual(values = rainbow(6), name = "Method") +
  scale_x_continuous(breaks = c(10,15,20))
ggsave("Figures/(Figure 10) AIC differences only R.jpg",w=12,h=6)
