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
      reps == 60 ~ "60 repetitions"))

Dataframe_pvalues$Duration[Dataframe_pvalues$Optimizer == "Julia: BOBYQA, fast"]
Dataframe_pvalues$Duration[Dataframe_pvalues$Optimizer == "Julia: LRT, fast"]
Dataframe_pvalues$Duration[Dataframe_pvalues$Optimizer == "Julia: BOBYQA, slow"]
Dataframe_pvalues$Duration[Dataframe_pvalues$Optimizer == "Julia: LRT, slow"]
Dataframe_pvalues$Duration[Dataframe_pvalues$Optimizer == "R: nloptwrap, fast"]
Dataframe_pvalues$Duration[Dataframe_pvalues$Optimizer == "R: LRT"]


Dataframe_pvalues = Dataframe_pvalues %>%
  #  filter(Effect == "No Effect") %>% 
  group_by(reps,n,iteration,Effect) %>% 
  mutate(AIC_Ratio = AIC/AIC[2],
         Duration_LRT_Julia_Fast = Duration[Optimizer == "Julia: BOBYQA, fast"] + Duration[Optimizer == "Julia: LRT, fast"],
         Duration_LRT_Julia_Slow = Duration[Optimizer == "Julia: BOBYQA, slow"] + Duration[Optimizer == "Julia: LRT, slow"],
         Duration_LRT_R = Duration[Optimizer == "R: nloptwrap, fast"] + Duration[Optimizer == "R: LRT"]) %>% 
  group_by(reps,n,Optimizer) %>% 
  mutate(Median_AIC_Ratio = median(AIC_Ratio))

Dataframe_pvalues$Duration[Dataframe_pvalues$Optimizer == "Julia: LRT, fast"] = 
  Dataframe_pvalues$Duration_LRT_Julia_Fast[Dataframe_pvalues$Optimizer == "Julia: LRT, fast"]
Dataframe_pvalues$Duration[Dataframe_pvalues$Optimizer == "Julia: LRT, slow"] = 
  Dataframe_pvalues$Duration_LRT_Julia_Slow[Dataframe_pvalues$Optimizer == "Julia: LRT, slow"]
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
  scale_color_manual(values = c(rainbow(13)[1:11],"black","grey"), name = "Method") +
  scale_x_continuous(breaks = c(10,15,20)) +
  ggtitle("A. All Configurations")

TimingPlot2 = ggplot(Dataframe_pvalues %>% 
                       filter(Optimizer  %in% c("Julia: BOBYQA, fast",
                                                "R: nloptwrap, fast",
                                                "Julia: LRT, fast",
                                                "R: LRT",
                                                "R: nloptwrap, slow")),
                     aes(n,MeanDuration, color = Optimizer)) +
  geom_point(size=2) +
  geom_line(size=1) +
  facet_grid(.~nTrials) +
  ylab("Mean Duration (s)") +
  scale_color_manual(values = c(rainbow(13)[1:11],"black","grey")[c(1,3,9,12,13)], name = "Method") +
  scale_x_continuous(breaks = c(10,15,20)) +
  ggtitle("B. Fastest Configurations")
plot_shared_legend(TimingPlot1,TimingPlot2)

ggsave("Figures/(Supplementary Figure 1) Different Durations.jpg",w=12,h=6)


#######False Positives
Dataframe_pvalues$Bin_Accuracy = 0
Dataframe_pvalues$Bin_Interaction = 0
for (i in (1:length(Dataframe_pvalues$iteration))){
  print(i)  
  Bins = seq(0,0.95,0.05)
  Dataframe_pvalues$Bin_Accuracy[i] = Bins[which.min(abs(Bins-Dataframe_pvalues$Pvalues_Accuracy[i]))]+0.025
  Dataframe_pvalues$Bin_Interaction[i] = Bins[which.min(abs(Bins-Dataframe_pvalues$Pvalues_Interaction[i]))]+0.025
}




length(Dataframe_pvalues$Bin_Interaction[Dataframe_pvalues$Optimizer == "Julia: Nelder-Mead, slow" & 
                                           Dataframe_pvalues$Bin_Interaction == sort(unique(Dataframe_pvalues$Bin_Interaction))[3] & 
                                           Dataframe_pvalues$Effect == "No Effect"])/
  length(Dataframe_pvalues$Bin_Interaction[Dataframe_pvalues$Optimizer == "Julia: Nelder-Mead, slow" & 
                                             Dataframe_pvalues$Effect == "No Effect"])
length(Dataframe_pvalues$Bin_Interaction[Dataframe_pvalues$Optimizer == "Julia: LRT" & 
                                           Dataframe_pvalues$Bin_Interaction == Dataframe_pvalues$Bin_Interaction[40] & 
                                           Dataframe_pvalues$Effect == "No Effect"])

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
  filter(!(Optimizer %in% c("Julia: LRT","R: LRT"))) %>% 
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
  filter(!(Optimizer %in% c("Julia: LRT","R: LRT"))) %>% 
  group_by(Optimizer) %>% 
  mutate(FalsePositiveRate = sum(PercentageAccuracy)) %>% 
  slice(1) %>% 
  arrange(desc(Optimizer))


NumberBins_Accuracy1 = length(unique((Dataframe_pvalues %>% 
                                        filter(Optimizer != "Julia: LRT, slow" &
                                                 Optimizer != "Julia: LRT, fast" &
                                               Optimizer != "R: LRT" &
                                               Effect == "No Effect"))$BinCountAccuracy))

PlotAccuracy = ggplot(Dataframe_pvalues %>% 
                        filter(Optimizer != "Julia: LRT, slow" &
                                 Optimizer != "Julia: LRT, fast" &
                                 Optimizer != "R: LRT" &
                                 Effect == "No Effect"),
                      aes(Bin_Accuracy,Optimizer, fill = as.factor(BinCountAccuracy))) +
  geom_tile() +
  xlab("p value") +
  scale_fill_manual(values=colorRampPalette(c(BlauUB,Red,Yellow))(NumberBins_Accuracy1)) +
  theme(legend.position = "") +
  geom_vline(aes(xintercept = 0.05), linetype = 2,color = "white", size = 1) +
  ylab("") +
  ggtitle("A. Accuracy, No Effect") +
  annotate(geom = "text", x = 1.1, y = 10,
           label = round(FalsePositiveRate_Accuracy$FalsePositiveRate[1],2)) +
  annotate(geom = "text", x = 1.1, y = 9,
           label = round(FalsePositiveRate_Accuracy$FalsePositiveRate[2],2)) +
  annotate(geom = "text", x = 1.1, y = 8,
           label = round(FalsePositiveRate_Accuracy$FalsePositiveRate[3],2)) +
  annotate(geom = "text", x = 1.1, y = 7,
           label = round(FalsePositiveRate_Accuracy$FalsePositiveRate[4],2)) +
  annotate(geom = "text", x = 1.1, y = 6,
           label = round(FalsePositiveRate_Accuracy$FalsePositiveRate[5],2)) +
  annotate(geom = "text", x = 1.1, y = 5,
           label = round(FalsePositiveRate_Accuracy$FalsePositiveRate[6],2)) +
  annotate(geom = "text", x = 1.1, y = 4,
           label = round(FalsePositiveRate_Accuracy$FalsePositiveRate[7],2)) +
  annotate(geom = "text", x = 1.1, y = 3,
           label = round(FalsePositiveRate_Accuracy$FalsePositiveRate[8],2)) +
  annotate(geom = "text", x = 1.1, y = 2,
           label = round(FalsePositiveRate_Accuracy$FalsePositiveRate[11],2)) +
  annotate(geom = "text", x = 1.1, y = 1,
           label = round(FalsePositiveRate_Accuracy$FalsePositiveRate[12],2))
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
  annotate(geom = "text", x = 1.1, y = 13,
           label = round(FalsePositiveRate_Interaction$FalsePositiveRate[1],2)) +  
  annotate(geom = "text", x = 1.1, y = 12,
           label = round(FalsePositiveRate_Interaction$FalsePositiveRate[2],2)) +
  annotate(geom = "text", x = 1.1, y = 11,
           label = round(FalsePositiveRate_Interaction$FalsePositiveRate[3],2)) +
  annotate(geom = "text", x = 1.1, y = 10,
           label = round(FalsePositiveRate_Interaction$FalsePositiveRate[4],2)) +
  annotate(geom = "text", x = 1.1, y = 9,
           label = round(FalsePositiveRate_Interaction$FalsePositiveRate[5],2)) +
  annotate(geom = "text", x = 1.1, y = 8,
           label = round(FalsePositiveRate_Interaction$FalsePositiveRate[6],2)) +
  annotate(geom = "text", x = 1.1, y = 7,
           label = round(FalsePositiveRate_Interaction$FalsePositiveRate[7],2)) +
  annotate(geom = "text", x = 1.1, y = 6,
           label = round(FalsePositiveRate_Interaction$FalsePositiveRate[8],2)) +
  annotate(geom = "text", x = 1.1, y = 5,
           label = round(FalsePositiveRate_Interaction$FalsePositiveRate[9],2)) +
  annotate(geom = "text", x = 1.1, y = 4,
           label = round(FalsePositiveRate_Interaction$FalsePositiveRate[10],2)) +
  annotate(geom = "text", x = 1.1, y = 3,
           label = round(FalsePositiveRate_Interaction$FalsePositiveRate[11],2)) +
  annotate(geom = "text", x = 1.1, y = 2,
           label = round(FalsePositiveRate_Interaction$FalsePositiveRate[12],2)) +
  annotate(geom = "text", x = 1.1, y = 1,
           label = round(FalsePositiveRate_Interaction$FalsePositiveRate[13],2))


NumberBins_Accuracy2 = length(unique((Dataframe_pvalues %>% 
                                        filter(Optimizer != "Julia: LRT, slow" &
                                                 Optimizer != "Julia: LRT, fast" &
                                                 Optimizer != "R: LRT" &
                                                 Effect == "Small Effect"))$BinCountAccuracy))
PlotAccuracy2 = ggplot(Dataframe_pvalues %>% 
                         filter(Effect == "Small Effect" &
                                  Optimizer != "Julia: LRT, slow" &
                                  Optimizer != "Julia: LRT, fast" &
                                  Optimizer != "R: LRT"),
                       aes(Bin_Accuracy,Optimizer, fill = as.factor(BinCountAccuracy))) +
  geom_tile() +
  geom_vline(aes(xintercept = 0.05), linetype = 2,color = "white", size = 1) +
  xlab("p value") +
  ylab("") +
  scale_fill_manual(values=colorRampPalette(c(BlauUB,Red,Yellow))(NumberBins_Accuracy2)) +
  theme(legend.position = "") +
  ggtitle("C. Accuracy, Small Effect") +
  annotate(geom = "text", x = 1.1, y = 10,
           label = round(FalsePositiveRate_Accuracy2$FalsePositiveRate[1],2)) +
  annotate(geom = "text", x = 1.1, y = 9,
           label = round(FalsePositiveRate_Accuracy2$FalsePositiveRate[2],2)) +
  annotate(geom = "text", x = 1.1, y = 8,
           label = round(FalsePositiveRate_Accuracy2$FalsePositiveRate[3],2)) +
  annotate(geom = "text", x = 1.1, y = 7,
           label = round(FalsePositiveRate_Accuracy2$FalsePositiveRate[4],2)) +
  annotate(geom = "text", x = 1.1, y = 6,
           label = round(FalsePositiveRate_Accuracy2$FalsePositiveRate[5],2)) +
  annotate(geom = "text", x = 1.1, y = 5,
           label = round(FalsePositiveRate_Accuracy2$FalsePositiveRate[6],2)) +
  annotate(geom = "text", x = 1.1, y = 4,
           label = round(FalsePositiveRate_Accuracy2$FalsePositiveRate[7],2)) +
  annotate(geom = "text", x = 1.1, y = 3,
           label = round(FalsePositiveRate_Accuracy2$FalsePositiveRate[8],2)) +
  annotate(geom = "text", x = 1.1, y = 2,
           label = round(FalsePositiveRate_Accuracy2$FalsePositiveRate[11],2)) +
  annotate(geom = "text", x = 1.1, y = 1,
           label = round(FalsePositiveRate_Accuracy2$FalsePositiveRate[12],2))


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
  annotate(geom = "text", x = 1.1, y = 13,
           label = round(FalsePositiveRate_Interaction2$FalsePositiveRate[1],2)) +  
  annotate(geom = "text", x = 1.1, y = 12,
           label = round(FalsePositiveRate_Interaction2$FalsePositiveRate[2],2)) +
  annotate(geom = "text", x = 1.1, y = 11,
           label = round(FalsePositiveRate_Interaction2$FalsePositiveRate[3],2)) +
  annotate(geom = "text", x = 1.1, y = 10,
           label = round(FalsePositiveRate_Interaction2$FalsePositiveRate[4],2)) +
  annotate(geom = "text", x = 1.1, y = 9,
           label = round(FalsePositiveRate_Interaction2$FalsePositiveRate[5],2)) +
  annotate(geom = "text", x = 1.1, y = 8,
           label = round(FalsePositiveRate_Interaction2$FalsePositiveRate[6],2)) +
  annotate(geom = "text", x = 1.1, y = 7,
           label = round(FalsePositiveRate_Interaction2$FalsePositiveRate[7],2)) +
  annotate(geom = "text", x = 1.1, y = 6,
           label = round(FalsePositiveRate_Interaction2$FalsePositiveRate[8],2)) +
  annotate(geom = "text", x = 1.1, y = 5,
           label = round(FalsePositiveRate_Interaction2$FalsePositiveRate[9],2)) +
  annotate(geom = "text", x = 1.1, y = 4,
           label = round(FalsePositiveRate_Interaction2$FalsePositiveRate[10],2)) +
  annotate(geom = "text", x = 1.1, y = 3,
           label = round(FalsePositiveRate_Interaction2$FalsePositiveRate[11],2)) +
  annotate(geom = "text", x = 1.1, y = 2,
           label = round(FalsePositiveRate_Interaction2$FalsePositiveRate[12],2)) +
  annotate(geom = "text", x = 1.1, y = 1,
           label = round(FalsePositiveRate_Interaction2$FalsePositiveRate[13],2))
plot_grid(PlotAccuracy,PlotInteraction,PlotAccuracy2,PlotInteraction2, nrow = 2)
ggsave("Figures/(Supplementary Figure 3) False Positives.jpg",w=12,h=8)

############AICs
ggplot(Dataframe_pvalues %>%
         filter(Optimizer != "Julia: LRT, fast" &
                  Optimizer != "Julia: LRT, slow" &
                  Optimizer != "R: LRT"),
       aes(n,Median_AIC_Ratio, color = Optimizer)) +
  geom_point(size=2) +
  geom_line(size=1) +
  facet_grid(.~nTrials) +
  ylab("AIC DIfference") +
  scale_color_manual(values = rainbow(12), name = "Method") +
  scale_x_continuous(breaks = c(10,15,20))
ggsave("Figures/(Supplementary Figure 2) AIC differences.jpg",w=12,h=6)
