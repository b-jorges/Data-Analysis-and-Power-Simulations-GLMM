require(dplyr)
require(quickpsy)
require(MASS)

Where_Am_I <- function(path=T){
  if (path == T){
    dirname(rstudioapi::getSourceEditorContext()$path)
  }
  else {
    rstudioapi::getSourceEditorContext()$path
  }
}
setwd(Where_Am_I()) #set path of this script as working directory

###get pilot data
Dataframe <- read.csv(header=TRUE,"PilotData.csv")

######Get psychometric functions using quickpsy
Dataframe = Dataframe %>% 
  mutate(
    Pest_Bigger = case_when(
      Response_Interval == Pest_Interval ~ 1,
      Response_Interval != Pest_Interval ~ 0),
    Difference = abs(velH_Pest)-abs(velH),
    Congruent = case_when(
      velH*velH_Subject < 0 ~ "incongruent",
      velH*velH_Subject > 0 ~ "congruent",
      velH*velH_Subject == 0 ~ "1no motion")) %>%
  filter(abs(velH_Pest) < abs(velH)*2 & Congruent != "1no motion")

PsychometricFunctions = quickpsy(Dataframe,Difference,Pest_Bigger,
                                                    grouping = .(Congruent,participant,velH),
                                                    bootstrap = "none")
plot(PsychometricFunctions)

PSEs = PsychometricFunctions$par %>% 
  filter(parn == "p1" & Congruent != "congruent")

SDs = PsychometricFunctions$par %>% 
  filter(parn == "p2" & Congruent != "congruent")

######PSE Difference
PSEs_Condition1_Absolute = (PSEs %>% filter(Congruent == "incongruent"))$par
velHs_Condition1 = abs((PSEs %>% filter(Congruent == "incongruent"))$velH)
PSEs_Condition1_Percentage = (PSEs_Condition1_Absolute)/velHs_Condition1
Mean_PSE_Condition1_Percentage = mean(PSEs_Condition1_Percentage)

PSEs_Condition2_Absolute = (PSEs %>% filter(Congruent == "1no motion"))$par
velHs_Condition2 = abs((PSEs %>% filter(Congruent == "1no motion"))$velH)
PSEs_Condition2_Percentage = (PSEs_Condition2_Absolute)/velHs_Condition2
Mean_PSE_Condition2_Percentage = mean(PSEs_Condition2_Percentage)

PSE_Difference = Mean_PSE_Condition1_Percentage-Mean_PSE_Condition2_Percentage
PSE_Difference


#######JND Difference
SDs_Condition1_Absolute = (SDs %>% filter(Congruent == "incongruent"))$par
velHs_Condition1 = abs((SDs %>% filter(Congruent == "incongruent"))$velH)
SDs_Condition1_Percentage = (SDs_Condition1_Absolute)/velHs_Condition1
Mean_SD_Condition1_Percentage = mean(SDs_Condition1_Percentage)

SDs_Condition2_Absolute = (SDs %>% filter(Congruent == "1no motion"))$par
velHs_Condition2 = abs((SDs %>% filter(Congruent == "1no motion"))$velH)
SDs_Condition2_Percentage = (SDs_Condition2_Absolute)/velHs_Condition2
Mean_SD_Condition2_Percentage = mean(SDs_Condition2_Percentage)

#Standard deviations are proportional to JNDs, so the percentages here are the same for both
JND_Difference = SDs_Difference = Mean_SD_Condition1_Percentage-Mean_SD_Condition2_Percentage
JND_Difference


########Mean Standard
Mean_Standard = mean(PSEs_Condition2_Percentage)

####Multiplicator_SD_Standard
Multiplicator_SD_Standard = mean(SDs_Condition2_Percentage)

####Mean_Variability_Between
sd(PSEs_Condition2_Percentage)

####SD_Variability_Between
sd(SDs_Condition2_Percentage)


####SD_ResponseFunction ... fitting Cauchy and normal functions!
ResponseDistribution = Dataframe %>% 
  group_by(participant,Congruent, velH) %>%
  mutate(Scale_Cauchy = fitdistr(velH_Pest/velH,"cauchy")$estimate[2],
         SD_Normal = fitdistr(velH_Pest/velH,"normal")$estimate[2],
         loglikelihood_Cauchy = fitdistr(velH_Pest/velH,"cauchy")$loglik,
         loglikelihood_Normal = fitdistr(velH_Pest/velH,"normal")$loglik,
         loglikelihood_Difference = loglikelihood_Cauchy-loglikelihood_Normal) %>% 
  dplyr::select(participant,Scale_Cauchy,loglikelihood_Cauchy,SD_Normal,loglikelihood_Normal, loglikelihood_Difference) %>% 
  slice(1) %>%
  ungroup() %>% 
  summarise(median_Scale_Cauchy = median(Scale_Cauchy),
            median_SD_Normal = median(SD_Normal),
            median_loglike_CauchyMinusNormal = median(loglikelihood_Difference))

if (ResponseDistribution[3] > 0){
  SD_ResponseFunction = ResponseDistribution[1] 
} else {
  SD_ResponseFunction = ResponseDistribution[1] 
}


