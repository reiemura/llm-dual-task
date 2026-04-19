library(tidyverse)
library(dplyr)
library(Matrix)
library(lme4)
library(lmerTest)
library(sjPlot)
library(emmeans)


df_original <- read.csv("df_original.csv", header=T, fill=T)

##### Data screening ####
df_qa <- df_original %>%
  #Exclude participants
  filter(!(participantID %in% bad_participants_eq)) %>%
  filter(!(participantID %in% bad_participants_qa)) %>%
  # Exclude Constructions
  filter(!(condition %in% bad_constructions)) %>%
  #Exclude trials with incorrect equation answers
  filter(eq_acc_trial == 1 | is.na(eq_acc_trial)) %>%
  mutate(task = recode(task,
                       "single" = "single_task", 
                       "noisy-single" = "noisy_single_task",
                       "dual" = "dual_task")) 

df_qa$plausibility <- relevel(factor(df_qa$plausibility), ref="plausible")
df_qa$task <- relevel(factor(df_qa$task), ref="dual_task")

write.csv(df_qa, "df_qa.csv", row.names=F)


###### SUMMARISE  #####
summary_qa <- df_qa %>%
  group_by(task, plausibility) %>%
  summarise(mean = mean(correctness), sd = sd(correctness))
View(summary_qa)

summary_qa_byconstruction <- df_qa %>%
  group_by(condition, task, plausibility) %>%
  summarise(mean = mean(correctness), sd = sd(correctness))
View(summary_qa_byconstruction)

summary_gap <- summary_qa %>%
  pivot_wider(names_from = plausibility, values_from = c(mean, sd)) %>%
  mutate(mean_gap = mean_plausible - mean_implausible)
View(summary_gap)

participant_list <- df_qa %>%
  distinct(participantID, age, sex)
participant_list
mean(participant_list$age) #39.07143
sd(participant_list$age) #12.3005
table(participant_list$sex)

