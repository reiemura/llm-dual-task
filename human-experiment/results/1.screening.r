library(MASS) #ver7.3.60
library(tidyr) #ver1.3.0
library(magrittr) #ver2.0.3
library(dplyr) #ver1.1.4
#library(pracma) #ver2.4.4
library(ggplot2) #ver3.4.4

df_original <- read.csv("df_original.csv", header=T, fill=T)

#Participant Screening
acc_qa_impsingle_by_participant <- df_original %>%
  filter(task == "single",
         plausibility == "implausible") %>%   # 条件を限定
  group_by(prolificID) %>%
  summarise(
    mean = mean(as.numeric(correctness), na.rm = TRUE),
    n_trials = n(),
    .groups = "drop"
  )
View(acc_qa_impsingle_by_participant)

#SingleのImplausibleが正答率70%未満の人は削除
bad_participants_qa <- acc_qa_impsingle_by_participant %>%
  filter(mean < 0.70) %>%
  pull(participantID)
bad_participants_qa

#Equation
acc_eq_by_participant <- df_original %>%
  filter(eq_n_trial > 0) %>%
  group_by(participantID) %>%
  summarise(
    mean = mean(eq_acc_trial),
    n_equations = sum(eq_n_trial),
    .groups = "drop"
)
View(acc_eq_by_participant)
  
# 正答率80%未満のparticipantを抽出
bad_participants_eq <- acc_eq_by_participant %>%
  filter(mean < 0.80) %>%
  pull(participantID)
bad_participants_eq

#Construction check
acc_qa_single_by_construction <- df_original %>%
  filter(!(participantID %in% bad_participants_eq)) %>%
  filter(!(participantID %in% bad_participants_qa)) %>%
  filter(task == "single") %>%
  group_by(condition, plausibility) %>%
  summarise(
    mean = mean(as.numeric(correctness), na.rm = TRUE),
    n_trials = n(),
    .groups = "drop"
  )
View(acc_qa_single_by_construction)

bad_constructions <- acc_qa_single_by_construction %>%
  filter(mean < 0.8) %>%
  pull(condition)
bad_constructions
