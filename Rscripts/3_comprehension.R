library(tidyverse)
library(dplyr)
library(Matrix)
library(lme4)
library(lmerTest)
library(sjPlot)
library(emmeans)

csv_qa_list<- list.files(path = 'results', 
                          pattern = "results_question_answering.*csv", full.names = T)

df_qa_original <- do.call(rbind, lapply(csv_qa_list, function(x) read.csv(x, col.names = c("item_id", "sentence", "sentence_with_equation", "question",
                                                                                                         "is_correct", "response", "correctness", "target_sentence_position", "memory_load", "condition", "plausibility", 
                                                                                                         "task", "arithmetic", "model", "density"
                                                                                                         ), header=T))) 
write.csv(df_qa_original, "df_qa_original.csv", row.names=F)


##### Filter ####
df_qa_original <- read.csv("df_qa_original.csv")
df_qa <- df_qa_original %>%
  #Exclude arithmetics lower than 60% accuracy
  anti_join(bad_models, by=c("model","arithmetic")) %>%
  #Exclude trials with incorrect equation answers
  anti_join(bad_answers_trials, by = c("item_id", "model", "task", "arithmetic")) %>%
  #Exclude trials with errors for question
  filter(response != "Error") %>%
df_qa$plausibility <- relevel(factor(df_qa$plausibility), ref="plausible")
df_qa$task <- relevel(factor(df_qa$task), ref="dual_task")

df_qa$task <- relevel(factor(df_qa$task), ref="dual_task")
df_qa$plausibility <- relevel(factor(df_qa$plausibility), ref="plausible")
#item ID
df_qa <- df_qa %>%
  mutate(item = as.integer(factor(question)))
df_qa <- df_qa %>%
  mutate(arithmetic = factor(arithmetic, levels = c("none",
                                                    "1digit_2addition", "1digit_3addition",
                                                    "3digit_2addition", "3digit_3addition",
                                                    "5digit_2addition", "5digit_3addition",
                                                    "10digit_2addition", "10digit_3addition",
                                                    "30digit_2addition", "30digit_3addition"))) 


write.csv(df_qa, "df_qa.csv", row.names=F)

df_qa_2add <- df_qa %>%
  filter(grepl(".*_2addition",arithmetic) | arithmetic == "none")
write.csv(df_qa_2add, "df_qa_2add.csv", row.names=F)

df_qa_3add <- df_qa_2add3add %>%
  filter(grepl(".*_3addition",arithmetic) | arithmetic == "none")
write.csv(df_qa_3add, "df_qa_3add.csv", row.names=F)


###### GPT4.1 #####
df_qa <- read.csv("df_qa.csv")
df_qa_gpt4.1 <- df_qa %>%
  filter(model == "gpt-4.1")

write.csv(df_qa_gpt4.1, "df_qa_gpt4.1.csv", row.names=F)

summary_qa_gpt4.1 <- df_qa_gpt4.1 %>%
  group_by(model, memory_load, arithmetic, task, plausibility) %>%
  summarise(mean = mean(correctness), sd = sd(correctness))


summary_gap_gpt4.1 <- summary_qa_gpt4.1 %>%
  pivot_wider(names_from = plausibility, values_from = c(mean, sd)) %>%
  mutate(mean_gap = mean_plausible - mean_implausible)



###### GPT4o #####
df_qa <- read.csv("df_qa.csv")
df_qa_gpt4o <- df_qa %>%
  filter(model == "gpt-4o")
write.csv(df_qa_gpt4o, "df_qa_gpt4o.csv", row.names=F)

summary_qa_gpt4o <- df_qa_gpt4o %>%
  group_by(model, memory_load, arithmetic, task, plausibility) %>%
  summarise(mean = mean(correctness), sd = sd(correctness), n=n())
#View(summary_qa_gpt4o)


summary_gap_gpt4o <- summary_qa_gpt4o %>%
  pivot_wider(names_from = plausibility, values_from = c(mean, sd)) %>%
  mutate(mean_gap = mean_plausible - mean_implausible)
#View(summary_gap_gpt4o)

#dual_taskでのみcorrectness==0となるitemを抽出する
#single_task & correctness==1のitemを特定
items_single_corr <- df_qa_gpt4o %>%
  filter(plausibility == "implausible") %>%
  filter(task == "single_task" & correctness == 1) %>%
  distinct(item)

#さらに抽出
items_selected <-  df_qa_gpt4o %>%
  filter(plausibility == "implausible") %>%
  filter(item %in% items_single_corr$item) %>%
  filter(task == "noisy_single_task" | task == "dual_task") %>%
  group_by(item, arithmetic) %>%
  filter(
    any(task == "noisy_single_task" & correctness == 1) &
      any(task == "dual_task" & correctness == 0)) %>%
  ungroup() %>%
  distinct(item, arithmetic)

df_implausible_incorrect_gpt4o <- df_qa_gpt4o %>%
  filter(plausibility == "implausible") %>%
  semi_join(items_selected, by=c("item", "arithmetic")) %>%
  filter(task == "dual_task")
write.csv(df_implausible_incorrect_gpt4o, "df_implausible_incorrect_gpt4o.csv", row.names=F)


###### Gemma-3 #####
df_qa_gemma3 <- df_qa %>%
  filter(model == "gemma-3")
write.csv(df_qa_gemma3, "df_qa_gemma3.csv", row.names=F)

summary_qa_gemma3 <- df_qa_gemma3 %>%
  group_by(model, memory_load, arithmetic, task, plausibility) %>%
  summarise(mean = mean(correctness), sd = sd(correctness))
#View(summary_qa_gemma3)

summary_gap_gemma3 <- summary_qa_gemma3 %>%
  pivot_wider(names_from = plausibility, values_from = c(mean, sd)) %>%
  mutate(mean_gap = mean_plausible - mean_implausible)
#View(summary_gap_gemma3)


###### DeepSeek-V3 #####
df_qa_DeepseekV3 <- df_qa %>%
  filter(model == "DeepSeek-V3")
write.csv(df_qa_DeepseekV3, "df_qa_DeepseekV3.csv", row.names=F)

summary_qa_DeepseekV3 <- df_qa_DeepseekV3 %>%
  group_by(model, memory_load, arithmetic, task, plausibility) %>%
  summarise(mean = mean(correctness), sd = sd(correctness))
#View(summary_qa_DeepseekV3)

summary_gap_DeepseekV3 <- summary_qa_DeepseekV3 %>%
  pivot_wider(names_from = plausibility, values_from = c(mean, sd)) %>%
  mutate(mean_gap = mean_plausible - mean_implausible)
#View(summary_gap_DeepseekV3)



###### o4-mini #####
df_qa_o4mini <- df_qa %>%
  filter(model == "o4-mini")
write.csv(df_qa_o4mini, "df_qa_o4mini.csv", row.names=F)

summary_qa_o4mini <- df_qa_o4mini %>%
  group_by(model, memory_load, arithmetic, task, plausibility) %>%
  summarise(mean = mean(correctness), sd = sd(correctness))
#View(summary_qa_o4mini)

summary_gap_o4mini <- summary_qa_o4mini %>%
  pivot_wider(names_from = plausibility, values_from = c(mean, sd)) %>%
  mutate(mean_gap = mean_plausible - mean_implausible)
#View(summary_gap_o4mini)

#dual_taskでのみcorrectness==0となるitemを抽出する
#single_task & correctness==1のitemを特定
items_single_corr <- df_qa_o4mini %>%
  filter(plausibility == "implausible") %>%
  filter(task == "single_task" & correctness == 1) %>%
  distinct(item)
#さらに抽出
items_selected <-  df_qa_o4mini %>%
  filter(plausibility == "implausible") %>%
  filter(item %in% items_single_corr$item) %>%
  filter(task == "noisy_single_task" | task == "dual_task") %>%
  group_by(item, arithmetic) %>%
  filter(
    any(task == "noisy_single_task" & correctness == 1) &
      any(task == "dual_task" & correctness == 0)) %>%
  ungroup() %>%
  distinct(item, arithmetic)

df_implausible_incorrect_o4mini <- df_qa_o4mini %>%
  filter(plausibility == "implausible") %>%
  semi_join(items_selected, by=c("item", "arithmetic")) %>%
  filter(task == "dual_task")
write.csv(df_implausible_incorrect_o4mini, "df_implausible_incorrect_o4mini.csv", row.names=F)



###### o3-mini #####
df_qa_o3mini <- df_qa %>%
  filter(model == "o3-mini")
write.csv(df_qa_o3mini, "df_qa_o3mini.csv", row.names=F)

summary_qa_o3mini <- df_qa_o3mini %>%
  group_by(model, memory_load, arithmetic, task, plausibility) %>%
  summarise(mean = mean(correctness), sd = sd(correctness))
#View(summary_qa_o3mini)

summary_gap_o3mini <- summary_qa_o3mini %>%
  pivot_wider(names_from = plausibility, values_from = c(mean, sd)) %>%
  mutate(mean_gap = mean_plausible - mean_implausible)
#View(summary_gap_o3mini)

#dual_taskでのみcorrectness==0となるitemを抽出する
#single_task & correctness==1のitemを特定
items_single_corr <- df_qa_o3mini %>%
  filter(plausibility == "implausible") %>%
  filter(task == "single_task" & correctness == 1) %>%
  distinct(item)

#さらに抽出
items_selected <-  df_qa_o3mini %>%
  filter(plausibility == "implausible") %>%
  filter(item %in% items_single_corr$item) %>%
  filter(task == "noisy_single_task" | task == "dual_task") %>%
  group_by(item, arithmetic) %>%
  filter(
    any(task == "noisy_single_task" & correctness == 1) &
      any(task == "dual_task" & correctness == 0)) %>%
  ungroup() %>%
  distinct(item, arithmetic)

df_implausible_incorrect_o3mini <- df_qa_o3mini %>%
  filter(plausibility == "implausible") %>%
  semi_join(items_selected, by=c("item", "arithmetic")) %>%
  filter(task == "dual_task")
write.csv(df_implausible_incorrect_o3mini, "df_implausible_incorrect_o3mini.csv", row.names=F)



###### Llama-3.3 #####
df_qa_llama3.3 <- df_qa %>%
  filter(model == "Llama-3.3")
write.csv(df_qa_llama3.3, "df_qa_llama3.3.csv", row.names=F)

summary_qa_llama3.3 <- df_qa_llama3.3 %>%
  group_by(model, memory_load, arithmetic, task, plausibility) %>%
  summarise(mean = mean(correctness), sd = sd(correctness))
#View(summary_qa_llama3.3)

summary_gap_llama3.3 <- summary_qa_llama3.3 %>%
  pivot_wider(names_from = plausibility, values_from = c(mean, sd)) %>%
  mutate(mean_gap = mean_plausible - mean_implausible)
#View(summary_gap_llama3.3)



