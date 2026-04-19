library(tidyverse)
library(dplyr)
library(lme4)
library(lmerTest)
library(sjPlot)
library(emmeans)


#single_task
csv_qa_list <- list.files(path = 'results_1st_single_task', 
                       pattern = "results_question_answering_single.*csv", full.names = T)

df_qa_original <- do.call(rbind, lapply(csv_qa_list, function(x) read.csv(x, col.names = c("item_id", "sentence", "sentence_with_equation", "question",
                                                                         "is_correct", "response", "correctness", "target_sentence_position", "memory_load", "condition", "plausibility", 
                                                                         "task", "arithmetic", "model", "density"
                                                                         ), header=T))) 
df_qa_original$plausibility <- relevel(factor(df_qa_original$plausibility), ref="plausible")

df_qa <- df_qa_original %>% 
  filter(response != "Error")

##### GPT-4.1 #####
summary_qa_gpt4.1_by_condition <- df_qa %>%
  filter(model == "gpt-4.1") %>%
  group_by(model, memory_load, task, condition, plausibility) %>%
  summarise(mean = mean(correctness), sd = sd(correctness))
low_accuracy_conditions_gpt4.1 <- summary_qa_gpt4.1_by_condition %>%
  filter(mean < 0.8)
print(low_accuracy_conditions_gpt4.1)

df_qa_new_gpt4.1 <- df_qa_original %>%
  filter(model == "gpt-4.1") %>%
  filter(!(condition %in% low_accuracy_conditions_gpt4.1$condition))
write.csv(df_qa_new_gpt4.1, 
          paste0(new_results_dir,"results_question_answering_single_task_gpt-4.1_three.csv"),
          row.names = F
          )


##### o4-mini #####
summary_qa_o4mini_by_condition <- df_qa %>%
  filter(model == "o4-mini") %>%
  group_by(model, memory_load, task, condition, plausibility) %>%
  summarise(mean = mean(correctness), sd = sd(correctness))
low_accuracy_conditions_o4mini <- summary_qa_o4mini_by_condition %>%
  filter(mean < 0.8)
print(low_accuracy_conditions_o4mini)

df_qa_new_o4mini <- df_qa_original %>%
  filter(model == "o4-mini") %>%
  filter(!(condition %in% low_accuracy_conditions_o4mini$condition))
write.csv(df_qa_new_o4mini, 
          paste0(new_results_dir,"results_question_answering_single_task_o4-mini_three.csv"),
          row.names = F
)

##### o3-mini ######
summary_qa_o3mini_by_condition <- df_qa %>%
  filter(model == "o3-mini") %>%
  group_by(model, memory_load, task, condition, plausibility) %>%
  summarise(mean = mean(correctness), sd = sd(correctness))
low_accuracy_conditions_o3mini <- summary_qa_o3mini_by_condition %>%
  filter(mean < 0.8)
print(low_accuracy_conditions_o3mini)

df_qa_new_o3mini <- df_qa_original %>%
  filter(model == "o3-mini") %>%
  filter(!(condition %in% low_accuracy_conditions_o3mini$condition))
write.csv(df_qa_new_o3mini, 
          paste0(new_results_dir,"results_question_answering_single_task_o3-mini_three.csv"),
          row.names = F
)

##### GPT-4o #####
summary_qa_gpt4o_by_condition <- df_qa %>%
  filter(model == "gpt-4o") %>%
  group_by(model, memory_load, task, condition, plausibility) %>%
  summarise(mean = mean(correctness), sd = sd(correctness))
low_accuracy_conditions_gpt4o <- summary_qa_gpt4o_by_condition %>%
  filter(mean < 0.8)
print(low_accuracy_conditions_gpt4o)

df_qa_new_gpt4o <- df_qa_original %>%
  filter(model == "gpt-4o") %>%
  filter(!(condition %in% low_accuracy_conditions_gpt4o$condition))
write.csv(df_qa_new_gpt4o, 
          paste0(new_results_dir,"results_question_answering_single_task_gpt-4o_three.csv"),
          row.names = F
)


###### Llama-3.3 #####
summary_qa_llama3.3_by_condition <- df_qa %>%
  filter(model == "Llama-3.3") %>%
  group_by(model, memory_load, task, condition, plausibility) %>%
  summarise(mean = mean(correctness), sd = sd(correctness))
low_accuracy_conditions_llama3.3 <- summary_qa_llama3.3_by_condition %>%
  filter(mean < 0.8)
print(low_accuracy_conditions_llama3.3)

df_qa_new_llama3.3 <- df_qa_original %>%
  filter(model == "Llama-3.3") %>%
  filter(!(condition %in% low_accuracy_conditions_o3mini$condition))
write.csv(df_qa_new_llama3.3, 
          paste0(new_results_dir,"results_question_answering_single_task_Llama-3.3_three.csv"),
          row.names = F
)

###### Gemma-3 #####
summary_qa_gemma3_by_condition <- df_qa %>%
  filter(model == "gemma-3") %>%
  group_by(model, memory_load, task, condition, plausibility) %>%
  summarise(mean = mean(correctness), sd = sd(correctness))
low_accuracy_conditions_gemma3 <- summary_qa_gemma3_by_condition %>%
  filter(mean < 0.8)
print(low_accuracy_conditions_gemma3)

df_qa_new_gemma3 <- df_qa_original %>%
  filter(model == "gemma-3") %>%
  filter(!(condition %in% low_accuracy_conditions_gemma3$condition))
write.csv(df_qa_new_gemma3, 
          paste0(new_results_dir,"results_question_answering_single_task_gemma-3_three.csv"),
          row.names = F
)

##### DeepSeek-V3 #####
summary_qa_DeepseekV3_by_condition <- df_qa %>%
  filter(model == "DeepSeek-V3") %>%
  group_by(model, memory_load, task, condition, plausibility) %>%
  summarise(mean = mean(correctness), sd = sd(correctness))
low_accuracy_conditions_DeepseekV3 <- summary_qa_DeepseekV3_by_condition %>%
  filter(mean < 0.8)
print(low_accuracy_conditions_DeepseekV3)

df_qa_new_DeepseekV3 <- df_qa_original %>%
  filter(model == "DeepSeek-V3") %>%
  filter(!(condition %in% low_accuracy_conditions_DeepseekV3$condition))
write.csv(df_qa_new_DeepseekV3, 
          paste0(new_results_dir,"results_question_answering_single_task_DeepSeek-V_three.csv"),
          row.names = F
)

