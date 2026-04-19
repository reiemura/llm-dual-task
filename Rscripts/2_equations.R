library(tidyverse)
library(Matrix)
library(dplyr)
library(lme4)
library(lmerTest)
library(sjPlot)
library(emmeans)


#Accuracy of Equation
csv_eq_list <- list.files(path = 'results', 
                          pattern = "results_equation.*csv", full.names = T)

df_eq_original <- do.call(rbind, lapply(csv_eq_list, function(x) 
  read.csv(x, col.names = c("equation_id", "equation", "result", "responded_equation", "response", "correctness",
                         "task", "arithmetic", "model", "density",
                         "item_id", "sentence", "question",
                         "is_correct", "target_sentence_position", "memory_load", "condition", "plausibility"
                         ), header=T))) 

##### Accuracy by model and math problem ####
df_eq <- df_eq_original %>% filter(response != "NoAnswer")
write.csv(df_eq, "df_eq.csv", row.names=F)


df_eq <- read.csv("df_eq.csv", header=T)
summary_eq_by_model <- df_eq %>%
  group_by(model, task, arithmetic) %>%
  summarise(mean = mean(correctness), sd = sd(correctness))
View(summary_eq_by_model)
write.csv(summary_eq_by_model, "summary_eq_by_model.csv", row.names=F)
bad_models <- summary_eq_by_model %>%
  filter(mean < 0.6)
View(bad_models)
write.csv(bad_models, "bad_models.csv", row.names=F)

#No Answer Rates
noAnswer_rate_by_model <- df_eq_original %>%
  group_by(model, task, arithmetic) %>%
  summarise(noAnswer = sum(response =="NoAnswer") / n(),
            .groups = "drop")
View(noAnswer_rate_by_model)

  
#### Exclude NoAnswer & Incorrect trials ####

bad_answers_trials <- df_eq_original %>%
  filter(response == "NoAnswer" | correctness == 0) %>%
  select(item_id, model, task, arithmetic, response, correctness)
View(bad_answers_trials)

