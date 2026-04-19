library(tidyverse)
library(dplyr)
library(tidyr)
library(purrr)
library(sjPlot)
library(emmeans)
library(Rcpp)

options(scipen = 5)
df_qa <- read.csv("df_qa.csv", header=T)

#### Test GPT-4o ####
#make dataframe
df_qa_gpt4o <- df_qa %>%
  filter(model == "gpt-4o") %>%
  mutate(
    correctness = as.integer(correctness),     # 0/1
    task = factor(task),                       # relevel if needed
    plausibility = factor(plausibility),
    item = factor(item)
  )
write.csv(df_qa_gpt4o, "df_qa_gpt4o.csv", row.names=F)

wilcox_bycond_gpt4o <- df_qa_gpt4o %>%
  group_by(condition) %>%
  group_split() %>%
  lapply(function(subdf) {
    meandf <- subdf %>%
      group_by(item, task, plausibility) %>%
      summarise(acc = mean(correctness), .groups="drop") %>%
      tidyr::pivot_wider(names_from = plausibility, values_from = acc, values_fill = NA_real_) %>%
      filter(!is.na(plausible), !is.na(implausible)) %>%
      mutate(delta = plausible - implausible) %>%
      select(-c(plausible, implausible))
    wide <- tidyr::pivot_wider(meandf, names_from = task, values_from = delta)
    
    wilcox_single <- wilcox.test(wide$`dual_task` - wide$`single_task`, alternative = "greater", exact = FALSE)
    wilcox_noisy <- wilcox.test(wide$`dual_task` - wide$`noisy_single_task`, alternative = "greater", exact = FALSE)
    
    print("----------------------------")
    print(unique(subdf$condition))
    print(wilcox_single)
    print(wilcox_noisy)
    print("----------------------------")
  }
  )


#### Test DeepSeek-V3 ####
#make dataframe
df_qa_DeepseekV3 <- df_qa %>%
  filter(model == "DeepSeek-V3") %>%
  mutate(
    correctness = as.integer(correctness),     # 0/1
    task = factor(task),                       # relevel if needed
    plausibility = factor(plausibility),
    item = factor(item)
  )
#View(df_qa_DeepseekV3)
write.csv(df_qa_DeepseekV3, "df_qa_DeepseekV3.csv", row.names=F)


wilcox_bycond_DeepseekV3 <- df_qa_DeepseekV3 %>%
  group_by(condition) %>%
  group_split() %>%
  lapply(function(subdf) {
    meandf <- subdf %>%
      group_by(item, task, plausibility) %>%
      summarise(acc = mean(correctness), .groups="drop") %>%
      tidyr::pivot_wider(names_from = plausibility, values_from = acc, values_fill = NA_real_) %>%
      filter(!is.na(plausible), !is.na(implausible)) %>%
      mutate(delta = plausible - implausible) %>%
      select(-c(plausible, implausible))
    wide <- tidyr::pivot_wider(meandf, names_from = task, values_from = delta)
    
    wilcox_single <- wilcox.test(wide$`dual_task` - wide$`single_task`, alternative = "greater", exact = FALSE)
    wilcox_noisy <- wilcox.test(wide$`dual_task` - wide$`noisy_single_task`, alternative = "greater", exact = FALSE)
    
    print("----------------------------")
    print(unique(subdf$condition))
    print(wilcox_single)
    print(wilcox_noisy)
    print("----------------------------")
  }
  )

#### Test o3-mini ####
#make dataframe
df_qa_o3mini <- df_qa %>%
  filter(model == "o3-mini") %>%
  mutate(
    correctness = as.integer(correctness),     # 0/1
    task = factor(task),                       # relevel if needed
    plausibility = factor(plausibility),
    item = factor(item)
  )
#View(df_qa_o3mini)
write.csv(df_qa_o3mini, "df_qa_o3mini.csv", row.names=F)

wilcox_bycond_o3mini <- df_qa_o3mini %>%
  group_by(condition) %>%
  group_split() %>%
  lapply(function(subdf) {
    meandf <- subdf %>%
      group_by(item, task, plausibility) %>%
      summarise(acc = mean(correctness), .groups="drop") %>%
      tidyr::pivot_wider(names_from = plausibility, values_from = acc, values_fill = NA_real_) %>%
      filter(!is.na(plausible), !is.na(implausible)) %>%
      mutate(delta = plausible - implausible) %>%
      select(-c(plausible, implausible))
    wide <- tidyr::pivot_wider(meandf, names_from = task, values_from = delta)
    
    wilcox_single <- wilcox.test(wide$`dual_task` - wide$`single_task`, alternative = "greater", exact = FALSE)
    wilcox_noisy <- wilcox.test(wide$`dual_task` - wide$`noisy_single_task`, alternative = "greater", exact = FALSE)
    
    print("----------------------------")
    print(unique(subdf$condition))
    print(wilcox_single)
    print(wilcox_noisy)
    print("----------------------------")
  }
  )

#### Test o4-mini ####
#make dataframe
df_qa_o4mini <- df_qa %>%
  filter(model == "o4-mini") %>%
  mutate(
    correctness = as.integer(correctness),     # 0/1
    task = factor(task),                       # relevel if needed
    plausibility = factor(plausibility),
    item = factor(item)
  )
#View(df_qa_o4mini)
write.csv(df_qa_o4mini, "df_qa_o4mini.csv", row.names=F)

wilcox_bycond_o4mini <- df_qa_o4mini %>%
  group_by(condition) %>%
  group_split() %>%
  lapply(function(subdf) {
    meandf <- subdf %>%
      group_by(item, task, plausibility) %>%
      summarise(acc = mean(correctness), .groups="drop") %>%
      tidyr::pivot_wider(names_from = plausibility, values_from = acc, values_fill = NA_real_) %>%
      filter(!is.na(plausible), !is.na(implausible)) %>%
      mutate(delta = plausible - implausible) %>%
      select(-c(plausible, implausible))
    wide <- tidyr::pivot_wider(meandf, names_from = task, values_from = delta)
    
    wilcox_single <- wilcox.test(wide$`dual_task` - wide$`single_task`, alternative = "greater", exact = FALSE)
    wilcox_noisy <- wilcox.test(wide$`dual_task` - wide$`noisy_single_task`, alternative = "greater", exact = FALSE)
    
    print("----------------------------")
    print(unique(subdf$condition))
    print(wilcox_single)
    print(wilcox_noisy)
    print("----------------------------")
  }
  )

#### Test GPT-4.1 ####
#make dataframe
df_qa_gpt4.1 <- df_qa %>%
  filter(model == "gpt-4.1") %>%
  mutate(
    correctness = as.integer(correctness),     # 0/1
    task = factor(task),                       # relevel if needed
    plausibility = factor(plausibility),
    item = factor(item)
  )
#View(df_qa_gpt4.1)
write.csv(df_qa_gpt4.1, "df_qa_gpt4.1.csv", row.names=F)

wilcox_bycond_gpt4.1 <- df_qa_gpt4.1 %>%
  group_by(condition) %>%
  group_split() %>%
  lapply(function(subdf) {
    meandf <- subdf %>%
      group_by(item, task, plausibility) %>%
      summarise(acc = mean(correctness), .groups="drop") %>%
      tidyr::pivot_wider(names_from = plausibility, values_from = acc, values_fill = NA_real_) %>%
      filter(!is.na(plausible), !is.na(implausible)) %>%
      mutate(delta = plausible - implausible) %>%
      select(-c(plausible, implausible))
    wide <- tidyr::pivot_wider(meandf, names_from = task, values_from = delta)
    
    wilcox_single <- wilcox.test(wide$`dual_task` - wide$`single_task`, alternative = "greater", exact = FALSE)
    wilcox_noisy <- wilcox.test(wide$`dual_task` - wide$`noisy_single_task`, alternative = "greater", exact = FALSE)
    
    print("----------------------------")
    print(unique(subdf$condition))
    print(wilcox_single)
    print(wilcox_noisy)
    print("----------------------------")
  }
  )

#### Test Gemma-3 ####
#make dataframe
df_qa_gemma3 <- df_qa %>% 
  filter(model == "gemma-3") %>%
  mutate(
    correctness = as.integer(correctness),     # 0/1
    task = factor(task),                       # relevel if needed
    plausibility = factor(plausibility),
    item = factor(item)
  )
#View(df_qa_gemma3)
write.csv(df_qa_gemma3, "df_qa_gemma3.csv", row.names=F)

wilcox_bycond_gemma3 <- df_qa_gemma3 %>%
  group_by(condition) %>%
  group_split() %>%
  lapply(function(subdf) {
    meandf <- subdf %>%
      group_by(item, task, plausibility) %>%
      summarise(acc = mean(correctness), .groups="drop") %>%
      tidyr::pivot_wider(names_from = plausibility, values_from = acc, values_fill = NA_real_) %>%
      filter(!is.na(plausible), !is.na(implausible)) %>%
      mutate(delta = plausible - implausible) %>%
      select(-c(plausible, implausible))
    wide <- tidyr::pivot_wider(meandf, names_from = task, values_from = delta)
    
    wilcox_single <- wilcox.test(wide$`dual_task` - wide$`single_task`, alternative = "greater", exact = FALSE)
    wilcox_noisy <- wilcox.test(wide$`dual_task` - wide$`noisy_single_task`, alternative = "greater", exact = FALSE)
    
    print("----------------------------")
    print(unique(subdf$condition))
    print(wilcox_single)
    print(wilcox_noisy)
    print("----------------------------")
  }
  )


#### Test Llama-3.3 ####
#make dataframe
df_qa_llama3.3 <- df_qa %>%
  filter(model == "Llama-3.3") %>%
  mutate(
    correctness = as.integer(correctness),     # 0/1
    task = factor(task),                       # relevel if needed
    plausibility = factor(plausibility),
    item = factor(item)
  )
#View(df_qa_llama3.3)
write.csv(df_qa_llama3.3, "df_qa_llama3.3.csv", row.names=F)

wilcox_bycond_llama3.3 <- df_qa_llama3.3 %>%
  group_by(condition) %>%
  group_split() %>%
  lapply(function(subdf) {
    meandf <- subdf %>%
      group_by(item, task, plausibility) %>%
      summarise(acc = mean(correctness), .groups="drop") %>%
      tidyr::pivot_wider(names_from = plausibility, values_from = acc, values_fill = NA_real_) %>%
      filter(!is.na(plausible), !is.na(implausible)) %>%
      mutate(delta = plausible - implausible) %>%
      select(-c(plausible, implausible))
    wide <- tidyr::pivot_wider(meandf, names_from = task, values_from = delta)
    
    wilcox_single <- wilcox.test(wide$`dual_task` - wide$`single_task`, alternative = "greater", exact = FALSE)
    wilcox_noisy <- wilcox.test(wide$`dual_task` - wide$`noisy_single_task`, alternative = "greater", exact = FALSE)
    
    print("----------------------------")
    print(unique(subdf$condition))
    print(wilcox_single)
    print(wilcox_noisy)
    print("----------------------------")
  }
  )


