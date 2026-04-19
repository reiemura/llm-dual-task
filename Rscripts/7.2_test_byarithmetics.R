library(tidyverse)
library(dplyr)
library(tidyr)
library(purrr)
library(sjPlot)
library(emmeans)
library(Rcpp)


df_qa <- read.csv("df_qa.csv")

arithmetic_list <- list(c("1digit_2addition", "1digit_3addition",
                     "3digit_2addition", "3digit_3addition", 
                     "5digit_2addition", "5digit_3addition", 
                     "10digit_2addition", "10digit_3addition", 
                     "30digit_2addition", "30digit_3addition"))
arithmetic_list_gpt4o <- list(c("1digit_2addition", "1digit_3addition",
                          "3digit_2addition", "3digit_3addition", 
                          "5digit_2addition", "5digit_3addition", 
                          "10digit_2addition", 
                          "30digit_2addition"))
arithmetic_list_DeepSeek <- list(c("1digit_2addition", "1digit_3addition",
                          "3digit_2addition", "3digit_3addition", 
                          "5digit_2addition", "5digit_3addition", 
                          "10digit_2addition", "10digit_3addition", 
                          "30digit_2addition"))
arithmetic_list_gemma3 <- list(c("1digit_2addition", "1digit_3addition",
                          "3digit_2addition", "3digit_3addition", 
                          "5digit_2addition", "5digit_3addition"))
arithmetic_list_llama3.3 <- list(c("1digit_2addition", "1digit_3addition",
                                   "3digit_2addition",  
                                   "5digit_2addition", 
                                   "10digit_2addition"))

arithmetic_level <- c("1digit_2addition", "1digit_3addition", 
                      "3digit_2addition", "3digit_3addition", 
                      "5digit_2addition", "5digit_3addition", 
                      "10digit_2addition", "10digit_3addition", 
                      "30digit_2addition", "30digit_3addition")

#### Test GPT-4o ####
#make dataframe
df_qa_gpt4o <- read.csv("df_qa_gpt4o.csv", header=T)

df_qa_byarith_add_gpt4o <- df_qa_gpt4o %>%
  filter(task == "single_task") %>%
  mutate(arithmetic = arithmetic_list_gpt4o) %>%
  tidyr::unnest(arithmetic) 

df_qa_byarith_gpt4o <- df_qa_gpt4o %>%
  bind_rows(df_qa_byarith_add_gpt4o) %>%
  filter(!(task == "single_task" & arithmetic == "none")) %>%
  mutate(
    correctness = as.integer(correctness),     # 0/1
    task = factor(task),                       # relevel if needed
    plausibility = factor(plausibility),
    item = factor(item),
    arithmetic = factor(arithmetic, levels=arithmetic_level)
  )


wilcox_byarith_gpt4o <- df_qa_byarith_gpt4o %>%
  group_by(arithmetic) %>%
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
    print(unique(subdf$arithmetic))
    print(wilcox_single)
    print(wilcox_noisy)
    print("----------------------------")
  }
  )


#### GLMM DeepSeek-V3 ####
#make dataframe
df_qa_DeepseekV3 <- read.csv("df_qa_DeepseekV3.csv", header=T)
df_qa_byarith_add_DeepseekV3 <- df_qa_DeepseekV3 %>%
  filter(task == "single_task") %>%
  mutate(arithmetic = arithmetic_list_DeepSeek) %>%
  tidyr::unnest(arithmetic) 

df_qa_byarith_DeepseekV3 <- df_qa_DeepseekV3 %>%
  bind_rows(df_qa_byarith_add_DeepseekV3) %>%
  filter(!(task == "single_task" & arithmetic == "none")) %>%
  mutate(
    correctness = as.integer(correctness),     # 0/1
    task = factor(task),                       # relevel if needed
    plausibility = factor(plausibility),
    item = factor(item),
    arithmetic = factor(arithmetic, levels=arithmetic_level)
  )


wilcox_byarith_DeepseekV3 <- df_qa_byarith_DeepseekV3 %>%
  group_by(arithmetic) %>%
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
    print(unique(subdf$arithmetic))
    print(wilcox_single)
    print(wilcox_noisy)
    print("----------------------------")
  }
  )


#### GLMM o3-mini ####
#make dataframe
df_qa_o3mini <- read.csv("df_qa_o3mini.csv", header=T)
df_qa_byarith_add_o3mini <- df_qa_o3mini %>%
  filter(task == "single_task") %>%
  mutate(arithmetic = arithmetic_list_DeepSeek) %>%
  tidyr::unnest(arithmetic) 

df_qa_byarith_o3mini <- df_qa_o3mini %>%
  bind_rows(df_qa_byarith_add_o3mini) %>%
  filter(!(task == "single_task" & arithmetic == "none")) %>%
  mutate(
    correctness = as.integer(correctness),     # 0/1
    task = factor(task),                       # relevel if needed
    plausibility = factor(plausibility),
    item = factor(item),
    arithmetic = factor(arithmetic, levels=arithmetic_level)
  )

wilcox_byarith_o3mini <- df_qa_byarith_o3mini %>%
  group_by(arithmetic) %>%
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
    print(unique(subdf$arithmetic))
    print(wilcox_single)
    print(wilcox_noisy)
    print("----------------------------")
  }
  )

#### GLMM o4-mini ####
#make dataframe
df_qa_o4mini <- read.csv("df_qa_o4mini.csv", header=T)
df_qa_byarith_add_o4mini <- df_qa_o4mini %>%
  filter(task == "single_task") %>%
  mutate(arithmetic = arithmetic_list) %>%
  tidyr::unnest(arithmetic) 

df_qa_byarith_o4mini <- df_qa_o4mini %>%
  bind_rows(df_qa_byarith_add_o4mini) %>%
  filter(!(task == "single_task" & arithmetic == "none")) %>%
  mutate(
    correctness = as.integer(correctness),     # 0/1
    task = factor(task),                       # relevel if needed
    plausibility = factor(plausibility),
    item = factor(item),
    arithmetic = factor(arithmetic, levels=arithmetic_level)
  )

wilcox_byarith_o4mini <- df_qa_byarith_o4mini %>%
  group_by(arithmetic) %>%
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
    print(unique(subdf$arithmetic))
    print(wilcox_single)
    print(wilcox_noisy)
    print("----------------------------")
  }
  )

#### GLMM GPT-4.1 ####
#make dataframe
df_qa_gpt4.1 <- read.csv("df_qa_gpt4.1.csv", header=T)
df_qa_byarith_add_gpt4.1 <- df_qa_gpt4.1 %>%
  filter(task == "single_task") %>%
  mutate(arithmetic = arithmetic_list_gpt4o) %>%
  tidyr::unnest(arithmetic) 

df_qa_byarith_gpt4.1 <- df_qa_gpt4.1 %>%
  bind_rows(df_qa_byarith_add_gpt4.1) %>%
  filter(!(task == "single_task" & arithmetic == "none")) %>%
  mutate(
    correctness = as.integer(correctness),     # 0/1
    task = factor(task),                       # relevel if needed
    plausibility = factor(plausibility),
    item = factor(item),
    arithmetic = factor(arithmetic, levels=arithmetic_level)
  )

wilcox_byarith_gpt4.1 <- df_qa_byarith_gpt4.1 %>%
  group_by(arithmetic) %>%
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
    print(unique(subdf$arithmetic))
    print(wilcox_single)
    print(wilcox_noisy)
    print("----------------------------")
  }
  )

#### GLMM Gemma-3 ####
#make dataframe
df_qa_gemma3 <- read.csv("df_qa_gemma3.csv", header=T)
df_qa_byarith_add_gemma3 <- df_qa_gemma3 %>%
  filter(task == "single_task") %>%
  mutate(arithmetic = arithmetic_list_gemma3) %>%
  tidyr::unnest(arithmetic) 

df_qa_byarith_gemma3 <- df_qa_gemma3 %>%
  bind_rows(df_qa_byarith_add_gemma3) %>%
  filter(!(task == "single_task" & arithmetic == "none")) %>%
  mutate(
    correctness = as.integer(correctness),     # 0/1
    task = factor(task),                       # relevel if needed
    plausibility = factor(plausibility),
    item = factor(item),
    arithmetic = factor(arithmetic, levels=arithmetic_level)
  )

wilcox_byarith_gemma3 <- df_qa_byarith_gemma3 %>%
  group_by(arithmetic) %>%
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
    print(unique(subdf$arithmetic))
    print(wilcox_single)
    print(wilcox_noisy)
    print("----------------------------")
  }
  )

#### GLMM Llama-3.3 ####
#make dataframe
df_qa_llama3.3 <- read.csv("df_qa_llama3.3.csv", header=T)
df_qa_byarith_add_llama3.3 <- df_qa_llama3.3 %>%
  filter(task == "single_task") %>%
  mutate(arithmetic = arithmetic_list_llama3.3) %>%
  tidyr::unnest(arithmetic) 

df_qa_byarith_llama3.3 <- df_qa_llama3.3 %>%
  bind_rows(df_qa_byarith_add_llama3.3) %>%
  filter(!(task == "single_task" & arithmetic == "none")) %>%
  mutate(
    correctness = as.integer(correctness),     # 0/1
    task = factor(task),                       # relevel if needed
    plausibility = factor(plausibility),
    item = factor(item),
    arithmetic = factor(arithmetic, levels=arithmetic_level)
  )

wilcox_byarith_llama3.3 <- df_qa_byarith_llama3.3 %>%
  group_by(arithmetic) %>%
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
    print(unique(subdf$arithmetic))
    print(wilcox_single)
    print(wilcox_noisy)
    print("----------------------------")
  }
  )



