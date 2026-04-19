library(tidyverse)
library(dplyr)
library(tidyr)
library(purrr)
library(sjPlot)
library(emmeans)
library(Rcpp)

options(scipen = 5)

#### Test GPT-4o ####
#make dataframe
df_qa_gpt4o <- read.csv("df_qa_gpt4o.csv", header=T)

#test
df_yes_gpt4o <- df_qa_gpt4o %>% filter(is_correct == "Yes")
df_yes_gpt4o <- df_yes_gpt4o %>%
  mutate(
    correctness = as.integer(correctness),     # 0/1
    task = factor(task),                       # relevel if needed
    plausibility = factor(plausibility),
    item = factor(item)
  )
with(df_yes_gpt4o, xtabs(~ correctness + task + plausibility))
meandf <- df_yes_gpt4o %>%
  group_by(item, task, plausibility) %>%
  summarise(acc = mean(correctness), .groups="drop") %>%
  tidyr::pivot_wider(names_from = plausibility, values_from = acc, values_fill = NA_real_) %>%
  filter(!is.na(plausible), !is.na(implausible)) %>%
  mutate(delta = plausible - implausible) %>%
  select(-c(plausible, implausible))

wide <- tidyr::pivot_wider(meandf, names_from = task, values_from = delta)

wilcox.test(wide$`dual_task` - wide$`single_task`, alternative = "greater", exact = FALSE)
wilcox.test(wide$`dual_task` - wide$`noisy_single_task`, alternative = "greater", exact = FALSE)


wilcox_byanswer_gpt4o <- df_qa_gpt4o %>%
  group_by(is_correct) %>%
  group_split() %>%
  lapply(function(subdf) {
    meandf <- subdf %>%
      group_by(item, task, plausibility) %>%
      summarise(acc = mean(correctness), .groups="drop") %>%
      tidyr::pivot_wider(names_from = plausibility, values_from = acc, values_fill = NA_real_) %>%
      filter(!is.na(plausible), !is.na(implausible)) %>%
      mutate(delta = plausible - implausible) %>%
      select(delta)
    wide <- tidyr::pivot_wider(meandf, names_from = task, values_from = delta)
    
    #wilcox_single <- wilcox.test(wide$`dual_task` - wide$`single_task`, alternative = "greater", exact = FALSE)
    #wilcox_noisy <- wilcox.test(wide$`dual_task` - wide$`noisy_single_task`, alternative = "greater", exact = FALSE)
    
    print("----------------------------")
    print(unique(subdf$is_correct))
    #print(wilcox_single)
    #print(wilcox_noisy)
    print("----------------------------")
  }
  )


#### GLMM DeepSeek-V3 ####
#make dataframe
df_qa_DeepseekV3 <- read.csv("df_qa_DeepseekV3.csv", header=T)
df_qa_DeepseekV3 <- df_qa_DeepseekV3 %>%
  mutate(
    correctness = as.integer(correctness),     # 0/1
    task = factor(task),                       # relevel if needed
    plausibility = factor(plausibility),
    item = factor(item)
  )

df_glm_DeepseekV3 <- df_qa_DeepseekV3 %>% filter(task != "single_task")
df_glm_DeepseekV3$task <- relevel(factor(df_glm_DeepseekV3$task), ref="noisy_single_task")
df_glm_DeepseekV3$plausibility <- relevel(factor(df_glm_DeepseekV3$plausibility), ref="plausible")


glm_DeepseekV3 <- df_glm_DeepseekV3 %>%
  group_by(arithmetic) %>%
  group_split() %>%
  lapply(function(subdf) {
    glm <- glmer(correctness ~ task * plausibility + (1|item), 
                 family = binomial(link="logit"),
                 data = subdf #,
                 #control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
    )
    emm <- emmeans(glm, pairwise ~ plausibility | task, type = "response")
    
    print(unique(subdf$arithmetic))
    print(summary(glm))
    print(emm$contrasts)
    print("--------------")
  }
  )

glm_bycond_DeepseekV3 <- df_glm_DeepseekV3 %>%
  group_by(condition) %>%
  group_split() %>%
  lapply(function(subdf) {
    glm <- glmer(correctness ~ task * plausibility + (1|item), 
                 family = binomial(link="logit"),
                 data = subdf, #,
                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
    )
    emm <- emmeans(glm, pairwise ~ plausibility | task, type = "response")
    
    print(unique(subdf$condition))
    print(summary(glm))
    print(emm$contrasts)
    #print(plot_model(glm))
    print("--------------")
  }
  )

brm_bycond_DeepseekV3 <- df_qa_DeepseekV3 %>%
  group_by(condition) %>%
  group_split() %>%
  lapply(function(subdf) {
    brm <- brm(
      correctness ~ task * plausibility + (1|item),
      data = subdf, family = bernoulli(link = "logit"),
      prior = priors,
      iter = 4000, chains = 4, cores = 4#,
      #adapt_delta = 0.99, max_treedepth = 15
    )
    
    emm_link <- emmeans(brm, ~ plausibility | task, type = "link")
    pl_eff   <- contrast(emm_link, method = "revpairwise")  # Plausible - Implausible (per task)
    # difference-in-differences: Dual - Single, Dual - Noisy
    did <- contrast(
      pl_eff,
      method = list(
        "Dual - Single" = c("single_task" = -1, "dual_task" = 1, "noisy_single_task" = 0),
        "Dual - Noisy"  = c("single_task" = 0,  "dual_task" = 1, "noisy_single_task" = -1)
      ),
      by = NULL,
      adjust = "none"
    )
    
    print(unique(subdf$condition))
    print(summary(brm))
    print(pl_eff)
    print(summary(did, infer = c(TRUE, TRUE)))  # 95% CrI と推定値
    #print(plot_model(glm))
    print("--------------")
  }
  )

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

#### GLMM o3-mini ####
#make dataframe
df_qa_o3mini <- read.csv("df_qa_o3mini.csv", header=T)
df_qa_o3mini <- df_qa_o3mini %>%
  mutate(
    correctness = as.integer(correctness),     # 0/1
    task = factor(task),                       # relevel if needed
    plausibility = factor(plausibility),
    item = factor(item)
  )

df_glm_o3mini <- df_qa_gpt4o %>% filter(task != "single_task")
df_glm_o3mini$task <- relevel(factor(df_glm_o3mini$task), ref="noisy_single_task")
df_glm_o3mini$plausibility <- relevel(factor(df_glm_o3mini$plausibility), ref="plausible")


glm_o3mini <- df_glm_o3mini %>%
  group_by(arithmetic) %>%
  group_split() %>%
  lapply(function(subdf) {
    glm <- glmer(correctness ~ task * plausibility + (1|item), 
                 family = binomial(link="logit"),
                 data = subdf #,
                 #control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
    )
    emm <- emmeans(glm, pairwise ~ plausibility | task, type = "response")
    
    print(unique(subdf$arithmetic))
    print(summary(glm))
    print(emm$contrasts)
    print("--------------")
  }
  )

glm_bycond_o3mini <- df_glm_o3mini %>%
  group_by(condition) %>%
  group_split() %>%
  lapply(function(subdf) {
    glm <- glmer(correctness ~ task * plausibility + (1|item), 
                 family = binomial(link="logit"),
                 data = subdf, 
                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
    )
    emm <- emmeans(glm, pairwise ~ plausibility | task, type = "response")
    
    print(unique(subdf$condition))
    print(summary(glm))
    print(emm$contrasts)
    #print(plot_model(glm))
    print("--------------")
  }
  )

brm_bycond_o3mini <- df_qa_o3mini %>%
  group_by(condition) %>%
  group_split() %>%
  lapply(function(subdf) {
    brm <- brm(
      correctness ~ task * plausibility + (1|item),
      data = subdf, family = bernoulli(link = "logit"),
      prior = priors,
      iter = 4000, chains = 4, cores = 4#,
      #adapt_delta = 0.99, max_treedepth = 15
    )
    
    emm_link <- emmeans(brm, ~ plausibility | task, type = "link")
    pl_eff   <- contrast(emm_link, method = "revpairwise")  # Plausible - Implausible (per task)
    # difference-in-differences: Dual - Single, Dual - Noisy
    did <- contrast(
      pl_eff,
      method = list(
        "Dual - Single" = c("single_task" = -1, "dual_task" = 1, "noisy_single_task" = 0),
        "Dual - Noisy"  = c("single_task" = 0,  "dual_task" = 1, "noisy_single_task" = -1)
      ),
      by = NULL,
      adjust = "none"
    )
    
    print(unique(subdf$condition))
    print(summary(brm))
    print(pl_eff)
    print(summary(did, infer = c(TRUE, TRUE)))  # 95% CrI と推定値
    #print(plot_model(glm))
    print("--------------")
  }
  )

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

#### GLMM o4-mini ####
#make dataframe
df_qa_o4mini <- read.csv("df_qa_o4mini.csv", header=T)
df_qa_o4mini <- df_qa_o4mini %>%
  mutate(
    correctness = as.integer(correctness),     # 0/1
    task = factor(task),                       # relevel if needed
    plausibility = factor(plausibility),
    item = factor(item)
  )
df_glm_o4mini <- df_qa_gpt4o %>% filter(task != "single_task")
df_glm_o4mini$task <- relevel(factor(df_glm_o4mini$task), ref="noisy_single_task")
df_glm_o4mini$plausibility <- relevel(factor(df_glm_o4mini$plausibility), ref="plausible")


glm_o4mini <- df_glm_o4mini %>%
  group_by(arithmetic) %>%
  group_split() %>%
  lapply(function(subdf) {
    glm <- glmer(correctness ~ task * plausibility + (1|item), 
                 family = binomial(link="logit"),
                 data = subdf #,
                 #control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
    )
    emm <- emmeans(glm, pairwise ~ plausibility | task, type = "response")
    
    print(unique(subdf$arithmetic))
    print(summary(glm))
    print(emm$contrasts)
    print("--------------")
  }
  )

brm_bycond_o4mini <- df_qa_o4mini %>%
  group_by(condition) %>%
  group_split() %>%
  lapply(function(subdf) {
    brm <- brm(
      correctness ~ task * plausibility + (1|item),
      data = subdf, family = bernoulli(link = "logit"),
      prior = priors,
      iter = 4000, chains = 4, cores = 4#,
      #adapt_delta = 0.99, max_treedepth = 15
    )
    
    emm_link <- emmeans(brm, ~ plausibility | task, type = "link")
    pl_eff   <- contrast(emm_link, method = "revpairwise")  # Plausible - Implausible (per task)
    # difference-in-differences: Dual - Single, Dual - Noisy
    did <- contrast(
      pl_eff,
      method = list(
        "Dual - Single" = c("single_task" = -1, "dual_task" = 1, "noisy_single_task" = 0),
        "Dual - Noisy"  = c("single_task" = 0,  "dual_task" = 1, "noisy_single_task" = -1)
      ),
      by = NULL,
      adjust = "none"
    )
    
    print(unique(subdf$condition))
    print(summary(brm))
    print(pl_eff)
    print(summary(did, infer = c(TRUE, TRUE)))  # 95% CrI と推定値
    #print(plot_model(glm))
    print("--------------")
  }
  )

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

#### GLMM GPT-4.1 ####
#make dataframe
df_qa_gpt4.1 <- read.csv("df_qa_gpt4.1.csv", header=T)
df_qa_gpt4.1 <- df_qa_gpt4.1 %>%
  mutate(
    correctness = as.integer(correctness),     # 0/1
    task = factor(task),                       # relevel if needed
    plausibility = factor(plausibility),
    item = factor(item)
  )
df_glm_gpt4.1 <- df_qa_gpt4.1 %>% filter(task != "single_task")
df_glm_gpt4.1$task <- relevel(factor(df_glm_gpt4.1$task), ref="noisy_single_task")
df_glm_gpt4.1$plausibility <- relevel(factor(df_glm_gpt4.1$plausibility), ref="plausible")


glm_gpt4.1 <- df_glm_gpt4.1 %>%
  group_by(arithmetic) %>%
  group_split() %>%
  lapply(function(subdf) {
    glm <- glmer(correctness ~ task * plausibility + (1|item), 
                 family = binomial(link="logit"),
                 data = subdf #,
                 #control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
    )
    emm <- emmeans(glm, pairwise ~ plausibility | task, type = "response")
    
    print(unique(subdf$arithmetic))
    print(summary(glm))
    print(emm$contrasts)
    print("--------------")
  }
  )

brm_bycond_gpt4.1 <- df_qa_gpt4.1 %>%
  group_by(condition) %>%
  group_split() %>%
  lapply(function(subdf) {
    brm <- brm(
      correctness ~ task * plausibility + (1|item),
      data = subdf, family = bernoulli(link = "logit"),
      prior = priors,
      iter = 4000, chains = 4, cores = 4#,
      #adapt_delta = 0.99, max_treedepth = 15
    )
    
    emm_link <- emmeans(brm, ~ plausibility | task, type = "link")
    pl_eff   <- contrast(emm_link, method = "revpairwise")  # Plausible - Implausible (per task)
    # difference-in-differences: Dual - Single, Dual - Noisy
    did <- contrast(
      pl_eff,
      method = list(
        "Dual - Single" = c("single_task" = -1, "dual_task" = 1, "noisy_single_task" = 0),
        "Dual - Noisy"  = c("single_task" = 0,  "dual_task" = 1, "noisy_single_task" = -1)
      ),
      by = NULL,
      adjust = "none"
    )
    
    print(unique(subdf$condition))
    print(summary(brm))
    print(pl_eff)
    print(summary(did, infer = c(TRUE, TRUE)))  # 95% CrI と推定値
    #print(plot_model(glm))
    print("--------------")
  }
  )

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

#### GLMM Gemma-3 ####
#make dataframe
df_qa_gemma3 <- read.csv("df_qa_gemma3.csv", header=T)
df_qa_gemma3 <- df_qa_gemma3 %>%
  mutate(
    correctness = as.integer(correctness),     # 0/1
    task = factor(task),                       # relevel if needed
    plausibility = factor(plausibility),
    item = factor(item)
  )
df_glm_gemma3 <- df_qa_gemma3 %>% filter(task != "single_task")
df_glm_gemma3$task <- relevel(factor(df_glm_gemma3$task), ref="noisy_single_task")
df_glm_gemma3$plausibility <- relevel(factor(df_glm_gemma3$plausibility), ref="plausible")


glm_gemma3 <- df_glm_gemma3 %>%
  group_by(arithmetic) %>%
  group_split() %>%
  lapply(function(subdf) {
    glm <- glmer(correctness ~ task * plausibility + (1|item), 
                 family = binomial(link="logit"),
                 data = subdf #,
                 #control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
    )
    emm <- emmeans(glm, pairwise ~ plausibility | task, type = "response")
    
    print(unique(subdf$arithmetic))
    print(summary(glm))
    print(emm$contrasts)
    print("--------------")
  }
  )

brm_bycond_gemma3 <- df_qa_gemma3 %>%
  group_by(condition) %>%
  group_split() %>%
  lapply(function(subdf) {
    brm <- brm(
      correctness ~ task * plausibility + (1|item),
      data = subdf, family = bernoulli(link = "logit"),
      prior = priors,
      iter = 4000, chains = 4, cores = 4#,
      #adapt_delta = 0.99, max_treedepth = 15
    )
    
    emm_link <- emmeans(brm, ~ plausibility | task, type = "link")
    pl_eff   <- contrast(emm_link, method = "revpairwise")  # Plausible - Implausible (per task)
    # difference-in-differences: Dual - Single, Dual - Noisy
    did <- contrast(
      pl_eff,
      method = list(
        "Dual - Single" = c("single_task" = -1, "dual_task" = 1, "noisy_single_task" = 0),
        "Dual - Noisy"  = c("single_task" = 0,  "dual_task" = 1, "noisy_single_task" = -1)
      ),
      by = NULL,
      adjust = "none"
    )
    
    print(unique(subdf$condition))
    print(summary(brm))
    print(pl_eff)
    print(summary(did, infer = c(TRUE, TRUE)))  # 95% CrI と推定値
    #print(plot_model(glm))
    print("--------------")
  }
  )

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


#### GLMM Llama-3.3 ####
#make dataframe
df_qa_llama3.3 <- read.csv("df_qa_llama3.3.csv", header=T)
df_qa_llama3.3 <- df_qa_llama3.3 %>%
  mutate(
    correctness = as.integer(correctness),     # 0/1
    task = factor(task),                       # relevel if needed
    plausibility = factor(plausibility),
    item = factor(item)
  )
df_glm_llama3.3 <- df_qa_gemma3 %>% filter(task != "single_task")
df_glm_llama3.3$task <- relevel(factor(df_glm_llama3.3$task), ref="noisy_single_task")
df_glm_llama3.3$plausibility <- relevel(factor(df_glm_llama3.3$plausibility), ref="plausible")


glm_llama3.3 <- df_glm_llama3.3 %>%
  group_by(arithmetic) %>%
  group_split() %>%
  lapply(function(subdf) {
    glm <- glmer(correctness ~ task * plausibility + (1|item), 
                 family = binomial(link="logit"),
                 data = subdf #,
                 #control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
    )
    emm <- emmeans(glm, pairwise ~ plausibility | task, type = "response")
    
    print(unique(subdf$arithmetic))
    print(summary(glm))
    print(emm$contrasts)
    print("--------------")
  }
  )

brm_bycond_llama3.3 <- df_qa_llama3.3 %>%
  group_by(condition) %>%
  group_split() %>%
  lapply(function(subdf) {
    brm <- brm(
      correctness ~ task * plausibility + (1|item),
      data = subdf, family = bernoulli(link = "logit"),
      prior = priors,
      iter = 4000, chains = 4, cores = 4#,
      #adapt_delta = 0.99, max_treedepth = 15
    )
    
    emm_link <- emmeans(brm, ~ plausibility | task, type = "link")
    pl_eff   <- contrast(emm_link, method = "revpairwise")  # Plausible - Implausible (per task)
    # difference-in-differences: Dual - Single, Dual - Noisy
    did <- contrast(
      pl_eff,
      method = list(
        "Dual - Single" = c("single_task" = -1, "dual_task" = 1, "noisy_single_task" = 0),
        "Dual - Noisy"  = c("single_task" = 0,  "dual_task" = 1, "noisy_single_task" = -1)
      ),
      by = NULL,
      adjust = "none"
    )
    
    print(unique(subdf$condition))
    print(summary(brm))
    print(pl_eff)
    print(summary(did, infer = c(TRUE, TRUE)))  # 95% CrI と推定値
    #print(plot_model(glm))
    print("--------------")
  }
  )

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


