library(MASS) #ver7.3.60
library(tidyr) #ver1.3.0
library(magrittr) #ver2.0.3
library(dplyr) #ver1.1.4
#library(pracma) #ver2.4.4
library(ggplot2) #ver3.4.4

wd <- '/Users/reiemura/Library/Mobile Documents/com~apple~CloudDocs/llm-dual-task/human/analysis'
setwd(wd)

######   Read data   ######

df_original <- read.csv("results.csv", header=F, sep=",", fill=T,
                        col.names=c("reception_time","ip_address","controller_name","item_order",
                                    "inner_element_number", "label", "latin_square_group", 
                                    "element_type", "element_name", "parameter", "value", "event_time", 
                                    "prolificID", "participantID", "age", "sex", "list", 
                                    "itemID", "item", "is_correct", "response", "correctness", "answeing_time",
                                    "target_sentence_position", "number_of_propositions", "condition", "plausibility",
                                    "arithmetic", "task", "r10_equationID", "r10_response", "r10_rt", 
                                    "r20_equationID", "r20_response", "r20_rt", 
                                    "r30_equationID", "r30_response", "r30_rt", 
                                    "r40_equationID", "r40_response", "r40_rt", 
                                    "comment")) %>%
  #mutate(item=as.numeric(item))%>%
  #mutate(time = as.numeric(time)) %>%
  filter(label == "main") %>%
  filter(element_name ==  "decisionKey") %>% 
  select(-c(reception_time, ip_address, controller_name, inner_element_number, label, latin_square_group,
           element_type, element_name, parameter, value, event_time)) 
  

eq_answer <- read.csv("eq_answer.csv", header=T, fill=T) %>%
  mutate(
    EquationID = as.character(EquationID),
    Result    = as.numeric(Result)
  ) %>%
  select(-Equation)

#r10
df_original <- df_original %>%
  mutate(
    r10_equationID = as.character(r10_equationID),
    r10_response   = as.numeric(r10_response)
  ) %>%
  left_join(eq_answer %>% rename(r10_is_correct = Result),
            by = c("r10_equationID" = "EquationID")) %>%
  mutate(
    r10_correctness = case_when(
      task != "dual"                    ~ NA_integer_,
      r10_equationID == "*"             ~ NA_integer_,
      is.na(r10_response)               ~ NA_integer_,
      r10_response == r10_is_correct    ~ 1L,
      TRUE                              ~ 0L
    )
  )

# r20
df_original <- df_original %>%
  mutate(
    r20_equationID = as.character(r20_equationID),
    r20_response   = as.numeric(r20_response)
  ) %>%
  left_join(eq_answer %>% rename(r20_is_correct = Result),
            by = c("r20_equationID" = "EquationID")) %>%
  mutate(
    r20_correctness = case_when(
      task != "dual"                 ~ NA_integer_,
      r20_equationID == "*"          ~ NA_integer_,
      is.na(r20_response)            ~ NA_integer_,
      r20_response == r20_is_correct ~ 1L,
      TRUE                           ~ 0L
    )
  )

# r30
df_original <- df_original %>%
  mutate(
    r30_equationID = as.character(r30_equationID),
    r30_response   = as.numeric(r30_response)
  ) %>%
  left_join(eq_answer %>% rename(r30_is_correct = Result),
            by = c("r30_equationID" = "EquationID")) %>%
  mutate(
    r30_correctness = case_when(
      task != "dual"                 ~ NA_integer_,
      r30_equationID == "*"          ~ NA_integer_,
      is.na(r30_response)            ~ NA_integer_,
      r30_response == r30_is_correct ~ 1L,
      TRUE                           ~ 0L
    )
  )

# r40
df_original <- df_original %>%
  mutate(
    r40_equationID = as.character(r40_equationID),
    r40_response   = as.numeric(r40_response)
  ) %>%
  left_join(eq_answer %>% rename(r40_is_correct = Result),
            by = c("r40_equationID" = "EquationID")) %>%
  mutate(
    r40_correctness = case_when(
      task != "dual"                 ~ NA_integer_,
      r40_equationID == "*"          ~ NA_integer_,
      is.na(r40_response)            ~ NA_integer_,
      r40_response == r40_is_correct ~ 1L,
      TRUE                           ~ 0L
    )
  )

df_original <- df_original %>%
  rowwise() %>%
  mutate(
    eq_acc_trial = mean(c(r10_correctness, r20_correctness, r30_correctness, r40_correctness),
                        na.rm = TRUE),
    eq_n_trial   = sum(!is.na(c(r10_correctness, r20_correctness, r30_correctness, r40_correctness)))
  ) %>%
  ungroup() 

write.csv(df_original, "df_original.csv", row.names=F)


