library(tidyverse)
library(dplyr)
library(lme4)
library(lmerTest)
library(sjPlot)
library(emmeans)

df_qa <- read.csv("df_qa.csv")


diff_cond <- df_qa %>%
  group_by(task, condition, model, plausibility) %>%
  summarise(
    acc = mean(correctness),
    n = n(),
    se = sqrt(acc * (1 - acc) / n),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = plausibility,
    values_from = c(acc, se, n),
    names_glue = "{.value}_{plausibility}"
  ) %>%
  mutate(
    diff = acc_plausible - acc_implausible,
    se_diff = sqrt(se_plausible^2 + se_implausible^2),  # SEの合成
    ci_lower = diff - qnorm(0.975) * se_diff,
    ci_upper = diff + qnorm(0.975) * se_diff
  )
view(diff_cond)


diff_cond$task <- factor(diff_cond$task, levels = c("single_task", "noisy_single_task", "dual_task"))

diff_cond_graph <- ggplot(diff_cond, aes(x = task, y = diff, group = model, color = model)) +
  geom_line(aes(linetype = model), position = position_dodge(width = 0.3)) +
  geom_point(position = position_dodge(width = 0.5), size = 0.5) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.1, position = position_dodge(width = 0.5)) +
  facet_wrap(~ condition) +
  labs(
    title = "Accuracy Difference (Plausible - Implausible)",
    x = "Task",
    y = "Accuracy Difference",
    color = "model",
    linetype = "model"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  coord_fixed(ratio = 7.0)
print(diff_cond_graph)
ggsave("diff_cond_graph.png", diff_cond_graph, width = 7, height = 12)

diff_cond_bymodel_graph <- ggplot(diff_cond, aes(x = task, y = diff, group = condition, color = condition)) +
  geom_line(aes(linetype = condition), position = position_dodge(width = 0.3)) +
  geom_point(position = position_dodge(width = 0.5), size = 0.5) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.1, position = position_dodge(width = 0.5)) +
  facet_wrap(~ model) +
  labs(
    title = "Accuracy Difference (Plausible - Implausible)",
    x = "Task",
    y = "Accuracy Difference",
    color = "condition",
    linetype = "condition"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  coord_fixed(ratio = 7.0)
print(diff_cond_bymodel_graph)
ggsave("diff_cond_bymodel_graph.png", diff_cond_bymodel_graph, width = 7, height = 12)


