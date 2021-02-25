library(tidyverse)
library(scales)

height_results <- read_csv("data/results/roberta-large-mnli__height_nli.csv")

age_results <- read_csv("data/results/roberta-large-mnli__age_nli.csv")

age_results %>%
  # mutate(contradiction = 1 - entailment)
  group_by(age, categories) %>%
  summarize(entailment = mean(entailment)) %>%
  # filter(categories == "young") %>%
  ggplot(aes(age, entailment, color = categories, fill = categories)) +
  geom_line()
  # geom_smooth(method = "gam")

height_results %>%
  mutate(height = feet*12 + inches) %>%
  group_by(height, categories) %>%
  summarize(entailment = mean(entailment)) %>%
  # filter(categories == "young") %>%
  ggplot(aes(height, entailment, color = categories, fill = categories)) +
  geom_line() +
  scale_y_continuous(limits = c(0, 1.0)) +
  theme(legend.position = "top")
