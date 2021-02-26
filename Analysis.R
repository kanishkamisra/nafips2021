library(tidyverse)
library(scales)
library(cartography)

height_results <- read_csv("data/results/roberta-large-mnli__height_nli.csv")

age_results <- read_csv("data/results/roberta-large-mnli__age_nli.csv")

age_results %>%
  # mutate(contradiction = 1 - entailment)
  group_by(age, categories, type) %>%
  summarize(entailment = mean(entailment)) %>%
  # filter(categories == "young") %>%
  ggplot(aes(age, entailment, color = categories, fill = categories)) +
  geom_line() +
  geom_smooth(method = "gam") +
  facet_wrap(~type)

height_results %>%
  mutate(height = feet*12 + inches) %>%
  group_by(height, categories) %>%
  summarize(entailment = mean(entailment)) %>%
  # filter(categories == "young") %>%
  ggplot(aes(height, entailment, color = categories, fill = categories)) +
  geom_line() +
  geom_smooth(method = "gam", se = FALSE) +
  scale_y_continuous(limits = c(0, 1.0)) +
  theme(legend.position = "top")

nounit_results <- read_csv("data/results/roberta-large-mnli__nounit_nli.csv")

nounit_results %>%
  mutate(categories = factor(categories, levels = c("freezing", "cold", "cool", "warm", "hot"))) %>%
  ggplot(aes(temperature, entailment, color = categories)) +
  geom_line() +
  geom_smooth(method = "gam") +
  scale_color_manual(values = carto.pal(pal1 = "blue.pal", n1 = 3,
                                        pal2 = "orange.pal", n2 = 2)) +
  scale_y_continuous(limits = c(0, 1)) +
  facet_wrap(~location) +
  theme_bw(base_family = "CMU Sans Serif Medium", base_size = 16) +
  theme(
    legend.position = "top",
    panel.grid = element_blank(),
    plot.margin = margin(0.2, 0.5, 0.2, 0.2, "cm")
  )

celsius_results <- read_csv("data/results/roberta-large-mnli__celsius_nli.csv")

celsius_results %>%
  mutate(categories = factor(categories, levels = c("freezing", "cold", "cool", "warm", "hot"))) %>%
  ggplot(aes(temperature, entailment, color = categories)) +
  geom_line() +
  geom_smooth(method = "gam") +
  scale_color_manual(values = carto.pal(pal1 = "blue.pal", n1 = 3,
                                        pal2 = "orange.pal", n2 = 2)) +
  scale_y_continuous(limits = c(0, 1)) +
  facet_wrap(~location) +
  theme_bw(base_family = "CMU Sans Serif Medium", base_size = 16) +
  theme(
    legend.position = "top",
    panel.grid = element_blank(),
    plot.margin = margin(0.2, 0.5, 0.2, 0.2, "cm")
  )

fahrenheit_results <- read_csv("data/results/roberta-large-mnli__fahrenheit_nli.csv")

fahrenheit_results %>%
  mutate(categories = factor(categories, levels = c("freezing", "cold", "cool", "warm", "hot"))) %>%
  ggplot(aes(temperature, entailment, color = categories)) +
  geom_line() +
  geom_smooth(method = "gam") +
  scale_color_manual(values = carto.pal(pal1 = "blue.pal", n1 = 3,
                                        pal2 = "orange.pal", n2 = 2)) +
  scale_y_continuous(limits = c(0, 1)) +
  facet_wrap(~location) +
  theme_bw(base_family = "CMU Sans Serif Medium", base_size = 16) +
  theme(
    legend.position = "top",
    panel.grid = element_blank(),
    plot.margin = margin(0.2, 0.5, 0.2, 0.2, "cm")
  )

