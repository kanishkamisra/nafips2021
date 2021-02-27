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

gbell <- function(x, width, peaking, center) {
  return(1/(1 + ((x - center)/width)^(2*peaking)))
}

fuzzy_con <- function(x) {
  x^2
}

fuzzy_intersection <- function(x, y) {
  min(c(x, y))
}

fuzzy_negation <- function(x) {
  1 - x
}

fuzzy_int <- function(x) {
  if(x >= 0 & x <= 0.5) {
    2*(x^2)
  } else {
    1 - (2 * (1 - x)^2)
  }
}

fuzzy_dil <- function(x) {
  sqrt(x)
}

fuzzy_pretty = function(x) {
  min(fuzzy_int(x), fuzzy_negation(fuzzy_int(fuzzy_con(x))))
}

fuzzy_rather = function(x) {
  fuzzy_int(fuzzy_con(x))
}


tibble(
  height = seq(150, 200, length = 1000)
) %>%
  mutate(
    # tall = (1 + ((height)/40)^-2)^-1,
    tall = gbell(height, 40, 3, 200),
    # tall = 1/(1 + exp(50 - height)),
    very_tall = fuzzy_con(tall),
    extremely_tall = fuzzy_con(very_tall),
    slightly_tall = tall^(0.5)
  ) %>%
  pivot_longer(-height) %>%
  mutate(
    name = factor(str_replace(name, "_", " "), levels = c("slightly tall", "tall", "very tall", "extremely tall"))
  ) %>%
  ggplot(aes(height, value, color = name)) + 
  geom_line(size = 1) +
  # scale_x_continuous(expand = c(0.02,0))+
  # scale_y_continuous(expand = c(0,0.02)) +
  scale_color_manual(values = carto.pal("wine.pal", n1 = 4)) +
  labs(
    y = "Membership",
    x = "Height (in cm)",
    color = expression(bold(Tallness))
  ) +
  theme_bw(base_size = 18) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 14),
    legend.title = element_blank(),
    # legend.position = c(0.80, 0.21),
    # panel.grid = element_blank(),
    plot.margin = margin(0.2, 0.5, 0.2, 0.2, "cm")
  )

ggsave("paper/example.pdf", height = 6, width = 6.5)
