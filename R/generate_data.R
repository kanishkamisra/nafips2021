library(tidyverse)
library(paletteer)

bert_vocab <- read_lines("https://huggingface.co/bert-base-uncased/raw/main/vocab.txt")

people_names <- babynames::babynames %>%
  filter(str_to_lower(name) %in% bert_vocab) %>%
  group_by(sex, name) %>%
  summarize(
    n = sum(n)
  ) %>%
  top_n(500, n)

phrasing <- c(
  "[NAME] 's height is [HEIGHT] cm.",
  "[NAME] is [HEIGHT] cm tall.",
  "[NAME] is [HEIGHT] cm."
)

trapezoidal <- function(x, a, b, c, d) {
  if (x < a | x > d){
    return(0)
  } else if (x >= a & x <= b){
    return((x - a)/(b - a))
  } else if (x >= b & x <= c) {
    return (1)
  } else {
    return((d - x)/(d - c))
  }
}

vshort <- function(height) {
  return(trapezoidal(height, 37.5, 50, 125, 137.5))
}

short <- function(height) {
  # return(trapezoidal(height, 125, 137.5, 150, 162.5))
  return(gbell(height, 12.5, 4, 143.75))
}

average <- function(height) {
  # return(trapezoidal(height, 150, 162.5, 175, 187.5))
  return(gbell(height, 12.5, 4, 168.25))
}

tall <- function(height) {
  # return(trapezoidal(height, 175, 187.5, 200, 212.5))
  return(gbell(height, 12.5, 4, 193.75))
}

vtall <- function(height) {
  return(trapezoidal(height, 200, 212.5, 300, 400))
}

set.seed(1234)

heights <- tibble(height = seq(50, 300, by = 0.01)) %>%
  mutate(
    vshort = map_dbl(height, vshort),
    short = map_dbl(height, short),
    average = map_dbl(height, average),
    tall = map_dbl(height, tall),
    vtall = map_dbl(height, vtall)
  ) %>%
  mutate(
    sentence = sample(phrasing, size = 25001, replace = TRUE),
    name = sample(people_names$name, size = 25001, replace = TRUE)
  ) %>%
  mutate(
    sentence = pmap(list(sentence, height, name), function(x, y, z) {
      s = x %>%
        str_replace("\\[NAME\\]", z) %>%
        str_replace("\\[HEIGHT\\]", as.character(y))
      return(s)
    }) 
  ) %>%
  unnest(sentence) %>%
  mutate(critical = height %in% critical_values) %>%
  group_by(critical) %>%
  nest() %>%
  mutate(
    split = case_when(
      critical == TRUE ~ map(data, function(x) {
        return(rep("train", nrow(x)))
      }),
      TRUE ~ map(data, function(x) {
        return(sample(c("train", "test", "dev"), prob = c(0.9, 0.1, 0.1), size = nrow(x), replace = TRUE))
      })
    )
  ) %>%
  unnest(c(data, split)) %>%
  arrange(height) %>%
  ungroup() %>%
  select(-critical)

heights %>%
  select(sentence, name, vshort, short, average, tall, vtall, split) %>%
  group_by(split) %>%
  nest() %>%
  mutate(
    file = walk2(split, data, function(x, y) {
      write_csv(y, paste0("data/height_", x,".csv"))
    })
  )

## Rules for train, dev, test -- all boundary values in train, 
## dev and test disjoint from train and from each other.
critical_values <- c(50, seq(125, 212.5, by = 12.5), 300)

seq(50, 300, by = 0.01) %>%
  discard(. %in% critical_values)

heights %>%
  pivot_longer(vshort:vtall, names_to = "fuzzyset", values_to = "membership") %>%
  mutate(fuzzyset = factor(fuzzyset, levels = c("vshort", "short", "average", "tall", "vtall"))) %>%
  ggplot(aes(height, membership, color = fuzzyset)) +
  geom_line(size = 1.2) +
  scale_x_continuous(breaks = seq(50, 300, by = 25), expand = c(0,0)) +
  # scale_color_brewer(palette = "BrBg") +
  scale_color_manual(values = cartography::carto.pal("blue.pal", 5)) +
  # scale_color_paletteer_d("ggsci::red_material", dynamic = TRUE) +
  labs(
    color = "Tallness",
    x = "height (in cm)"
  ) +
  theme_bw(base_family = "Helvetica", base_size = 16) +
  theme(
    legend.position = "top", 
    panel.grid = element_blank(),
    plot.margin = margin(0.2, 0.5, 0.2, 0.2, "cm")
  )

tibble(height = 0:100) %>%
  mutate(y = map_dbl(height, trapezoidal, a = -25, b = 0, c = 50, d = 75)) %>%
  ggplot(aes(height, y)) +
  geom_line()

gbell <- function(x, width, peaking, center) {
  return(1/(1 + ((x - center)/width)^(2*peaking)))
}

tibble(x = seq(100, 300, by = 0.01), y = gbell(x, 12.5, 4, 143.75)) %>%
  ggplot(aes(x, y)) +
  geom_line()
