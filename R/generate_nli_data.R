library(tidyverse)

people_names <- babynames::babynames %>%
  group_by(sex, name) %>%
  summarize(
    n = sum(n)
  ) %>%
  top_n(10, n)

people_names$name

# expand.grid()
heights <- expand_grid(name = people_names$name, feet = 2:8, inches = 1:11, categories = c("tall", "short")) %>%
  mutate(
    premise = pmap_chr(list(name, feet, inches), function(name, feet, inches) {
      glue::glue("{name} is {feet} feet {inches} inches.") %>% as.character()
    }),
    hypothesis = map2_chr(name, categories, function(x, y) {
      glue::glue("{x} is {y}.") %>% as.character()
    })
  )


ages <- expand_grid(name = people_names$name, age = 1:100, categories = c("young", "middle aged", "old")) %>%
  mutate(
    premise = pmap_chr(list(name, age), function(name, age) {
      if(age == 1){
        glue::glue("{name} is {age} year old.") %>% as.character()
      }
      else {
        glue::glue("{name} is {age} years old.") %>% as.character()
      }
    }),
    hypothesis = map2_chr(name, categories, function(x, y) {
      glue::glue("{x} is {y}.") %>% as.character()
    })
  )

write_csv(heights, "data/height_nli.csv")
write_csv(ages, "data/age_nli.csv")



