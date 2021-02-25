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

nounit <- expand_grid(location = c("noloc", "inside", "in the bedroom", "in the living room", "in the basement", "outside"), temperature = -50:122, categories = c("freezing", "cold", "cool", "warm", "hot")) %>%
  mutate(
    premise = pmap_chr(list(location, temperature), function(location, temperature) {
      if(location == "noloc"){
        glue::glue("It is {temperature} degrees.") %>% as.character()
      }
      else {
        glue::glue("It is {temperature} degrees {location}.") %>% as.character()
      }
    }),
    hypothesis = pmap_chr(list(location, categories), function(location, categories) {
      if(location == "noloc"){
        glue::glue("It is {categories}.") %>% as.character()
      }
      else {
        glue::glue("It is {categories} {location}.") %>% as.character()
      }
    })
  )

fahrenheit <- expand_grid(location = c("noloc", "inside", "in the bedroom", "in the living room", "in the basement", "outside"), temperature = -50:122, categories = c("freezing", "cold", "cool", "warm", "hot")) %>%
  mutate(
    premise = pmap_chr(list(location, temperature), function(location, temperature) {
      if(location == "noloc"){
        glue::glue("It is {temperature} degrees fahrenheit.") %>% as.character()
      }
      else {
        glue::glue("It is {temperature} degrees fahrenheit {location}.") %>% as.character()
      }
    }),
    hypothesis = pmap_chr(list(location, categories), function(location, categories) {
      if(location == "noloc"){
        glue::glue("It is {categories}.") %>% as.character()
      }
      else {
        glue::glue("It is {categories} {location}.") %>% as.character()
      }
    })
  )

celsius <- expand_grid(location = c("noloc", "inside", "in the bedroom", "in the living room", "in the basement", "outside"), temperature = -50:50, categories = c("freezing", "cold", "cool", "warm", "hot")) %>%
  mutate(
    premise = pmap_chr(list(location, temperature), function(location, temperature) {
      if(location == "noloc"){
        glue::glue("It is {temperature} degrees celsius.") %>% as.character()
      }
      else {
        glue::glue("It is {temperature} degrees celsius {location}.") %>% as.character()
      }
    }),
    hypothesis = pmap_chr(list(location, categories), function(location, categories) {
      if(location == "noloc"){
        glue::glue("It is {categories}.") %>% as.character()
      }
      else {
        glue::glue("It is {categories} {location}.") %>% as.character()
      }
    })
  )

write_csv(heights, "data/height_nli.csv")
write_csv(ages, "data/age_nli.csv")
write_csv(nounit, "data/nounit_nli.csv")
write_csv(fahrenheit, "data/fahrenheit_nli.csv")
write_csv(celsius, "data/celsius_nli.csv")


