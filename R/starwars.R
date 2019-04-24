library(tidyverse)

starwars

starwars %>%
  select(-films, -vehicles, -starships) %>%
  summary

starwars %>%
  distinct(homeworld)

starwars %>%
  map(function(x)sum(is.na(x)))

starwars %>%
  unnest(films) %>%
  group_by(films) %>%
  summarise_all(function(x)sum(is.na(x)))

starwars %>%
  map(function(x){length(unique(unlist(x)))})

starwars %>%
  unnest(films) %>%
  group_by(films) %>%
  summarize(n = n_distinct(species)) %>%
  arrange(n) %>%
  qplot(x = films, y = n, data = ., geom = "col")

starwars %>%
  unnest(films) %>%
  group_by(films, species) %>%
  distinct(films, species) %>%
  qplot(x = films, data = ., geom = "bar")

starwars %>%
  qplot(x = gender, y = height, data = ., geom = "violin")

starwars %>%
  filter(mass < 500) %>%
  qplot(x = mass, y = height, data = ., color = gender, geom = "point", size = 3)

starwars %>%
  unnest(vehicles)

starwars %>%
  mutate(n_vehicles = map_int(vehicles, length))

starwars %>%
  mutate(intervals = cut_number(x = height, n = 8)) %>%
  qplot(data = ., x = intervals)
