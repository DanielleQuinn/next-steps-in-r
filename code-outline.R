
library(tidyverse)

library(gapminder)

## Datasets

gm_car_deaths <- read_tsv("data/gapminder-car-deaths.txt")
gm_cells      <- read_tsv("data/gapminder-cell-phones.txt")
gm_60plus     <- read_tsv("data/gapminder_population_60plus.csv")

## Let's put all of this data in a single tidy dataset

gm_car_deaths_tidy <- gm_car_deaths %>%
    rename(country = car_deaths_per_100k) %>%
    gather(year, car_deaths, -country)


gm_cells_tidy <- gm_cells %>%
    rename(country = cell_phones) %>%
    gather(year, cell_subscribers, -country)


gm_60plus_tidy <- gm_60plus %>%
    rename(country = population_60plus) %>%
    gather(year, pop_60plus, -country)

## Use left-join to put the data together

gapminder <- gapminder %>%
    left_join(gm_car_deaths_tidy, by = c("country", "year"))

## teach about type transformation

gm_car_deaths_tidy <- gm_car_deaths_tidy %>%
    mutate(year = as.integer(year),
           car_deaths = as.integer(car_deaths))

gm_cells_tidy <- gm_cells_tidy %>%
    mutate(year = as.integer(year),
           cell_subscribers = as.integer(cell_subscribers))

gm_60plus_tidy <- gm_60plus_tidy %>%
    mutate(year = as.integer(year),
           pop_60plus = as.integer(pop_60plus))

## Let's try the join again

gm <- gapminder %>%
    left_join(gm_car_deaths_tidy, by = c("country", "year")) %>%
    left_join(gm_cells_tidy, by = c("country", "year")) %>%
    left_join(gm_60plus_tidy, by = c("country", "year"))

## demonstrate transmute and mutate on cell_subscribers

gm %>%
    mutate(cell_subscribers_100k = cell_subscribers/1e5)

gm %>%
    transmute(cell_subscribers_100k = cell_subscribers/1e5)

### exploring data


### Summarizing data

gm %>%
    count(continent)

gm %>%
    filter(year == 2007) %>%
    count(continent)

gm %>%
    filter(year == 2002 | year == 2007) %>%
    count(continent, year)
