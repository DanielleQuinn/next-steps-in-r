
library(tidyverse)

library(gapminder)

## Datasets

gm_car_deaths <- read_tsv("data/gapminder-car-deaths.txt")
gm_cells      <- read_tsv("data/gapminder-cell-phones.txt")

## Let's put all of this data in a single tidy dataset

gm_car_deaths_tidy <- gm_car_deaths %>%
    rename(country = car_deaths_per_100k) %>%
    gather(year, car_deaths, -country)


gm_cells_tidy <- gm_cells %>%
    rename(country = cell_phones) %>%
    gather(year, cell_subscribers, -country)

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

## Let's try the join again

gm <- gapminder %>%
    left_join(gm_car_deaths_tidy, by = c("country", "year")) %>%
    left_join(gm_cells_tidy, by = c("country", "year"))

## demonstrate transmute and mutate on cell_subscribers

gm <- gm %>%
    mutate(cell_subscribers_pct = cell_subscribers / pop)

gm %>%
    transmute(cell_subscribers_pct = cell_subscribers / pop)

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

### number of cell phones through time

gm %>%
    filter(year > 1982) %>%
    group_by(continent, country, year) %>%
    summarize(cell_subscribers = sum(cell_subscribers_pct, na.rm = TRUE)) %>%
    ggplot() +
    geom_line(aes(x = year, y = cell_subscribers, colour = country)) +
    facet_wrap(~ continent)

## number of car deaths vs number of cell phone subscribers

gm %>%
    filter(year > 1980) %>%
    group_by(continent, year) %>%
    summarize(cell_subscribers = sum(cell_subscribers_pct, na.rm = TRUE),
              car_deaths = sum(car_deaths, na.rm = TRUE)) %>%
    ggplot(aes(x = cell_subscribers, y = car_deaths, size = year)) +
    geom_point() +
    facet_wrap(~ continent) +
    scale_x_log10()

## purrr: let's identify the countries for which the correlation between
## cell phone adoption and car deaths are the most correlated

## which year do we have data for?
gm %>%
    filter(!is.na(car_deaths)) %>%
    count(year)

gm %>%
    filter(!is.na(cell_subscribers_pct) & cell_subscribers_pct > 0) %>%
    count(year)


not_na_not_zero <- function(x) {
    all(x > 0 & !is.na(x))
}

cell_deaths_model <- function(df) {
    lm(car_deaths ~ cell_subscribers_pct, df)
}

gm_nested <- gm %>%
    filter(year > 1995) %>%
    group_by(country) %>%
    nest()

gm_nested %>%
    mutate(is_missing_cells = purrr::map_lgl(data, ~ all(.$cell_subscribers_pct > 0 & !is.na(.$cell_subscribers_pct))),
           is_missing_car_deaths = purrr::map_lgl(data, ~ not_na_not_zero(.$car_deaths))) %>%
    filter(is_missing_cells & is_missing_car_deaths) %>%
    mutate(mdl = purrr::map(data, cell_deaths_model),
           results = purrr::map(mdl, broom::glance)) %>%
    unnest(results) %>%
    arrange(desc(r.squared)) %>%
    select(r.squared, statistic) %>%
    filter(r.squared > .85) %>%
    select(country, data) %>%
    unnest() %>%
    ggplot(aes(x = cell_subscribers_pct, y = car_deaths, color = country)) +
    geom_point() +
    facet_wrap(~ country)
