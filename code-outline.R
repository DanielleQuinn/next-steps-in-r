
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

### cell phone adoption rate through time

gm %>%
    filter(year > 1982) %>%
    ggplot() +
    geom_line(aes(x = year, y = cell_subscribers_pct, colour = country)) +
    facet_wrap(~ continent)

### keep only the countries with highest level of adoption

max_adoption_cell <- gm %>%
    filter(year == max(year)) %>%
    group_by(continent) %>%
    filter(cell_subscribers_pct == max(cell_subscribers_pct)) %>%
    select(country)

## talk about meaning of message here. Mention ungroup().

## what would be the difference if we did this instead?
max_adoption_cell <- gm %>%
    group_by(continent) %>%
    filter(year == max(year)) %>%
    filter(cell_subscribers_pct == max(cell_subscribers_pct)) %>%
    select(country)

gm %>%
    semi_join(max_adoption_cell, by = c("continent", "country")) %>%
    ggplot() +
    geom_line(aes(x = year, y = cell_subscribers_pct, colour = country)) +
    facet_wrap(~ continent)

## mention use of %in% instead of the semi_join

### keep only the top 3 countries with highest level of adoption
## (make this a challenge)

## most people will come up with this approach
top_three_adoption_cell <- gm %>%
    filter(year == max(year)) %>%
    group_by(continent) %>%
    arrange(desc(cell_subscribers_pct)) %>%
    slice(1:3) %>%
    select(continent, country)

## demonstrate this one
top_three_adoption_cell <- gm %>%
    filter(year == max(year)) %>%
    group_by(continent) %>%
    top_n(3, cell_subscribers_pct) %>%
    select(continent, country)

gm %>%
    semi_join(top_three_adoption_cell) %>%
    ggplot() +
    geom_line(aes(x = year, y = cell_subscribers_pct, colour = country)) +
    facet_wrap(~ continent)

## to revisit, once we introduce purrr
## doesn't quite work, and maybe overly complicated
## gm %>%
##     filter(year > 1977) %>%
##     group_by(continent, country) %>%
##     nest() %>%
##     mutate(max_adoption = purrr::map_dbl(data, function(x) {
##         if (all(is.na(x$cell_subscribers_pct))) return(NA_real_)
##         x %>%
##             filter(year == max(year),   # careful, here
##                    cell_subscribers_pct == max(cell_subscribers_pct, na.rm = TRUE)) %>%
##             pull(cell_subscribers_pct)
##     })) %>%
##     group_by(continent, country) %>%
##     top_n(3, max_adoption) %>%
##     select(-max_adoption) %>%
##     unnest() %>%
##     ggplot() +
##     geom_line(aes(x = year, y = cell_subscribers_pct, colour = country)) +
##     facet_wrap(~ continent)


## number of car deaths vs number of cell phone subscribers

gm %>%
    filter(year > 1980) %>%
    group_by(continent, year) %>%
    summarize(cell_subscribers = sum(cell_subscribers_pct, na.rm = TRUE),
              car_deaths = sum(car_deaths, na.rm = TRUE)) %>%
    ggplot(aes(x = cell_subscribers, y = car_deaths, colour = as.factor(year))) +
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




not_na <- function(x) {
    all(!is.na(x))
}

cell_deaths_model <- function(df) {
    lm(car_deaths ~ cell_subscribers_pct, df)
}

res <- gm %>%
    filter(year > 1995) %>%
    group_by(country) %>%
    nest() %>%
    mutate(is_missing_cells = purrr::map_lgl(data, ~ all(!is.na(.$cell_subscribers_pct))),
           is_missing_car_deaths = purrr::map_lgl(data, ~ not_na_not_zero(.$car_deaths))) %>%
    filter(is_missing_cells & is_missing_car_deaths) %>%
    mutate(mdl = purrr::map(data, cell_deaths_model),
           results = purrr::map(mdl, broom::glance)) %>%
    unnest(results)


res %>%
    arrange(desc(r.squared)) %>%
    filter(r.squared > .85) %>%
    select(country, data) %>%
    unnest() %>%
    ggplot(aes(x = cell_subscribers_pct, y = car_deaths, color = country)) +
    geom_point() +
    facet_wrap(~ country)

gm %>%
    filter(year > 1995) %>%
    group_by(country) %>%
    nest() %>%
    mutate(is_missing_cells = purrr::map_lgl(data, ~ all(.$cell_subscribers_pct > 0 & !is.na(.$cell_subscribers_pct))),
           is_missing_car_deaths = purrr::map_lgl(data, ~ not_na_not_zero(.$car_deaths))) %>%
    filter(is_missing_cells & is_missing_car_deaths) %>%
    mutate(mdl = purrr::map(data, cell_deaths_model),
           coefs = purrr::map(mdl, broom::tidy)) %>%
    unnest(coefs) %>%
    filter(grepl("cell", term)) %>%
    arrange(desc(estimate)) %>%
    left_join(select(res, country, data), by = "country") %>%
    slice(1:6) %>%
    select(country, data) %>%
    unnest() %>%
    ggplot(aes(x = cell_subscribers_pct, y = car_deaths, color = country)) +
    geom_point() +
    facet_wrap(~ country)


### Reading multiple files from a directory

data_files <- dir(path = "data", pattern = "number.csv$", full.names = TRUE)

data_files_details <- tibble(
    files = data_files
) %>%
    mutate(key = as.character(row_number()),
           age_slice = gsub("gapminder_|_number.csv", "", basename(files)))

data_files %>%
    map_df(~ read_csv(.), .id = "key") %>%
    left_join(select(data_files_details, key, age_slice),
              by = "key") %>%
    select(age_slice, everything(), -key) %>%
    gather(year, population, -age_slice, -country) %>%
    mutate(year = as.integer(year)) %>%
    filter(country == "Ethiopia") %>%
    ggplot(aes(x = year, y = population, colour = age_slice)) +
    geom_point() +
    geom_line()
