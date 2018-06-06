# ---- Load Packages ----
# If you don't have these packages installed, use
# install.packages("gapminder")
# install.packages("tidyverse")
library(gapminder)
library(tidyverse)

# ---- Import Data ----
# This dataset is found in the gapminder package
data("gapminder")
# These files can be downloaded from
# https://daniellequinn.github.io/next-steps-in-r/data/gapminder-next-steps.zip
gm_car_deaths <- read_tsv("data/gapminder-car-deaths.txt")
gm_cells <- read_tsv("data/gapminder-cell-phones.txt")

# ---- Explore Data ----
str(gapminder)
glimpse(gapminder) # a tidier way of looking at the data structure
glimpse(gm_cells)
glimpse(gm_car_deaths)

# ---- Exploring Variables ----
# Use select() to choose what columns you want to look at
names(gm_cells) # Returns all column names

# From gapminder, return the column called `pop`
gapminder %>% select(pop)

# From gm_cells, return all columns that start with `197`
gm_cells %>% select(starts_with("197"))

# From gm_cells, return all columns that end with `5`
gm_cells %>% select(ends_with("5"))

# From gapminder, return all columns that contain the character `a`
gapminder %>% select(contains("a"))

# From gm_cells, return all columns that, when a given prefix is removed
# and the names converted to a numeric value, is found between 1995 and 1999
gm_cells %>% select(num_range(prefix = "", range = 1995:1999))

# Use filter() to choose what records you want to look at
# From gapminder, return any record where country is `Canada`
gapminder %>% filter(country == "Canada")

# From gapminder, find all records where country contains the characters `ad`
# and return the unique values of country
gapminder %>% 
  filter(grepl("ad", country)) %>% 
  distinct(country)

# ---- Tidy Data ----
View(gm_cells) # We can see that the gm_cells data is in wide format, with
# years as column names
# We can also see that the first column is called cell_phones but contains countries

gm_cells_tidy <- gm_cells %>%
  rename(country = cell_phones) %>% # rename the cell_phones column to `country`
  gather(key = "year", value = "cell_subscribers", -country)

# How does gather work?
# gather() converts data from wide format to long format
# you provide the names of the new columns; value describes the numeric values,
# key describes an additional factor
# gather() will by default gather all of the columns; in this case we don't want country
# to be included, so we add `-country` as a third argument

# Remember: for a change to be permanent, you need to place the output into a
# new object or overwrite the existing object

# Exercise: Use rename() and gather() to tidy the gm_car_deaths data
# call the new column containing the numeric values `car_deaths`
# place the output into a new object called `gm_car_deaths_tidy`
gm_car_deaths_tidy <- gm_car_deaths %>%
  rename(country = car_deaths_per_100k) %>%
  gather(key = "year", value = "car_deaths", -country)

# ---- Merge Data ----
# Look at the first few rows of both gapminder and gm_cells
head(gapminder)
had(gm_cells)

# We can see that we would like to add the cell phone information
# to our gapminder data set, using `country` and `year` to match records
gapminder %>%
  left_join(gm_cells_tidy, by = c("country", "year"))

# Why does this give us an error?
glimpse(gapminder)
glimpse(gm_cells_tidy)
# `year` is not stored as an integer in both datasets
# Use mutate() to convert `year` to an integer
# Remember that we want this change to be permanent, so we'll
# overwrite the existing `gm_cells_tidy` data frame
gm_cells_tidy <- gm_cells_tidy %>%
  mutate(year = as.integer(year))

# Try merging again
gapminder %>%
  left_join(gm_cells_tidy, by = c("country", "year"))
# When you can confirm it has worked, overwrite `gapminder` with this new version
gapminder <- gapminder %>%
  left_join(gm_cells_tidy, by = c("country", "year"))

# Exercise: Merge `gapminder` and `gm_car_deaths`
gm_car_deaths_tidy <- gm_car_deaths_tidy %>%
  mutate(year = as.integer(year))
gapminder <- gapminder %>%
  left_join(gm_car_deaths_tidy, by = c("country", "year"))

# If you glimpse at gapminder, you should see those changes
glimpse(gapminder)

# ---- Manipulating Data ----
# While cell_subscribers is a total value, car_deaths is per 100K population
# Use mutate to also scale cell_subscribers to be the percent of the population subscribed
# Step one: convert cell_subscribers to integer
# Step two: create a new column called cell_subscribers_pct that contains the new values
gapminder <- gapminder %>%
  mutate(cell_subscribers = as.integer(cell_subscribers),
         cell_subscribers_pct = cell_subscribers / pop)

# You can also use mutate to generate a lag or lead effect
# Note here that we are not making these changes permanent because we are not overwriting 
# our object
gapminder %>%
  mutate(lifeExp_lag = lag(lifeExp, 5)) %>% # apply a timestep lag of 5
  select(year, lifeExp, lifeExp_lag) # To view results, just return the appropraite columns

# Mutate can be used to rank and scale values
gapminder %>%
  mutate(pop_rank = dense_rank(pop)) %>% # There are various rank functions, dense_rank used here
  select(starts_with("pop")) # View results

# Scaling values between 0 and 1
gapminder %>%
  mutate(pop_rank = percent_rank(pop)) %>%
  select(starts_with("pop"))

# ---- Summarize Data ----
# Case One: One function on one variable
# Find the maximum population, by continent
gapminder %>%
  group_by(continent) %>%
  summarize(max(pop))

# Case Two: Multiple functions on one variable
# Find the maximum and mean population, by continent
gapminder %>%
  group_by(continent) %>%
  summarize(max(pop),
            mean(pop))
# OR, using summarize_at
# summarize_at requires you specify the variables to be used in vars()
# and the functions to be applied in funs()
gapminder %>%
  group_by(continent) %>%
  summarize_at(vars("pop"), funs(max, mean))

# Case Three: One function, multiple variables
# Find the maximum of population & life expectancy, by continent
gapminder %>%
  group_by(continent) %>%
  summarize(max(pop),
            max(lifeExp))
# OR, using summarise_at
gapminder %>%
  group_by(continent) %>%
  summarize_at(vars("pop", "lifeExp"), funs(max))

# Case Four: multiple functions on multiple variables
# Find the maximum and mean of population, life expectancy, and 
# GDP per capita, by continent
gapminder %>%
  group_by(continent) %>%
  summarize(max(pop),
            mean(pop),
            max(lifeExp),
            mean(lifeExp),
            max(gdpPercap),
            mean(gdpPercap)) %>%
  View()  # Add View() to display the results in a new window
# OR, using summarize_at
gapminder %>%
  group_by(continent) %>%
  summarize_at(vars("pop", "lifeExp", "gdpPercap"), funs(max, mean)) %>%
  View()

# ---- ggplot2 ----
# You can pipe directly into a ggplot() object...!
gapminder %>% 
  filter(year > 1977) %>% 
  ggplot() +
    geom_line(aes(x = year, y = cell_subscribers_pct, colour = country)) +
    facet_wrap(~ continent) +
    theme(legend.position = "none")
