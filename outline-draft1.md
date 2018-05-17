## Doing More With {dplyr}

### Manipulating Data

*Data Sets*

- gapminder: standard data set used in lessons (`gapminder`)  
- gapminder: number of cell phone subscribers (`gm_cells`)  
- gapminder: number of car-related deaths per 100,000 (`gm_cars`)  
- gapminder: population, broken down by age category (multiple data frames) (`gm_pop_xx`)  

**Objective: reshape, manipulate, and merge these data sets**

- `gather()` and `merge()` to reshape `gm_cells` and `gm_cars`; review `spread()`  
- `left_join()` to merge with `gapminder`; review other join functions  
- `transmute()` to convert `gm$cell_phones` to be per 100K; compare with `mutate()`  
- reshape each `gm_pop_xx` and merge with `gapminder`; set this process up as a function or loop  
- use two of these data frames to demonstrate `bind_rows()` and compare with `union()`, and `intersect()`  
- `rename()` to rename variables  

### Exploring Data

- identify errors (added to the data) to be converted to NA using `na_if()`  
- `starts_with()` to `select()` population columns (start w/ `pop`)  
- `contains()` to `select()` age-specific population columns (contain `pop_`) **need better example...**  
- `everything()` to `select()` all columns following a specified column  
- `grepl()` to use with `filter()`  

### Summarizing Data

- review of `summarize()`  
- `count()` vs `n()` vs `tally()` vs `n_distinct()`; where and when each should be used most efficiently  
- summarizing multiple columns using `summarize_at()' or `summarize_if()`  
- defining custom functions for use inside `summarize()`  
- `do()` as a general purpose function