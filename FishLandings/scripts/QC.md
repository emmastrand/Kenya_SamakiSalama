Quality Control of Fishing Landings dataset
================
Emma Strand; <emma_strand@uri.edu>

## How to run this with a future xlsx file

1.  Replace raw data file name in the function that creates the
    following variables: fishing\_operation, catch\_composition, and
    validation\_lists.

## Load all libraries

``` r
library(plyr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(readxl)
```

## Create dataframe

**Load in raw data files.**

[Reading in an excel datafile instead of a
csv](http://www.sthda.com/english/wiki/reading-data-from-excel-files-xls-xlsx-into-r).
This requires the `readxl` package listed in load all libraries step.

``` r
fishing_operation <- read_excel("data/Fishlandings-data_21052022-May_update-cleaning in progress.xlsx", sheet = "fishing_operation") %>%
  select(-'...21', -'...22', -'...23') %>%
  rename(Operation_date = date_dd_mm_yyyy)

catch_composition <- read_excel("data/Fishlandings-data_21052022-May_update-cleaning in progress.xlsx", sheet = "catch_composition") %>%
  select(-'...10', -'...11', -'...12', -'...13') %>%
  rename(Operation_date = Date)

validation_lists <- read_excel("data/Fishlandings-data_21052022-May_update-cleaning in progress.xlsx", sheet = "validation_lists")

df <- full_join(fishing_operation, catch_composition, by = c("fisher_id", "Operation_date")) 
```

## Quality Control

``` r
head(df)
```

    ## # A tibble: 6 × 27
    ##   Operation_date      enumerator       landing_site BMU   fisher_id fisher_phone
    ##   <dttm>              <chr>            <chr>        <chr> <chr>     <chr>       
    ## 1 2021-12-02 00:00:00 Celestinar N Ali Mayungu      Mayu… SS/MAY/S… <NA>        
    ## 2 2021-12-02 00:00:00 Celestinar N Ali Mayungu      Mayu… SS/MAY/S… <NA>        
    ## 3 2021-12-02 00:00:00 Celestinar N Ali Mayungu      Mayu… SS/MAY/S… <NA>        
    ## 4 2021-12-02 00:00:00 Celestinar N Ali Mayungu      Mayu… SS/MAY/S… <NA>        
    ## 5 2021-12-02 00:00:00 Celestinar N Ali Mayungu      Mayu… SS/MAY/S… <NA>        
    ## 6 2021-12-02 00:00:00 Celestinar N Ali Mayungu      Mayu… SS/MAY/S… <NA>        
    ## # … with 21 more variables: household_id <chr>, trap_type <chr>,
    ## #   total_traps_collected <dbl>, date_set_dd_mm_yyyy <dttm>,
    ## #   `time_set_24hh:mm` <chr>, date_collected_dd_mm_yyyy <dttm>,
    ## #   `time_collected_24hh:mm` <chr>, `time_in_water (effort)` <lgl>,
    ## #   total_weight_kg <dbl>, take_home_weight_kg <dbl>, total_value_KES <dbl>,
    ## #   take_home_value_KES <dbl>, `No. of fishers in crew` <dbl>,
    ## #   general_notes <lgl>, Kiswahili_name <chr>, SPECIES <chr>, …

**Operation\_date**: in the above chunk, these dates are automatically
read in as *dttm* format (date and time).

**enumerator**
