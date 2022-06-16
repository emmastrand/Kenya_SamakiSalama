Analysis of Fishing Landings dataset
================
Author: Emma Strand; <emma_strand@uri.edu>

## Goal

1.  Total catch per unit effort between modified and traditional traps.
    It would be great to see this as grams captured per trap set.  
2.  Species catch per unit effort between modified and traditional
    traps. Take the top 3-5 species and run \#1 for them separately.  
3.  Total mature fish catch per unit effort between modified and
    traditional traps. This will have to be for the top 3-5 species
    separately. Go to Fishbase and find the length at first maturity for
    that particular species, then assign each fish a “mature” or
    “immature” status in the data and calculate.  
4.  Average length of catch versus length at first maturity (Lmat). Take
    the difference for each fish in the data against its length at first
    maturity and then calculate a weighted value for modified versus
    traditional traps where a value above 0 represents a fish above Lmat
    and a value below represents a fish below Lmat.  
5.  Length frequency of top 3-5 species in modified versus traditional
    (different colors) with Lmat etc. indicators pulled from Fishbase.

## Contents

-   [**Reading in datafiles**](#data)  
-   [**Total catch per unit effort**](#catch_effort)

## <a name="data"></a> **Reading in datafiles**

``` r
library(plyr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(readxl)
library(lubridate)
library(Hmisc)
library(writexl)
library(naniar)
library(Rmisc)
```

Read in the data frame that is the output of the QC script.

``` r
# read in excel file
data <- read_excel("data/Fishlandings-cleaned-21052022-May.xlsx") 
```

## <a name="catch_effort"></a> **Total catch per unit effort**

Grouping by fisher\_id but this might be effective to group by
enumerator once I have correct list of names. There are 3 boat trips
recorded with the exact same fish data that are under 3 different fisher
ID names but all the same enumerator.. come back to this in QC.

Goal: grams captured per trap set.

``` r
data <- data %>% unite(survey_id, Operation_date, fisher_id, sep = " ") %>%
  dplyr::group_by(survey_id) %>% 
  mutate(total_catch = sum(number_of_fish),
         grams_per_trap = total_weight_kg/total_traps_collected)

modified_trap_df <- data %>% subset(trap_type == "MODIFIED" | trap_type == "UNMODIFIED")

modified_trap_df %>% 
  ggplot(aes(x=trap_type, y=grams_per_trap, color=trap_type)) + geom_boxplot() + theme_bw() + 
  ylab("Grams of fish per trap") + xlab("Type of trap") +
  theme(axis.text.x = element_text(angle = 60, vjust = 1.1, hjust = 1.3)) #Set the text angle
```

    ## Warning: Removed 231 rows containing non-finite values (stat_boxplot).

![](Analysis_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
t.test(grams_per_trap~trap_type, data = modified_trap_df)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  grams_per_trap by trap_type
    ## t = -2.9848, df = 2003.7, p-value = 0.002872
    ## alternative hypothesis: true difference in means between group MODIFIED and group UNMODIFIED is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.08549912 -0.01769500
    ## sample estimates:
    ##   mean in group MODIFIED mean in group UNMODIFIED 
    ##                0.9033104                0.9549074
