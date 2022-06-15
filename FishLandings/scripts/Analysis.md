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
```
