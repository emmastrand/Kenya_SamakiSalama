Relative abundance analysis of Fishing Landings dataset
================
Author: Emma Strand; <emma_strand@uri.edu>

## Aims and Results

**1. Quantify and visualize relative abundance for each modified and
unmodified trap using phyloseq.**

**2. Alpha and beta diversity for fish species between modified and
unmodified traps. Does using modified traps affect the diversity of fish
caught?**

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
library(Rmisc)
library(stats)
library(lme4)
library(car)
```

Read in the data frame that is the output of the QC script and create
matrix for phyloseq input.
