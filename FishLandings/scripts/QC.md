Quality Control of Fishing Landings dataset
================
Author: Emma Strand; <emma_strand@uri.edu>

## Contents

- [**Protocol to run this with a future xlsx file**](#protocol)  
- [**Load all libraries**](#libraries)  
- [**Create dataframe**](#df)  
- [**Quality Control: enumerator**](#Enumerator)  
- [**Quality Control: landing_site and BMU**](#Landing_site)  
- [**Quality Control: fisher information**](#fisher_info)  
- [**Quality Control: trap information**](#trap)  
- [**Quality Control: catch information**](#catch)  
- [**Gear type, and fish numbers/final destination**](#gear)  
- [**Quality Control: final check for notes written by field
  team**](#notes)  
- [**Exporting cleaned dataset**](#export)

## <a name="protocol"></a> **Protocol to run this with a future xlsx file**

1.  In the toolbar above, hit the arrow next to `Knit`. Scroll down to
    `Knit Directory` and select the option `Project Directory`.  
2.  **In `Create dataframe` code chunk**: Replace raw data file name in
    the function that creates the following variables:
    fishing_operation, catch_composition, and validation_lists.  
3.  **In `Enumerator`, `Landing_site`, and `BMU` code chunks**: Run the
    unique() function and double check that this list is the correct
    names.  
4.  **In Trap information**, run code chunk functions for `trap_type`
    and `total_traps_collected` sections and double check the output is
    as expected.  
5.  **In catch information**, run code chunk functions for
    `weight and value measures`, `number of fishers in crew`, and
    `Kiswahili_name` and double check the output is as expected.  
6.  **In Species/Scientific name**, run code chunk functions and check
    output to make sure it is as expected.  
7.  **In length, gear type, number of fish, and desintation sections**,
    run code chunk to double check the output of ranges is as
    expected.  
8.  In the export section, rename the new datafile excel file.

## <a name="libraries"></a> **Load all libraries**

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

## <a name="df"></a> **Create dataframe**

**Load in raw data files.**

Most updated file: `Fishlandings-data-FEB 2023 JMCC`.

Previously used this df
`Fishlandings-data- CC-JM-Clay-IW updated 04-09-2022`: The date is
formatted DD-MM-YYYY. This file is from September.

[Reading in an excel datafile instead of a
csv](http://www.sthda.com/english/wiki/reading-data-from-excel-files-xls-xlsx-into-r).
This requires the `readxl` package listed in load all libraries step.

``` r
## when running future iterations of raw data file, replace the file name below 
fishing_operation <- read_excel("data/Fishlandings-data-FEB 2023 JMCC.xlsx", sheet = "fishing_operation",
                                col_types = c("date", "text", "text", "text", "text", "text", "text", 
                                              "text", "numeric", "date", "text", "date", "text", "numeric", 
                                              "numeric", "numeric", "numeric", "numeric", "numeric","text")) %>%
  rename(Operation_date = date_dd_mm_yyyy)

nrow(fishing_operation) 
```

    ## [1] 8851

``` r
# 10,670 July updated file; fishing operations (keep this # in mind for sanity check at the end)
# 8,463 September updated file
# 8,851 February updated file

## when running future iterations of raw data file, replace the file name below 
catch_composition <- read_excel("data/Fishlandings-data-FEB 2023 JMCC.xlsx", 
                                sheet = "catch_composition",
                                col_types = c("date", "text", "text", "text", "text", 
                                              "text", "numeric", "text", "text")) %>%
  rename(Operation_date = Date)

nrow(catch_composition) 
```

    ## [1] 47070

``` r
# 58,441 July updated file; fish observations (keep this # in mind for sanity check at the end)
# 65,357 September updated file
# 47,070 February updated file

## when running future iterations of raw data file, replace the file name below 
validation_lists <- read_excel("data/Fishlandings-data-FEB 2023 JMCC.xlsx", sheet = "validation_lists")

# read in enumerator names file 
enumerator_list <- read_excel("data/enumerator_list.xlsx")
```

Errors found in fisher_id column are mostly capitalization errors.

``` r
fishing_operation$fisher_id <- toupper(fishing_operation$fisher_id)
catch_composition$fisher_id <- toupper(catch_composition$fisher_id)
```

Creating a larger dataframe to work with for the rest of quality
control.

``` r
df <- full_join(fishing_operation, catch_composition, by = c("fisher_id", "Operation_date")) %>%
  select(-`time_in_water (effort)`) %>% # take this out if we start collecting time in water data
  rename(fishing_operation_notes= general_notes) %>%
  rename(catch_composition_notes = notes_picture) %>%
  rename(scientific_name = SPECIES) %>%
  rename(total_biomass_kg = total_weight_kg) 
```

    ## Warning in full_join(fishing_operation, catch_composition, by = c("fisher_id", : Detected an unexpected many-to-many relationship between `x` and `y`.
    ## ℹ Row 1 of `x` matches multiple rows in `y`.
    ## ℹ Row 5 of `y` matches multiple rows in `x`.
    ## ℹ If a many-to-many relationship is expected, set `relationship =
    ##   "many-to-many"` to silence this warning.

## Quality Control

### Operation_date

In the above chunk, these dates are automatically read in as *dttm*
format (date and time).

### <a name="Enumerator"></a> **Enumerator**

[Replace character string with another in
R](https://stackoverflow.com/questions/11936339/replace-specific-characters-within-strings)

A lot of errors occur with different lower and upper case iterations of
names. I replaced this information with all upper case to collapse this
information. For example, “kadzo baya” and “Kadzo baya” are the same
fisherman but were reading as different categories.

Kadzo Baya and Kadzo Kazungu refer to the same person.

``` r
# change all lower case to upper case
df$enumerator <- toupper(df$enumerator)
enumerator_list$enumerator <- toupper(enumerator_list$enumerator)

# replace incorrect spellings 
df$enumerator <- gsub("CELESTINER N ALI", "CELESTINE N. ALI", df$enumerator)
df$enumerator <- gsub("CELESTINAR.N.ALI", "CELESTINE N. ALI", df$enumerator)
df$enumerator <- gsub("^CELESTINAR$", "CELESTINE N. ALI", df$enumerator)
df$enumerator <- gsub("CELESTINAR NALI", "CELESTINE N. ALI", df$enumerator)
df$enumerator <- gsub("CELESTINER NALI", "CELESTINE N. ALI", df$enumerator)
df$enumerator <- gsub("^CLAPERTON$", "CLAPERTON KAZUNGU", df$enumerator)
df$enumerator <- gsub("MACKSON KAZUNGU", "MAXSON KAZUNGU", df$enumerator)
df$enumerator <- gsub("BIDALA RASHID", "BIDALLA RASHID", df$enumerator)
df$enumerator <- gsub("GARAMA YERI", "GARAMA K. YERI", df$enumerator)
df$enumerator <- gsub("GARAMA K YERI", "GARAMA K. YERI", df$enumerator)
df$enumerator <- gsub("BRUNO MUYE", "BRUNO MOYE", df$enumerator)
df$enumerator <- gsub("^ALI$", "CELESTINE N. ALI", df$enumerator) #^ and $ indicate start and end of phrase
df$enumerator <- gsub("KADZO KAZUNGU", "KADZO BAYA", df$enumerator)
df$enumerator <- gsub("KADZU BAYA", "KADZO BAYA", df$enumerator)
df$enumerator <- gsub("ANTONY JUMA", "ANTHONY JUMA", df$enumerator)
df$enumerator <- gsub("CLAPETRON", "CLAPERTON KAZUNGU", df$enumerator)
df$enumerator <- gsub("KARIM NYINGE", "KARIMA NYINGE", df$enumerator)
df$enumerator <- gsub("KARIMA NYINGEE", "KARIMA NYINGE", df$enumerator)
df$enumerator <- gsub("^BRUNO$", "BRUNO MOYE", df$enumerator)
df$enumerator <- gsub("MARKSON KAZUNGU", "MAXSON KAZUNGU", df$enumerator)
df$enumerator <- gsub("BIDAKA RASHID", "BIDALLA RASHID", df$enumerator)

# compare df list to the enumerator list 
# result is those that appear in the df but not validated enumerator list
setdiff(df$enumerator, enumerator_list$enumerator)
```

    ## [1] "SAIDI MKALI" "MWANAPILI"   NA

``` r
## Step #2 in protocol at the top of this script 
unique(sort(df$enumerator)) # at this point, double check that this list are all individual fishermen 
```

    ##  [1] "ANTHONY JUMA"      "BASHIR SAID"       "BIDALLA RASHID"   
    ##  [4] "BRUNO MOYE"        "CELESTINE N. ALI"  "CLAPERTON KAZUNGU"
    ##  [7] "FRANKLINE KAZUNGU" "GARAMA K. YERI"    "GILBERT NZAI"     
    ## [10] "KADZO BAYA"        "KARIMA NYINGE"     "KITSAO KARISA"    
    ## [13] "MAXSON KAZUNGU"    "MWANAPILI"         "NGALA"            
    ## [16] "OMAR ALI"          "SAIDI MKALI"

### <a name="Landing_site"></a> **Landing_site and BMU**

### Landing site

``` r
df$landing_site <- toupper(df$landing_site)
enumerator_list$landing_site <- toupper(enumerator_list$landing_site)

df$landing_site <- gsub("KIRKLAND", "KARKLAND", df$landing_site)
df$landing_site <- gsub("KITANGONI", "KITANGANI", df$landing_site)
df$landing_site <- gsub("KIUKONI", "KIVUKONI", df$landing_site)
df$landing_site <- gsub("KIVUKANI", "KIVUKONI", df$landing_site)
df$landing_site <- gsub("KIVUKUNI", "KIVUKONI", df$landing_site)

# compare df list to the enumerator list 
# result is those that appear in the df but not validated enumerator list
setdiff(df$landing_site, enumerator_list$landing_site)
```

    ## [1] "WHISPERING" "TAKAUNGU"   NA

``` r
unique(sort(df$landing_site))
```

    ##  [1] "BURENI"          "CHAUREMBO"       "KANAMAI"         "KARKLAND"       
    ##  [5] "KIJANGWANI"      "KITANGANI"       "KIVUKONI"        "KIVULINI"       
    ##  [9] "KURUWITU"        "MAWE YA KATI"    "MAYUNGU"         "MWANAMIA"       
    ## [13] "MWENDO WA PANYA" "NGOLOKO"         "SUN N SAND"      "TAKAUNGU"       
    ## [17] "UYOMBO"          "VIPINGO"         "VUMA"            "WHISPERING"

``` r
## Step #3 in protocol to double check this output list is all the correct site names 
## (if the only output from setdiff() is NA then this list is correct)
```

### BMU

``` r
df$BMU <- toupper(df$BMU)
enumerator_list$BMU <- toupper(enumerator_list$BMU)

# compare df list to the enumerator list 
# result is those that appear in the df but not validated enumerator list
setdiff(df$BMU, enumerator_list$BMU)
```

    ## [1] NA

``` r
unique(sort(df$BMU))
```

    ## [1] "KANAMAI"  "KURUWITU" "MAYUNGU"  "TAKAUNGU" "UYOMBO"

``` r
## Step #4 in protocol to double check this output list is all the correct BMU names 
## (if the only output from setdiff() is NA then this list is correct)
```

### <a name="Fisher_info"></a> **Fisher information**

We don’t have a current need to correct fisher phone number right now.

### household_id

The only issue I can detect here is some lower case vs upper case.

It would be nice to have a list of expected household and fisher ID’s.

Some IDs only have 2 digits at the end - are these missing an zero in
front of them? I.e. 90 vs 090?

``` r
df$household_id <- toupper(df$household_id)
unique(df$household_id)
```

    ##   [1] "SS/UYO/SB/091"  "SS/UYO/SB/085"  "SS/UYO/SB/089"  "SS/UYO/SB/088" 
    ##   [5] "SS/UYO/SB/090"  "SS/UYO/SB/092"  "SS/UYO/SB/094"  "SS/UYO/SB/095" 
    ##   [9] "SS/UYO/SB/100"  "SS/UYO/SB/099"  "SS/MAY/SB/002"  "SS/MAY/SB/001" 
    ##  [13] "SS/MAY/SB/013"  "SS/MAY/SB/018"  "SS/MAY/SB/021"  "SS/MAY/SB/049" 
    ##  [17] "SS/MAY/SB/033"  "SS/MAY/SB/032"  "SS/MAY/SB/011"  "SS/MAY/SB/003" 
    ##  [21] "SS/MAY/SB/006"  "SS/MAY/SB/041"  "SS/MAY/SB/074"  "SS/MAY/SB/030" 
    ##  [25] "SS/MAY/SB/077"  "SS/MAY/SB/010"  "SS/MAY/SB/043"  "SS/MAY/SB/042" 
    ##  [29] "SS/MAY/SB/083"  "SS/MAY/SB/052"  "SS/MAY/SB/028"  "SS/MAY/SB/082" 
    ##  [33] "SS/MAY/SB/047"  "SS/MAY/SB/046"  "SS/MAY/SB/048"  "SS/MAY/SB/044" 
    ##  [37] "SS/MAY/SB/036"  "SS/UYO/SB/096"  "SS/MAY/SB/034"  "SS/MAY/SB/038" 
    ##  [41] "SS/MAY/SB/058"  "SS/MAY/SB/017"  "SS/MAY/SB/081"  "SS/MAY/SB/029" 
    ##  [45] "SS/MAY/SB/014"  "SS/MAY/SB/015"  "SS/MAY/SB/053"  "SS/UYO/SB/086" 
    ##  [49] "SS/UYO/SB/097"  "SS/MAY/SB/064"  "SS/MAY/SB/063"  "SS/MAY/SB/076" 
    ##  [53] "SS/UYO/SB/082"  "SS/UYO/SB/079"  "SS/UYO/SB/071"  "SS/UYO/SB/039" 
    ##  [57] "SS/MAY/SB/012"  "SS/MAY/SB/031"  "SS/MAY/SB/070"  "SS/UYO/SB/020" 
    ##  [61] "SS/UYO/SB/093"  "SS/MAY/SB/045"  "SS/MAY/SB/022"  "SS/MAY/SB/078" 
    ##  [65] "SS/MAY/SB/009"  "SS/MAY/SB/065"  "SS/MAY/SB/027"  "SS/MAY/SB/026" 
    ##  [69] "SS/MAY/SB/066"  "SS/MAY/SB/025"  "SS/MAY/SB/068"  "SS/KUR/SG/092" 
    ##  [73] "SS/KUR/SG/020"  "SS/KUR/SG/081"  "SS/KUR/SG/086"  "SS/KUR/SG/029" 
    ##  [77] "SS/KUR/SG/090"  "SS/KUR/SG/087"  "SS/KUR/SG/030"  "SS/KUR/SG/077" 
    ##  [81] "SS/KUR/SG/060"  "SS/KUR/SG/073"  "SS/KUR/SG/075"  "SS/KUR/SG/069" 
    ##  [85] "SS/KUR/SG/064"  "SS/KUR/SG/068"  "SS/KUR/SG/061"  "SS/KUR/SG/071" 
    ##  [89] "SS/KUR/SG/072"  "SS/KUR/SG/010"  "SS/KUR/SG/095"  "SS/KUR/SG/046" 
    ##  [93] "SS/KUR/SG/018"  "SS/KUR/SG/011"  "SS/KUR/SG/021"  "SS/KUR/SG/016" 
    ##  [97] "SS/KUR/SG/038"  "SS/KUR/SG/034"  "SS/KUR/SG/035"  "SS/KUR/SG/048" 
    ## [101] "SS/KUR/SG/096"  "SS/KUR/SG/037"  "SS/KUR/SG/097"  "SS/KUR/SG/040" 
    ## [105] "SS/KUR/SG/098"  "SS/KUR/SG/047"  "SS/KUR/SG/050"  "SS/KUR/SG/091" 
    ## [109] "SS/KUR/SG/070"  "SS/KUR/SG/008"  "SS/KUR/SG/007"  "SS/KUR/SG/022" 
    ## [113] "SS/KUR/SG/083"  "SS/KUR/SG/027"  "SS/KUR/SG/001"  "SS/MAY/SB/020" 
    ## [117] "SS/MAY/SB/059"  "SS/KUR/SG/094"  "SS/KUR/SG/039"  "SS/KUR/SG/036" 
    ## [121] "SS/KUR/SG/033"  "SS/KUR/SG/049"  "SS/KUR/SG/065"  "SS/KUR/SG/099" 
    ## [125] "SS/KUR/SG/054"  "SS/KUR/SG/043"  "SS/KUR/SG/100"  "SS/KUR/SG/082" 
    ## [129] "SS/KUR/SG/002"  "SS/KUR/SG/005"  "SS/KUR/SG/009"  "SS/KUR/SG/013" 
    ## [133] "SS/KUR/SG/004"  "SS/KUR/SG/015"  "SS/KUR/SG/078"  "SS/KUR/SG/032" 
    ## [137] "SS/KUR/SG/006"  "SS/KUR/SG/084"  "SS/KUR/SG/012"  "SS/KUR/SG/024" 
    ## [141] "SS/KUR/SG/026"  "SS/KUR/SG/017"  "SS/KUR/SG/014"  "SS/KUR/SG/028" 
    ## [145] "SS/KUR/SG/062"  "SS/KUR/SG/074"  "SS/KAN/CO/038"  "SS/KAN/CO/042" 
    ## [149] "SS/KAN/CO/043"  "SS/KAN/CO/041"  "SS/KAN/CO/040"  "SS/KAN/CO/039" 
    ## [153] "SS/KAN/CO/117"  "SS/KAN/CO/137"  "SS/KAN/CO/044"  "SS/KAN/CO/113" 
    ## [157] "SS/KAN/CO/024"  "SS/KAN/CO/030"  "SS/KAN/CO/135"  "SS/KAN/CO/088" 
    ## [161] "SS/KAN/CO/036"  "SS/KAN/CO/035"  "SS/KAN/CO/031"  "SS/KAN/CO/025" 
    ## [165] "SS/KAN/CO/026"  "SS/KAN/CO/023"  "SS/KAN/CO/047"  "SS/KAN/CO/022" 
    ## [169] "SS/KAN/CO/020"  "SS/TAK/CO/192"  "SS/TAK/CO/147"  "SS/TAK/CO/157" 
    ## [173] "SS/TAK/CO/164"  "SS/TAK/CO/151"  "SS/TAK/CO/153"  "SS/TAK/CO/152" 
    ## [177] "SS/TAK/CO/146"  "SS/TAK/CO/159"  "SS/TAK/CO/166"  "SS/TAK/CO/160" 
    ## [181] "SS/TAK/CO/150"  "SS/KAN/CO/001"  "SS/KAN/CO/054"  "SS/KAN/CO/076" 
    ## [185] "SS/KAN/CO/051"  "SS/KAN/CO/091"  "SS/KAN/CO/093"  "SS/KAN/CO/075" 
    ## [189] "SS/KAN/CO/092"  "SS/KAN/CO/057"  "SS/KAN/CO/065"  "SS/KAN/CO/090" 
    ## [193] "SS/KAN/CO/060"  "SS/TAK/CO/173"  "SS/TAK/CO/172"  "SS/TAK/CO/167" 
    ## [197] "SS/TAK/CO/168"  "SS/TAK/CO/199"  "SS/TAK/CO/182"  "SS/TAK/CO/183" 
    ## [201] "SS/TAK/CO/143"  "SS/TAK/CO/171"  "SS/TAK/CO/169"  "SS/TAK/CO/201" 
    ## [205] "SS/TAK/CO/191"  "SS/TAK/CO/170"  "SS/TAK/CO/120"  "SS/UYO/SB/084" 
    ## [209] "SS/KUR/SG/041"  "SS/KAN/CO/012"  "SS/KAN/CO/013"  "SS/KAN/CO/014" 
    ## [213] "SS/KAN/CO/016"  "SS/KAN/CO/017"  "SS/KAN/CO/082"  "SS/KAN/CO/083" 
    ## [217] "SS/KAN/CO/084"  "SS/KAN/CO/086"  "SS/KAN/CO/136"  "SS/KAN/CO/067" 
    ## [221] "SS/KAN/CO/106"  "SS/KAN/CO/127"  "SS/KAN/CO/071"  "SS/KAN/CO/107" 
    ## [225] "SS/TAK/CO/149"  "SS/TAK/CO/206"  "SS/TAK/CO/165"  "SS/TAK/CO/158" 
    ## [229] "SS/TAK/CO/163"  "SS/TAK/CO/155"  "SS/KAN/CO/018"  "SS/TAK/CO/145" 
    ## [233] "SS/TAK/CO/154"  "SS/TAK/CO/148"  "SS/KAN/CO/077"  "SS/KAN/CO/072" 
    ## [237] "SS/KAN/CO/172"  "SS/KAN/CO/183"  "SS/KAN/CO/171"  "SS/KAN/CO/167" 
    ## [241] "SS/KAN/CO/173"  "SS/TAK/CO/205"  "SS/TAK/CO/161"  "SS/TAK/CO/067" 
    ## [245] "SS/TAK/CO/162"  "SS/TAK/CO/174"  "SS/TAK/CO/038"  "SS/KAN/CO/034" 
    ## [249] "SS/KAN/CO/085"  "SS/KAN/CO/133"  "SS/KAN/CO/078"  "SS/KAN/CO/140" 
    ## [253] "SS/TAK/CO/195"  "SS/TAK/CO/144"  "SS/TAK/CO/142"  "SS/TAK/CO/176" 
    ## [257] "SS/TAK/CO/198"  "SS/TAK/CO/179"  "SS/KUR/SG/133"  "SS/TAK/CO/197" 
    ## [261] "SS/TAK/CO/194"  "SS/TAK/CO/175"  "SS/TAK/CO/178"  "SS/KAN/CO/073" 
    ## [265] "SS/KUR/SG/042"  "SS/KUR/SG/076"  "SS/KAN/CO/099"  "SS/KAN/CO/074" 
    ## [269] "SS/TAK/CO/177"  "SS/KAN/CO/015"  "SS/KUR/SG/069F" "SS/TAK/CO/203" 
    ## [273] "SS/TAK/CO/117"  "SS/KAN/CO/102"  "SS/TAK/CO/180"  "SS/TAK/CO/181" 
    ## [277] "SS/KUR/SG/057"  "SS/KAN/CO/011"  "SS/TAK/CO/196"  "SS/TAK/CO/135" 
    ## [281] "SS/TAK/CO/036"  "SS/TAK/CO/035"  "SS/TAK/CO/031"  "SS/TAK/CO/023" 
    ## [285] "SS/TAK/CO/026"  "SS/TAK/CO/030"  "SS/TAK/CO/088"  "SS/TAK/CO/024" 
    ## [289] "SS/MAY/SB/035"  "SS/MAY/SB/039"  "SS/MAY/SB/071"  "SS/MAY/SB/084" 
    ## [293] "SS/MAY/SB/090"  "SS/MAY/SB/096"  "SS/MAY/SB/019"  "SS/MAY/SB/072" 
    ## [297] "SS/MAY/SB/004"  "SS/UYO/SB/021"  "SS/MAY/SB/005"  NA

``` r
df$household_id <- gsub("/FF", "", df$household_id)
```

### <a name="trap"></a> **Trap information**

### trap_type

The only issue I can detect here is some lower case vs upper case.

``` r
df$trap_type <- toupper(df$trap_type)
unique(sort(df$trap_type))
```

    ##  [1] "CASTNET"                "GILLNET"                "HANDLINE"              
    ##  [4] "MODIFIED TRAP"          "MONOFILAMENT"           "OCTOPUS HOOK"          
    ##  [7] "SEINE NET"              "SPEARGUN"               "SPEARGUN AND SEINE NET"
    ## [10] "UNMODIFIED"

### total_traps_collected

View the values put in the df here to double check all values make
sense.

``` r
total_traps_collected <- df %>% select(total_traps_collected) %>% na.omit()
range(total_traps_collected)
```

    ## [1]  1 48

``` r
## filtering traps to below 40
df$total_traps_collected <- ifelse(df$total_traps_collected > 16, NA, df$total_traps_collected)

## Protocol with new df: double check the below range is expected 
hist(df$total_traps_collected)
```

![](QC_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

### Date and time set; date and time collected

In the first chunk of code, these dates are automatically read in as
*dttm* format (date and time). This new columns will be more useful for
data analysis later.

``` r
# making new columns for date and time 
df$date_time_set <- paste(df$date_set_dd_mm_yyyy, df$`time_set_24hh:mm`, sep = " ")
df$date_time_collected <- paste(df$date_collected_dd_mm_yyyy, df$`time_collected_24hh:mm`, sep = " ")

# removing the 'hrs' from observations in this column
df$date_time_set <- gsub("hrs", "", df$date_time_set)
df$date_time_collected <- gsub("hrs", "", df$date_time_collected)

# converting to date and time format 
df$date_time_set <- parse_date_time(df$date_time_set, orders = "ymdHM")
df$date_time_collected <- parse_date_time(df$date_time_collected, orders = "ymdHM")

## any failed to parse error messages will be from rows that do not have a date and time
```

### <a name="catch"></a> **Catch information**

### Biomass and value measures

Step \#4 from protocol: Double check the below values are in the correct
ranges

``` r
##### TOTAL BIOMASS 
total_biomass_kg <- df %>% select(total_biomass_kg) %>% na.omit()
range(total_biomass_kg)
```

    ## [1]   0.125 490.000

``` r
# filtering biomass kg to under 100 kg 
df$total_biomass_kg <- ifelse(df$total_biomass_kg > 100, NA, df$total_biomass_kg)
hist(df$total_biomass_kg)
```

![](QC_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
##### TAKE HOME BIOMASS 
# filtering take home weight to be under 10kg 
take_home_weight_kg <- df %>% select(take_home_weight_kg) %>% na.omit()
df$take_home_weight_kg <- ifelse(df$take_home_weight_kg > 10, NA, df$take_home_weight_kg)

range(take_home_weight_kg)
```

    ## [1]     0 23040

``` r
hist(df$take_home_weight_kg)
```

![](QC_files/figure-gfm/unnamed-chunk-12-2.png)<!-- -->

``` r
unique(sort(df$take_home_weight_kg))
```

    ##   [1] 0.0000 0.1000 0.1500 0.2000 0.2150 0.2390 0.2500 0.2870 0.2940 0.3000
    ##  [11] 0.3110 0.3670 0.3700 0.3900 0.3950 0.4000 0.4050 0.4100 0.4130 0.4200
    ##  [21] 0.4700 0.4750 0.4800 0.4900 0.5000 0.5100 0.5200 0.5300 0.5350 0.5400
    ##  [31] 0.5600 0.5700 0.5800 0.5900 0.5980 0.6000 0.6200 0.6210 0.6370 0.6380
    ##  [41] 0.6400 0.6500 0.6590 0.6640 0.6700 0.6720 0.6800 0.7000 0.7100 0.7120
    ##  [51] 0.7200 0.7300 0.7350 0.7380 0.7400 0.7500 0.7600 0.7800 0.7850 0.7900
    ##  [61] 0.8000 0.8100 0.8200 0.8400 0.8550 0.8600 0.8650 0.8700 0.8900 0.9000
    ##  [71] 0.9100 0.9200 0.9300 0.9350 0.9400 0.9450 0.9500 0.9550 0.9600 0.9650
    ##  [81] 0.9670 0.9700 0.9750 0.9800 0.9820 0.9870 1.0000 1.0110 1.0300 1.0350
    ##  [91] 1.0500 1.0900 1.1000 1.1250 1.1550 1.1600 1.1700 1.2000 1.2100 1.2500
    ## [101] 1.2700 1.3000 1.3100 1.3120 1.3200 1.3700 1.3900 1.4000 1.4100 1.4120
    ## [111] 1.4200 1.4700 1.5000 1.5112 1.5120 1.5170 1.5350 1.5450 1.5700 1.5900
    ## [121] 1.6000 1.6120 1.6130 1.6170 1.6180 1.6270 1.6300 1.6320 1.6700 1.6810
    ## [131] 1.7000 1.7010 1.7030 1.7110 1.7800 1.8000 1.8100 1.8110 1.8120 1.9000
    ## [141] 1.9030 2.0000 2.0100 2.1000 2.1100 2.2000 2.3000 2.4000 2.5000 2.6000
    ## [151] 2.6200 2.7000 2.7100 2.8000 2.8900 2.9000 3.0000 3.1000 3.2400 3.3000
    ## [161] 3.4000 3.4800 3.5000 3.7780 4.0000 4.2000 4.5000 6.0000 7.0000 8.0000
    ## [171] 9.0000

``` r
##### TOTAL VALUE 
total_value_KES <- df %>% select(total_value_KES) %>% na.omit()
range(total_value_KES)
```

    ## [1]      1 137200

``` r
# filtering to below 5,000 
df$total_value_KES <- ifelse(df$total_value_KES > 5000, NA, df$total_value_KES)
hist(df$total_value_KES)
```

![](QC_files/figure-gfm/unnamed-chunk-12-3.png)<!-- -->

``` r
##### TAKE HOME VALUE 
take_home_value_KES <- df %>% select(take_home_value_KES) %>% na.omit()
df$take_home_value_KES <- ifelse(df$take_home_value_KES > 5000, NA, df$take_home_value_KES)

range(take_home_value_KES)
```

    ## [1]    0 7500

``` r
boxplot(df$take_home_value_KES)
```

![](QC_files/figure-gfm/unnamed-chunk-12-4.png)<!-- -->

``` r
unique(sort(df$take_home_value_KES))
```

    ##   [1]    0.00   20.00   30.00   31.00   40.00   43.05   44.00   48.00   50.00
    ##  [10]   51.00   52.50   55.98   60.00   61.00   62.00   66.00   67.00   70.00
    ##  [19]   75.00   77.00   80.00   84.00   85.00   88.00   90.00   93.00   95.00
    ##  [28]   96.00   97.50   98.00  100.00  102.00  104.00  105.00  108.00  109.00
    ##  [37]  110.00  110.25  113.00  116.00  117.90  119.00  120.00  122.00  125.00
    ##  [46]  126.00  127.60  130.00  131.00  132.00  135.00  136.00  140.00  141.00
    ##  [55]  146.25  147.00  150.00  153.00  154.00  154.50  155.00  160.00  163.00
    ##  [64]  165.00  167.00  168.00  170.00  175.00  176.00  180.00  183.00  187.50
    ##  [73]  188.10  189.00  190.00  195.00  198.00  200.00  201.00  201.25  204.00
    ##  [82]  210.00  210.10  212.00  215.00  220.00  221.00  225.00  231.00  232.00
    ##  [91]  234.00  240.00  245.00  250.00  252.00  255.00  260.00  261.00  265.00
    ## [100]  270.00  280.00  283.50  285.00  286.00  288.00  290.00  294.00  300.00
    ## [109]  308.00  309.00  310.00  315.00  320.00  325.00  330.00  337.50  338.00
    ## [118]  340.00  345.00  347.00  360.00  375.00  378.00  380.00  390.00  400.00
    ## [127]  405.00  420.00  435.00  440.00  450.00  460.00  475.00  480.00  484.00
    ## [136]  495.00  500.00  510.00  520.00  525.00  540.00  560.00  600.00  625.00
    ## [145]  650.00  700.00  750.00  800.00  845.00  880.00  900.00  924.00  938.00
    ## [154]  973.00 1044.00 1050.00 1120.00 1133.00 1200.00 1400.00 2000.00 2600.00
    ## [163] 2700.00 3500.00 4000.00 4500.00 5000.00

### No. of fishers in crew

Crews above 7 people are unrealistic. I’m changing that data to ‘NA’ for
now.

``` r
fishermen_no <- df %>% select(`No. of fishers in crew`) %>% na.omit()

hist(fishermen_no)
```

![](QC_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
## Protocol: Double check the below values are in the correct ranges
range(fishermen_no)
```

    ## [1]  1 38

``` r
unique(sort(fishermen_no$`No. of fishers in crew`))
```

    ##  [1]  1  2  3  4  5  6  7  8 10 12 16 18 19 20 22 23 24 34 38

``` r
df %>% filter(`No. of fishers in crew` > 7)
```

    ## # A tibble: 482 × 28
    ##    Operation_date      enumerator      landing_site BMU   fisher_id fisher_phone
    ##    <dttm>              <chr>           <chr>        <chr> <chr>     <chr>       
    ##  1 2021-05-18 00:00:00 CELESTINE N. A… MAYUNGU      MAYU… SS/MAY/S… 706822197   
    ##  2 2021-05-18 00:00:00 CELESTINE N. A… MAYUNGU      MAYU… SS/MAY/S… 706822197   
    ##  3 2021-05-18 00:00:00 CELESTINE N. A… MAYUNGU      MAYU… SS/MAY/S… 706822197   
    ##  4 2021-05-18 00:00:00 CELESTINE N. A… MAYUNGU      MAYU… SS/MAY/S… 706822197   
    ##  5 2021-05-18 00:00:00 CELESTINE N. A… MAYUNGU      MAYU… SS/MAY/S… 706822197   
    ##  6 2021-05-18 00:00:00 CELESTINE N. A… MAYUNGU      MAYU… SS/MAY/S… 706822197   
    ##  7 2021-05-18 00:00:00 CELESTINE N. A… MAYUNGU      MAYU… SS/MAY/S… 706822197   
    ##  8 2021-05-18 00:00:00 CELESTINE N. A… MAYUNGU      MAYU… SS/MAY/S… 706822197   
    ##  9 2021-11-15 00:00:00 SAIDI MKALI     KIVUKONI     TAKA… SS/TAK/C… <NA>        
    ## 10 2021-11-26 00:00:00 KITSAO KARISA   VUMA         TAKA… SS/TAK/C… 791377013   
    ## # ℹ 472 more rows
    ## # ℹ 22 more variables: household_id <chr>, trap_type <chr>,
    ## #   total_traps_collected <dbl>, date_set_dd_mm_yyyy <dttm>,
    ## #   `time_set_24hh:mm` <chr>, date_collected_dd_mm_yyyy <dttm>,
    ## #   `time_collected_24hh:mm` <chr>, total_biomass_kg <dbl>,
    ## #   take_home_weight_kg <dbl>, total_value_KES <dbl>,
    ## #   take_home_value_KES <dbl>, `No. of fishers in crew` <dbl>, …

``` r
# df <- df %>%
#  mutate(crew_size = case_when(
#     `No. of fishers in crew` > 7 ~ "NA"))

df$crew_size_corrected <- df$`No. of fishers in crew`

# replacing values higher than 5 with NA
df <- df %>%
 replace_with_na_at(
    .vars = 'crew_size_corrected',
    condition = ~(.x > 7))

# double checking that the above worked
unique(sort(df$crew_size_corrected))
```

    ## [1] 1 2 3 4 5 6 7

### Kiswahili_name

``` r
df$Kiswahili_name <- toupper(df$Kiswahili_name)
unique(sort(df$Kiswahili_name))
```

    ##   [1] "BARAKUDA"                 "BATANI"                  
    ##   [3] "BIRINZI"                  "BOCHO"                   
    ##   [5] "BUA"                      "BUA/MBORO YA MVUA"       
    ##   [7] "BUNDU"                    "BUNJU"                   
    ##   [9] "CHAA"                     "CHALE"                   
    ##  [11] "CHANGU"                   "CHANGU GAMWE"            
    ##  [13] "CHANGU MACHO"             "CHANGU NDOMO"            
    ##  [15] "CHANGU TAWA"              "CHEMBEU"                 
    ##  [17] "CHENA"                    "DOME"                    
    ##  [19] "FUTE"                     "FUTE KUMBI"              
    ##  [21] "FUTE MLEA"                "FUTE MOSHI"              
    ##  [23] "FUTE MRABA"               "FUTE MRAMBA"             
    ##  [25] "GENDA"                    "GONA"                    
    ##  [27] "GONA SHARIFU"             "GONO"                    
    ##  [29] "JAME"                     "JANARE"                  
    ##  [31] "JODARI"                   "KADA"                    
    ##  [33] "KADIFU"                   "KAKURUWENDE"             
    ##  [35] "KAMBA KOLOLO"             "KAMBA SHUARI"            
    ##  [37] "KAMBA SIMBA"              "KAMBA WINDU"             
    ##  [39] "KANGAJA"                  "KANGAJA HEWANI"          
    ##  [41] "KANGAJA HEWENI"           "KANGAJI HEWANI"          
    ##  [43] "KASIKI"                   "KATATANGE"               
    ##  [45] "KERENGE"                  "KHADA"                   
    ##  [47] "KHADA/TEWA"               "KIBAA"                   
    ##  [49] "KIBOMA"                   "KIFUDU"                  
    ##  [51] "KIFUDUU"                  "KIFUVUU"                 
    ##  [53] "KIJAME"                   "KIKANDE"                 
    ##  [55] "KIKOKWE"                  "KINAUCHI"                
    ##  [57] "KINGOE"                   "KINWA UCHI"              
    ##  [59] "KINYWAUCHI"               "KIPEPEO"                 
    ##  [61] "KITATANGE"                "KIUNGA"                  
    ##  [63] "KOLEKOLE"                 "KOPWE"                   
    ##  [65] "KORIS"                    "KORISI"                  
    ##  [67] "KOTWE"                    "KUFI"                    
    ##  [69] "KUFI SAFARI"              "KUFI/KIMBULIMBULI/KUKUSI"
    ##  [71] "KUMBA"                    "KUMBI"                   
    ##  [73] "KUMBI FUTE"               "KUNGU"                   
    ##  [75] "LWAYOO"                   "MABACHO"                 
    ##  [77] "MBININI"                  "MBONO"                   
    ##  [79] "MBORO YA MVUVI"           "MCHAKUFA"                
    ##  [81] "MGENDA"                   "MKIZI"                   
    ##  [83] "MKIZI KOMWE"              "MKORE"                   
    ##  [85] "MKORWE"                   "MKUNDAJI"                
    ##  [87] "MKUNGA"                   "MKUNGA CHAI"             
    ##  [89] "MKUNGA CHUI"              "MKUNGA IBRAHIM"          
    ##  [91] "MKUNGA MBONO"             "MKUNGA SAMAKI"           
    ##  [93] "MKUNGAJI"                 "MLEA"                    
    ##  [95] "MLEYA"                    "MNGENDA"                 
    ##  [97] "MTANI"                    "MTONZI"                  
    ##  [99] "MTUMBO WA DAU"            "MTUMBUA"                 
    ## [101] "MTUMBUA DAU"              "MTUMBUU"                 
    ## [103] "MTUNE"                    "MUGENDA"                 
    ## [105] "NGAGU"                    "NGANGU"                  
    ## [107] "NGINDO"                   "NGISI"                   
    ## [109] "NGOGO"                    "NGURU"                   
    ## [111] "NJANA"                    "NUMBA"                   
    ## [113] "NYAVI"                    "NYENGA"                  
    ## [115] "PAKOE"                    "PAMAMBA"                 
    ## [117] "PANDU"                    "PANGA"                   
    ## [119] "PANGA SAMAKI"             "PAPA"                    
    ## [121] "PAROTI"                   "PONO"                    
    ## [123] "PONO  MWANI"              "PONO BLUE"               
    ## [125] "PONO BLUE FISH"           "PONO CHANI"              
    ## [127] "PONO DASI"                "PONO DATSI"              
    ## [129] "PONO KABANGI"             "PONO KADIFI"             
    ## [131] "PONO KADIFU"              "PONO KASIKI"             
    ## [133] "PONO KIDIFU"              "PONO MWANI"              
    ## [135] "PONO SUNGURA"             "PUJI"                    
    ## [137] "PUJI PEMBE"               "PUJU"                    
    ## [139] "PUJU PEMBE"               "PUNDU"                   
    ## [141] "PWEZA"                    "SANGE"                   
    ## [143] "SENDENGOMANI"             "SHANA"                   
    ## [145] "SHANA/TUGUU"              "SHARIFU"                 
    ## [147] "SIMU"                     "SITEFUE"                 
    ## [149] "STEFUE"                   "SULISULI"                
    ## [151] "TAA"                      "TAA YEDA"                
    ## [153] "TAF KITUMBO"              "TAFI"                    
    ## [155] "TAFI KITUMBO"             "TAFI MAENGA"             
    ## [157] "TAFI MAENGE"              "TAFI MANGA"              
    ## [159] "TAFI MIMBA"               "TAFI MWAMBA"             
    ## [161] "TAKUANA"                  "TAWE JESHI"              
    ## [163] "TEMBO"                    "TENGESI"                 
    ## [165] "TEWA"                     "TEWA JESHI"              
    ## [167] "TEWA KALESO"              "TEWA KOPE"               
    ## [169] "TEWA KOPWE"               "TEWA LESO"               
    ## [171] "TEWA MOSHI"               "TEWA THARAFA"            
    ## [173] "TEWA WIMBI"               "TEWEJESHI"               
    ## [175] "TOA"                      "TOGOO"                   
    ## [177] "TONDO"                    "TUGUU"                   
    ## [179] "TUKUANA"                  "TUNDU"                   
    ## [181] "UKUZI"                    "UNA"                     
    ## [183] "USENDE NGOMANI"           "VEMBEU"                  
    ## [185] "VUMBAMA"                  "VUMBANA"                 
    ## [187] "WAYO"                     "WAYO ULIMI"              
    ## [189] "WAYO ULIMI NG'OMBE"       "WAYO/ULIMI WA NG'OMBE"   
    ## [191] "WAYOO"

### SPECIES / Scientific name

**This is a hard-coding way to do this.. ideally we could downloand a
dataset from fishbase and create a compare function that could recognize
a name that is a letter or 2 off from a name in fishbase and then create
suggestions…**

We can pull in the validation_lists df to double check these spellings.

``` r
# Taking out double spaces in between genus and species 
validation_lists$scientific_name <- gsub("  ", " ", validation_lists$scientific_name)
```

Sorting through df for errors.

``` r
# create capitalize function for upper case genus and lower case species 
capitalize_fxn <- function(x){
  first <- toupper(substr(x, start=1, stop=1)) ## capitalize first letter
  rest <- tolower(substr(x, start=2, stop=nchar(x)))   ## everything else lowercase
  paste0(first, rest)
}

df$scientific_name <- capitalize_fxn(df$scientific_name)
validation_lists$scientific_name <- capitalize_fxn(validation_lists$scientific_name)

# Taking out double spaces in between genus and species 
df$scientific_name <- gsub("  ", " ", df$scientific_name)

# Correcting commonly misspelled genus names 

# this chunk is for one genus. This is a data input issue that is difficult to catch and fix in R
df$scientific_name <- gsub("Acanthrus", "Acanthurus", df$scientific_name)
df$scientific_name <- gsub("Acantharus", "Acanthurus", df$scientific_name)
df$scientific_name <- gsub("Acantrus", "Acanthurus", df$scientific_name)
df$scientific_name <- gsub("Acantarus", "Acanthurus", df$scientific_name)
df$scientific_name <- gsub("Acathurus", "Acanthurus", df$scientific_name)
df$scientific_name <- gsub("Acronthurus", "Acanthurus", df$scientific_name)
df$scientific_name <- gsub("Acanthrurus", "Acanthurus", df$scientific_name)
df$scientific_name <- gsub("dossumieri", "dussumieri", df$scientific_name)
df$scientific_name <- gsub("dusimieri", "dussumieri", df$scientific_name)
df$scientific_name <- gsub("dusimii", "dussumieri", df$scientific_name)
df$scientific_name <- gsub("dussimieri", "dussumieri", df$scientific_name)
df$scientific_name <- gsub("Abdefduf", "Abudefduf", df$scientific_name)
df$scientific_name <- gsub("Cheilo", "Cheilio", df$scientific_name)
df$scientific_name <- gsub("inemis", "inermis", df$scientific_name)
df$scientific_name <- gsub("argentmaculatus", "argentimaculatus", df$scientific_name)
df$scientific_name <- gsub("Cheillinus", "Cheilinus", df$scientific_name)
df$scientific_name <- gsub("candiculatus", "canaliculatus", df$scientific_name)
df$scientific_name <- gsub("canaliculutus", "canaliculatus", df$scientific_name)
df$scientific_name <- gsub("Cholurururs", "Chlorurus", df$scientific_name)
df$scientific_name <- gsub("stronycephalus", "strongylocephalus", df$scientific_name)
df$scientific_name <- gsub("Gymonthorax", "Gymnothorax", df$scientific_name)
df$scientific_name <- gsub("Gymothorax", "Gymnothorax", df$scientific_name)
df$scientific_name <- gsub("javanicus", "favagineus", df$scientific_name)
df$scientific_name <- gsub("vaiginsis", "vaigiensis", df$scientific_name)
df$scientific_name <- gsub("semicirculatus", "semicirculatus", df$scientific_name)
df$scientific_name <- gsub("semisulcatus", "semicirculatus", df$scientific_name)
df$scientific_name <- gsub("Pomacnathus", "Pomacanthus", df$scientific_name)
df$scientific_name <- gsub("granoculis", "grandoculis", df$scientific_name)
df$scientific_name <- gsub("malanostigma", "melanostigma", df$scientific_name)
df$scientific_name <- gsub("hard", "harid", df$scientific_name)
df$scientific_name <- gsub("sexfaciatus", "sexfasciatus", df$scientific_name)
df$scientific_name <- gsub("dussumiera", "dussumieri", df$scientific_name)
df$scientific_name <- gsub("caeruleopanctatus", "caeruleopunctatus", df$scientific_name)
df$scientific_name <- gsub("hebei", "heberi", df$scientific_name)
df$scientific_name <- gsub("kippos", "hippos", df$scientific_name)
df$scientific_name <- gsub("Carnx", "Caranx", df$scientific_name)
df$scientific_name <- gsub("coioidea", "coioides", df$scientific_name)
df$scientific_name <- gsub("monochrou", "monochrous", df$scientific_name)
df$scientific_name <- gsub("monochrouss", "monochrous", df$scientific_name)
df$scientific_name <- gsub("Kyphasus", "Kyphosus", df$scientific_name)
df$scientific_name <- gsub("Lenthrinus", "Lethrinus", df$scientific_name)
df$scientific_name <- gsub("Leturinus", "Lethrinus", df$scientific_name)
df$scientific_name <- gsub("vaiguensis", "vaigiensis", df$scientific_name)
df$scientific_name <- gsub("bornonicus", "borbonicus", df$scientific_name)
df$scientific_name <- gsub("nebulosis", "nebulosus", df$scientific_name)
df$scientific_name <- gsub("nebulous", "nebulosus", df$scientific_name)
df$scientific_name <- gsub("Leptoscaus", "Leptoscarus", df$scientific_name)
df$scientific_name <- gsub("fluluiflamma", "fulviflamma", df$scientific_name)
df$scientific_name <- gsub("flavlineathe", "flavolineatus", df$scientific_name)
df$scientific_name <- gsub("taeniourous", "taeniorus", df$scientific_name)
df$scientific_name <- gsub("Navaculichthys", "Novaculichthys", df$scientific_name)
df$scientific_name <- gsub("taeniorus", "taeniourus", df$scientific_name)
df$scientific_name <- gsub("Parupeneus sp nov.", "Parupeneus", df$scientific_name)
df$scientific_name <- gsub("Platux", "Platax", df$scientific_name)
df$scientific_name <- gsub("platyyuna", "platyura", df$scientific_name)
df$scientific_name <- gsub("playfair", "playfairi", df$scientific_name)
df$scientific_name <- gsub("Plectorhincus", "Plectorhinchus", df$scientific_name)
df$scientific_name <- gsub("Plectorhnichus", "Plectorhinchus", df$scientific_name)
df$scientific_name <- gsub("Plotasus", "Plotosus", df$scientific_name)
df$scientific_name <- gsub("Pomatonus", "Pomatomus", df$scientific_name)
df$scientific_name <- gsub("Rhinecanthurus", "Rhineacanthus", df$scientific_name)
df$scientific_name <- gsub("vubroviolaceus", "rubroviolaceus", df$scientific_name)
df$scientific_name <- gsub("sirubroviolaceus", "rubroviolaceus", df$scientific_name)
df$scientific_name <- gsub("Scromberomorus", "Scomerommorus", df$scientific_name)
df$scientific_name <- gsub("Sphraena", "Sphyraena", df$scientific_name)
df$scientific_name <- gsub("meyeri", "meyeni", df$scientific_name)
df$scientific_name <- gsub("triostregus", "triostegus", df$scientific_name)
df$scientific_name <- gsub("Adudefduf", "Abudefduf", df$scientific_name)
df$scientific_name <- gsub("scoplas", "scopas", df$scientific_name)
df$scientific_name <- gsub("xanthonta", "xanthonota", df$scientific_name)
df$scientific_name <- gsub("Carangoifes", "Carangoides", df$scientific_name)
df$scientific_name <- gsub("vippos", "hippos", df$scientific_name)
df$scientific_name <- gsub("Cephelopholu", "Cephalopholis", df$scientific_name)
df$scientific_name <- gsub("Chaetadon", "Chaetodon", df$scientific_name)
df$scientific_name <- gsub("auringa", "auriga", df$scientific_name)
df$scientific_name <- gsub("selen$", "selene", df$scientific_name) # $ indicates end of phrase; didnt use ^ because this is species name is the 2nd word 
df$scientific_name <- gsub("trilohatus", "trilobatus", df$scientific_name)
df$scientific_name <- gsub("Cheiellinus", "Cheilinus", df$scientific_name)
df$scientific_name <- gsub("Cheillnus", "Cheilinus", df$scientific_name)
df$scientific_name <- gsub("inerms", "inermis", df$scientific_name)
df$scientific_name <- gsub("piinnulatus", "pinnulatus", df$scientific_name)
df$scientific_name <- gsub("pinnulatrus", "pinnulatus", df$scientific_name)
df$scientific_name <- gsub("Cirrihitus", "Cirrhitus", df$scientific_name)
df$scientific_name <- gsub("farmosa", "formosa", df$scientific_name)
df$scientific_name <- gsub("Cymonthorax", "Gymnothorax", df$scientific_name)
df$scientific_name <- gsub("Cynoglassus", "Cynoglossus", df$scientific_name)
df$scientific_name <- gsub("lachnen", "lachneri", df$scientific_name)
df$scientific_name <- gsub("luchneri", "lachneri", df$scientific_name)
df$scientific_name <- gsub("Epimephelus", "Epinephelus", df$scientific_name)
df$scientific_name <- gsub("colodes", "coioides", df$scientific_name)
df$scientific_name <- gsub("coicoides", "coioides", df$scientific_name)
df$scientific_name <- gsub("coloides", "coioides", df$scientific_name)
df$scientific_name <- gsub("faragineus", "favagineus", df$scientific_name)
df$scientific_name <- gsub("favagineous", "favagineus", df$scientific_name)
df$scientific_name <- gsub("hortulatus", "hortulanus", df$scientific_name)
df$scientific_name <- gsub("Himantur", "Himantura", df$scientific_name)
df$scientific_name <- gsub("Himanturaa", "Himantura", df$scientific_name)
df$scientific_name <- gsub("Hippscarus", "Hipposcarus", df$scientific_name)
df$scientific_name <- gsub("vagiensis", "vaigiensis", df$scientific_name)
df$scientific_name <- gsub("vaigienesis", "vaigiensis", df$scientific_name)
df$scientific_name <- gsub("fuaviflamma", "fulviflamma", df$scientific_name)
df$scientific_name <- gsub("fluliuflamma", "fulviflamma", df$scientific_name)
df$scientific_name <- gsub("fuluvifiamma", "fulviflamma", df$scientific_name)
df$scientific_name <- gsub("fulvifiamma", "fulviflamma", df$scientific_name)
df$scientific_name <- gsub("Latjanus", "Lutjanus", df$scientific_name)
df$scientific_name <- gsub("conchiliatus", "conchliatus", df$scientific_name)
df$scientific_name <- gsub("conchuliatutus", "conchliatus", df$scientific_name)
df$scientific_name <- gsub("conchyliantus", "conchliatus", df$scientific_name)
df$scientific_name <- gsub("hara$", "harak", df$scientific_name)
df$scientific_name <- gsub("harar$", "harak", df$scientific_name)
df$scientific_name <- gsub("letjan$", "lentjan", df$scientific_name)
df$scientific_name <- gsub("olivacous$", "olivaceus", df$scientific_name)
df$scientific_name <- gsub("Letjanus", "Lutjanus", df$scientific_name)
df$scientific_name <- gsub("Liza", "Planiliza", df$scientific_name)
df$scientific_name <- gsub("alatar$", "alata", df$scientific_name)
df$scientific_name <- gsub("argemtimaculutus", "argentimaculatus", df$scientific_name)
df$scientific_name <- gsub("argentinmaculatus", "argentimaculatus", df$scientific_name)
df$scientific_name <- gsub("ghibbon$", "gibbus", df$scientific_name)
df$scientific_name <- gsub("Lutjan", "Lutjanus", df$scientific_name)
df$scientific_name <- gsub("Lutjanusus", "Lutjanus", df$scientific_name)
df$scientific_name <- gsub("Migul", "Mugil", df$scientific_name)
df$scientific_name <- gsub("Monodactytus", "Monodactylus", df$scientific_name)
df$scientific_name <- gsub("bernditi", "berndti", df$scientific_name)
df$scientific_name <- gsub("berndt", "berndti", df$scientific_name)
df$scientific_name <- gsub("berndtii", "berndti", df$scientific_name)
df$scientific_name <- gsub("annalutus", "annulatus", df$scientific_name)
df$scientific_name <- gsub("anna$", "annulatus", df$scientific_name)
df$scientific_name <- gsub("annaturus", "annulatus", df$scientific_name)
df$scientific_name <- gsub("annulator", "annulatus", df$scientific_name)
df$scientific_name <- gsub("annulutus", "annulatus", df$scientific_name)
df$scientific_name <- gsub("annunthurus$", "annulatus", df$scientific_name)
df$scientific_name <- gsub("annuthurus$", "annulatus", df$scientific_name)
df$scientific_name <- gsub("brachycentus$", "brachycentron", df$scientific_name)
df$scientific_name <- gsub("bracycentron", "brachycentron", df$scientific_name)
df$scientific_name <- gsub("branchycentron", "brachycentron", df$scientific_name)
df$scientific_name <- gsub("unicaris", "unicornis", df$scientific_name)
df$scientific_name <- gsub("oyanea$", "cyanea", df$scientific_name)
df$scientific_name <- gsub("Panacirus", "Panulirus", df$scientific_name)
df$scientific_name <- gsub("Panilirus", "Panulirus", df$scientific_name)
df$scientific_name <- gsub("Panulinus", "Panulirus", df$scientific_name)
df$scientific_name <- gsub("homaruis", "homarus", df$scientific_name)
df$scientific_name <- gsub("humarus", "homarus", df$scientific_name)
df$scientific_name <- gsub("hurmarus", "homarus", df$scientific_name)
df$scientific_name <- gsub("pencillatus", "penicillatus", df$scientific_name)
df$scientific_name <- gsub("Paraparenus", "Parupeneus", df$scientific_name)
df$scientific_name <- gsub("Parapeneneus", "Parupeneus", df$scientific_name)
df$scientific_name <- gsub("Parapenenus", "Parupeneus", df$scientific_name)
df$scientific_name <- gsub("Parapeneous", "Parupeneus", df$scientific_name)
df$scientific_name <- gsub("Parapeneus", "Parupeneus", df$scientific_name)
df$scientific_name <- gsub("Parapenious", "Parupeneus", df$scientific_name)
df$scientific_name <- gsub("Parapenous", "Parupeneus", df$scientific_name)
df$scientific_name <- gsub("Paraperenus", "Parupeneus", df$scientific_name)
df$scientific_name <- gsub("Parupenenus", "Parupeneus", df$scientific_name)
df$scientific_name <- gsub("Parupeneous", "Parupeneus", df$scientific_name)
df$scientific_name <- gsub("Parupenenus", "Parupeneus", df$scientific_name)
df$scientific_name <- gsub("Perepeneus", "Parupeneus", df$scientific_name)
df$scientific_name <- gsub("baberinus", "barberinus", df$scientific_name)
df$scientific_name <- gsub("indica$", "indicus", df$scientific_name)
df$scientific_name <- gsub("Plactorhinches", "Plectorhinchus", df$scientific_name)
df$scientific_name <- gsub("Plactorhinchus", "Plectorhinchus", df$scientific_name)
df$scientific_name <- gsub("Platasus", "Plotosus", df$scientific_name)
df$scientific_name <- gsub("Platxbelone", "Platybelone", df$scientific_name)
df$scientific_name <- gsub("fiavomaculatus", "flavomaculatus", df$scientific_name)
df$scientific_name <- gsub("flavamaculatus", "flavomaculatus", df$scientific_name)
df$scientific_name <- gsub("plaxfairi", "playfairi", df$scientific_name)
df$scientific_name <- gsub("playfairii", "playfairi", df$scientific_name)
df$scientific_name <- gsub("sardidus", "sordidus", df$scientific_name)
df$scientific_name <- gsub("Plectorhinechus", "Plectorhinchus", df$scientific_name)
df$scientific_name <- gsub("Plectorhines", "Plectorhinchus", df$scientific_name)
df$scientific_name <- gsub("Plectorhninus", "Plectorhinchus", df$scientific_name)
df$scientific_name <- gsub("Plectorihinchus", "Plectorhinchus", df$scientific_name)
df$scientific_name <- gsub("Plectrorchinchw", "Plectorhinchus", df$scientific_name)
df$scientific_name <- gsub("Plectrorhinchw", "Plectorhinchus", df$scientific_name)
df$scientific_name <- gsub("Priacanthurus", "Priacanthus", df$scientific_name)
df$scientific_name <- gsub("Pricanthurus", "Priacanthus", df$scientific_name)
df$scientific_name <- gsub("hamsur$", "hamrur", df$scientific_name)
df$scientific_name <- gsub("Psedorhombus", "Pseudorhombus", df$scientific_name)
df$scientific_name <- gsub("mile$", "miles", df$scientific_name)
df$scientific_name <- gsub("Rhineacanthus", "Rhinecanthus", df$scientific_name)
df$scientific_name <- gsub("aculateus$", "aculeatus", df$scientific_name)
df$scientific_name <- gsub("Sardinelle", "Sardinella", df$scientific_name)
df$scientific_name <- gsub("Scarrus", "Scarus", df$scientific_name)
df$scientific_name <- gsub("Scarua", "Scarus", df$scientific_name)
df$scientific_name <- gsub("Scarus$", "Scarus sp.", df$scientific_name)
df$scientific_name <- gsub("ghoban$", "ghobban", df$scientific_name)
df$scientific_name <- gsub("ghobbao", "ghobban", df$scientific_name)
df$scientific_name <- gsub("nuselii", "russelii", df$scientific_name)
df$scientific_name <- gsub("risselii", "russelii", df$scientific_name)
df$scientific_name <- gsub("ruselii", "russelii", df$scientific_name)
df$scientific_name <- gsub("psittatus", "psittacus", df$scientific_name)
df$scientific_name <- gsub("phargonis", "pharaonis", df$scientific_name)
df$scientific_name <- gsub("fluscence", "fuscescens", df$scientific_name)
df$scientific_name <- gsub("fluscenscens", "fuscescens", df$scientific_name)
df$scientific_name <- gsub("fluscescens", "fuscescens", df$scientific_name)
df$scientific_name <- gsub("gittatus", "guttatus", df$scientific_name)
df$scientific_name <- gsub("guitatus", "guttatus", df$scientific_name)
df$scientific_name <- gsub("Signus", "Siganus", df$scientific_name)
df$scientific_name <- gsub("Sphyreana", "Sphyraena", df$scientific_name)
df$scientific_name <- gsub("Spinephelus", "Epinephelus", df$scientific_name)
df$scientific_name <- gsub("iciura$", "leiura", df$scientific_name)
df$scientific_name <- gsub("satheta$", "sathete", df$scientific_name)
df$scientific_name <- gsub("Strougylura", "Strongylura", df$scientific_name)
df$scientific_name <- gsub("Suffiamen", "Sufflamen", df$scientific_name)
df$scientific_name <- gsub("Sufiamen", "Sufflamen", df$scientific_name)
df$scientific_name <- gsub("bymma", "lymma", df$scientific_name)
df$scientific_name <- gsub("chiltanae", "chiltonae", df$scientific_name)
df$scientific_name <- gsub("chittonae", "chiltonae", df$scientific_name)
df$scientific_name <- gsub("chillonae", "chiltonae", df$scientific_name)
df$scientific_name <- gsub("Thysanophys", "Thysanophrys", df$scientific_name)
df$scientific_name <- gsub("lepsurus", "lepturus", df$scientific_name)
df$scientific_name <- gsub("duaucelii", "duvauceli", df$scientific_name)
df$scientific_name <- gsub("duraucelii", "duvauceli", df$scientific_name)
df$scientific_name <- gsub("duvaucelii", "duvauceli", df$scientific_name)
df$scientific_name <- gsub("nigricaudus", "nigricauda", df$scientific_name)
df$scientific_name <- gsub("Ancanthurus", "Acanthurus", df$scientific_name)
df$scientific_name <- gsub("Elinephelus", "Epinephelus", df$scientific_name)
df$scientific_name <- gsub("Etinephelus", "Epinephelus", df$scientific_name)
df$scientific_name <- gsub("Gomphesus", "Gomphosus", df$scientific_name)
df$scientific_name <- gsub("Laptoscarus", "Leptoscarus", df$scientific_name)
df$scientific_name <- gsub("lebulous", "nebulosus", df$scientific_name)
df$scientific_name <- gsub("varigatus", "variegatus", df$scientific_name)
df$scientific_name <- gsub("veriegatus", "variegatus", df$scientific_name)
df$scientific_name <- gsub("flulviflamma", "fulviflamma", df$scientific_name)
df$scientific_name <- gsub("fluviflamma", "fulviflamma", df$scientific_name)
df$scientific_name <- gsub("bernati", "berndti", df$scientific_name)
df$scientific_name <- gsub("Ovaculichthys", "Novaculichthys", df$scientific_name)
df$scientific_name <- gsub("berbarinus", "barberinus", df$scientific_name)
df$scientific_name <- gsub("semcirculotus", "semicirculatus", df$scientific_name)
df$scientific_name <- gsub("Scolopis", "Scolopsis", df$scientific_name)
df$scientific_name <- gsub("Siggg", "Siganus", df$scientific_name)
df$scientific_name <- gsub("Sinagus", "Siganus", df$scientific_name)
df$scientific_name <- gsub("sandwichlensis", "sandwichiensis", df$scientific_name)
df$scientific_name <- gsub("lacheri", "lachneri", df$scientific_name)
df$scientific_name <- gsub("Leptoscurus", "Leptoscarus", df$scientific_name)
df$scientific_name <- gsub("haak", "harak", df$scientific_name)
df$scientific_name <- gsub("Lethritus", "Lethrinus", df$scientific_name)
df$scientific_name <- gsub("Luthrinus", "Lethrinus", df$scientific_name)
df$scientific_name <- gsub("fulfuiflamma", "fulviflamma", df$scientific_name)
df$scientific_name <- gsub("orgentimoeulatus", "argentimaculatus", df$scientific_name)
df$scientific_name <- gsub("argentues", "argenteus", df$scientific_name)
df$scientific_name <- gsub("orgentues", "argenteus", df$scientific_name)
df$scientific_name <- gsub("Parapenes", "Parupeneus", df$scientific_name)
df$scientific_name <- gsub("Parapenius", "Parupeneus", df$scientific_name)
df$scientific_name <- gsub("Parupeneus", "Parupeneus", df$scientific_name)
df$scientific_name <- gsub("barberins", "barberinus", df$scientific_name)
df$scientific_name <- gsub("berberinus", "barberinus", df$scientific_name)
df$scientific_name <- gsub("Platorhinchus", "Plectorhinchus", df$scientific_name)
df$scientific_name <- gsub("Plectorhichus", "Plectorhinchus", df$scientific_name)
df$scientific_name <- gsub("Plectohinchus", "Plectorhinchus", df$scientific_name)
df$scientific_name <- gsub("Pleitorhinchus", "Plectorhinchus", df$scientific_name)
df$scientific_name <- gsub("Platybalone", "Platybelone", df$scientific_name)
df$scientific_name <- gsub("Pomacanthurus", "Pomacanthus", df$scientific_name)
df$scientific_name <- gsub("Pseudorhombuo", "Pseudorhombus", df$scientific_name)
df$scientific_name <- gsub("Scanus", "Scarus", df$scientific_name)
df$scientific_name <- gsub("globiseps", "globiceps", df$scientific_name)
df$scientific_name <- gsub("acutiplanis", "acutipinnis", df$scientific_name)
df$scientific_name <- gsub("Utoreuthis", "Uroteuthis", df$scientific_name)
df$scientific_name <- gsub("Scarus tricolo$", "Scarus tricolor", df$scientific_name)
df$scientific_name <- gsub("Parupeneous macronema$", "Parupeneus macronemus", df$scientific_name)
df$scientific_name <- gsub("Parupeneus macronema$", "Parupeneus macronemus", df$scientific_name)
df$scientific_name <- gsub("Scarus russelli$", "Scarus russelii", df$scientific_name)
df$scientific_name <- gsub("Acanthurus tennentii", "Acanthurus tennenti", df$scientific_name)
df$scientific_name <- gsub("Lethrinus conchliatus", "Lethrinus conchyliatus", df$scientific_name)
df$scientific_name <- gsub("Naso brachycention", "Naso brachycentron", df$scientific_name)
df$scientific_name <- gsub("Acathopacrus berda", "Acanthopagrus berda", df$scientific_name)
df$scientific_name <- gsub("Colotomus carolinus", "Calotomus carolinus", df$scientific_name)
df$scientific_name <- gsub("Coranx hippos", "Caranx hippos", df$scientific_name)
df$scientific_name <- gsub("Cynoglossus lancheri", "Cynoglossus lachneri", df$scientific_name)
df$scientific_name <- gsub("Gomphosus caeruleos", "Gomphosus caeruleus", df$scientific_name)
df$scientific_name <- gsub("Gompheus caeruleus", "Gomphosus caeruleus", df$scientific_name)
df$scientific_name <- gsub("Gymnothirax favagineus", "Gymnothorax favagineus", df$scientific_name)
df$scientific_name <- gsub("Hinecanthurus aculeatus", "Rhinecanthus aculeatus", df$scientific_name)
df$scientific_name <- gsub("Hipposcarus globiceps", "Hipposcarus longiceps", df$scientific_name)
df$scientific_name <- gsub("Leptoscarus vaigiencies", "Leptoscarus vaigiensis", df$scientific_name)
df$scientific_name <- gsub("Lethhrinus lentjan", "Lethrinus lentjan", df$scientific_name)
df$scientific_name <- gsub("Lethrinas harak", "Lethrinus harak", df$scientific_name)
df$scientific_name <- gsub("Lethrnus nebulosus", "Lethrinus nebulosus", df$scientific_name)
df$scientific_name <- gsub("Litjanus argentimaculatus", "Lutjanus argentimaculatus", df$scientific_name)
df$scientific_name <- gsub("Litjanus fluiviflamma", "Lutjanus fulviflamma", df$scientific_name)
df$scientific_name <- gsub("Litjanus fulviflamma", "Lutjanus fulviflamma", df$scientific_name)
df$scientific_name <- gsub("Litjanus gibbus", "Lutjanus gibbus", df$scientific_name)
df$scientific_name <- gsub("Lutjanus flaviflammma", "Lutjanus fulviflamma", df$scientific_name)
df$scientific_name <- gsub("Ostracion cubicum", "Ostracion cubicus", df$scientific_name)
df$scientific_name <- gsub("Ostracion nasus", "Rhynchostracion nasus", df$scientific_name)
df$scientific_name <- gsub("Panalirus versicolor", "Panulirus versicolor", df$scientific_name)
df$scientific_name <- gsub("Panulirus penicillatus", "Panulirus penicilatus", df$scientific_name)
df$scientific_name <- gsub("Parupeneus indius", "Parupeneus indicus", df$scientific_name)
df$scientific_name <- gsub("Paruperenus macronemus", "Parupeneus macronemus", df$scientific_name)
df$scientific_name <- gsub("Perupeneus macronemus", "Parupeneus macronemus", df$scientific_name)
df$scientific_name <- gsub("Priacanthus humrur", "Priacanthus hamrur", df$scientific_name)
df$scientific_name <- gsub("Scarius rubroviolaceus", "Scarus rubroviolaceus", df$scientific_name)
df$scientific_name <- gsub("Scarus ghobbon", "Scarus ghobban", df$scientific_name)
df$scientific_name <- gsub("Siganas guttatus", "Siganus guttatus", df$scientific_name)
df$scientific_name <- gsub("Sthrophidon sathete", "Strophidon sathete", df$scientific_name)
df$scientific_name <- gsub("Uroteuthis duvaucellii", "Uroteuthis duvauceli", df$scientific_name)

# correcting spellings in validation list 
validation_lists$scientific_name <- gsub("Gymonthorax", "Gymnothorax", validation_lists$scientific_name)
validation_lists$scientific_name <- gsub("Pomatonus", "Pomatomus", validation_lists$scientific_name)
validation_lists$scientific_name <- gsub("Scarus tricolo$", "Scarus tricolor", validation_lists$scientific_name)
validation_lists$scientific_name <- gsub("Parupeneous macronema$", "Parupeneus macronemus", validation_lists$scientific_name)
validation_lists$scientific_name <- gsub("Parupeneus macronema$", "Parupeneus macronemus", validation_lists$scientific_name)
validation_lists$scientific_name <- gsub("Scarus russelli$", "Scarus russelii", validation_lists$scientific_name)
validation_lists$scientific_name <- gsub("Acanthurus tennentii", "Acanthurus tennenti", validation_lists$scientific_name)
validation_lists$scientific_name <- gsub("Lethrinus conchliatus", "Lethrinus conchyliatus", validation_lists$scientific_name)
validation_lists$scientific_name <- gsub("Naso brachycention", "Naso brachycentron", validation_lists$scientific_name)
```

Double checking our df against the valid names so we know what names are
typos.

``` r
# making df of names that are in the catch_composition (df) but are not in the validation_lists
# these names are typos - fix with gsub functions above 
valid_names <- validation_lists %>% select(scientific_name)
catch_names <- df %>% select(scientific_name)

unvalidated_names <- setdiff(catch_names, valid_names) %>% 
  filter(!scientific_name == "NANA") %>% filter(!scientific_name == "Nana")

# prints list that appear in survey dataset but don't match the validation list we have 
unique(sort(unvalidated_names$scientific_name))
```

    ##  [1] "Acanthopagrus berda"          "Acanthurus duvauceli"        
    ##  [3] "Aethaloperca rogaa"           "Amanses scopas"              
    ##  [5] "Auxis thazard"                "Carangoides flavimarginatus" 
    ##  [7] "Caranx hippos"                "Cyprichromis leptosoma"      
    ##  [9] "Epinephelus spilotoceps"      "Gymnothorax"                 
    ## [11] "Gymnothorax flavimarginatus"  "Halichoeres hartulanus"      
    ## [13] "Himantura gerrardi"           "Hipposcarus longiceps"       
    ## [15] "Kyphosus bigibbus"            "Leptoscarus triostegus"      
    ## [17] "Lethrinus macronemus"         "Lutjanus canius"             
    ## [19] "Monodactylus argentimailatus" "Monotaxis grandoculis"       
    ## [21] "Mugil cephalus"               "Mulloidichthys pfluegeri"    
    ## [23] "Naso stellatus"               "Panulirus homarus"           
    ## [25] "Panulirus ornatus"            "Panulirus penicilatus"       
    ## [27] "Panulirus versicolor"         "Parupeneus semicirculatus"   
    ## [29] "Planiliza alata"              "Planiliza sp."               
    ## [31] "Platybelone platyura"         "Plectorhinchus"              
    ## [33] "Plectorhinchus plagiodesmus"  "Plectorhinchus playfairi"    
    ## [35] "Plotosus canius"              "Pono"                        
    ## [37] "Rhynchostracion nasus"        "Sardinella melanura"         
    ## [39] "Scarus carolinus"             "Scarus sutor"                
    ## [41] "Scarus vaigiensis"            "Scolopsis bimaculata"        
    ## [43] "Siganus canaliculatus"        "Siganus fuscescens"          
    ## [45] "Siganus guttatus"             "Sphyraena japonica"          
    ## [47] "Sphyraena leiura"             "Taeniura meyeni"             
    ## [49] "Tafi sutor"                   "Terapon theraps"             
    ## [51] "Thunnus albacares"            "Thysanophyrys chiltonae"     
    ## [53] "Uroteuthis cynea"             "Uroteuthis duvauceli"        
    ## [55] "Uroteuthis lineatus"

``` r
# checking to see how many times a particular fish appears in the dataset
filter(df, scientific_name == "Scylla serrata") %>%
  select(number_of_fish) %>% filter(!is.na(number_of_fish)) %>%
  mutate(count = n(),
         sum = sum(number_of_fish))
```

    ## # A tibble: 0 × 3
    ## # ℹ 3 variables: number_of_fish <dbl>, count <int>, sum <dbl>

1.) In catch composition and in fishbase but not on validation list.
**Suggested fix: address if these are reasonable to find in Ghana and if
they are, keep these entries.**

- Acanthopagrus berda  
- Acanthurus tennenti  
- Aethaloperca rogaa  
- Alepes djedaba  
- Amanses scopas  
- Auxis thazard  
- Caranx hippos  
- Cephalopholis spp.  
- Cyprichromis leptosoma  
- Epinephelus melanostigma  
- Epinephelus spilotoceps  
- Gompheus caeruleus  
- Gymnothorax flavimarginatus  
- Gymnothorax monochrous  
- Himantura gerrardi  
- Hipposcarus longiceps  
- Kyphosus bigibbus  
- Lethrinus conchliatus  
- Lujtanus argentimaculatus  
- Monotaxis grandoculis  
- Mugil cephalus  
- Mulloidichthys pfluegeri  
- Naso brachycentron  
- Naso thynnoides  
- Ostracion nasus  
- Panulirus homarus  
- Panulirus ornatus  
- Panulirus penicillatus  
- Panulirus versicolor  
- Paracanthurus hepatus  
- Planiliza alata  
- Platybelone platyura  
- Plectorhinchus plagiodesmus  
- Plectorhinchus playfairi  
- Plotosus canius  
- Pomacanthus maculosus  
- Pseudorhombus arsius  
- Sardinella melanura  
- Scarus vaigiensis  
- Scolopsis bimaculata  
- Scylla serrata (this is a mud crab..)  
- Sepia pharaonis  
- Siganus canaliculatus  
- Siganus fuscescens  
- Siganus guttatus  
- Sphyraena japonica  
- Taeniura meyeni  
- Terapon theraps  
- Thunnus albacares  
- Thysanophrys chiltonae  
- Uroteuthis duvauceli

2.) In catch composition but not in validation list or on fishbase (not
close to a name we have so unsure what it is supposed to be).
**Suggested fix: if there is not a clear answer to what these are
supposed to be, filter them out.**

- Acanthurus duvauceli  
- Acanthurus harak  
- Carangoides flavimarginatus  
- Carangoides florimaginatus  
- Cirnhitus lentjan  
- Halichoeres hartulanus  
- Hipposcarus scarus  
- Leptoscarus triostegus  
- Lethrinus macronemus  
- Lethrinus sutor  
- Lethrinus vaigiensis  
- Lutjanus canius  
- Monodactylus argentimailatus  
- Naso stellatus  
- Panulirus merra
- Parapenes barberins  
- Parupeneus semicirculatus  
- Platycephalus crocodilas  
- Pono blue fish (probably meant to be a common name..)  
- Scarus carolinus  
- Scarus sutor  
- Sphyraena leiura  
- Tafi sutor  
- Uroteuthis cynea  
- Uroteuthis lineatus

3.) In validation list but is not on fish base. **No fix needed here,
just an FYI.**.

- Acanthurus vaigiensis

#### Filtering out species that may be incorrect or unexpected in our target area.

*Plotosus canius* is a catfish. Austin wanted to ask Chris about this
one but for now it’s removed.

``` r
# list of species to filter out
filter_out_species <- c("Pono blue fish", "Acanthopagrus berda", "Acanthurus duvauceli", "Aethaloperca rogaa", "Alepes djedaba", "Amanses scopas",
                        "Baracuda", "Carangoides flavimarginatus", "Carangoides florimaginatus", "Cirnhitus lentjan", "Cyprichromis leptosoma",
                        "Gompheus caeruleus", "Halichoeres hartulanus", "Leptoscarus triostegus", "Lethrinus macronemus", "Lujtanus argentimaculatus",
                        "Lutjanus canius", "Mulloidichthys pfluegeri", "Naso thynnoides", "Ostracion nasus", "Panulirus merra", "Panulirus ornatus", 
                        "Panulirus versicolor", "Paracanthurus hepatus", "Parupeneus semicirculatus", "Platycephalus crocodilas", "Plectorhinchus plagiodesmus",
                        "Pomacanthus maculosus", "Scarus sutor", "Scarus vaigiensis", "Scolopsis bimaculata", "Scylla serrata", "Taeniura meyeni", "Uroteuthis cynea",
                        "Uroteuthis lineatus", "Sardinella melanura", "Plotosus canius")

filter_sppdf <- data.frame(filter_out_species) %>%
  rename(scientific_name = filter_out_species)

df <- df %>% 
  filter(!scientific_name %in% filter_sppdf$scientific_name)

# checking this was removed
unique(sort(df$scientific_name))
```

    ##   [1] "Abudefduf sexfasciatus"        "Acanthurus dussumieri"        
    ##   [3] "Acanthurus lineatus"           "Acanthurus nigricauda"        
    ##   [5] "Acanthurus nigrofuscus"        "Acanthurus tennenti"          
    ##   [7] "Acanthurus triostegus"         "Acanthurus xanthopterus"      
    ##   [9] "Auxis thazard"                 "Balistapus undulatus"         
    ##  [11] "Caesio caerulaurea"            "Caesio xanthonota"            
    ##  [13] "Calotomus carolinus"           "Cantherhines sandwichiensis"  
    ##  [15] "Carangoides ferdau"            "Caranx hippos"                
    ##  [17] "Cephalopholis argus"           "Cephalopholis spiloparaea"    
    ##  [19] "Chaetodon auriga"              "Chaetodon kleinii"            
    ##  [21] "Chaetodon selene"              "Chaetodon trifasciatus"       
    ##  [23] "Cheilinus chlorourus"          "Cheilinus trilobatus"         
    ##  [25] "Cheilio inermis"               "Chlorurus strongylocephalus"  
    ##  [27] "Cirrhitus pinnulatus"          "Coris formosa"                
    ##  [29] "Cynoglossus lachneri"          "Diagramma pictum"             
    ##  [31] "Diodon liturosus"              "Epinephelus coioides"         
    ##  [33] "Epinephelus fasciatus"         "Epinephelus fuscoguttatus"    
    ##  [35] "Epinephelus melanostigma"      "Epinephelus merra"            
    ##  [37] "Epinephelus spilotoceps"       "Gerres oyena"                 
    ##  [39] "Gomphosus caeruleus"           "Gymnothorax"                  
    ##  [41] "Gymnothorax favagineus"        "Gymnothorax flavimarginatus"  
    ##  [43] "Gymnothorax griseus"           "Gymnothorax monochrous"       
    ##  [45] "Halichoeres hortulanus"        "Heniochus acuminatus"         
    ##  [47] "Heniochus monoceros"           "Himantura gerrardi"           
    ##  [49] "Hipposcarus harid"             "Hipposcarus longiceps"        
    ##  [51] "Kyphosus bigibbus"             "Kyphosus vaigiensis"          
    ##  [53] "Leptoscarus vaigiensis"        "Leptoscarus variegatus"       
    ##  [55] "Lethrinus conchyliatus"        "Lethrinus harak"              
    ##  [57] "Lethrinus lentjan"             "Lethrinus mahsena"            
    ##  [59] "Lethrinus nebulosus"           "Lethrinus obsoletus"          
    ##  [61] "Lethrinus olivaceus"           "Lethrinus variegatus"         
    ##  [63] "Lutjanus argentimaculatus"     "Lutjanus fulviflamma"         
    ##  [65] "Lutjanus gibbus"               "Lutjanus johnii"              
    ##  [67] "Lutjanus lentjan"              "Monodactylus argenteus"       
    ##  [69] "Monodactylus argentimailatus"  "Monotaxis grandoculis"        
    ##  [71] "Mugil cephalus"                "Mulloidichthys flavolineatus" 
    ##  [73] "Myripristis berndti"           "NANA"                         
    ##  [75] "Naso annulatus"                "Naso brachycentron"           
    ##  [77] "Naso hexacanthus"              "Naso lituratus"               
    ##  [79] "Naso stellatus"                "Naso unicornis"               
    ##  [81] "Novaculichthys taeniourus"     "Octopus cyanea"               
    ##  [83] "Ostracion cubicus"             "Panulirus homarus"            
    ##  [85] "Panulirus penicilatus"         "Parupeneus barberinus"        
    ##  [87] "Parupeneus indicus"            "Parupeneus macronemus"        
    ##  [89] "Planiliza alata"               "Planiliza sp."                
    ##  [91] "Platax teira"                  "Platybelone platyura"         
    ##  [93] "Plectorhinchus"                "Plectorhinchus flavomaculatus"
    ##  [95] "Plectorhinchus gaterinus"      "Plectorhinchus playfairi"     
    ##  [97] "Plectorhinchus sordidus"       "Plectorhinchus vittatus"      
    ##  [99] "Plotosus lineatus"             "Pomacanthus semicirculatus"   
    ## [101] "Pomadasys argenteus"           "Pomatomus saltatrix"          
    ## [103] "Pono"                          "Priacanthus hamrur"           
    ## [105] "Pseudorhombus arsius"          "Pterois miles"                
    ## [107] "Rhinecanthus aculeatus"        "Rhynchostracion nasus"        
    ## [109] "Scarus carolinus"              "Scarus coeruleus"             
    ## [111] "Scarus frenatus"               "Scarus ghobban"               
    ## [113] "Scarus globiceps"              "Scarus psittacus"             
    ## [115] "Scarus rubroviolaceus"         "Scarus russelii"              
    ## [117] "Scarus sp."                    "Scomberomorus commerson"      
    ## [119] "Sepia pharaonis"               "Siganus argenteus"            
    ## [121] "Siganus canaliculatus"         "Siganus fuscescens"           
    ## [123] "Siganus guttatus"              "Siganus stellatus"            
    ## [125] "Siganus sutor"                 "Sphyraena acutipinnis"        
    ## [127] "Sphyraena barracuda"           "Sphyraena japonica"           
    ## [129] "Sphyraena leiura"              "Strongylura leiura"           
    ## [131] "Strophidon sathete"            "Sufflamen chrysopterum"       
    ## [133] "Taeniura lymma"                "Tafi sutor"                   
    ## [135] "Terapon jarbua"                "Terapon theraps"              
    ## [137] "Thunnus albacares"             "Thysanophrys chiltonae"       
    ## [139] "Thysanophyrys chiltonae"       "Trichiurus lepturus"          
    ## [141] "Upeneus sulphureus"            "Upeneus tragula"              
    ## [143] "Uroteuthis duvauceli"

#### Changing incorrect scientific names

``` r
df$scientific_name <- gsub("Scarus carolinus", "Calotomus carolinus", df$scientific_name)
df$scientific_name <- gsub("Hipposcarus scarus", "Hipposcarus longiceps", df$scientific_name)
df$scientific_name <- gsub("Lethrinus vaigiensis", "Kyphosus vaigiensis", df$scientific_name)
df$scientific_name <- gsub("Acanthurus harak", "Lethrinus harak", df$scientific_name)
df$scientific_name <- gsub("Monodactylus argentimailatus", "Monodactylus argenteus", df$scientific_name)
df$scientific_name <- gsub("Lethrinus sutor", "Siganus sutor", df$scientific_name)
df$scientific_name <- gsub("Tafi sutor", "Siganus sutor", df$scientific_name)
df$scientific_name <- gsub("Sphyraena leiura", "Sphyraena japonica", df$scientific_name)
```

### Length (cm)

This column is a character for because of the “\<” and “-”.

1.) 3 observations (rows) have 2 length values and multiple fish.
changed these values to NA for now.

- “16-20 ,46-50”  
- “26-30,21- 25”

2.) Many operations by CLAPERTON KAZUNGU include a length value of 4488
which is not realistic so I changed these to NA for now.

3.) Some ranges weren’t correct like “16-25” and only have \<10
observations like that so I changed them to the nearest possible
category. e.g. “16-25” to “16-20”. “21-30” to “21-25”.

``` r
unique(sort(df$length_cm))
```

    ##   [1] "˂10"                "<10"                "<11"               
    ##   [4] "<12"                "<13"                "<14"               
    ##   [7] "<15"                "<16"                "<17"               
    ##  [10] ">10"                ">50"                ">50 write in:"     
    ##  [13] ">50 write in: 101"  ">50 write in: 102"  ">50 write in: 103" 
    ##  [16] ">50 write in: 104"  ">50 write in: 105"  ">50 write in: 107" 
    ##  [19] ">50 write in: 109"  ">50 write in: 110"  ">50 write in: 111" 
    ##  [22] ">50 write in: 112"  ">50 write in: 118"  ">50 write in: 173" 
    ##  [25] ">50 write in: 176"  ">50 write in: 181"  ">50 write in: 183" 
    ##  [28] ">50 write in: 51"   ">50 write in: 52"   ">50 write in: 53"  
    ##  [31] ">50 write in: 54"   ">50 write in: 55"   ">50 write in: 56.2"
    ##  [34] ">50 write in: 57"   ">50 write in: 58"   ">50 write in: 58.6"
    ##  [37] ">50 write in: 59"   ">50 write in: 60.3" ">50 write in: 62"  
    ##  [40] ">50 write in: 62.9" ">50 write in: 63"   ">50 write in: 64.4"
    ##  [43] ">50 write in: 64.8" ">50 write in: 65"   ">50 write in: 65.1"
    ##  [46] ">50 write in: 66.5" ">50 write in: 67"   ">50 write in: 67.6"
    ##  [49] ">50 write in: 67.7" ">50 write in: 68"   ">50 write in: 68.7"
    ##  [52] ">50 write in: 69"   ">50 write in: 69.5" ">50 write in: 69.8"
    ##  [55] ">50 write in: 70"   ">50 write in: 71.2" ">50 write in: 71.9"
    ##  [58] ">50 write in: 72"   ">50 write in: 72.2" ">50 write in: 72.3"
    ##  [61] ">50 write in: 74.3" ">50 write in: 75"   ">50 write in: 75.5"
    ##  [64] ">50 write in: 79.2" ">50 write in: 80"   ">50 write in: 80.2"
    ##  [67] ">50 write in: 81.2" ">50 write in: 82"   ">50 write in: 85"  
    ##  [70] ">50 write in: 86"   ">50 write in: 87"   ">50 write in: 88"  
    ##  [73] ">50 write in: 89"   ">50 write in: 90"   ">50 write in: 91"  
    ##  [76] ">50 write in: 92"   ">50 write in: 93"   ">50 write in: 94"  
    ##  [79] ">50 write in: 96"   ">50 write in: 97"   ">50 write in: 98"  
    ##  [82] ">50 write in:100"   ">50 write in:101"   ">50 write in:152"  
    ##  [85] ">50 write in:55"    ">50 write in:59"    ">50 write in:61"   
    ##  [88] ">50 write in:65.7"  ">50 write in:68"    ">50 write in:69.3" 
    ##  [91] ">50 write in:72.1"  "1-15"               "102"               
    ##  [94] "103"                "104"                "105"               
    ##  [97] "106"                "107"                "108"               
    ## [100] "109"                "11-15"              "11-16"             
    ## [103] "110"                "111"                "112"               
    ## [106] "114"                "115"                "117"               
    ## [109] "119"                "123"                "128"               
    ## [112] "129"                "16"                 "16-20"             
    ## [115] "17"                 "170"                "173"               
    ## [118] "177"                "181"                "198"               
    ## [121] "21-25"              "21-26"              "25-30"             
    ## [124] "26-20"              "26-25"              "26-30"             
    ## [127] "26-31"              "26-35"              "31-25"             
    ## [130] "31-35"              "36-34"              "36-40"             
    ## [133] "41-45"              "41-50"              "46-50"             
    ## [136] "51"                 "52"                 "54"                
    ## [139] "55"                 "56"                 "58"                
    ## [142] "59"                 "60"                 "60.3"              
    ## [145] "60.4"               "61"                 "62"                
    ## [148] "63"                 "64.8"               "64.9"              
    ## [151] "65"                 "65.6"               "65.8"              
    ## [154] "66"                 "67"                 "67.1"              
    ## [157] "67.7"               "68"                 "68.1"              
    ## [160] "68.2"               "68.4"               "68.5"              
    ## [163] "68.7"               "68.9"               "69"                
    ## [166] "69.7"               "69.8"               "69.9"              
    ## [169] "70"                 "70.5"               "71.8"              
    ## [172] "72"                 "72.2"               "72.3"              
    ## [175] "72.4"               "72.6"               "73"                
    ## [178] "74"                 "74.1"               "74.2"              
    ## [181] "74.3"               "74.5"               "74.7"              
    ## [184] "75"                 "75.5"               "75.6"              
    ## [187] "75.7"               "75.8"               "75.9"              
    ## [190] "76"                 "76.4"               "76.6"              
    ## [193] "76.7"               "76.8"               "76.9"              
    ## [196] "77"                 "77.2"               "77.8"              
    ## [199] "78"                 "78.1"               "78.2"              
    ## [202] "78.4"               "78.5"               "78.6"              
    ## [205] "78.8"               "79"                 "79.1"              
    ## [208] "79.2"               "79.4"               "79.5"              
    ## [211] "79.8"               "80"                 "80.2"              
    ## [214] "80.3"               "81"                 "82"                
    ## [217] "82.1"               "82.2"               "82.3"              
    ## [220] "82.4"               "83"                 "84"                
    ## [223] "85"                 "86"                 "87"                
    ## [226] "88"                 "89"                 "90"                
    ## [229] "91"                 "92"                 "93"                
    ## [232] "94"                 "95"                 "96"                
    ## [235] "97"                 "98"                 "99"

``` r
# replace the write in verbiage with no characters
df$length_cm <- gsub(">50 write in: ", "", df$length_cm)
df$length_cm <- gsub(">50 write in:", "", df$length_cm)
df$length_cm <- gsub(">50", "", df$length_cm)

# replacing values that don't make sense
df$length_cm <- gsub("269-30", "26-30", df$length_cm)
df$length_cm <- gsub("˂10", "<10", df$length_cm)
df$length_cm <- gsub(">10", "<10", df$length_cm)

# correcting incorrect ranges
df$length_cm <- gsub("31-15", "31-35", df$length_cm)
df$length_cm <- gsub("10-15", "11-15", df$length_cm)
df$length_cm <- gsub("31-37", "31-35", df$length_cm)
df$length_cm <- gsub("31-36", "31-35", df$length_cm)
df$length_cm <- gsub("16-25", "16-20", df$length_cm)
df$length_cm <- gsub("21-30", "21-25", df$length_cm)
df$length_cm <- gsub("26-35", "26-30", df$length_cm)
df$length_cm <- gsub("31.-35", "31-35", df$length_cm)
df$length_cm <- gsub("36-34", "36-40", df$length_cm)
df$length_cm <- gsub("25-30", "26-30", df$length_cm)
df$length_cm <- gsub("45-50", "46-50", df$length_cm)
df$length_cm <- gsub("31-25", "31-35", df$length_cm)
df$length_cm <- gsub("1-15", "11-15", df$length_cm)
df$length_cm <- gsub("26-31", "26-30", df$length_cm)
df$length_cm <- gsub("26-20", "26-30", df$length_cm)
df$length_cm <- gsub("21-26", "21-25", df$length_cm)
df$length_cm <- gsub("21-24", "21-25", df$length_cm)
df$length_cm <- gsub("16-30", "16-20", df$length_cm)
df$length_cm <- gsub("110-15", "11-15", df$length_cm)
df$length_cm <- gsub("111-15", "11-15", df$length_cm)
df$length_cm <- gsub("11-16", "11-15", df$length_cm)
df$length_cm <- gsub("41-46", "41-45", df$length_cm)
df$length_cm <- gsub("26-25", "21-25", df$length_cm)
df$length_cm <- gsub("41-50", "46-50", df$length_cm)

# taking out double values 
df$length_cm <- gsub("21-25 26-30", NA, df$length_cm)
df$length_cm <- gsub("21-25 26-31", NA, df$length_cm)
df$length_cm <- gsub("21-25, 26-30", NA, df$length_cm)
df$length_cm <- gsub("^$", NA, df$length_cm)

# converting <10 to a range of 0-10 
df <- df %>% 
  mutate(length_cm = if_else(length_cm == "<10", "0-10", length_cm),
         length_cm = if_else(length_cm == "<11", "11-15", length_cm),
         length_cm = if_else(length_cm == "<12", "11-15", length_cm),
         length_cm = if_else(length_cm == "<13", "11-15", length_cm),
         length_cm = if_else(length_cm == "<14", "11-15", length_cm),
         length_cm = if_else(length_cm == "<15", "11-15", length_cm),
         length_cm = if_else(length_cm == "<16", "16-20", length_cm),
         length_cm = if_else(length_cm == "<17", "16-20", length_cm))

# converting numerical values to ranges 
df$length_calc <- ifelse(grepl("-",df$length_cm), NA, df$length_cm) # if there is a "-" in the observation, then replace with NA and if not, put that value
df$length_calc <- as.numeric(df$length_calc) # converting this to numeric so I can use the next mutate function to change these values to bins

df <- df %>%
 mutate(length_calc = case_when(
    length_calc >= 0 & length_calc <= 10.5 ~ "0-10",
    length_calc >= 10.5 & length_calc <= 15.4 ~ "11-15",
    length_calc >= 15.5 & length_calc <= 20.4 ~ "16-20",
    length_calc >= 20.5 & length_calc <= 25.4 ~ "21-25",
    length_calc >= 25.5 & length_calc <= 30.4 ~ "26-30",
    length_calc >= 30.5 & length_calc <= 35.4 ~ "31-35",
    length_calc >= 35.5 & length_calc <= 40.4 ~ "36-40",
    length_calc >= 40.5 & length_calc <= 45.4 ~ "41-45",
    length_calc >= 45.5 & length_calc <= 50.4 ~ "46-50",
    length_calc >= 50.5 & length_calc <= 60.4 ~ "51-60",
    length_calc >= 60.5 & length_calc <= 70.4 ~ "61-70",
    length_calc >= 70.5 & length_calc <= 80.4 ~ "71-80",
    length_calc >= 80.5 & length_calc <= 90.4 ~ "81-90",
    length_calc > 90.5 ~ ">90")) 

df <- df %>%
  mutate(length_corrected = if_else(is.na(length_calc), length_cm, length_calc))

# double checking that worked for the corrected column 
unique(sort(df$length_cm))
```

    ##   [1] "0-10"  "100"   "101"   "102"   "103"   "104"   "105"   "106"   "107"  
    ##  [10] "108"   "109"   "11-15" "110"   "111"   "112"   "114"   "115"   "117"  
    ##  [19] "118"   "119"   "123"   "128"   "129"   "152"   "16"    "16-20" "17"   
    ##  [28] "170"   "173"   "176"   "177"   "181"   "183"   "198"   "21-25" "26-30"
    ##  [37] "31-35" "36-40" "41-45" "46-50" "51"    "52"    "53"    "54"    "55"   
    ##  [46] "56"    "56.2"  "57"    "58"    "58.6"  "59"    "60"    "60.3"  "60.4" 
    ##  [55] "61"    "62"    "62.9"  "63"    "64.4"  "64.8"  "64.9"  "65"    "65.1" 
    ##  [64] "65.6"  "65.7"  "65.8"  "66"    "66.5"  "67"    "67.1"  "67.6"  "67.7" 
    ##  [73] "68"    "68.1"  "68.2"  "68.4"  "68.5"  "68.7"  "68.9"  "69"    "69.3" 
    ##  [82] "69.5"  "69.7"  "69.8"  "69.9"  "70"    "70.5"  "71.2"  "71.8"  "71.9" 
    ##  [91] "72"    "72.1"  "72.2"  "72.3"  "72.4"  "72.6"  "73"    "74"    "74.1" 
    ## [100] "74.2"  "74.3"  "74.5"  "74.7"  "75"    "75.5"  "75.6"  "75.7"  "75.8" 
    ## [109] "75.9"  "76"    "76.4"  "76.6"  "76.7"  "76.8"  "76.9"  "77"    "77.2" 
    ## [118] "77.8"  "78"    "78.1"  "78.2"  "78.4"  "78.5"  "78.6"  "78.8"  "79"   
    ## [127] "79.1"  "79.2"  "79.4"  "79.5"  "79.8"  "80"    "80.2"  "80.3"  "81"   
    ## [136] "81.2"  "82"    "82.1"  "82.2"  "82.3"  "82.4"  "83"    "84"    "85"   
    ## [145] "86"    "87"    "88"    "89"    "90"    "91"    "92"    "93"    "94"   
    ## [154] "95"    "96"    "97"    "98"    "99"

``` r
unique(sort(df$length_corrected))
```

    ##  [1] ">90"   "0-10"  "11-15" "16-20" "21-25" "26-30" "31-35" "36-40" "41-45"
    ## [10] "46-50" "51-60" "61-70" "71-80" "81-90"

Correct output for length_corrected:
`">90"   "0-10"  "11-15" "16-20" "21-25" "26-30" "31-35" "36-40" "41-45" "46-50" "51-60" "61-70" "71-80" "81-90"`.

### Comparing Lmax to median length caught

Decide if we need to take these out or not.

``` r
fishbase <- read_excel("data/fishbase.xlsx", sheet = "life history") %>% #read in excel file 
  select(scientific_name, Lm, Lm_se_min, Lm_se_max, Lmax)

length_check <- full_join(df, fishbase, by = "scientific_name") %>%
  select(Operation_date, fisher_id, scientific_name, Lmax, length_corrected) %>%
  mutate(median_length = case_when(
    length_corrected == "0-10" ~ 5,
    length_corrected == "11-15" ~ 13,
    length_corrected == "16-20" ~ 18,
    length_corrected == "21-25" ~ 23,
    length_corrected == "26-30" ~ 28,
    length_corrected == "31-35" ~ 33,
    length_corrected == "36-40" ~ 38,
    length_corrected == "41-45" ~ 43,
    length_corrected == "46-50" ~ 48,
    length_corrected == "51-60" ~ 55.5,
    length_corrected == "61-70" ~ 65.5,
    length_corrected == "71-80" ~ 75.5,
    length_corrected == "81-90" ~ 85.5,
    length_corrected == ">90" ~ 100)) %>% ### circle back to what value to use here
  mutate(length_check = if_else(median_length > Lmax, "over", "under")) %>%
  filter(length_check == "over") %>%
  group_by(length_corrected, scientific_name) %>% 
  mutate(count = n()) %>% select(-Operation_date, -fisher_id) %>% distinct(); length_check
```

    ## # A tibble: 16 × 6
    ## # Groups:   length_corrected, scientific_name [16]
    ##    scientific_name        Lmax length_corrected median_length length_check count
    ##    <chr>                 <dbl> <chr>                    <dbl> <chr>        <int>
    ##  1 Acanthurus nigrofusc…  21   31-35                     33   over            37
    ##  2 Parupeneus macronemus  40   41-45                     43   over             3
    ##  3 Acanthurus nigrofusc…  21   41-45                     43   over             8
    ##  4 Chaetodon selene       16   21-25                     23   over             8
    ##  5 Acanthurus nigrofusc…  21   21-25                     23   over           118
    ##  6 Cephalopholis argus    60   81-90                     85.5 over             2
    ##  7 Acanthurus nigrofusc…  21   36-40                     38   over            32
    ##  8 Leptoscarus vaigiens…  35.2 41-45                     43   over             1
    ##  9 Leptoscarus vaigiens…  35.2 36-40                     38   over            21
    ## 10 Chaetodon selene       16   16-20                     18   over            86
    ## 11 Acanthurus nigrofusc…  21   26-30                     28   over            72
    ## 12 Siganus stellatus      40   41-45                     43   over             4
    ## 13 Gerres oyena           30   31-35                     33   over            32
    ## 14 Leptoscarus vaigiens…  35.2 51-60                     55.5 over             6
    ## 15 Gerres oyena           30   36-40                     38   over             2
    ## 16 Acanthurus triostegus  27   31-35                     33   over             4

#### Correcting those size bin lengths that are over Lmax

``` r
df$length_corrected[df$scientific_name == "Acanthurus dussumieri" & df$length_corrected == "51-60"] <- "46-50"
df$length_corrected[df$scientific_name == "Parupeneus macronemus" & df$length_corrected == "41-45"] <- "36-40"
df$length_corrected[df$scientific_name == "Siganus stellatus" & df$length_corrected == "41-45"] <- "36-40"

df$length_corrected[df$scientific_name == "Acanthurus triostegus" & df$length_corrected == "36-40"] <- "21-25"
df$length_corrected[df$scientific_name == "Acanthurus triostegus" & df$length_corrected == "31-35"] <- "21-25"
df$length_corrected[df$scientific_name == "Acanthurus triostegus" & df$length_corrected == "26-30"] <- "21-25"

df$length_corrected[df$scientific_name == "Gerres oyena" & df$length_corrected == "36-40"] <- "26-30"
df$length_corrected[df$scientific_name == "Gerres oyena" & df$length_corrected == "31-35"] <- "26-30"

df$length_corrected[df$scientific_name == "Leptoscarus vaigiensis" & df$length_corrected == "36-40"] <- "31-35"
df$length_corrected[df$scientific_name == "Leptoscarus vaigiensis" & df$length_corrected == "41-45"] <- "31-35"
df$length_corrected[df$scientific_name == "Leptoscarus vaigiensis" & df$length_corrected == "51-60"] <- "31-35"

df$length_corrected[df$scientific_name == "Lutjanus fulviflamma" & df$length_corrected == "61-70"] <- "31-35"
df$length_corrected[df$scientific_name == "Lutjanus fulviflamma" & df$length_corrected == "51-60"] <- "31-35"

df$length_corrected[df$scientific_name == "Acanthurus nigrofuscus" & df$length_corrected == "31-35"] <- "16-20"
df$length_corrected[df$scientific_name == "Acanthurus nigrofuscus" & df$length_corrected == "41-45"] <- "16-20"
df$length_corrected[df$scientific_name == "Acanthurus nigrofuscus" & df$length_corrected == "21-25"] <- "16-20"
df$length_corrected[df$scientific_name == "Acanthurus nigrofuscus" & df$length_corrected == "36-40"] <- "16-20"
df$length_corrected[df$scientific_name == "Acanthurus nigrofuscus" & df$length_corrected == "26-30"] <- "16-20"

df$length_corrected[df$scientific_name == "Parupeneus macronemus" & df$length_corrected == "41-45"] <- "36-40"
df$length_corrected[df$scientific_name == "Chaetodon selene" & df$length_corrected == "21-25"] <- "11-15"
df$length_corrected[df$scientific_name == "Chaetodon selene" & df$length_corrected == "16-20"] <- "11-15"
df$length_corrected[df$scientific_name == "Cephalopholis argus" & df$length_corrected == "81-90"] <- "51-60"
```

## <a name="gear"></a> **Gear type, and fish numbers/final destination**

### Gear type

Double check that this list looks right. Left off at looking at this
list and then

``` r
df$`gear type` <- toupper(df$`gear type`)

df$`gear type` <- gsub("MONOFILLAMENT", "MONOFILAMENT", df$`gear type`)
df$`gear type` <- gsub("MONOFILLAMNET", "MONOFILAMENT", df$`gear type`)
df$`gear type` <- gsub("UNMODIFIED TRAP", "UNMODIFIED", df$`gear type`)
df$`gear type` <- gsub("MODIFIED TRAP", "MODIFIED", df$`gear type`)
df$`gear type` <- gsub("SPEAR GUN", "SPEARGUN", df$`gear type`)
df$`gear type` <- gsub("SEINE NETS", "SEINE NET", df$`gear type`)

unique(sort(df$`gear type`))
```

    ## [1] "MODIFIED"                 "MONOFILAMENT"            
    ## [3] "OTHER (SPECIFY IN NOTES)" "SPEAR"                   
    ## [5] "UNMODIFIED"

### Number of fish

Doube check this range is what is expected.

``` r
unique(sort(df$number_of_fish))
```

    ##  [1]    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15
    ## [16]   16   17   18   19   20   21   22   23   24   25   26   27   28   29   30
    ## [31]   31   32   33   34   35   36   37   38   39   40   41   42   43   44   45
    ## [46]   46   47   48   49   50   51   52   53   54   55   56   57   58   60   61
    ## [61]   62   65   66   67   68   69   70   72   75   76   77   78   80   82   83
    ## [76]   84   85   87   89   90   92   94   96   98  103  111  120  129  130  140
    ## [91]  150  153  160  170  192  234  270 2375

``` r
df %>% ggplot(., aes(y=number_of_fish)) + geom_boxplot() + theme_bw()
```

    ## Warning: Removed 248 rows containing non-finite values (`stat_boxplot()`).

![](QC_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

``` r
#### filtering out 6,000 and 30,0000 
df$number_of_fish <- ifelse(df$number_of_fish > 20, NA, df$number_of_fish)
hist(df$number_of_fish)
```

![](QC_files/figure-gfm/unnamed-chunk-24-2.png)<!-- -->

``` r
unique(sort(df$number_of_fish))
```

    ##  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20

``` r
unique(sort(df$trap_type))
```

    ##  [1] "CASTNET"                "GILLNET"                "HANDLINE"              
    ##  [4] "MODIFIED TRAP"          "MONOFILAMENT"           "OCTOPUS HOOK"          
    ##  [7] "SEINE NET"              "SPEARGUN"               "SPEARGUN AND SEINE NET"
    ## [10] "UNMODIFIED"

``` r
# 
# df %>% 
#   mutate(crew_size = case_when(trap_type == "MODIFIED TRAP" ~ "MODIFIED")) %>%
#   subset(trap_type == "MODIFIED TRAP" | trap_type == "UNMODIFIED") %>%
#   filter(number_of_fish > 100)
## 103 is highest value for unmodified and modified traps so I am changing the above filter to 150
## large majority is under 50
```

### Destination of fish

``` r
df$destination <- toupper(df$destination)

df$destination <- gsub("OTHER WRITE IN:", "", df$destination)
df$destination <- gsub(" LOCAL CONSUMER", "LOCAL CONSUMER", df$destination)
df$destination <- gsub("LOCAL CONSUMERS", "LOCAL CONSUMER", df$destination)
df$destination <- gsub(" MZUNGU", "MZUNGU", df$destination)

unique(sort(df$destination))
```

    ##  [1] " BAR"                 " BAR & RESTAURANT"    "FISH DEALER"         
    ##  [4] "GIFT"                 "HOME"                 "LOCAL CONSUMER"      
    ##  [7] "MAMA KARANGA"         "MZUNGU"               "OTHER"               
    ## [10] "OTHER: TOURIST"       "OTHER:LOCAL CONSUMER"

## <a name="notes"></a> **Final check: any notes from both datasets**

``` r
## check for any notes that the data collectors left for analysis 
unique(df$fishing_operation_notes)
```

    ## [1] NA                                                                                                                                                       
    ## [2] "Innocent: highlighted cell because need to check accuracy (Aug 24)"                                                                                     
    ## [3] "Innocent: highlighted rows because they seem too similar to one another, particularly columns K - M. Need to check with enumerator on accuracy (Aug 24)"
    ## [4] "Innocent: need to update column M -- check with enumerator (Aug 24)"

``` r
unique(df$catch_composition_notes)
```

    ##   [1] NA                                                                                                               
    ##   [2] "Mwani"                                                                                                          
    ##   [3] "Blue Fish"                                                                                                      
    ##   [4] "Pono Mwani"                                                                                                     
    ##   [5] "Pareti"                                                                                                         
    ##   [6] "mwani"                                                                                                          
    ##   [7] "Pono kadifu"                                                                                                    
    ##   [8] "Green highlight = Austin added a zero in front of numbers in the ID to maintain 3-digit consistency with others"
    ##   [9] "51-60"                                                                                                          
    ##  [10] "50-65"                                                                                                          
    ##  [11] "kadifu"                                                                                                         
    ##  [12] "gamwe"                                                                                                          
    ##  [13] "magamwe"                                                                                                        
    ##  [14] "Changu"                                                                                                         
    ##  [15] "Changu ndomo"                                                                                                   
    ##  [16] "Changu mgamwe"                                                                                                  
    ##  [17] "pareti"                                                                                                         
    ##  [18] "fute yea"                                                                                                       
    ##  [19] "90"                                                                                                             
    ##  [20] "Changu tawa"                                                                                                    
    ##  [21] "Changu tuku"                                                                                                    
    ##  [22] "changu tawa"                                                                                                    
    ##  [23] "0.7 kg"                                                                                                         
    ##  [24] "1.1 kg"                                                                                                         
    ##  [25] "1.1 kg  all take home"                                                                                          
    ##  [26] "1.6 kg"                                                                                                         
    ##  [27] "2.3 kg"                                                                                                         
    ##  [28] "1 kg"                                                                                                           
    ##  [29] "1.5 kg"                                                                                                         
    ##  [30] "1.2 kg"                                                                                                         
    ##  [31] "0.4 kg"                                                                                                         
    ##  [32] "0.5 kg"                                                                                                         
    ##  [33] "0.9 kg"                                                                                                         
    ##  [34] "1.3 kg"                                                                                                         
    ##  [35] "1.5 kg all take home"                                                                                           
    ##  [36] "0.6 kg"                                                                                                         
    ##  [37] "1.7 kg"                                                                                                         
    ##  [38] "2.2 kg"                                                                                                         
    ##  [39] "0.7 kg all take home"                                                                                           
    ##  [40] "0.6 kg all take home"                                                                                           
    ##  [41] "0.3 kg"                                                                                                         
    ##  [42] "1.8 kg"                                                                                                         
    ##  [43] "0.8 kg all take home"                                                                                           
    ##  [44] "0.8 kg"                                                                                                         
    ##  [45] "1.3 kg all take home"                                                                                           
    ##  [46] "0.4 kg all take home"                                                                                           
    ##  [47] "1.4 kg"                                                                                                         
    ##  [48] "1 kg all take home"                                                                                             
    ##  [49] "800g , ksh 80"                                                                                                  
    ##  [50] "1kg, kshs. 100"                                                                                                 
    ##  [51] "1.5kg, kshs 150"                                                                                                
    ##  [52] "1.8kg kshs 360"                                                                                                 
    ##  [53] "2kg Kshs 300"                                                                                                   
    ##  [54] "1.5kg, kshs 200"                                                                                                
    ##  [55] "1.8kg kshs 270"                                                                                                 
    ##  [56] "2kg Kshs 400"                                                                                                   
    ##  [57] "1.5kg Kshs. 150"                                                                                                
    ##  [58] "1.5kg Kshs. 300"                                                                                                
    ##  [59] "1kg Kshs. 200"                                                                                                  
    ##  [60] "1.5 kg kshs. 150"                                                                                               
    ##  [61] "1.3kg Kshs. 195"                                                                                                
    ##  [62] "0.53kg  Kshs. 53 . Modified"                                                                                    
    ##  [63] "0.5kg Kshs. 50"                                                                                                 
    ##  [64] "0.98kg Kshs. 98"                                                                                                
    ##  [65] "2.8kg Kshs. 560."                                                                                               
    ##  [66] "0.93kg. Kshs. 93"                                                                                               
    ##  [67] "1.3 kg Kshs. 260"                                                                                               
    ##  [68] "1.3kg Kshs. 260"                                                                                                
    ##  [69] "2.5kg Kshs. 250"                                                                                                
    ##  [70] "2kg Kshs. 200"                                                                                                  
    ##  [71] "1.3kg Kshs. 300"                                                                                                
    ##  [72] "1.3kg Kshs 300"                                                                                                 
    ##  [73] "2.5kg Kshs. 300"                                                                                                
    ##  [74] "1.9kg Kshs. 190"                                                                                                
    ##  [75] "0.8kg Kshs. 160"                                                                                                
    ##  [76] "1.2kg Kshs 120"                                                                                                 
    ##  [77] "0.8kg Kshs. 80"                                                                                                 
    ##  [78] "1kg. Kshs 200"                                                                                                  
    ##  [79] "1kg. Kshs.100"                                                                                                  
    ##  [80] "1.3kg Kshs. 130"                                                                                                
    ##  [81] "1.3kg Kshs 130"                                                                                                 
    ##  [82] "2.1kg Kshs.210"                                                                                                 
    ##  [83] "1.5kg Kshs 150"                                                                                                 
    ##  [84] "1kg Kshs. 100"                                                                                                  
    ##  [85] "2.1kg Kshs. 210"                                                                                                
    ##  [86] "0.5kg Kshs. 100"                                                                                                
    ##  [87] "1.8kg Kshs 180"                                                                                                 
    ##  [88] "0.5kg Kshs 50"                                                                                                  
    ##  [89] "0.6kg Kshs. 60"                                                                                                 
    ##  [90] "0.4kg Kshs. 40"                                                                                                 
    ##  [91] "1.3kg Kshs 260"                                                                                                 
    ##  [92] "0.75kg Kshs .150"                                                                                               
    ##  [93] "0.9 kg kshs 180"                                                                                                
    ##  [94] "1 kg kshs.200"                                                                                                  
    ##  [95] "0.4 kg kshs 80"                                                                                                 
    ##  [96] "1.2 kg kshs.240"                                                                                                
    ##  [97] "nyavu ya uzi"                                                                                                   
    ##  [98] "octopus hook"                                                                                                   
    ##  [99] "handline"                                                                                                       
    ## [100] "monofilament gillnet"                                                                                           
    ## [101] "nyavu"                                                                                                          
    ## [102] "beachseine"                                                                                                     
    ## [103] "castnet"                                                                                                        
    ## [104] "Spear"                                                                                                          
    ## [105] "reef seine"                                                                                                     
    ## [106] "nyavu ya kutega"                                                                                                
    ## [107] "Handline"                                                                                                       
    ## [108] "monofilament"                                                                                                   
    ## [109] "other (specify in notes)"                                                                                       
    ## [110] "spear"                                                                                                          
    ## [111] "seine net"                                                                                                      
    ## [112] "modified trap"                                                                                                  
    ## [113] "Seine net"                                                                                                      
    ## [114] "1.931=190"                                                                                                      
    ## [115] "1.721=315"                                                                                                      
    ## [116] "1.511=150"                                                                                                      
    ## [117] "1kg=200"                                                                                                        
    ## [118] "1.3kg=260"                                                                                                      
    ## [119] "1.711kg"                                                                                                        
    ## [120] "trip 2"                                                                                                         
    ## [121] "unmodified trap"

## <a name="export"></a> **Exporting cleaned dataset**

``` r
head(df)
```

    ## # A tibble: 6 × 31
    ##   Operation_date      enumerator       landing_site BMU   fisher_id fisher_phone
    ##   <dttm>              <chr>            <chr>        <chr> <chr>     <chr>       
    ## 1 2021-05-18 00:00:00 CLAPERTON KAZUN… UYOMBO       UYOM… SS/UYO/S… 799198738   
    ## 2 2021-05-18 00:00:00 CLAPERTON KAZUN… UYOMBO       UYOM… SS/UYO/S… 799198738   
    ## 3 2021-05-18 00:00:00 CLAPERTON KAZUN… UYOMBO       UYOM… SS/UYO/S… 799198738   
    ## 4 2021-05-18 00:00:00 CLAPERTON KAZUN… UYOMBO       UYOM… SS/UYO/S… 799198738   
    ## 5 2021-05-18 00:00:00 CLAPERTON KAZUN… UYOMBO       UYOM… SS/UYO/S… 769642401   
    ## 6 2021-05-18 00:00:00 CLAPERTON KAZUN… UYOMBO       UYOM… SS/UYO/S… 769642401   
    ## # ℹ 25 more variables: household_id <chr>, trap_type <chr>,
    ## #   total_traps_collected <dbl>, date_set_dd_mm_yyyy <dttm>,
    ## #   `time_set_24hh:mm` <chr>, date_collected_dd_mm_yyyy <dttm>,
    ## #   `time_collected_24hh:mm` <chr>, total_biomass_kg <dbl>,
    ## #   take_home_weight_kg <dbl>, total_value_KES <dbl>,
    ## #   take_home_value_KES <dbl>, `No. of fishers in crew` <dbl>,
    ## #   fishing_operation_notes <chr>, Kiswahili_name <chr>, …

``` r
nrow(df)
```

    ## [1] 74488

``` r
write_xlsx(df, "data/cleaned-Fishlandings-data-FEB 2023 JMCC.xlsx")
```
