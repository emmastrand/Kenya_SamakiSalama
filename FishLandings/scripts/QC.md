Quality Control of Fishing Landings dataset
================
Author: Emma Strand; <emma_strand@uri.edu>

## Questions for Austin and Clay

Crew size range - double check

## Contents

-   [**Protocol to run this with a future xlsx file**](#protocol)  
-   [**Load all libraries**](#libraries)  
-   [**Create dataframe**](#df)  
-   [**Quality Control: enumerator**](#Enumerator)  
-   [**Quality Control: landing\_site and BMU**](#Landing_site)  
-   [**Quality Control: fisher information**](#fisher_info)  
-   [**Quality Control: trap information**](#trap)  
-   [**Quality Control: catch information**](#catch)  
-   [**Gear type, and fish numbers/final destination**](#gear)  
-   [**Quality Control: final check for notes written by field
    team**](#notes)  
-   [**Exporting cleaned dataset**](#export)

## <a name="protocol"></a> **Protocol to run this with a future xlsx file**

1.  In the toolbar above, hit the arrow next to `Knit`. Scroll down to
    `Knit Directory` and select the option `Project Directory`.  
2.  **In `Create dataframe` code chunk**: Replace raw data file name in
    the function that creates the following variables:
    fishing\_operation, catch\_composition, and validation\_lists.  
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

[Reading in an excel datafile instead of a
csv](http://www.sthda.com/english/wiki/reading-data-from-excel-files-xls-xlsx-into-r).
This requires the `readxl` package listed in load all libraries step.

``` r
## when running future iterations of raw data file, replace the file name below 
fishing_operation <- read_excel("data/Fishlandings-data- CC-JM-Clay-IW combined 7-28-2022.xlsx", sheet = "fishing_operation",
                                col_types = c("date", "text", "text", "text", "text", "text", "text", 
                                              "text", "numeric", "date", "text", "date", "text", "numeric", 
                                              "numeric", "numeric", "numeric", "text", "skip", "skip")) %>%
  rename(Operation_date = date_dd_mm_yyyy)

nrow(fishing_operation) #10670 fishing operations (keep this # in mind for sanity check at the end)
```

    ## [1] 10670

``` r
## when running future iterations of raw data file, replace the file name below 
catch_composition <- read_excel("data/Fishlandings-data- CC-JM-Clay-IW combined 7-28-2022.xlsx", 
                                sheet = "catch_composition",
                                col_types = c("date", "text", "text", "text", "text", "text", "numeric",
                                              "text", "text")) %>%
  rename(Operation_date = Date)

nrow(catch_composition) #58441 fish observations (keep this # in mind for sanity check at the end)
```

    ## [1] 58441

``` r
## when running future iterations of raw data file, replace the file name below 
validation_lists <- read_excel("data/Fishlandings-data- CC-JM-Clay-IW combined 7-28-2022.xlsx", sheet = "validation_lists")

# read in enumerator names file 
enumerator_list <- read_excel("data/enumerator_list.xlsx")
```

Errors found in fisher\_id column are mostly capitalization errors.

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
  rename(scientific_name = SPECIES)
```

## Quality Control

### Operation\_date

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

    ## [1] NA

``` r
## Step #2 in protocol at the top of this script 
unique(sort(df$enumerator)) # at this point, double check that this list are all individual fishermen 
```

    ##  [1] "ANTHONY JUMA"      "BASHIR SAID"       "BIDALLA RASHID"   
    ##  [4] "BRUNO MOYE"        "CELESTINE N. ALI"  "CLAPERTON KAZUNGU"
    ##  [7] "FRANKLINE KAZUNGU" "GARAMA K. YERI"    "GILBERT NZAI"     
    ## [10] "KADZO BAYA"        "KARIMA NYINGE"     "KITSAO KARISA"    
    ## [13] "MAXSON KAZUNGU"    "NGALA"             "OMAR ALI"

### <a name="Landing_site"></a> **Landing\_site and BMU**

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

    ## [1] NA

``` r
unique(sort(df$landing_site))
```

    ##  [1] "BURENI"          "CHAUREMBO"       "KANAMAI"         "KARKLAND"       
    ##  [5] "KIJANGWANI"      "KITANGANI"       "KIVUKONI"        "KIVULINI"       
    ##  [9] "KURUWITU"        "MAWE YA KATI"    "MAYUNGU"         "MWANAMIA"       
    ## [13] "MWENDO WA PANYA" "NGOLOKO"         "SUN N SAND"      "UYOMBO"         
    ## [17] "VIPINGO"         "VUMA"

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

    ## [1] "MAWE YA KATI" "KIVULINI"     NA

``` r
unique(sort(df$BMU))
```

    ## [1] "KANAMAI"      "KIVULINI"     "KURUWITU"     "MAWE YA KATI" "MAYUNGU"     
    ## [6] "TAKAUNGU"     "UYOMBO"

``` r
## Step #4 in protocol to double check this output list is all the correct BMU names 
## (if the only output from setdiff() is NA then this list is correct)
```

### <a name="Fisher_info"></a> **Fisher information**

We don’t have a current need to correct fisher phone number right now.

### household\_id

The only issue I can detect here is some lower case vs upper case.

It would be nice to have a list of expected household and fisher ID’s.

``` r
df$household_id <- toupper(df$household_id)
unique(df$household_id)
```

    ##   [1] "SS/UYO/SB/091"    "SS/UYO/SB/085"    "SS/UYO/SB/089"   
    ##   [4] "SS/UYO/SB/088"    "SS/UYO/SB/090"    "SS/UYO/SB/092"   
    ##   [7] "SS/UYO/SB/094"    "SS/UYO/SB/94"     "SS/UYO/SB/95"    
    ##  [10] "SS/UYO/SB/100"    "SS/UYO/SB/099"    "SS/MAY/SB/002"   
    ##  [13] "SS/MAY/SB/001"    "SS/MAY/SB/013"    "SS/MAY/SB/018"   
    ##  [16] "SS/MAY/SB/021"    "SS/MAY/SB/049"    "SS/MAY/SB/033"   
    ##  [19] "SS/MAY/SB/032"    "SS/MAY/SB/011"    "SS/MAY/SB/003"   
    ##  [22] "SS/MAY/SB/006"    "SS/MAY/SB/041"    "SS/MAY/SB/074"   
    ##  [25] "SS/MAY/SB/030"    "SS/MAY/SB/077"    "SS/MAY/SB/010"   
    ##  [28] "SS/MAY/SB/043"    "SS/MAY/SB/042"    "SS/MAY/SB/083"   
    ##  [31] "SS/MAY/SB/052"    "SS/MAY/SB/028"    "SS/MAY/SB/082"   
    ##  [34] "SS/MAY/SB/047"    "SS/MAY/SB/046"    "SS/MAY/SB/048"   
    ##  [37] "SS/MAY/SB/044"    "SS/MAY/SB/036"    "SS/UYO/SB/96"    
    ##  [40] "SS/MAY/SB/034"    "SS/MAY/SB/038"    "SS/MAY/SB/058"   
    ##  [43] "SS/MAY/SB/017"    "SS/MAY/SB/081"    "SS/MAY/SB/029"   
    ##  [46] "SS/MAY/SB/014"    "SS/MAY/SB/015"    "SS/MAY/SB/053"   
    ##  [49] "SS/UYO/SB/095"    "SS/UYO/SB/086"    "SS/UYO/SB/097"   
    ##  [52] "SS/UYO/SB/90"     "SS/UYO/SB/99"     "SS/UYO/SB/88"    
    ##  [55] "SS/UYO/SB/86"     "SS/UYO/SB/91"     "SS/UYO/SB/85"    
    ##  [58] "SS/UYO/SB/096"    "SS/MAY/SB/064"    "SS/MAY/SB/063"   
    ##  [61] "SS/MAY/SB/076"    "SS/UYO/SB/082"    "SS/UYO/SB/079"   
    ##  [64] "SS/UYO/SB/071"    "SS/UYO/SB/039"    "SS/MAY/SB/012"   
    ##  [67] "SS/MAY/SB/031"    "SS/MAY/SB/070"    "SS/UYO/SB/020"   
    ##  [70] "SS/UYO/SB/093"    "SS/MAY/SB/045"    "SS/MAY/SB/022"   
    ##  [73] "SS/MAY/SB/078"    "SS/MAY/SB/009"    "SS/MAY/SB/065"   
    ##  [76] "SS/MAY/SB/027"    "SS/KUR/SG/032"    "SS/KUR/SG/008"   
    ##  [79] "SS/KUR/SG/005"    "SS/KUR/SG/009"    "SS/KUR/SG/027"   
    ##  [82] "SS/KUR/SG/002"    "SS/KUR/SG/012"    "SS/KUR/SG/083"   
    ##  [85] "SS/KUR/SG/001"    "SS/UYO/SB/084"    "SS/KUR/SG/007"   
    ##  [88] "SS/KUR/SG/065"    "SS/KUR/SG/100"    "SS/KUR/SG/082"   
    ##  [91] "SS/KUR/SG/054"    "SS/KUR/SG/099"    "SS/KUR/SG/043"   
    ##  [94] "SS/KUR/SG/040"    "SS/KUR/SG/048"    "SS/KUR/SG/039"   
    ##  [97] "SS/KUR/SG/038"    "SS/KUR/SG/034"    "SS/KUR/SG/037"   
    ## [100] "SS/KUR/SG/050"    "SS/KUR/SG/033"    "SS/KUR/SG/095"   
    ## [103] "SS/KUR/SG/096"    "SS/KUR/SG/046"    "SS/KUR/SG/036"   
    ## [106] "SS/KUR/SG/035"    "SS/KUR/SG/049"    "SS/KUR/SG/098"   
    ## [109] "SS/KUR/SG/097"    "SS/KUR/SG/094"    "SS/KUR/SG/041"   
    ## [112] "SS/KUR/SG/047"    "SS/KAN/CO/012"    "SS/KAN/CO/013"   
    ## [115] "SS/KAN/CO/014"    "SS/KAN/CO/016"    "SS/KAN/CO/017"   
    ## [118] "SS/KAN/CO/082"    "SS/KAN/CO/083"    "SS/KAN/CO/084"   
    ## [121] "SS/KAN/CO/086"    "SS/KAN/CO/136"    "SS/KAN/CO/067"   
    ## [124] "SS/KAN/CO/106"    "SS/KAN/CO/127"    "SS/KAN/CO/071"   
    ## [127] "SS/KAN/CO/107"    "SS/MAY/SB/068"    "SS/MAY/SB/066"   
    ## [130] "SS/MAY/SB/020"    "SS/MAY/SB/004"    "SS/MAY/SB/072"   
    ## [133] "SS/MAY/SB/019"    "SS/TAK/CO/174"    "SS/TAK/CO/194"   
    ## [136] "SS/TAK/CO/195"    "SS/TAK/CO/144"    "SS/TAK/CO/197"   
    ## [139] "SS/TAK/CO/142"    "SS/TAK/CO/175"    "SS/TAK/CO/198"   
    ## [142] "SS/TAK/CO/196"    "SS/TAK/CO/178"    "SS/TAK/CO/179"   
    ## [145] "SS/TAK/CO/177"    "SS/TAK/CO/176"    "SS/TAK/CO/180"   
    ## [148] "SS/KAN/CO/113"    "SS/TAK/CO/135"    "SS/TAK/CO/036"   
    ## [151] "SS/TAK/CO/035"    "SS/TAK/CO/031"    "SS/TAK/CO/023"   
    ## [154] "SS/KAN/CO/020"    "SS/TAK/CO/026"    "SS/TAK/CO/030"   
    ## [157] "SS/TAK/CO/088"    "SS/TAK/CO/117"    "SS/TAK/CO/024"   
    ## [160] "SS/KAN/CO/026"    "SS/KAN/CO/088"    "SS/KAN/CO/036"   
    ## [163] "SS/KAN/CO/031"    "SS/KAN/CO/085"    "SS/KAN/CO/076"   
    ## [166] "SS/KAN/CO/133"    "SS/KAN/CO/078"    "SS/KAN/CO/077"   
    ## [169] "SS/KAN/CO/140"    "SS/KAN/CO/072"    "SS/KAN/CO/075"   
    ## [172] "SS/KAN/CO/102"    "SS/KAN/CO/073"    "SS/MAY/SB/025"   
    ## [175] "SS/MAY/SB/026"    "SS/MAY/SB/035"    "SS/MAY/SB/050"   
    ## [178] "SS/MAY/SB/060"    "SS/MAY/SB/005"    "SS/MAY/SB/059"   
    ## [181] "SS/MAY/SB/027/"   "SS/TAK/CO/149"    "SS/TAK/CO/147"   
    ## [184] "SS/TAK/CO/206"    "SS/TAK/CO/160"    "SS/TAK/CO/150"   
    ## [187] "SS/TAK/CO/165"    "SS/TAK/CO/158"    "SS/TAK/CO/163"   
    ## [190] "SS/TAK/CO/152"    "SS/TAK/CO/146"    "SS/TAK/CO/192"   
    ## [193] "SS/TAK/CO/151"    "SS/TAK/CO/155"    "SS/KAN/CO/018"   
    ## [196] "SS/TAK/CO/145"    "SS/TAK/CO/154"    "SS/TAK/CO/148"   
    ## [199] "SS/TAK/CO/166"    "SS/TAK/CO/159"    "SS/KAN/CO/038"   
    ## [202] "SS/KAN/CO/043"    "SS/KAN/CO/042"    "SS/KAN/CO/044"   
    ## [205] "SS/KAN/CO/117"    "SS/KAN/CO/024"    "SS/KAN/CO/030"   
    ## [208] "SS/KAN/CO/035"    "SS/KAN/CO/015"    "SS/KAN/CO/025"   
    ## [211] "SS/KAN/CO/002"    "SS/KAN/CO/009"    "SS/KAN/CO/070"   
    ## [214] "SS/KAN/CO/028"    "SS/KAN/CO/172"    "SS/KAN/CO/183"   
    ## [217] "SS/KAN/CO/171"    "SS/KAN/CO/167"    "SS/KAN/CO/173"   
    ## [220] "SS/KAN/CO/182"    "SS/TAK/CO/205"    "SS/TAK/CO/161"   
    ## [223] "SS/TAK/CO/067"    "SS/TAK/CO/162"    "SS/TAK/CO/157"   
    ## [226] "SS/TAK/CO/172"    "SS/TAK/CO/171"    "SS/TAK/CO/170"   
    ## [229] "SS/TAK/CO/167"    "SS/TAK/CO/191"    "SS/TAK/CO/173"   
    ## [232] "SS/TAK/CO/168"    "SS/TAK/CO/183"    "SS/TAK/CO/182"   
    ## [235] "SS/TAK/CO/038"    "SS/KAN/CO/034"    "SS/KAN/CO/041"   
    ## [238] "SS/KAN/CO/022"    "SS/MAY/SB/039"    "SS/TAK/CO/169"   
    ## [241] "SS/MAY/SB/055"    "SS/KUR/SG/090"    "SS/KUR/SG/030"   
    ## [244] "SS/KUR/SG/020"    "SS/KUR/SG/029"    "SS/KUR/SG/133"   
    ## [247] "SS/KAN/CO/023"    "SS/KAN/CO/135"    NA                
    ## [250] "SS/KUR/SG/086"    "SS/KUR/SG/081"    "SS/KUR/SG/021"   
    ## [253] "SS/KUR/SG/064"    "SS/KUR/SG/011"    "SS/KUR/SG/061"   
    ## [256] "SS/KUR/SG/042"    "SS/KUR/SG/016"    "SS/KUR/SG/077"   
    ## [259] "SS/KUR/SG/018"    "SS/KUR/SG/073"    "SS/KUR/SG/068"   
    ## [262] "SS/KUR/SG/069"    "SS/KUR/SG/060"    "SS/KUR/SG/072"   
    ## [265] "SS/KUR/SG/078"    "SS/KUR/SG/076"    "SS/KUR/SG/010"   
    ## [268] "SS/KAN/CO/099"    "SS/KAN/CO/074"    "SS/KAN/CO/039"   
    ## [271] "SS/KAN/CO/040"    "SS/KUR/SG/069F"   "SS/UYO/SB/021"   
    ## [274] "SS/UYO/SB/035"    "SS/TAK/CO/164"    "SS/TAK/CO/203"   
    ## [277] "SS/UYO/SB/147"    "SS/UYO/SB/152"    "SS/UYO/SB/083"   
    ## [280] "SS/TAK/CO/181"    "SS/TAK/CO/199"    "SS/KUR/SG/057"   
    ## [283] "SS/AY/SB/025"     "SS/AY/SB/002"     "SS/AY/SB/015"    
    ## [286] "SS/AY/SB/028"     "SS/AY/SB/012"     "SS/AY/SB/070"    
    ## [289] "SS/AY/SB/009"     "SS/AY/SB/017"     "SS/AY/SB/083"    
    ## [292] "SS/AY/SB/082"     "SS/AY/SB/030"     "SS/AY/SB/029"    
    ## [295] "SS/AY/SB/058"     "SS/AY/SB/064"     "SS/AY/SB/068"    
    ## [298] "SS/AY/SB/059"     "SS/AY/SB/063"     "SS/AY/SB/027"    
    ## [301] "SS/AY/SB/026"     "SS/AY/SB/033"     "SS/AY/SB/042"    
    ## [304] "SS/MAY/SB/168"    "SS/KAN/CO/011"    "SS/UYO/SB/028"   
    ## [307] "SS/UYO/SB/090/FF" "SS/UYO/SB/71"     "SS/MAY/SB/085"   
    ## [310] "SS/MAY/SB/089"    "SS/MAY/SB/071"    "SS/MAY/SB/095"   
    ## [313] "SS/MAY/SB/086"    "SS/MAY/SB/094"    "SS/MAY/SB/100"   
    ## [316] "SS/MAY/SB/099"    "SS/MAY/SB/090"    "SS/MAY/SB/091"   
    ## [319] "SS/MAY/SB/092"    "SS/MAY/SB/088"

### <a name="trap"></a> **Trap information**

### trap\_type

The only issue I can detect here is some lower case vs upper case.

``` r
df$trap_type <- toupper(df$trap_type)
unique(sort(df$trap_type))
```

    ##  [1] "BUNDUKI"                     "HANDLINE"                   
    ##  [3] "MKANO"                       "MKANO,BUNDUKI"              
    ##  [5] "MODIFIED"                    "MONOFILAMENT GILLNET"       
    ##  [7] "MONOFILLAMENT"               "MSHIPI"                     
    ##  [9] "NETI YA MKANO"               "NETI YA UZI"                
    ## [11] "NYAVU"                       "NYAVU MSHIPI"               
    ## [13] "NYAVU UZI"                   "NYAVU YA KUTEGA"            
    ## [15] "NYAVU YA MKANO"              "NYAVU YA MKANO/OCTOPUS HOOK"
    ## [17] "NYAVU YA UZI"                "REEF SEINE"                 
    ## [19] "SEINE NET"                   "SPEAR GUN"                  
    ## [21] "SPEAR GUN AND SEINE NET"     "SPEARGUN"                   
    ## [23] "UNMODIFIED"

### total\_traps\_collected

View the values put in the df here to double check all values make
sense.

``` r
total_traps_collected <- df %>% select(total_traps_collected) %>% na.omit()
range(total_traps_collected)
```

    ## [1]  0 63

``` r
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

### Weight and value measures

``` r
total_weight_kg <- df %>% select(total_weight_kg) %>% na.omit()
#take_home_weight_kg <- df %>% select(take_home_weight_kg) %>% na.omit()
total_value_KES <- df %>% select(total_value_KES) %>% na.omit()
#take_home_value_KES <- df %>% select(take_home_value_KES) %>% na.omit()

## Step #4 from protocol: Double check the below values are in the correct ranges
range(total_weight_kg)
```

    ## [1]   0 490

``` r
#range(take_home_weight_kg)
range(total_value_KES)
```

    ## [1]     0 23040

``` r
#range(take_home_value_KES)

hist(df$total_weight_kg)
```

![](QC_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
#hist(df$take_home_weight_kg)
hist(df$total_value_KES)
```

![](QC_files/figure-gfm/unnamed-chunk-12-2.png)<!-- -->

``` r
#hist(df$take_home_value_KES)
```

### No. of fishers in crew

Crews above 5 people are unrealistic. I’m changing that data to ‘NA’ for
now.

``` r
fishermen_no <- df %>% select(`No. of fishers in crew`) %>% na.omit()

## Protocol: Double check the below values are in the correct ranges
range(fishermen_no)
```

    ## [1]      1 137200

``` r
unique(sort(fishermen_no$`No. of fishers in crew`))
```

    ##   [1]      1.00      1.32      2.00      3.00      4.00      5.00      6.00
    ##   [8]     12.00     18.00     40.00     50.00     60.00     75.00     80.00
    ##  [15]     88.00     90.00    100.00    105.00    120.00    125.00    128.00
    ##  [22]    136.00    140.00    150.00    153.00    160.00    175.00    176.00
    ##  [29]    180.00    187.00    190.00    200.00    210.00    215.00    220.00
    ##  [36]    221.00    225.00    225.90    230.00    238.00    240.00    242.00
    ##  [43]    245.00    250.00    255.00    260.00    264.00    265.00    270.00
    ##  [50]    272.00    275.00    279.00    280.00    285.00    286.00    290.00
    ##  [57]    300.00    306.00    310.00    320.00    320.01    323.00    325.00
    ##  [64]    330.00    340.00    350.00    352.00    357.00    360.00    370.00
    ##  [71]    374.00    375.00    376.00    378.00    380.00    390.00    391.00
    ##  [78]    395.00    396.00    400.00    405.00    415.00    418.00    419.00
    ##  [85]    420.00    425.00    430.00    440.00    442.00    450.00    453.00
    ##  [92]    459.00    460.00    462.00    470.00    476.00    480.00    483.00
    ##  [99]    484.00    485.00    490.00    493.00    500.00    508.00    510.00
    ## [106]    515.00    520.00    522.00    525.00    527.00    528.00    536.00
    ## [113]    540.00    544.00    550.00    560.00    561.00    567.00    570.00
    ## [120]    575.00    578.00    580.00    588.00    590.00    595.00    600.00
    ## [127]    606.00    609.00    615.00    616.00    619.00    620.00    625.00
    ## [134]    629.00    630.00    638.00    640.00    646.00    650.00    651.00
    ## [141]    660.00    665.00    670.00    672.00    675.00    679.50    680.00
    ## [148]    682.00    684.00    688.00    690.00    691.50    693.00    697.50
    ## [155]    700.00    704.00    705.00    708.00    714.00    715.00    720.00
    ## [162]    725.00    735.00    740.00    745.00    750.00    752.00    758.00
    ## [169]    759.00    760.00    767.25    768.00    775.00    777.00    780.00
    ## [176]    782.00    790.00    792.00    800.00    801.00    809.00    810.00
    ## [183]    814.00    815.00    816.00    820.00    832.00    835.00    836.00
    ## [190]    840.00    844.00    845.00    850.00    860.00    864.00    867.00
    ## [197]    870.00    875.00    880.00    882.00    889.00    891.00    895.50
    ## [204]    900.00    901.00    902.00    906.00    912.00    913.00    920.00
    ## [211]    924.00    925.00    929.00    930.00    935.00    936.00    938.00
    ## [218]    940.00    945.00    946.00    950.00    960.00    966.00    967.00
    ## [225]    975.00    978.00    980.00    987.00    990.00    999.00   1000.00
    ## [232]   1006.00   1008.00   1010.00   1012.00   1020.00   1026.00   1030.00
    ## [239]   1037.00   1040.00   1042.00   1044.00   1050.00   1053.00   1054.00
    ## [246]   1055.00   1056.00   1060.00   1064.00   1070.00   1071.00   1080.00
    ## [253]   1083.00   1085.20   1090.00   1092.00   1096.00   1100.00   1102.00
    ## [260]   1107.00   1110.00   1114.00   1120.00   1125.00   1132.00   1135.00
    ## [267]   1139.00   1140.00   1144.00   1148.00   1150.00   1160.00   1170.00
    ## [274]   1190.00   1197.00   1200.00   1220.00   1222.00   1222.50   1223.00
    ## [281]   1224.00   1228.70   1230.00   1232.00   1238.25   1240.00   1250.00
    ## [288]   1260.00   1270.00   1280.00   1285.00   1290.00   1300.00   1302.00
    ## [295]   1305.00   1318.00   1320.00   1323.00   1340.00   1342.00   1344.00
    ## [302]   1350.00   1360.00   1364.00   1365.00   1375.00   1380.00   1386.00
    ## [309]   1400.00   1404.00   1407.00   1408.00   1410.00   1411.00   1415.00
    ## [316]   1420.00   1425.00   1428.00   1439.40   1440.00   1447.00   1450.00
    ## [323]   1452.00   1460.00   1461.00   1470.00   1474.00   1480.00   1485.00
    ## [330]   1497.00   1500.00   1502.00   1504.00   1506.00   1510.00   1512.00
    ## [337]   1515.00   1520.00   1530.00   1534.00   1537.50   1540.00   1545.00
    ## [344]   1548.00   1550.00   1557.00   1560.00   1584.00   1586.00   1586.30
    ## [351]   1588.00   1590.00   1591.00   1594.50   1600.00   1601.00   1605.00
    ## [358]   1610.00   1620.00   1621.00   1625.00   1628.00   1635.00   1638.00
    ## [365]   1640.00   1650.00   1653.00   1653.60   1656.30   1660.00   1665.00
    ## [372]   1669.50   1670.00   1675.00   1680.00   1682.00   1692.30   1695.10
    ## [379]   1696.00   1700.00   1710.00   1717.00   1719.00   1720.00   1720.70
    ## [386]   1722.00   1740.00   1743.00   1745.00   1750.00   1760.00   1770.00
    ## [393]   1773.00   1775.00   1776.00   1780.00   1782.00   1784.00   1785.00
    ## [400]   1800.00   1804.00   1805.00   1807.00   1820.00   1830.00   1833.00
    ## [407]   1834.00   1840.00   1848.00   1850.00   1854.00   1860.00   1864.00
    ## [414]   1869.00   1870.00   1875.00   1878.80   1880.00   1890.00   1892.00
    ## [421]   1899.00   1900.00   1902.00   1920.00   1930.00   1940.00   1945.00
    ## [428]   1950.00   1954.50   1960.00   1968.60   1970.00   1974.00   1980.00
    ## [435]   1983.30   1986.00   1990.00   1991.30   1992.70   2000.00   2010.00
    ## [442]   2019.00   2020.00   2024.00   2025.00   2032.50   2037.00   2040.00
    ## [449]   2043.00   2050.00   2055.00   2070.00   2080.00   2100.00   2120.00
    ## [456]   2125.00   2130.00   2140.00   2150.00   2151.40   2157.10   2160.00
    ## [463]   2170.00   2190.00   2200.00   2210.00   2223.00   2224.20   2240.00
    ## [470]   2250.00   2257.50   2260.00   2270.40   2280.00   2290.00   2300.00
    ## [477]   2310.00   2320.00   2326.96   2328.00   2340.00   2350.00   2360.00
    ## [484]   2361.30   2370.00   2375.00   2380.00   2385.00   2400.00   2415.00
    ## [491]   2420.00   2430.00   2435.00   2440.00   2460.00   2480.00   2500.00
    ## [498]   2505.00   2520.00   2530.00   2540.00   2541.00   2550.00   2565.00
    ## [505]   2580.00   2600.00   2603.00   2610.00   2620.00   2630.00   2631.00
    ## [512]   2635.00   2640.00   2645.00   2650.00   2670.00   2680.00   2687.00
    ## [519]   2688.00   2695.00   2700.00   2701.00   2715.00   2720.00   2722.00
    ## [526]   2725.00   2730.00   2750.00   2752.20   2755.00   2765.00   2772.00
    ## [533]   2775.00   2780.00   2800.00   2803.30   2814.00   2820.00   2823.00
    ## [540]   2835.00   2850.00   2860.00   2875.00   2900.00   2910.00   2920.00
    ## [547]   2940.00   2950.00   2960.10   2970.00   2974.50   2990.00   3000.00
    ## [554]   3015.00   3024.00   3040.00   3045.00   3050.00   3060.00   3080.00
    ## [561]   3082.00   3100.00   3120.00   3140.00   3150.00   3173.00   3177.30
    ## [568]   3180.00   3186.70   3200.00   3207.20   3218.60   3240.00   3242.00
    ## [575]   3255.00   3260.00   3299.60   3300.00   3318.00   3335.00   3360.00
    ## [582]   3380.00   3400.00   3420.00   3430.00   3440.00   3444.00   3450.00
    ## [589]   3460.00   3465.00   3477.10   3500.00   3525.00   3540.00   3550.00
    ## [596]   3570.00   3595.35   3600.00   3604.05   3608.00   3612.00   3650.00
    ## [603]   3665.00   3674.00   3678.40   3680.00   3700.00   3740.00   3750.00
    ## [610]   3760.00   3780.00   3800.00   3802.70   3840.00   3847.80   3885.00
    ## [617]   3900.00   3910.00   3919.30   3948.20   4000.00   4050.00   4055.00
    ## [624]   4080.00   4100.00   4118.64   4122.80   4160.00   4200.00   4235.00
    ## [631]   4250.00   4300.00   4320.00   4350.00   4370.00   4397.80   4400.00
    ## [638]   4410.00   4443.80   4500.00   4600.00   4620.00   4650.00   4666.00
    ## [645]   4700.00   4721.00   4725.00   4800.00   4851.00   4860.00   4862.00
    ## [652]   4893.00   4900.00   5000.00   5040.00   5065.50   5100.00   5130.00
    ## [659]   5150.00   5160.00   5200.00   5280.00   5300.00   5368.00   5381.42
    ## [666]   5400.00   5454.00   5489.00   5500.00   5522.00   5535.00   5543.00
    ## [673]   5560.00   5599.30   5600.00   5700.00   5780.00   5800.00   5880.00
    ## [680]   5890.50   6000.00   6100.00   6110.00   6120.00   6170.00   6200.00
    ## [687]   6290.00   6400.00   6424.00   6436.00   6450.00   6480.00   6500.00
    ## [694]   6514.00   6600.00   6630.00   6666.00   6690.00   6712.50   6720.00
    ## [701]   6750.00   6780.00   6800.00   6840.00   6900.00   7000.00   7016.90
    ## [708]   7200.00   7310.00   7400.00   7560.00   7620.00   7650.00   7800.00
    ## [715]   7820.00   7960.00   8000.00   8100.00   8200.00   8280.00   8400.00
    ## [722]   8500.00   8600.00   8640.00   8944.00   8981.50   9120.00   9240.00
    ## [729]   9400.00   9540.00   9600.00   9680.00   9720.00   9800.00   9930.00
    ## [736]  10000.00  10080.00  10200.00  10250.00  10260.00  10350.00  10400.00
    ## [743]  10540.00  10600.00  10800.00  10829.00  10980.00  11050.00  11250.00
    ## [750]  11400.00  11475.00  11760.00  11900.00  11960.00  11970.00  12160.00
    ## [757]  12320.00  12400.00  12600.00  12800.00  12820.00  13000.00  13280.00
    ## [764]  13400.00  13500.00  13600.00  13800.00  14000.00  14040.00  14200.00
    ## [771]  14280.00  14400.00  14560.00  14580.00  15250.00  15390.00  15640.00
    ## [778]  16400.00  16600.00  16800.00  17000.00  17910.00  17964.00  18200.00
    ## [785]  19050.00  19600.00  20060.00  20310.00  21200.00  22400.00  28950.00
    ## [792]  30000.00  33600.00  36400.00  36960.00  43700.00  45000.00  72500.00
    ## [799]  80000.00  81200.00 137200.00

``` r
# df %>% filter(`No. of fishers in crew` > 5)
# 
# df <- df %>% 
#  mutate(crew_size = case_when(
#     `No. of fishers in crew` > 5 ~ "NA"))

df$crew_size_corrected <- df$`No. of fishers in crew`

# # replacing values higher than 5 with NA
# df <- df %>%
#  replace_with_na_at(
#     .vars = 'crew_size_corrected',
#     condition = ~(.x > 6))
#   
# # double checking that the above worked
# unique(sort(df$crew_size_corrected))
```

### Kiswahili\_name

``` r
df$Kiswahili_name <- toupper(df$Kiswahili_name)
unique(sort(df$Kiswahili_name))
```

    ##   [1] "BARAKUDA"                 "BATANI"                  
    ##   [3] "BIRINZI"                  "BOCHO"                   
    ##   [5] "BUA"                      "BUA/MBORO YA MVUA"       
    ##   [7] "BUNDU"                    "BUNJU"                   
    ##   [9] "CHAA"                     "CHALE"                   
    ##  [11] "CHANG GAMWE"              "CHANGA"                  
    ##  [13] "CHANGE GAMWE"             "CHANGO NDOMO"            
    ##  [15] "CHANGU"                   "CHANGU GAMWE"            
    ##  [17] "CHANGU MACHO"             "CHANGU MDOMO"            
    ##  [19] "CHANGU NDOMO"             "CHANGU NDOWA"            
    ##  [21] "CHANGU TAWA"              "CHANGUTAWA"              
    ##  [23] "CHEMBEU"                  "CHENA"                   
    ##  [25] "DOME"                     "FUTE"                    
    ##  [27] "FUTE KUMBI"               "FUTE MLEA"               
    ##  [29] "FUTE MOSHI"               "FUTE MRABA"              
    ##  [31] "FUTE MRAMBA"              "GENDA"                   
    ##  [33] "GENDE"                    "GONA"                    
    ##  [35] "GONA SHARIFU"             "GONO"                    
    ##  [37] "HAMISI MACHO"             "JAME"                    
    ##  [39] "JANARE"                   "JODARI"                  
    ##  [41] "KABANGI"                  "KADA"                    
    ##  [43] "KADIFU"                   "KAKURUWENDE"             
    ##  [45] "KAMBA KOLOLO"             "KAMBA SHUARI"            
    ##  [47] "KAMBA SIMBA"              "KAMBA WINDU"             
    ##  [49] "KANG'AJA"                 "KANGAJA"                 
    ##  [51] "KANGAJA HEWANI"           "KANGAJA HEWENI"          
    ##  [53] "KANGAJE"                  "KANGAJI HEWANI"          
    ##  [55] "KATATANGE"                "KERENGE"                 
    ##  [57] "KHADA"                    "KHADA/TEWA"              
    ##  [59] "KIBOMA"                   "KIFUDU"                  
    ##  [61] "KIFUVU"                   "KIFUVUU"                 
    ##  [63] "KIJAME"                   "KIKANDE"                 
    ##  [65] "KIKOKWE"                  "KINAUCHI"                
    ##  [67] "KINGOE"                   "KINWAUCHI"               
    ##  [69] "KINYWAUCHI"               "KIPEPEO"                 
    ##  [71] "KITAME"                   "KITATANGA"               
    ##  [73] "KITATANGE"                "KIUNGA"                  
    ##  [75] "KIVUVU"                   "KOLE KOLE"               
    ##  [77] "KOLEKOLE"                 "KORIS"                   
    ##  [79] "KOTOWE"                   "KOTWE"                   
    ##  [81] "KUFI"                     "KUFI SAFARI"             
    ##  [83] "KUFI/KIMBULIMBULI/KUKUSI" "KUMBA"                   
    ##  [85] "KUMBI"                    "KUMBI FUTE"              
    ##  [87] "KUWAUCHI"                 "LWAYOO"                  
    ##  [89] "MABACHO"                  "MBININI"                 
    ##  [91] "MBONO"                    "MBORO YA MVUVI"          
    ##  [93] "MCHAKUFA"                 "MGENDA"                  
    ##  [95] "MKIZI"                    "MKORE"                   
    ##  [97] "MKORWE"                   "MKUNAJI"                 
    ##  [99] "MKUNDAJA"                 "MKUNDAJE"                
    ## [101] "MKUNDAJI"                 "MKUNDJI"                 
    ## [103] "MKUNGA"                   "MKUNGA CHAI"             
    ## [105] "MKUNGA CHUI"              "MKUNGA IBRAHIM"          
    ## [107] "MKUNGA MBONO"             "MKUNGA SAMAKI"           
    ## [109] "MKUNGA WIMBI"             "MLEA"                    
    ## [111] "MLEYA"                    "MNGENDA"                 
    ## [113] "MTANI"                    "MTONZI"                  
    ## [115] "MTUMBUA"                  "MTUMBUA DAU"             
    ## [117] "MTUMBUU"                  "MTUNE"                   
    ## [119] "MUGENDA"                  "NGAGU"                   
    ## [121] "NGANGU"                   "NGINDO"                  
    ## [123] "NGISI"                    "NGOGO"                   
    ## [125] "NGURU"                    "NJANA"                   
    ## [127] "NUMBA"                    "NYAVI"                   
    ## [129] "NYENGA"                   "PAKOE"                   
    ## [131] "PAMAMBA"                  "PANDA"                   
    ## [133] "PANDU"                    "PANGA"                   
    ## [135] "PANGA SAMAKI"             "PAROTI"                  
    ## [137] "PONO"                     "PONO BLEU FISH"          
    ## [139] "PONO BLUE"                "PONO BLUE FIN"           
    ## [141] "PONO BLUE FISH"           "PONO BLUEFISH"           
    ## [143] "PONO CHANI"               "PONO KABANGI"            
    ## [145] "PONO KADIFI"              "PONO KADIFU"             
    ## [147] "PONO KASIKI"              "PONO MAENGE"             
    ## [149] "PONO MWANI"               "PONO SUNGURA"            
    ## [151] "PUJU"                     "PUJU PEMBE"              
    ## [153] "PUNDU"                    "PWEZA"                   
    ## [155] "SANGE"                    "SENDENGOMANI"            
    ## [157] "SHANA"                    "SHARIFU"                 
    ## [159] "SIMU"                     "SITEFUE"                 
    ## [161] "STEFUE"                   "TAA"                     
    ## [163] "TAA YEDA"                 "TAF MANGA"               
    ## [165] "TAFI"                     "TAFI  SIGANUS"           
    ## [167] "TAFI KITUMBO"             "TAFI MAENGA"             
    ## [169] "TAFI MAENGE"              "TAFI MAENGU"             
    ## [171] "TAFI MANGA"               "TAFI MIMBA"              
    ## [173] "TAFI MWAMBA"              "TAKAUNA"                 
    ## [175] "TAKUANA"                  "TEMBO"                   
    ## [177] "TENGESI"                  "TEWA"                    
    ## [179] "TEWA JESHI"               "TEWA KALESO"             
    ## [181] "TEWA KOPE"                "TEWA KOPWE"              
    ## [183] "TEWA LESO"                "TEWA MOSHI"              
    ## [185] "TEWA THARAFA"             "TEWA THARAKA"            
    ## [187] "TEWA WIMBI"               "TEWE"                    
    ## [189] "TEWEJESHI"                "TOA"                     
    ## [191] "TOGOO"                    "TUFUANA"                 
    ## [193] "TUGUU"                    "TUKUANA"                 
    ## [195] "TUNDU"                    "TUTE"                    
    ## [197] "USENDE NGOMANI"           "VUMBAMA"                 
    ## [199] "VUMBANA"                  "WAYO"                    
    ## [201] "WAYO ULIMI NG'OMBE"       "WAYOO"

### SPECIES / Scientific name

**This is a hard-coding way to do this.. ideally we could downloand a
dataset from fishbase and create a compare function that could recognize
a name that is a letter or 2 off from a name in fishbase and then create
suggestions…**

We can pull in the validation\_lists df to double check these spellings.

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

# correcting spellings in validation list 
validation_lists$scientific_name <- gsub("Gymonthorax", "Gymnothorax", validation_lists$scientific_name)
validation_lists$scientific_name <- gsub("Pomatonus", "Pomatomus", validation_lists$scientific_name)
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
    ##  [3] "Acanthurus harak"             "Aethaloperca rogaa"          
    ##  [5] "Amanses scopas"               "Auxis thazard"               
    ##  [7] "Carangoides flavimarginatus"  "Carangoides florimaginatus"  
    ##  [9] "Caranx hippos"                "Cyprichromis leptosoma"      
    ## [11] "Epinephelus melanostigma"     "Epinephelus spilotoceps"     
    ## [13] "Gompheus caeruleus"           "Gymnothorax flavimarginatus" 
    ## [15] "Gymnothorax monochrous"       "Gymothorax favagineus"       
    ## [17] "Himantura gerrardi"           "Hipposcarus scarus"          
    ## [19] "Kyphosus bigibbus"            "Leptoscarus triostegus"      
    ## [21] "Lethrinus conchliatus"        "Lethrinus macronemus"        
    ## [23] "Lethrinus sutor"              "Lethrinus vaigiensis"        
    ## [25] "Lutjanus canius"              "Monodactylus argentimailatus"
    ## [27] "Monotaxis grandoculis"        "Mugil cephalus"              
    ## [29] "Mulloidichthys pfluegeri"     "Naso brachycentron"          
    ## [31] "Ostracion nasus"              "Panulirus homarus"           
    ## [33] "Panulirus ornatus"            "Panulirus penicillatus"      
    ## [35] "Panulirus versicolor"         "Parupeneus semicirculatus"   
    ## [37] "Planiliza alata"              "Planiliza sp."               
    ## [39] "Platybelone platyura"         "Plectorhinchus plagiodesmus" 
    ## [41] "Plectorhinchus playfairi"     "Plotosus canius"             
    ## [43] "Pono blue fish"               "Pseudorhombus arsius"        
    ## [45] "Sardinella melanura"          "Scarus carolinus"            
    ## [47] "Scarus sutor"                 "Scarus vaigiensis"           
    ## [49] "Scolopsis bimaculata"         "Sepia pharaonis"             
    ## [51] "Siganus canaliculatus"        "Siganus fuscescens"          
    ## [53] "Siganus guttatus"             "Sphyraena japonica"          
    ## [55] "Sphyraena leiura"             "Taeniura meyeni"             
    ## [57] "Tafi sutor"                   "Terapon theraps"             
    ## [59] "Thunnus albacares"            "Thysanophrys chiltonae"      
    ## [61] "Uroteuthis cynea"             "Uroteuthis duvauceli"        
    ## [63] "Uroteuthis lineatus"

1.) In catch composition and in fishbase but not on validation list.
**Suggested fix: address if these are reasonable to find in Ghana and if
they are, keep these entries.**

-   Acanthopagrus berda  
-   Aethaloperca rogaa  
-   Amanses scopas  
-   Auxis thazard  
-   Caranx hippos  
-   Cyprichromis leptosoma  
-   Epinephelus melanostigma  
-   Epinephelus spilotoceps  
-   Gompheus caeruleus  
-   Gymothorax favagineus  
-   Gymnothorax flavimarginatus  
-   Gymnothorax monochrous  
-   Himantura gerrardi  
-   Kyphosus bigibbus  
-   Lethrinus conchliatus  
-   Monotaxis grandoculis  
-   Mugil cephalus  
-   Mulloidichthys pfluegeri  
-   Naso brachycentron  
-   Ostracion nasus  
-   Panulirus homarus  
-   Panulirus ornatus  
-   Panulirus penicillatus  
-   Panulirus versicolor  
-   Planiliza alata  
-   Platybelone platyura  
-   Plectorhinchus plagiodesmus  
-   Plectorhinchus playfairi  
-   Plotosus canius  
-   Pseudorhombus arsius  
-   Sardinella melanura  
-   Scarus vaigiensis  
-   Scolopsis bimaculata  
-   Sepia pharaonis  
-   Siganus canaliculatus  
-   Siganus fuscescens  
-   Siganus guttatus  
-   Sphyraena japonica  
-   Taeniura meyeni  
-   Terapon theraps  
-   Thunnus albacares  
-   Thysanophrys chiltonae  
-   Uroteuthis duvauceli

2.) In catch composition but not in validation list or on fishbase (not
close to a name we have so unsure what it is supposed to be).
**Suggested fix: if there is not a clear answer to what these are
supposed to be, filter them out.**

-   Acanthurus duvauceli  
-   Acanthurus harak  
-   Carangoides flavimarginatus  
-   Carangoides florimaginatus  
-   Hipposcarus scarus  
-   Leptoscarus triostegus  
-   Lethrinus macronemus  
-   Lethrinus sutor  
-   Lethrinus vaigiensis  
-   Lutjanus canius  
-   Monodactylus argentimailatus  
-   Parupeneus semicirculatus  
-   Pono blue fish (probably meant to be a common name..)  
-   Scarus carolinus  
-   Scarus sutor  
-   Sphyraena leiura  
-   Tafi sutor  
-   Uroteuthis cynea  
-   Uroteuthis lineatus

3.) In validation list but is not on fish base. **No fix needed here,
just an FYI.**.

-   Acanthurus vaigiensis

### Length (cm)

This column is a character for because of the “&lt;” and “-”.

1.) 3 observations (rows) have 2 length values and multiple fish.
changed these values to NA for now.

-   “16-20 ,46-50”  
-   “26-30,21- 25”

2.) Many operations by CLAPERTON KAZUNGU include a length value of 4488
which is not realistic so I changed these to NA for now.

3.) Some ranges weren’t correct like “16-25” and only have &lt;10
observations like that so I changed them to the nearest possible
category. e.g. “16-25” to “16-20”. “21-30” to “21-25”.

``` r
unique(sort(df$length_cm))
```

    ##   [1] "˂10"                "<10"                "<11"               
    ##   [4] "<12"                "<13"                "<14"               
    ##   [7] "<15"                "<16"                "<17"               
    ##  [10] ">10"                ">50 write in:"      ">50 write in: 101" 
    ##  [13] ">50 write in: 102"  ">50 write in: 103"  ">50 write in: 104" 
    ##  [16] ">50 write in: 105"  ">50 write in: 107"  ">50 write in: 109" 
    ##  [19] ">50 write in: 110"  ">50 write in: 111"  ">50 write in: 56.2"
    ##  [22] ">50 write in: 58"   ">50 write in: 58.6" ">50 write in: 60.3"
    ##  [25] ">50 write in: 62"   ">50 write in: 62.9" ">50 write in: 64.4"
    ##  [28] ">50 write in: 64.8" ">50 write in: 65"   ">50 write in: 65.1"
    ##  [31] ">50 write in: 66.5" ">50 write in: 67.6" ">50 write in: 67.7"
    ##  [34] ">50 write in: 68"   ">50 write in: 68.7" ">50 write in: 69.5"
    ##  [37] ">50 write in: 69.8" ">50 write in: 71.2" ">50 write in: 71.9"
    ##  [40] ">50 write in: 72"   ">50 write in: 72.2" ">50 write in: 72.3"
    ##  [43] ">50 write in: 74.3" ">50 write in: 75"   ">50 write in: 75.5"
    ##  [46] ">50 write in: 79.2" ">50 write in: 80"   ">50 write in: 80.2"
    ##  [49] ">50 write in: 81.2" ">50 write in: 82"   ">50 write in: 85"  
    ##  [52] ">50 write in: 86"   ">50 write in: 87"   ">50 write in: 89"  
    ##  [55] ">50 write in: 90"   ">50 write in: 92"   ">50 write in: 93"  
    ##  [58] ">50 write in: 94"   ">50 write in: 96"   ">50 write in: 98"  
    ##  [61] ">50 write in:101"   ">50 write in:59"    ">50 write in:61"   
    ##  [64] ">50 write in:65.7"  ">50 write in:69.3"  ">50 write in:72.1" 
    ##  [67] ">50 write in:75"    "1-15"               "100"               
    ##  [70] "102"                "103"                "105"               
    ##  [73] "106"                "107"                "108"               
    ##  [76] "109"                "11-15"              "11-16"             
    ##  [79] "110"                "111"                "112"               
    ##  [82] "114"                "115"                "117"               
    ##  [85] "119"                "123"                "129"               
    ##  [88] "134"                "16"                 "16-20"             
    ##  [91] "16-30"              "17"                 "170"               
    ##  [94] "173"                "176"                "177"               
    ##  [97] "181"                "183"                "189"               
    ## [100] "198"                "21-24"              "21-25"             
    ## [103] "21-25 26-30"        "21-25 26-31"        "21-25, 26-30"      
    ## [106] "21-26"              "25-30"              "26-20"             
    ## [109] "26-30"              "26-31"              "269-30"            
    ## [112] "31-25"              "31-35"              "31-36"             
    ## [115] "31.-35"             "36-34"              "36-40"             
    ## [118] "41-45"              "45-50"              "46-50"             
    ## [121] "52"                 "54"                 "55"                
    ## [124] "56"                 "58"                 "59"                
    ## [127] "60"                 "60.3"               "60.4"              
    ## [130] "61"                 "62"                 "63"                
    ## [133] "64.8"               "64.9"               "65"                
    ## [136] "65.6"               "65.8"               "66"                
    ## [139] "67"                 "67.1"               "67.7"              
    ## [142] "68"                 "68.1"               "68.2"              
    ## [145] "68.4"               "68.5"               "68.7"              
    ## [148] "68.9"               "69"                 "69.7"              
    ## [151] "69.8"               "69.9"               "70"                
    ## [154] "70.5"               "71.8"               "72"                
    ## [157] "72.2"               "72.3"               "72.4"              
    ## [160] "72.6"               "73"                 "74"                
    ## [163] "74.1"               "74.2"               "74.3"              
    ## [166] "74.5"               "74.7"               "75"                
    ## [169] "75.5"               "75.6"               "75.7"              
    ## [172] "75.8"               "75.9"               "76"                
    ## [175] "76.4"               "76.6"               "76.7"              
    ## [178] "76.8"               "76.9"               "77"                
    ## [181] "77.2"               "77.8"               "78"                
    ## [184] "78.1"               "78.2"               "78.4"              
    ## [187] "78.5"               "78.6"               "78.8"              
    ## [190] "79"                 "79.1"               "79.2"              
    ## [193] "79.4"               "79.5"               "79.8"              
    ## [196] "80"                 "80.2"               "80.3"              
    ## [199] "81"                 "82"                 "82.1"              
    ## [202] "82.2"               "82.3"               "82.4"              
    ## [205] "83"                 "84"                 "85"                
    ## [208] "86"                 "87"                 "88"                
    ## [211] "89"                 "90"                 "91"                
    ## [214] "92"                 "93"                 "94"                
    ## [217] "95"                 "96"                 "97"                
    ## [220] "98"                 "99"

``` r
# replace the write in verbiage with no characters
df$length_cm <- gsub(">50 write in: ", "", df$length_cm)
df$length_cm <- gsub(">50 write in:", "", df$length_cm)

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
    length_calc >= 50.5 & length_calc <= 75 ~ ">50",
    length_calc > 75 ~ ">75")) 

df <- df %>%
  mutate(length_corrected = if_else(is.na(length_calc), length_cm, length_calc))

# double checking that worked for the corrected column 
unique(sort(df$length_cm))
```

    ##   [1] "0-10"  "100"   "101"   "102"   "103"   "104"   "105"   "106"   "107"  
    ##  [10] "108"   "109"   "11-15" "110"   "111"   "112"   "114"   "115"   "117"  
    ##  [19] "119"   "123"   "129"   "134"   "16"    "16-20" "17"    "170"   "173"  
    ##  [28] "176"   "177"   "181"   "183"   "189"   "198"   "21-25" "26-30" "31-35"
    ##  [37] "36-40" "41-45" "46-50" "52"    "54"    "55"    "56"    "56.2"  "58"   
    ##  [46] "58.6"  "59"    "60"    "60.3"  "60.4"  "61"    "62"    "62.9"  "63"   
    ##  [55] "64.4"  "64.8"  "64.9"  "65"    "65.1"  "65.6"  "65.7"  "65.8"  "66"   
    ##  [64] "66.5"  "67"    "67.1"  "67.6"  "67.7"  "68"    "68.1"  "68.2"  "68.4" 
    ##  [73] "68.5"  "68.7"  "68.9"  "69"    "69.3"  "69.5"  "69.7"  "69.8"  "69.9" 
    ##  [82] "70"    "70.5"  "71.2"  "71.8"  "71.9"  "72"    "72.1"  "72.2"  "72.3" 
    ##  [91] "72.4"  "72.6"  "73"    "74"    "74.1"  "74.2"  "74.3"  "74.5"  "74.7" 
    ## [100] "75"    "75.5"  "75.6"  "75.7"  "75.8"  "75.9"  "76"    "76.4"  "76.6" 
    ## [109] "76.7"  "76.8"  "76.9"  "77"    "77.2"  "77.8"  "78"    "78.1"  "78.2" 
    ## [118] "78.4"  "78.5"  "78.6"  "78.8"  "79"    "79.1"  "79.2"  "79.4"  "79.5" 
    ## [127] "79.8"  "80"    "80.2"  "80.3"  "81"    "81.2"  "82"    "82.1"  "82.2" 
    ## [136] "82.3"  "82.4"  "83"    "84"    "85"    "86"    "87"    "88"    "89"   
    ## [145] "90"    "91"    "92"    "93"    "94"    "95"    "96"    "97"    "98"   
    ## [154] "99"

``` r
unique(sort(df$length_corrected))
```

    ##  [1] ">50"   ">75"   "0-10"  "11-15" "16-20" "21-25" "26-30" "31-35" "36-40"
    ## [10] "41-45" "46-50"

Correct output for length\_corrected:
`">50"   ">75"   "0-10"  "11-15" "16-20" "21-25" "26-30" "31-35" "36-40" "41-45" "46-50"`.

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

    ##  [1] "BUNDUKI"                  "GILLNET"                 
    ##  [3] "HANDLINE"                 "LOCAL CONSUMER"          
    ##  [5] "MODIFIED"                 "MONOFILAMENT"            
    ##  [7] "MSHIPI"                   "NETI YA MKANO"           
    ##  [9] "NYAVU MSHIPI"             "NYAVU UZI"               
    ## [11] "OTHER (SPECIFY IN NOTES)" "SEINE NET"               
    ## [13] "SPEAR"                    "SPEARGUN"                
    ## [15] "SPEARGUN AND SEINE NET"   "UNMODIFIED"              
    ## [17] "UZI"

### Number of fish

Doube check this range is what is expected.

``` r
unique(sort(df$number_of_fish))
```

    ##   [1]     0     1     2     3     4     5     6     7     8     9    10    11
    ##  [13]    12    13    14    15    16    17    18    19    20    21    22    23
    ##  [25]    24    25    26    27    28    29    30    31    32    33    34    35
    ##  [37]    36    37    38    39    40    41    42    43    44    45    46    47
    ##  [49]    48    49    50    51    52    53    54    55    56    57    58    59
    ##  [61]    60    61    62    65    66    67    68    69    70    72    75    76
    ##  [73]    77    78    80    82    83    84    85    87    89    92    94    96
    ##  [85]    98   103   111   120   129   140   150   153   160   170   192   234
    ##  [97]   270   300  6000 30000

``` r
df %>% ggplot(., aes(y=number_of_fish)) + geom_boxplot() + theme_bw()
```

    ## Warning: Removed 815 rows containing non-finite values (stat_boxplot).

![](QC_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
#### filtering out 6,000 and 30,0000 
df <- df %>%
  filter(number_of_fish < 1000)
```

### Destination of fish

``` r
df$destination <- toupper(df$destination)

df$destination <- gsub("OTHER WRITE IN:", "", df$destination)
df$destination <- gsub(" LOCAL CONSUMER", "LOCAL CONSUMER", df$destination)
df$destination <- gsub("LOCAL CONSUMERS", "LOCAL CONSUMER", df$destination)

unique(sort(df$destination))
```

    ## [1] " MZUNGU"        "FISH DEALER"    "GIFT"           "HOME"          
    ## [5] "LOCAL CONSUMER" "MAMA KARANGA"   "MZUNGU"         "OTHER"

## <a name="notes"></a> **Final check: any notes from both datasets**

``` r
## check for any notes that the data collectors left for analysis 
unique(df$fishing_operation_notes)
```

    ##   [1] NA                                                                                                                                                       
    ##   [2] "Innocent: highlighted cell because need to check accuracy (Aug 24)"                                                                                     
    ##   [3] "Innocent: highlighted rows because they seem too similar to one another, particularly columns K - M. Need to check with enumerator on accuracy (Aug 24)"
    ##   [4] "Innocent: need to update column M -- check with enumerator (Aug 24)"                                                                                    
    ##   [5] "153"                                                                                                                                                    
    ##   [6] "800"                                                                                                                                                    
    ##   [7] "200"                                                                                                                                                    
    ##   [8] "240"                                                                                                                                                    
    ##   [9] "400"                                                                                                                                                    
    ##  [10] "160"                                                                                                                                                    
    ##  [11] "260"                                                                                                                                                    
    ##  [12] "600"                                                                                                                                                    
    ##  [13] "220"                                                                                                                                                    
    ##  [14] "480"                                                                                                                                                    
    ##  [15] "280"                                                                                                                                                    
    ##  [16] "520"                                                                                                                                                    
    ##  [17] "320"                                                                                                                                                    
    ##  [18] "440"                                                                                                                                                    
    ##  [19] "100"                                                                                                                                                    
    ##  [20] "120"                                                                                                                                                    
    ##  [21] "80"                                                                                                                                                     
    ##  [22] "136"                                                                                                                                                    
    ##  [23] "60"                                                                                                                                                     
    ##  [24] "90"                                                                                                                                                     
    ##  [25] "20"                                                                                                                                                     
    ##  [26] "40"                                                                                                                                                     
    ##  [27] "168"                                                                                                                                                    
    ##  [28] "294"                                                                                                                                                    
    ##  [29] "315"                                                                                                                                                    
    ##  [30] "420"                                                                                                                                                    
    ##  [31] "210"                                                                                                                                                    
    ##  [32] "252"                                                                                                                                                    
    ##  [33] "189"                                                                                                                                                    
    ##  [34] "105"                                                                                                                                                    
    ##  [35] "84"                                                                                                                                                     
    ##  [36] "147"                                                                                                                                                    
    ##  [37] "126"                                                                                                                                                    
    ##  [38] "378"                                                                                                                                                    
    ##  [39] "150"                                                                                                                                                    
    ##  [40] "231"                                                                                                                                                    
    ##  [41] "308"                                                                                                                                                    
    ##  [42] "924"                                                                                                                                                    
    ##  [43] "265"                                                                                                                                                    
    ##  [44] "270"                                                                                                                                                    
    ##  [45] "880"                                                                                                                                                    
    ##  [46] "360"                                                                                                                                                    
    ##  [47] "290"                                                                                                                                                    
    ##  [48] "212"                                                                                                                                                    
    ##  [49] "300"                                                                                                                                                    
    ##  [50] "98"                                                                                                                                                     
    ##  [51] "85"                                                                                                                                                     
    ##  [52] "70"                                                                                                                                                     
    ##  [53] "0"                                                                                                                                                      
    ##  [54] "500"                                                                                                                                                    
    ##  [55] "50"                                                                                                                                                     
    ##  [56] "170"                                                                                                                                                    
    ##  [57] "180"                                                                                                                                                    
    ##  [58] "215"                                                                                                                                                    
    ##  [59] "4000"                                                                                                                                                   
    ##  [60] "250"                                                                                                                                                    
    ##  [61] "225"                                                                                                                                                    
    ##  [62] "650"                                                                                                                                                    
    ##  [63] "75"                                                                                                                                                     
    ##  [64] "175"                                                                                                                                                    
    ##  [65] "155"                                                                                                                                                    
    ##  [66] "110"                                                                                                                                                    
    ##  [67] "140"                                                                                                                                                    
    ##  [68] "190"                                                                                                                                                    
    ##  [69] "108"                                                                                                                                                    
    ##  [70] "285"                                                                                                                                                    
    ##  [71] "245"                                                                                                                                                    
    ##  [72] "484"                                                                                                                                                    
    ##  [73] "350"                                                                                                                                                    
    ##  [74] "trip 2"                                                                                                                                                 
    ##  [75] "2700"                                                                                                                                                   
    ##  [76] "625"                                                                                                                                                    
    ##  [77] "700"                                                                                                                                                    
    ##  [78] "375"                                                                                                                                                    
    ##  [79] "338"                                                                                                                                                    
    ##  [80] "347"                                                                                                                                                    
    ##  [81] "973"                                                                                                                                                    
    ##  [82] "122"                                                                                                                                                    
    ##  [83] "201"                                                                                                                                                    
    ##  [84] "1133"                                                                                                                                                   
    ##  [85] "309"                                                                                                                                                    
    ##  [86] "119"                                                                                                                                                    
    ##  [87] "232"                                                                                                                                                    
    ##  [88] "845"                                                                                                                                                    
    ##  [89] "345"                                                                                                                                                    
    ##  [90] "97.5"                                                                                                                                                   
    ##  [91] "1044"                                                                                                                                                   
    ##  [92] "221"                                                                                                                                                    
    ##  [93] "938"                                                                                                                                                    
    ##  [94] "525"                                                                                                                                                    
    ##  [95] "1050"                                                                                                                                                   
    ##  [96] "900"                                                                                                                                                    
    ##  [97] "104"                                                                                                                                                    
    ##  [98] "330"                                                                                                                                                    
    ##  [99] "88"                                                                                                                                                     
    ## [100] "540"                                                                                                                                                    
    ## [101] "340"                                                                                                                                                    
    ## [102] "450"                                                                                                                                                    
    ## [103] "286"                                                                                                                                                    
    ## [104] "130"                                                                                                                                                    
    ## [105] "66"                                                                                                                                                     
    ## [106] "43.05"                                                                                                                                                  
    ## [107] "44.16"                                                                                                                                                  
    ## [108] "390"                                                                                                                                                    
    ## [109] "560"                                                                                                                                                    
    ## [110] "1.5"                                                                                                                                                    
    ## [111] "154"                                                                                                                                                    
    ## [112] "44"                                                                                                                                                     
    ## [113] "132"                                                                                                                                                    
    ## [114] "7500"                                                                                                                                                   
    ## [115] "5000"                                                                                                                                                   
    ## [116] "4500"                                                                                                                                                   
    ## [117] "3500"                                                                                                                                                   
    ## [118] "2000"                                                                                                                                                   
    ## [119] "125"                                                                                                                                                    
    ## [120] "261"                                                                                                                                                    
    ## [121] "310"                                                                                                                                                    
    ## [122] "187.5"                                                                                                                                                  
    ## [123] "146.25"                                                                                                                                                 
    ## [124] "283.5"                                                                                                                                                  
    ## [125] "188.1"                                                                                                                                                  
    ## [126] "337.5"                                                                                                                                                  
    ## [127] "154.5"                                                                                                                                                  
    ## [128] "110.25"                                                                                                                                                 
    ## [129] "210.1"                                                                                                                                                  
    ## [130] "51"                                                                                                                                                     
    ## [131] "102"                                                                                                                                                    
    ## [132] "1120"                                                                                                                                                   
    ## [133] "204"                                                                                                                                                    
    ## [134] "176"                                                                                                                                                    
    ## [135] "127.6"                                                                                                                                                  
    ## [136] "198"                                                                                                                                                    
    ## [137] "1.8"                                                                                                                                                    
    ## [138] "460"

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
    ##  [15] "Pono  sitefue"                                                                                                  
    ##  [16] "Changu ndomo"                                                                                                   
    ##  [17] "Changu mgamwe"                                                                                                  
    ##  [18] "pareti"                                                                                                         
    ##  [19] "fute yea"                                                                                                       
    ##  [20] "90"                                                                                                             
    ##  [21] "Changu tawa"                                                                                                    
    ##  [22] "Changu tuku"                                                                                                    
    ##  [23] "changu tawa"                                                                                                    
    ##  [24] "0.7 kg"                                                                                                         
    ##  [25] "1.1 kg"                                                                                                         
    ##  [26] "1.1 kg  all take home"                                                                                          
    ##  [27] "1.6 kg"                                                                                                         
    ##  [28] "2.3 kg"                                                                                                         
    ##  [29] "1 kg"                                                                                                           
    ##  [30] "1.5 kg"                                                                                                         
    ##  [31] "1.2 kg"                                                                                                         
    ##  [32] "0.4 kg"                                                                                                         
    ##  [33] "0.5 kg"                                                                                                         
    ##  [34] "0.9 kg"                                                                                                         
    ##  [35] "1.3 kg"                                                                                                         
    ##  [36] "1.5 kg all take home"                                                                                           
    ##  [37] "0.6 kg"                                                                                                         
    ##  [38] "1.7 kg"                                                                                                         
    ##  [39] "2.2 kg"                                                                                                         
    ##  [40] "modified trap"                                                                                                  
    ##  [41] "0.7 kg all take home"                                                                                           
    ##  [42] "0.6 kg all take home"                                                                                           
    ##  [43] "0.3 kg"                                                                                                         
    ##  [44] "1.8 kg"                                                                                                         
    ##  [45] "0.8 kg all take home"                                                                                           
    ##  [46] "0.8 kg"                                                                                                         
    ##  [47] "1.3 kg all take home"                                                                                           
    ##  [48] "0.4 kg all take home"                                                                                           
    ##  [49] "1.4 kg"                                                                                                         
    ##  [50] "1 kg all take home"                                                                                             
    ##  [51] "800g , ksh 80"                                                                                                  
    ##  [52] "1kg, kshs. 100"                                                                                                 
    ##  [53] "1.5kg, kshs 150"                                                                                                
    ##  [54] "1.8kg kshs 360"                                                                                                 
    ##  [55] "2kg Kshs 300"                                                                                                   
    ##  [56] "1.5kg, kshs 200"                                                                                                
    ##  [57] "1.8kg kshs 270"                                                                                                 
    ##  [58] "2kg Kshs 400"                                                                                                   
    ##  [59] "1.5kg Kshs. 150"                                                                                                
    ##  [60] "1.5kg Kshs. 300"                                                                                                
    ##  [61] "1kg Kshs. 200"                                                                                                  
    ##  [62] "1.5 kg kshs. 150"                                                                                               
    ##  [63] "1.3kg Kshs. 195"                                                                                                
    ##  [64] "Modified trap"                                                                                                  
    ##  [65] "0.53kg  Kshs. 53 . Modified"                                                                                    
    ##  [66] "monofilament gillnet"                                                                                           
    ##  [67] "handline"                                                                                                       
    ##  [68] "reef seine"                                                                                                     
    ##  [69] "nyavu ya kutega"                                                                                                
    ##  [70] "couldn’t quantify take home"                                                                                    
    ##  [71] "Take home volume are 4 buckets"                                                                                 
    ##  [72] "0.5kg Kshs. 50"                                                                                                 
    ##  [73] "0.98kg Kshs. 98"                                                                                                
    ##  [74] "2.8kg Kshs. 560."                                                                                               
    ##  [75] "0.93kg. Kshs. 93"                                                                                               
    ##  [76] "1.3 kg Kshs. 260"                                                                                               
    ##  [77] "1.3kg Kshs. 260"                                                                                                
    ##  [78] "2.5kg Kshs. 250"                                                                                                
    ##  [79] "2kg Kshs. 200"                                                                                                  
    ##  [80] "1.3kg Kshs. 300"                                                                                                
    ##  [81] "1.3kg Kshs 300"                                                                                                 
    ##  [82] "2.5kg Kshs. 300"                                                                                                
    ##  [83] "1.9kg Kshs. 190"                                                                                                
    ##  [84] "0.8kg Kshs. 160"                                                                                                
    ##  [85] "1.2kg Kshs 120"                                                                                                 
    ##  [86] "0.8kg Kshs. 80"                                                                                                 
    ##  [87] "1kg. Kshs 200"                                                                                                  
    ##  [88] "1kg. Kshs.100"                                                                                                  
    ##  [89] "1.3kg Kshs. 130"                                                                                                
    ##  [90] "1.3kg Kshs 130"                                                                                                 
    ##  [91] "2.1kg Kshs.210"                                                                                                 
    ##  [92] "1.5kg Kshs 150"                                                                                                 
    ##  [93] "1kg Kshs. 100"                                                                                                  
    ##  [94] "2.1kg Kshs. 210"                                                                                                
    ##  [95] "0.5kg Kshs. 100"                                                                                                
    ##  [96] "1.8kg Kshs 180"                                                                                                 
    ##  [97] "0.5kg Kshs 50"                                                                                                  
    ##  [98] "0.6kg Kshs. 60"                                                                                                 
    ##  [99] "0.4kg Kshs. 40"                                                                                                 
    ## [100] "1.3kg Kshs 260"                                                                                                 
    ## [101] "0.75kg Kshs .150"                                                                                               
    ## [102] "0.5 kg kshs.75"                                                                                                 
    ## [103] "0.375 kg kshs.56.25"                                                                                            
    ## [104] "0.5 kg Kshs.100"                                                                                                
    ## [105] "0.65 kg Kshs.130"                                                                                               
    ## [106] "0.35 kg Kshs.52.5"                                                                                              
    ## [107] "0.43 kg Kshs.86"                                                                                                
    ## [108] "0.5 kg Ksh. 75"                                                                                                 
    ## [109] "0.83 kg Kshs.166"                                                                                               
    ## [110] "0.84 kg Kshs.168"                                                                                               
    ## [111] "0.4 kg Kshs.80"                                                                                                 
    ## [112] "0.4 kg Kshs.60"                                                                                                 
    ## [113] "0.5 kg Ksh. 100"                                                                                                
    ## [114] "0.32 kg kshs.48"                                                                                                
    ## [115] "1 kg Ksh.200"                                                                                                   
    ## [116] "0.72 kg Kshs.144"                                                                                               
    ## [117] "1.3 kg Kshs.260"                                                                                                
    ## [118] "1.25 kg kshs.250"                                                                                               
    ## [119] "0.6 kg Ksh.120"                                                                                                 
    ## [120] "trip 2"                                                                                                         
    ## [121] "0.77 kg Ksh. 154"                                                                                               
    ## [122] "0.224kg Kshs 22"                                                                                                
    ## [123] "0.4kg Kshs. 60"                                                                                                 
    ## [124] "0.4kg Kshs 60"                                                                                                  
    ## [125] "0.3kg Kshs.30"                                                                                                  
    ## [126] "0.75kg Kshs. 150"                                                                                               
    ## [127] "1.1kg Kshs. 220"                                                                                                
    ## [128] "0.2kg Kshs.20"                                                                                                  
    ## [129] "0.393kg Kshs. 40"                                                                                               
    ## [130] "1.8kg Kshs. 360"                                                                                                
    ## [131] "1.1kg Kshs. 210"                                                                                                
    ## [132] "0.5kg Kshs 75"                                                                                                  
    ## [133] "0.3kg Kshs 60"                                                                                                  
    ## [134] "0.8kg Kshs 80"                                                                                                  
    ## [135] "0.8kg Kshs 160"                                                                                                 
    ## [136] "0.87 kg Kshs.174"                                                                                               
    ## [137] "0.73 kg Kshs. 109.5"                                                                                            
    ## [138] "0.31 kg kshs.46.5"                                                                                              
    ## [139] "0.32 kg kshs.64"                                                                                                
    ## [140] "0.4 kg kshs.80"                                                                                                 
    ## [141] "0.2 kg Kshs.30"                                                                                                 
    ## [142] "0.42 kg Kshs.63"                                                                                                
    ## [143] "0.5 kg Kshs. 75"                                                                                                
    ## [144] "0.9 kg kshs.135"                                                                                                
    ## [145] "0.41 kg kshs.82"                                                                                                
    ## [146] "0.41 kg kshs.61.5"                                                                                              
    ## [147] "0.297 kg kshs.59.4"                                                                                             
    ## [148] "0.394 kg kshs.59.1"                                                                                             
    ## [149] "0.403 kg kshs.60.45"                                                                                            
    ## [150] "0.404 kg kshs.60.6"                                                                                             
    ## [151] "0.343 kg kshs.51.45"                                                                                            
    ## [152] "1.3 kg Kshs.195"                                                                                                
    ## [153] "1.3 kg kshs.260"                                                                                                
    ## [154] "0.9 kg kshs.180"                                                                                                
    ## [155] "0.92 kg kshs.184"                                                                                               
    ## [156] "0.89 kg kshs.178"                                                                                               
    ## [157] "1 kg kshs.200"                                                                                                  
    ## [158] "0.7 kg Ksh.140"                                                                                                 
    ## [159] "0.47 kg kshs.94"                                                                                                
    ## [160] "1.2 kg kshs.240"                                                                                                
    ## [161] "0.6 kg Ksh.90"                                                                                                  
    ## [162] "0.5 kg Ksh.75"                                                                                                  
    ## [163] "1.2 kg Ksh.180"                                                                                                 
    ## [164] "1.4 kg Kshs.210"                                                                                                
    ## [165] "0.9 kg Kshs.135"                                                                                                
    ## [166] "0.8 kg Kshs.120"                                                                                                
    ## [167] "2.3 kg Kshs.345"                                                                                                
    ## [168] "1.1 kg Kshs.165"                                                                                                
    ## [169] "0.7 kg Kshs.105"                                                                                                
    ## [170] "2.2 kg Kshs.330"                                                                                                
    ## [171] "1.6 kg kshs.320"                                                                                                
    ## [172] "1 kg Ksh.150"                                                                                                   
    ## [173] "1.8 kg Kshs.270"                                                                                                
    ## [174] "2.5 kg Kshs.375"                                                                                                
    ## [175] "1kg Kshs.150"                                                                                                   
    ## [176] "2.1 kg kshs.315"                                                                                                
    ## [177] "1.741= 250"                                                                                                     
    ## [178] "2.110=400"                                                                                                      
    ## [179] "1.731="                                                                                                         
    ## [180] "1.673=160"                                                                                                      
    ## [181] "2.5kg-425sh"                                                                                                    
    ## [182] "3kg-540"                                                                                                        
    ## [183] "5kg-850"                                                                                                        
    ## [184] "2.5 kg-450sh"                                                                                                   
    ## [185] "Local consumer"                                                                                                 
    ## [186] "1.511=150"                                                                                                      
    ## [187] "1.513=160"                                                                                                      
    ## [188] "2.110=210"                                                                                                      
    ## [189] "2.110=300"

## <a name="export"></a> **Exporting cleaned dataset**

``` r
head(df)
```

    ## # A tibble: 6 × 29
    ##   Operation_date      enumerator       landing_site BMU   fisher_id fisher_phone
    ##   <dttm>              <chr>            <chr>        <chr> <chr>     <chr>       
    ## 1 2021-05-18 00:00:00 CLAPERTON KAZUN… UYOMBO       UYOM… SS/UYO/S… 799198738   
    ## 2 2021-05-18 00:00:00 CLAPERTON KAZUN… UYOMBO       UYOM… SS/UYO/S… 799198738   
    ## 3 2021-05-18 00:00:00 CLAPERTON KAZUN… UYOMBO       UYOM… SS/UYO/S… 799198738   
    ## 4 2021-05-18 00:00:00 CLAPERTON KAZUN… UYOMBO       UYOM… SS/UYO/S… 799198738   
    ## 5 2021-05-18 00:00:00 CLAPERTON KAZUN… UYOMBO       UYOM… SS/UYO/S… 769642401   
    ## 6 2021-05-18 00:00:00 CLAPERTON KAZUN… UYOMBO       UYOM… SS/UYO/S… 769642401   
    ## # … with 23 more variables: household_id <chr>, trap_type <chr>,
    ## #   total_traps_collected <dbl>, date_set_dd_mm_yyyy <dttm>,
    ## #   `time_set_24hh:mm` <chr>, date_collected_dd_mm_yyyy <dttm>,
    ## #   `time_collected_24hh:mm` <chr>, total_weight_kg <dbl>,
    ## #   total_value_KES <dbl>, `No. of fishers in crew` <dbl>,
    ## #   fishing_operation_notes <chr>, Kiswahili_name <chr>, scientific_name <chr>,
    ## #   length_cm <chr>, `gear type` <chr>, number_of_fish <dbl>, …

``` r
nrow(df)
```

    ## [1] 150363

``` r
write_xlsx(df, "data/cleaned-Fishlandings-data- CC-JM-Clay-IW combined 7-28-2022.xlsx")
```
