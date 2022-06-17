Quality Control of Fishing Landings dataset
================
Author: Emma Strand; <emma_strand@uri.edu>

## Questions for Austin and Clay

1.  Is there a correct list of fisher ID’s? Fisher ID and enumerators
    are mixed up. e.g. CELESTINA NALI has several different fisher ID’s
    connected to it.  
2.  Trap type list? I have more than modified and unmodified traps.  
3.  EDWARD YAA and NGALA I have data from but they were not on Clay’s
    enumerator list he sent. Correspond to other names?  
4.  In the Catching information section under the Species / Scientific
    name section, take a look at \#1 and \#2 and the suggestion fixes on
    those. What do you think?  
5.  Is &gt;15 realistic for total traps collected?  
6.  Is &gt;40 kg realistic for total kg weight?  
7.  Is &gt;2 kg realistic for take home weight?  
8.  Is &gt;10,000 realistic for total value KES?  
9.  Is &gt;400 realistic for take home value KES?  
10. Is &gt;100 realistic for \# of fish per species caught on each boat
    trip (number\_of\_fish column)?

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
fishing_operation <- read_excel("data/Fishlandings-data_21052022-May_update-27052022.xlsx", sheet = "fishing_operation") %>%
  select(-'...21', -'...22', -'...23') %>%
  rename(Operation_date = date_dd_mm_yyyy)

nrow(fishing_operation) #2407 fishing operations (keep this # in mind for sanity check at the end)
```

    ## [1] 2407

``` r
## when running future iterations of raw data file, replace the file name below 
catch_composition <- read_excel("data/Fishlandings-data_21052022-May_update-27052022.xlsx", sheet = "catch_composition") %>%
  select(-'...10', -'...11', -'...12', -'...13') %>%
  rename(Operation_date = Date)

nrow(catch_composition) #14761 fish observations (keep this # in mind for sanity check at the end)
```

    ## [1] 14760

``` r
## when running future iterations of raw data file, replace the file name below 
validation_lists <- read_excel("data/Fishlandings-data_21052022-May_update-27052022.xlsx", sheet = "validation_lists")

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
df$enumerator <- gsub("CELESTINAR N ALI", "CELESTINE N. ALI", df$enumerator)
df$enumerator <- gsub("CELESTINA NALI", "CELESTINE N. ALI", df$enumerator)
df$enumerator <- gsub("^CLAPERTON$", "CLAPERTON KAZUNGU", df$enumerator)
df$enumerator <- gsub("MACKSON KAZUNGU", "MAXSON KAZUNGU", df$enumerator)
df$enumerator <- gsub("BIDALA RASHID", "BIDALLA RASHID", df$enumerator)
df$enumerator <- gsub("GARAMA YERI", "GARAMA K. YERI", df$enumerator)
df$enumerator <- gsub("BRUNO MUYE", "BRUNO MOYE", df$enumerator)
df$enumerator <- gsub("^ALI$", "CELESTINE N. ALI", df$enumerator) #^ and $ indicate start and end of phrase
df$enumerator <- gsub("KADZO KAZUNGU", "KADZO BAYA", df$enumerator)

# compare df list to the enumerator list 
# result is those that appear in the df but not validated enumerator list
setdiff(df$enumerator, enumerator_list$enumerator)
```

    ## [1] "EDWARD YAA" "NGALA"      NA

``` r
## Step #2 in protocol at the top of this script 
unique(sort(df$enumerator)) # at this point, double check that this list are all individual fishermen 
```

    ##  [1] "BASHIR SAID"       "BIDALLA RASHID"    "BRUNO MOYE"       
    ##  [4] "CELESTINE N. ALI"  "CLAPERTON KAZUNGU" "EDWARD YAA"       
    ##  [7] "FRANKLINE KAZUNGU" "GARAMA K. YERI"    "GILBERT NZAI"     
    ## [10] "KADZO BAYA"        "KARIMA NYINGE"     "KITSAO KARISA"    
    ## [13] "MAXSON KAZUNGU"    "NGALA"             "OMAR ALI"

### <a name="Landing_site"></a> **Landing\_site and BMU**

### Landing site

``` r
df$landing_site <- toupper(df$landing_site)
enumerator_list$landing_site <- toupper(enumerator_list$landing_site)

df$landing_site <- gsub("KIRKLAND", "KARKLAND", df$landing_site)

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
    ## [17] "VUMA"

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

### household\_id

The only issue I can detect here is some lower case vs upper case.

It would be nice to have a list of expected household and fisher ID’s.

``` r
df$household_id <- toupper(df$household_id)
unique(df$household_id)
```

    ##   [1] "SS/MAY/SB/047"    "SS/MAY/SB/045"    "SS/MAY/SB/048"   
    ##   [4] "SS/MAY/SB/017"    "SS/MAY/SB/083"    "SS/MAY/SB/082"   
    ##   [7] "SS/MAY/SB/028"    "SS/MAY/SB/052"    "SS/MAY/SB/029"   
    ##  [10] "SS/MAY/SB/011"    "SS/MAY/SB/074"    "SS/MAY/SB/062"   
    ##  [13] "SS/MAY/SB/078"    "SS/MAY/SB/004"    "SS/MAY/SB/003"   
    ##  [16] "SS/MAY/SB/072"    "SS/MAY/SB/006"    "SS/MAY/SB/038"   
    ##  [19] "SS/MAY/SB/034"    "SS/MAY/SB/049"    "SS/MAY/SB/081"   
    ##  [22] "SS/MAY/SB/058"    "SS/MAY/SB/063"    "SS/MAY/SB/064"   
    ##  [25] "SS/MAY/SB/036"    "SS/MAY/SB/014"    "SS/MAY/SB/015"   
    ##  [28] "SS/MAY/SB/077"    "SS/MAY/SB/030"    "SS/MAY/SB/021"   
    ##  [31] "SS/MAY/SB/018"    "SS/MAY/SB/032"    "SS/MAY/SB/010"   
    ##  [34] "SS/MAY/SB/002"    "SS/MAY/SB/001"    "SS/MAY/SB/013"   
    ##  [37] "SS/MAY/SB/035"    "SS/MAY/SB/033"    "SS/MAY/SB/012"   
    ##  [40] "SS/MAY/SB/005"    "SS/MAY/SB/007"    "SS/MAY/SB/031"   
    ##  [43] "SS/MAY/SB/076"    "SS/MAY/SB/066"    "SS/MAY/SB/067"   
    ##  [46] "SS/MAY/SB/068"    "SS/MAY/SB/019"    "SS/MAY/SB/041"   
    ##  [49] "SS/UYO/SB/100"    "SS/UYO/SB/099"    "SS/MAY/SB/042"   
    ##  [52] "SS/MAY/SB/071"    "SS/MAY/SB/070"    "SS/UYO/SB/091"   
    ##  [55] "SS/UYO/SB/090"    "SS/UYO/SB/095"    "SS/UYO/SB/085"   
    ##  [58] "SS/UYO/SB/089"    "SS/UYO/SB/094"    "SS/UYO/SB/086"   
    ##  [61] "SS/UYO/SB/088"    "SS/UYO/SB/096"    "SS/UYO/SB/092"   
    ##  [64] "SS/UYO/SB/028"    "SS/UYO/SB/071"    "SS/UYO/SB/91"    
    ##  [67] "SS/UYO/SB/079"    "SS/UYO/SB/097"    "SS/UYO/SB/098"   
    ##  [70] "SS/MAY/SB/022"    "SS/UYO/SB/070"    "SS/UYO/SB/002"   
    ##  [73] "SS/UYO/SB/074"    "SS/UYO/SB/003"    "SS/UYO/SB/011"   
    ##  [76] "SS/UYO/SB/036"    "SS/UYO/SB/010"    "SS/UYO/SB/015"   
    ##  [79] "SS/UYO/SB/001"    "SS/UYO/SB/013"    "SS/UYO/SB/030"   
    ##  [82] "SS/UYO/SB/031"    "SS/UYO/SB/052"    "SS/UYO/SB/017"   
    ##  [85] "SS/UYO/SB/076"    "SS/UYO/SB/082"    "SS/UYO/SB/063"   
    ##  [88] "SS/UYO/SB/048"    "SS/UYO/SB/020"    "SS/UYO/SB/021"   
    ##  [91] "SS/UYO/SB/018"    "SS/UYO/SB/045"    "SS/UYO/SB/049"   
    ##  [94] "SS/MAY/SB/088"    "SS/MAY/SB/060"    "SS/KAN/CO/140"   
    ##  [97] "SS/KAN/CO/085"    "SS/KAN/CO/076"    "SS/UYO/SB/99"    
    ## [100] "SS/UYO/SB/004"    "SS/UYO/SB/014"    "SS/UYO/SB/038"   
    ## [103] "SS/UYO/SB/006"    "SS/MAY/SB/085"    "SS/KUR/SG/100"   
    ## [106] "SS/KUR/SG/077"    "SS/KUR/SG/069"    "SS/KUR/SG/0077"  
    ## [109] "SS/KUR/SG/060"    "SS/KUR/SG/018"    "SS/KUR/SG/010"   
    ## [112] "SS/KUR/SG/016"    "SS/KUR/SG/072"    "SS/KUR/SG/071"   
    ## [115] "SS/KUR/SG/068"    "SS/KUR/SG/064"    "SS/KUR/SG/073"   
    ## [118] "SS/KUR/SG/061"    "SS/KUR/SG/011"    "SS/KUR/SG/062"   
    ## [121] "SS/KAN/CO/135"    "SS/KAN/CO/031"    "SS/KAN/CO/035"   
    ## [124] "SS/KAN/CO/030"    "SS/KAN/CO/113"    "SS/KAN/CO/036"   
    ## [127] "SS/KAN/CO/088"    "SS/KAN/CO/026"    "SS/KAN/CO/117"   
    ## [130] "SS/KAN/CO/023"    "SS/KAN/CO/020"    "SS/KAN/CO/024"   
    ## [133] "SS/KAN/CO/133"    "SS/KAN/CO/078"    "SS/KAN/CO/075"   
    ## [136] "SS/KAN/CO/072"    "SS/KAN/CO/077"    "SS/KAN/CO/051"   
    ## [139] "SS/MAY/SB/026"    "SS/KAN/CO/012"    "SS/KAN/CO/013"   
    ## [142] "SS/KAN/CO/014"    "SS/KAN/CO/015"    "SS/TAK/CO/145"   
    ## [145] "SS/TAK/CO/152"    "SS/TAK/CO/146"    "SS/TAK/CO/147"   
    ## [148] "SS/TAK/CO/151"    "SS/TAK/CO/154"    "SS/TAK/CO/165"   
    ## [151] "SS/TAK/CO/160"    "SS/KAN/CO/044"    "SS/KAN/CO/038"   
    ## [154] "SS/KAN/CO/043"    "SS/KAN/CO/042"    "SS/KAN/CO/041"   
    ## [157] "SS/KAN/CO/064"    "SS/KAN/CO/O41"    "SS/KAN/CO/034"   
    ## [160] "SS/KAN/CO/037"    "SS/KUR/SG/097"    "SS/KUR/SG/037"   
    ## [163] "SS/KUR/SG/038"    "SS/KUR/SG/034"    "SS/KUR/SG/050"   
    ## [166] "SS/KUR/SG/40"     "SS/KUR/SG/035"    "SS/KUR/SG/098"   
    ## [169] "SS/KUR/SG/033"    "SS/KUR/SG/041"    "SS/KUR/SG/046"   
    ## [172] "SS/KUR/SG/036"    "SS/TAK/CO/183"    "SS/TAK/CO/173"   
    ## [175] "SS/TAK/CO/172"    "SS/TAK/CO/168"    "SS/TAK/CO/167"   
    ## [178] "SS/TAK/CO/191"    "SS/TAK/CO/182"    "SS/TAK/CO/170"   
    ## [181] "SS/TAK/CO/171"    "SS/TAK/CO/164"    "SS/TAK/CO/077"   
    ## [184] "SS/MAY/SB/065"    "SS/MAY/SB/009"    "SS/MAY/SB/025"   
    ## [187] "SS/MAY/SB/023"    "SS/MAY/SB/027"    "SS/MAY/SB/O72"   
    ## [190] "SS/MAY/SB/019/FF" "SS/MAY/SB/045/FF" "SS/MAY/SB/001/FF"
    ## [193] "SS/MAY/SB/018/FF" "SS/MAY/SB/022/FF" "SS/MAY/SB/010/FF"
    ## [196] "SS/MAY/SB/072/FF" "SS/MAY/SB/074/FF" "SS/MAY/SB/011/FF"
    ## [199] "SS/MAY/SB/003/FF" "SS/MAY/SB/031/FF" "SS/MAY/SB/049/FF"
    ## [202] "SS/MAY/SB/077/FF" "SS/MAY/SB/013/FF" "SS/MAY/SB/006/FF"
    ## [205] "SS/MAY/SB/004/FF" "SS/MAY/SB/034/FF" "SS/MAY/SB/033/FF"
    ## [208] "SS/KUR/SG/099"    "SS/KUR/SG/082"    "SS/KUR/SG/054"   
    ## [211] "SS/KUR/SG/043"    "SS/KUR/SG/021"    "SS/KUR/SG/090"   
    ## [214] "SS/KUR/SG/029"    "SS/KUR/SG/020"    "SS/KUR/SG/030"   
    ## [217] "SS/KUR/SG/022"    "SS/KUR/SG/081"    "SS/KUR/SG/086"   
    ## [220] "SS/KUR/SG/065"    "SS/TAK/CO/176"    "SS/TAK/CO/174"   
    ## [223] "SS/TAK/CO/144"    "SS/TAK/CO/142"    "SS/TAK/CO/195"   
    ## [226] "SS/TAK/CO/198"    "SS/TAK/CO/179"    "SS/TAK/CO/194"   
    ## [229] "SS/TAK/CO/175"    "SS/TAK/CO/197"    "SS/TAK/CO/178"   
    ## [232] "SS/TAK/CO/180"    "SS/KAN/CO/136"    "SS/KAN/CO/084"   
    ## [235] "SS/KAN/CO/083"    "SS/KAN/CO/018"    "SS/KAN/CO/017"   
    ## [238] "SS/KAN/CO/086"    "SS/KAN/CO/082"    "SS/KAN/CO/016"   
    ## [241] "SS/TAK/CO/150"    "SS/TAK/CO/161"    "SS/TAK/CO/192"   
    ## [244] "SS/TAK/CO/205"    "SS/TAK/CO/166"    "SS/KAN/CO/073"   
    ## [247] "SS/KAN/CO/102"    "SS/UYO/SB/039"    "SS/UYO/SB/093"   
    ## [250] "SS/UYO/SB/029"    "SS/KAN/CO/106"    "SS/KAN/CO/127"   
    ## [253] "SS/TAK/CO/177"    "SS/TAK/CO/102"    "SS/TAK/CO/072"   
    ## [256] "SS/TAK/CO/071"    NA

### <a name="trap"></a> **Trap information**

### trap\_type

The only issue I can detect here is some lower case vs upper case.

``` r
df$trap_type <- toupper(df$trap_type)
unique(sort(df$trap_type))
```

    ##  [1] "BUNDUKI"        "MKANO"          "MODIFIED"       "MSHIPI"        
    ##  [5] "NETI YA MKANO"  "NYAVU"          "NYAVU UZI"      "NYAVU YA MKANO"
    ##  [9] "NYAVU YA UZI"   "SPEAR"          "UNMODIFIED"     "WAYAA"

### total\_traps\_collected

View the values put in the df here to double check all values make
sense.

``` r
total_traps_collected <- df %>% select(total_traps_collected) %>% na.omit()
range(total_traps_collected)
```

    ## [1]  2 24

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
take_home_weight_kg <- df %>% select(take_home_weight_kg) %>% na.omit()
total_value_KES <- df %>% select(total_value_KES) %>% na.omit()
take_home_value_KES <- df %>% select(take_home_value_KES) %>% na.omit()

## Step #4 from protocol: Double check the below values are in the correct ranges
range(total_weight_kg)
```

    ## [1]  0.2 78.0

``` r
range(take_home_weight_kg)
```

    ## [1] 0 5

``` r
range(total_value_KES)
```

    ## [1]    40 21200

``` r
range(take_home_value_KES)
```

    ## [1]   0 900

``` r
hist(df$total_weight_kg)
```

![](QC_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
hist(df$take_home_weight_kg)
```

![](QC_files/figure-gfm/unnamed-chunk-12-2.png)<!-- -->

``` r
hist(df$total_value_KES)
```

![](QC_files/figure-gfm/unnamed-chunk-12-3.png)<!-- -->

``` r
hist(df$take_home_value_KES)
```

![](QC_files/figure-gfm/unnamed-chunk-12-4.png)<!-- -->

### No. of fishers in crew

Crews above 5 people are unrealistic. I’m changing that data to ‘NA’ for
now.

``` r
fishermen_no <- df %>% select(`No. of fishers in crew`) %>% na.omit()

## Protocol: Double check the below values are in the correct ranges
range(fishermen_no)
```

    ## [1]  1 18

``` r
unique(sort(fishermen_no$`No. of fishers in crew`))
```

    ## [1]  1.0  2.0  2.2  3.0  4.0 14.0 16.0 18.0

``` r
# 
# df %>% filter(`No. of fishers in crew` > 5)
# 
# df <- df %>% 
#  mutate(crew_size = case_when(
#     `No. of fishers in crew` > 5 ~ "NA"))

df$crew_size_corrected <- df$`No. of fishers in crew`

# replacing values higher than 5 with NA
df <- df %>%
 replace_with_na_at(
    .vars = 'crew_size_corrected',
    condition = ~(.x > 6))
  
# double checking that the above worked
unique(sort(df$crew_size_corrected))
```

    ## [1] 1.0 2.0 2.2 3.0 4.0

### Kiswahili\_name

``` r
df$Kiswahili_name <- toupper(df$Kiswahili_name)
unique(sort(df$Kiswahili_name))
```

    ##   [1] "BIRINZI"                      "BUA"                         
    ##   [3] "BUNDU"                        "BUNJU"                       
    ##   [5] "CHAA"                         "CHALE"                       
    ##   [7] "CHANGO"                       "CHANGU"                      
    ##   [9] "CHANGU GAMWE"                 "CHANGU MACHO"                
    ##  [11] "CHANGU NDOMO"                 "CHEMBEU"                     
    ##  [13] "CHENA"                        "DOME"                        
    ##  [15] "DZAME"                        "FUTE"                        
    ##  [17] "FUTE MLEA"                    "FUTE MRABA"                  
    ##  [19] "GADA"                         "GENDA"                       
    ##  [21] "GENGA"                        "GONA"                        
    ##  [23] "GONA SHARIF"                  "GONA/KIKANDE"                
    ##  [25] "JAME"                         "JODARI"                      
    ##  [27] "KADA"                         "KAKURUWENDE"                 
    ##  [29] "KAMBA KOLOLO"                 "KANG'AA"                     
    ##  [31] "KANG'AJA"                     "KANG`AJA"                    
    ##  [33] "KANGA'AJA"                    "KANGAJA"                     
    ##  [35] "KANGAJA HEWANI"               "KATATANGE"                   
    ##  [37] "KHADA"                        "KHAELA"                      
    ##  [39] "KIBOMA"                       "KIFUDI"                      
    ##  [41] "KIFUDO"                       "KIFUDU"                      
    ##  [43] "KIFUVU"                       "KIJAME"                      
    ##  [45] "KIKANDE"                      "KINANZUA"                    
    ##  [47] "KINGOE"                       "KITAME"                      
    ##  [49] "KITATANGE"                    "KIUMBA"                      
    ##  [51] "KIUNGA"                       "KOLEKOLE"                    
    ##  [53] "KORIS"                        "KUFI"                        
    ##  [55] "KUFI SAFARI"                  "KUFII"                       
    ##  [57] "KUFU"                         "KUMBA"                       
    ##  [59] "KUMBI"                        "KUNGA"                       
    ##  [61] "KUNGU"                        "MAGENDA"                     
    ##  [63] "MAPANGA"                      "MATONZI"                     
    ##  [65] "MBININI"                      "MBORO YA MVUVI"              
    ##  [67] "MCHAKUFA"                     "MGENDA"                      
    ##  [69] "MKIZI"                        "MKORE"                       
    ##  [71] "MKUDAJI"                      "MKUNDAJI"                    
    ##  [73] "MKUNDAJI CHUI"                "MKUNGA"                      
    ##  [75] "MKUNGA (GYMNOTHORAX GRISEUS)" "MKUNGA CHUI"                 
    ##  [77] "MKUNGA IBRAHIM"               "MKUNGA SAMAKI"               
    ##  [79] "MKUNGU IBRAHIM"               "MKUNGUNGA"                   
    ##  [81] "MLEA"                         "MNGENDA"                     
    ##  [83] "MTONZI"                       "MTUMBUU"                     
    ##  [85] "MTUNZI"                       "MUGUNDA"                     
    ##  [87] "MULEA"                        "NG'OMBE BAHARI"              
    ##  [89] "NGINDO"                       "NGISI"                       
    ##  [91] "NGOMBE BAHARI"                "NGURU"                       
    ##  [93] "NJANA"                        "NUMBA"                       
    ##  [95] "NYENGA"                       "NYINDO"                      
    ##  [97] "PANDU"                        "PANGA"                       
    ##  [99] "PARATI"                       "PIYU"                        
    ## [101] "POMO KADIFU"                  "PONI KADIFU"                 
    ## [103] "PONO"                         "PONO (ADULT MALE)"           
    ## [105] "PONO (ADULT)"                 "PONO (JUVENILE)"             
    ## [107] "PONO BLUE FISH"               "PONO DASI"                   
    ## [109] "PONO KABANGI"                 "PONO KADIFU"                 
    ## [111] "PONO KASIKI"                  "PONO MWANI"                  
    ## [113] "PONO MWAWI"                   "PONO SITEFUE"                
    ## [115] "PONO SUNGURA"                 "PONOMWANI"                   
    ## [117] "PONP"                         "PUJU"                        
    ## [119] "PUJU JEMBE"                   "PUJU PEMBE"                  
    ## [121] "PUNO"                         "PUTU"                        
    ## [123] "PWEZA"                        "RIBOMA"                      
    ## [125] "SANDENGOMANI"                 "SENDENGOMANI"                
    ## [127] "SHANA"                        "SHARIF"                      
    ## [129] "SHARIFU"                      "SIMU"                        
    ## [131] "SITEFUE"                      "STEFUE"                      
    ## [133] "SUNGRA"                       "TAA"                         
    ## [135] "TAA YEDA"                     "TAFI"                        
    ## [137] "TAFI KITOMBO"                 "TAFI KITUMBO"                
    ## [139] "TAFI MAENGA"                  "TAFI MAENGE"                 
    ## [141] "TEMBO"                        "TEWA"                        
    ## [143] "TEWA  THARAFA"                "TEWA (MALABARICUS)"          
    ## [145] "TEWA JESHI"                   "TEWA KADA"                   
    ## [147] "TEWA KOPWE"                   "TEWA MOSHI"                  
    ## [149] "TEWA TARAFA"                  "TEWA THARAFA"                
    ## [151] "TEWA WIMBI"                   "TEWAJESHI"                   
    ## [153] "TOA"                          "TOGOO"                       
    ## [155] "TUGUU"                        "TUKUANA"                     
    ## [157] "TUNDU"                        "TUTE"                        
    ## [159] "UKURUKWENDE"                  "VEMBEU"                      
    ## [161] "VUMBAMA"                      "VUMBANA"                     
    ## [163] "VYEMBEU"                      "WAYO"

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
df$scientific_name <- gsub("Acanthrus", "Acanthurus", df$scientific_name)
df$scientific_name <- gsub("Acantrus", "Acanthurus", df$scientific_name)
df$scientific_name <- gsub("Acathurus", "Acanthurus", df$scientific_name)
df$scientific_name <- gsub("Acronthurus", "Acanthurus", df$scientific_name)
df$scientific_name <- gsub("Abdefduf", "Abudefduf", df$scientific_name)
df$scientific_name <- gsub("Cheilo", "Cheilio", df$scientific_name)
df$scientific_name <- gsub("inemis", "inermis", df$scientific_name)
df$scientific_name <- gsub("argentmaculatus", "argentimaculatus", df$scientific_name)
df$scientific_name <- gsub("Cheillinus", "Cheilinus", df$scientific_name)
df$scientific_name <- gsub("candiculatus", "canaliculutus", df$scientific_name)
df$scientific_name <- gsub("canaliculatus", "canaliculutus", df$scientific_name)
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
df$scientific_name <- gsub("brachycentron", "brachycention", df$scientific_name)
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

unique(sort(unvalidated_names$scientific_name))
```

    ##  [1] "Acanthopagrus berda"         "Acanthurus blochii"         
    ##  [3] "Acanthurus mata"             "Aethaloperca rogaa"         
    ##  [5] "Auxis thazard"               "Caranx caninus"             
    ##  [7] "Caranx heberi"               "Caranx hippos"              
    ##  [9] "Cyrnecranius randoculis"     "Epinephelus faveatus"       
    ## [11] "Epinephelus malabaricus"     "Epinephelus melanostigma"   
    ## [13] "Epinephelus spilotoceps"     "Fistularia petimba"         
    ## [15] "Gymnothorax flavimarginatus" "Gymnothorax monochrous"     
    ## [17] "Himantura gerrardi"          "Kyphosus bigibbus"          
    ## [19] "Lethrinus  harak"            "Lethrinus guttatus"         
    ## [21] "Lethrinus vaigiensis"        "Monotaxis grandoculis"      
    ## [23] "Mugil cephalus"              "Mugil cephalus"             
    ## [25] "Mulloidichthys pfluegeri"    "Myripristis formosa"        
    ## [27] "Novaculichthys nitaeniourus" "Ostracion nasus"            
    ## [29] "Panulirus versicolor"        "Parupeneus"                 
    ## [31] "Planiliza alata"             "Platybelone platyura"       
    ## [33] "Plectorhinchus playfairi"    "Plotosus canius"            
    ## [35] "Pomatomus semicirculatus"    "Pseudorhombus arsius"       
    ## [37] "Rhineacanthus aculeatus"     "Sardinella melanura"        
    ## [39] "Scarus guttatus"             "Scomerommorus commerson"    
    ## [41] "Sepia pharaonis"             "Siganus fuscescens"         
    ## [43] "Siganus guttatus"            "Siganus luridus"            
    ## [45] "Sphyraena"                   "Sphyraena leiura"           
    ## [47] "Taeniura meyeni"             "Thunnus albacares"          
    ## [49] "Thysanophrys chiltonae"      "Upeneus japonicus"          
    ## [51] "Uroteuthis duvaucelii"

1.) In catch composition and in fishbase but not on validation list.
**Suggested fix: address if these are reasonable to find in Ghana and if
they are, keep these entries.**

-   Acanthopagrus berda  
-   Acanthurus blochii  
-   Acanthurus mata  
-   Aethaloperca rogaa  
-   Auxis thazard  
-   Caranx caninus  
-   Caranx heberi  
-   Caranx hippos  
-   Epinephelus faveatus  
-   Epinephelus malabaricus  
-   Epinephelus melanostigma  
-   Epinephelus spilotoceps  
-   Fistularia petimba  
-   Gymnothorax flavimarginatus  
-   Gymnothorax monochrous  
-   Himantura gerrardi  
-   Kyphosus bigibbus  
-   Monotaxis grandoculis  
-   Mugil cephalus  
-   Mulloidichthys pfluegeri  
-   Myripristis formosa  
-   Planiliza alata
-   Platybelone platyura
-   Plectorhinchus playfairi  
-   Plotasus canius
-   Pseudorhombus arsius  
-   Rhineacanthus aculeatus  
-   Sardinella melanura  
-   Scomerommorus commerson  
-   Siganus fuscescens  
-   Siganus guttatus  
-   Siganus luridus  
-   Thunnus albacares  
-   Thysanophrys chiltonae  
-   Upeneus japonicus

2.) In catch composition but not in validation list or on fishbase (not
close to a name we have so unsure what it is supposed to be).
**Suggested fix: if there is not a clear answer to what these are
supposed to be, filter them out.**

-   Cyrnecranius randoculis  
-   Lethrinus guttatus  
-   Lethrinus vaigiensis  
-   Navaculichthys nitaeniourous  
-   Ostracion nasus  
-   Panulirus versicolor  
-   Pomatomus semicirculatus  
-   Scarus guttatus  
-   Scarus sirubroviolaceus  
-   Sepia pharaonis  
-   Sphyraena leiura  
-   Taeniura meyeni (could be one of two: Taeniura sp. or Taeniurops
    meyeni)
-   Uroteuthis duvaucelii

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
## viewing those that have two observations in one row (should have been 2 rows)
df %>% filter(length_cm == "16-20   ,46-50" | length_cm == "26-30,21- 25")
```

    ## # A tibble: 3 × 29
    ##   Operation_date      enumerator landing_site BMU     fisher_id     fisher_phone
    ##   <dttm>              <chr>      <chr>        <chr>   <chr>         <chr>       
    ## 1 2021-06-27 00:00:00 KADZO BAYA MAYUNGU      MAYUNGU SS/UYO/SB/07… 0755223621  
    ## 2 2021-06-27 00:00:00 KADZO BAYA MAYUNGU      MAYUNGU SS/UYO/SB/07… 0755223621  
    ## 3 2021-06-14 00:00:00 <NA>       <NA>         <NA>    SS/UYO/SB/09… <NA>        
    ## # … with 23 more variables: household_id <chr>, trap_type <chr>,
    ## #   total_traps_collected <dbl>, date_set_dd_mm_yyyy <dttm>,
    ## #   `time_set_24hh:mm` <chr>, date_collected_dd_mm_yyyy <dttm>,
    ## #   `time_collected_24hh:mm` <chr>, total_weight_kg <dbl>,
    ## #   take_home_weight_kg <dbl>, total_value_KES <dbl>,
    ## #   take_home_value_KES <dbl>, `No. of fishers in crew` <dbl>,
    ## #   fishing_operation_notes <lgl>, Kiswahili_name <chr>, …

``` r
## viewing what observation have a really high value  
## many operations by CLAPERTON KAZUNGU include a length value of 44880
df %>% filter(length_cm == "NA")
```

    ## # A tibble: 0 × 29
    ## # … with 29 variables: Operation_date <dttm>, enumerator <chr>,
    ## #   landing_site <chr>, BMU <chr>, fisher_id <chr>, fisher_phone <chr>,
    ## #   household_id <chr>, trap_type <chr>, total_traps_collected <dbl>,
    ## #   date_set_dd_mm_yyyy <dttm>, time_set_24hh:mm <chr>,
    ## #   date_collected_dd_mm_yyyy <dttm>, time_collected_24hh:mm <chr>,
    ## #   total_weight_kg <dbl>, take_home_weight_kg <dbl>, total_value_KES <dbl>,
    ## #   take_home_value_KES <dbl>, No. of fishers in crew <dbl>, …

``` r
# converting length values to NA that don't make sense
df$length_cm <- gsub("44880", "NA", df$length_cm)
df$length_cm <- gsub("16-20   ,46-50", "NA", df$length_cm)
df$length_cm <- gsub("26-30,21- 25", "NA", df$length_cm)

# fixing the < and > mix-up 
df$length_cm <- gsub("<75", ">75", df$length_cm)

# converting <10 to a range of 0-10 
df <- df %>% 
  mutate(length_cm = if_else(length_cm == "<10", "0-10", length_cm))

# converting ranges that don't make sense 
df$length_cm <- gsub("31-15", "31-35", df$length_cm)
df$length_cm <- gsub("10-15", "11-15", df$length_cm)
df$length_cm <- gsub("31-37", "31-35", df$length_cm)
df$length_cm <- gsub("31-36", "31-35", df$length_cm)
df$length_cm <- gsub("16-25", "16-20", df$length_cm)
df$length_cm <- gsub("21-30", "21-25", df$length_cm)
df$length_cm <- gsub("26-35", "26-30", df$length_cm)

# converting numerical values to ranges 
df <- df %>% 
 mutate(length_corrected = case_when(
    length_cm >= 0 & length_cm <= 10.5 ~ "0-10",
    length_cm >= 10.5 & length_cm <= 15.4 ~ "11-15",
    length_cm >= 15.5 & length_cm <= 20.4 ~ "16-20",
    length_cm >= 20.5 & length_cm <= 25.4 ~ "21-25",
    length_cm >= 25.5 & length_cm <= 30.4 ~ "26-30",
    length_cm >= 30.5 & length_cm <= 35.4 ~ "31-35",
    length_cm >= 35.5 & length_cm <= 40.4 ~ "36-40",
    length_cm >= 40.5 & length_cm <= 45.4 ~ "41-45",
    length_cm >= 45.5 & length_cm <= 50.4 ~ "46-50",
    length_cm >= 50.5 & length_cm <= 75 ~ ">50",
    length_cm > 75 ~ ">75"))

# double checking that worked for the corrected column 
unique(sort(df$length_cm))
```

    ##  [1] ">50"   ">75"   "0-10"  "101"   "105"   "109"   "11-15" "110"   "111"  
    ## [10] "112"   "115"   "117"   "119"   "122"   "123"   "125"   "126"   "129"  
    ## [19] "16-20" "167"   "168"   "170"   "198"   "21-25" "26-30" "31-35" "36-40"
    ## [28] "41-45" "42.5"  "46-50" "51"    "52"    "53"    "54.25" "56"    "57"   
    ## [37] "58"    "58.2"  "59"    "60"    "61"    "62"    "63"    "63.2"  "64"   
    ## [46] "65.5"  "65.7"  "66"    "67"    "67.8"  "68"    "68.3"  "68.5"  "69.1" 
    ## [55] "69.7"  "70"    "70.2"  "70.4"  "70.6"  "71"    "72.1"  "72.5"  "72.7" 
    ## [64] "73"    "74"    "74.1"  "74.4"  "75.5"  "76"    "77"    "78.3"  "78.5" 
    ## [73] "80"    "81"    "82"    "86"    "87"    "90"    "92"    "93"    "96"   
    ## [82] "98"    "99"    "NA"

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
df$`gear type` <- gsub("UNMODIFIED TRAP", "UNMODIFIED", df$`gear type`)
df$`gear type` <- gsub("MODIFIED TRAP", "MODIFIED", df$`gear type`)

unique(sort(df$`gear type`))
```

    ## [1] "GILLNET"                 "HANDLINE"               
    ## [3] "MODIFIED"                "MONOFILAMENT"           
    ## [5] "MONOFILAMENT/HOOK&STICK" "SPEAR"                  
    ## [7] "SPEARGUN"                "UNMODIFIED"

### Number of fish

Doube check this range is what is expected.

``` r
unique(sort(df$number_of_fish))
```

    ##  [1]   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19
    ## [20]  20  21  22  23  24  25  26  27  28  29  30  32  33  34  35  36  37  38  39
    ## [39]  40  42  43  44  46  47  48  50  51  53  57  58  59  60  68  70 120 140 216
    ## [58] 310 351 480

``` r
df %>% ggplot(., aes(y=number_of_fish)) + geom_boxplot() + theme_bw()
```

    ## Warning: Removed 206 rows containing non-finite values (stat_boxplot).

![](QC_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

### Destination of fish

``` r
df$destination <- toupper(df$destination)

df <- df %>%
  filter(!destination == "OTHER WRITE IN:")

df$destination <- gsub("OTHER WRITE IN:", "", df$destination)

unique(sort(df$destination))
```

    ## [1] "FISH DEALER"    "GIFT"           "HOME"           "LOCAL CONSUMER"
    ## [5] "MAMA KARANGA"

## <a name="notes"></a> **Final check: any notes from both datasets**

``` r
## check for any notes that the data collectors left for analysis 
unique(df$fishing_operation_notes)
```

    ## [1] NA

``` r
unique(df$catch_composition_notes)
```

    ## [1] NA

## <a name="export"></a> **Exporting cleaned dataset**

``` r
head(df)
```

    ## # A tibble: 6 × 30
    ##   Operation_date      enumerator       landing_site BMU   fisher_id fisher_phone
    ##   <dttm>              <chr>            <chr>        <chr> <chr>     <chr>       
    ## 1 2021-12-02 00:00:00 CELESTINE N. ALI MAYUNGU      MAYU… SS/MAY/S… <NA>        
    ## 2 2021-12-02 00:00:00 CELESTINE N. ALI MAYUNGU      MAYU… SS/MAY/S… <NA>        
    ## 3 2021-12-02 00:00:00 CELESTINE N. ALI MAYUNGU      MAYU… SS/MAY/S… <NA>        
    ## 4 2021-12-02 00:00:00 CELESTINE N. ALI MAYUNGU      MAYU… SS/MAY/S… <NA>        
    ## 5 2021-12-02 00:00:00 CELESTINE N. ALI MAYUNGU      MAYU… SS/MAY/S… <NA>        
    ## 6 2021-12-02 00:00:00 CELESTINE N. ALI MAYUNGU      MAYU… SS/MAY/S… <NA>        
    ## # … with 24 more variables: household_id <chr>, trap_type <chr>,
    ## #   total_traps_collected <dbl>, date_set_dd_mm_yyyy <dttm>,
    ## #   `time_set_24hh:mm` <chr>, date_collected_dd_mm_yyyy <dttm>,
    ## #   `time_collected_24hh:mm` <chr>, total_weight_kg <dbl>,
    ## #   take_home_weight_kg <dbl>, total_value_KES <dbl>,
    ## #   take_home_value_KES <dbl>, `No. of fishers in crew` <dbl>,
    ## #   fishing_operation_notes <lgl>, Kiswahili_name <chr>, …

``` r
nrow(df)
```

    ## [1] 26180

``` r
write_xlsx(df, "data/Fishlandings-cleaned-21052022-May.xlsx")
```
