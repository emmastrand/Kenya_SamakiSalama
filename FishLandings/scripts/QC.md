Quality Control of Fishing Landings dataset
================
Emma Strand; <emma_strand@uri.edu>

## Questions for Austin

1.  Is there a set list of options for categories like enumerator,
    landing\_site, BMU, and trap\_type that I can compare the dataset
    against? e.g. are “KIJANGWANI” and “KITANGANI” the same landing site
    or 2 different ones? A validation list for the fishing operation
    sheet? I have the validation list for the catch composition.  
2.  Are phone numbers a certain \# of digits?  
3.  Time in water (effort) doesn’t have any data so I removed it for
    now. Is this supposed to?  
4.  What are the expected ranges for total traps collected, weight and
    value measures, and no of fishers in crew?

## Protocol to run this with a future xlsx file

1.  In the toolbar above, hit the arrow next to `Knit`. Scroll down to
    `Knit Directory` and select the option `Project Directory`.  
2.  **In `Create dataframe` code chunk**: Replace raw data file name in
    the function that creates the following variables:
    fishing\_operation, catch\_composition, and validation\_lists.  
3.  **In `Enumerator`, `Landing_site`, `BMU`, and `trap_type` code
    chunks**: Run the unique() function and double check that this list
    is the correct names.  
4.  **In `total_traps_collected`, `Weight and value measures`, and
    `No. of fishers in crew` code chunks**: Run range() functions on new
    dfs to double check these ranges are expected.  
5.  

## Load all libraries

``` r
library(plyr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(readxl)
library(lubridate)
library(Hmisc)
```

## Create dataframe

**Load in raw data files.**

[Reading in an excel datafile instead of a
csv](http://www.sthda.com/english/wiki/reading-data-from-excel-files-xls-xlsx-into-r).
This requires the `readxl` package listed in load all libraries step.

``` r
## when running future iterations of raw data file, replace the file name below 
fishing_operation <- read_excel("data/Fishlandings-data_21052022-May_update-cleaning in progress.xlsx", sheet = "fishing_operation") %>%
  select(-'...21', -'...22', -'...23') %>%
  rename(Operation_date = date_dd_mm_yyyy)

nrow(fishing_operation) #2407 fishing operations (keep this # in mind for sanity check at the end)
```

    ## [1] 2407

``` r
## when running future iterations of raw data file, replace the file name below 
catch_composition <- read_excel("data/Fishlandings-data_21052022-May_update-cleaning in progress.xlsx", sheet = "catch_composition") %>%
  select(-'...10', -'...11', -'...12', -'...13') %>%
  rename(Operation_date = Date)

nrow(catch_composition) #14761 fish observations (keep this # in mind for sanity check at the end)
```

    ## [1] 14761

``` r
## when running future iterations of raw data file, replace the file name below 
validation_lists <- read_excel("data/Fishlandings-data_21052022-May_update-cleaning in progress.xlsx", sheet = "validation_lists")
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

### Enumerator

[Replace character string with another in
R](https://stackoverflow.com/questions/11936339/replace-specific-characters-within-strings)

A lot of errors occur with different lower and upper case iterations of
names. I replaced this information with all upper case to collapse this
information. For example, “kadzo baya” and “Kadzo baya” are the same
fisherman but were reading as different categories.

``` r
# change all lower case to upper case
df$enumerator <- toupper(df$enumerator)

unique(df$enumerator) # prints all categories that show up in that column 
```

    ##  [1] "CELESTINAR N ALI"  "KADZO BAYA"        "CLAPERTON KAZUNGU"
    ##  [4] "EDWARD YAA"        "CELESTINA NALI"    "BIDALA RASHID"    
    ##  [7] "KADZO KAZUNGU"     "GILBERT NZAI"      "MACKSON KAZUNGU"  
    ## [10] "BRUNO MUYE"        "FRANKLINE KAZUNGU" "ALI"              
    ## [13] "BASHIR SAID"       "GARAMA YERI"       "KITSAO KARISA"    
    ## [16] "KARIMA NYINGE"     "NGALA"             "CLAPERTON"        
    ## [19] "OMAR ALI"          NA

``` r
## at this point, assess errors in that list for spelling mistakes to replace below 
```

``` r
# replace incorrect spellings 
df$enumerator <- gsub("CELESTINAR N ALI", "CELESTINA NALI", df$enumerator)
df$enumerator <- gsub("CLAPERTON", "CLAPERTON KAZUNGU", df$enumerator)
df$enumerator <- gsub("CLAPERTON KAZUNGU KAZUNGU", "CLAPERTON KAZUNGU", df$enumerator)

## Step #2 in protocol at the top of this script 
unique(df$enumerator) # at this point, double check that this list are all individual fishermen 
```

    ##  [1] "CELESTINA NALI"    "KADZO BAYA"        "CLAPERTON KAZUNGU"
    ##  [4] "EDWARD YAA"        "BIDALA RASHID"     "KADZO KAZUNGU"    
    ##  [7] "GILBERT NZAI"      "MACKSON KAZUNGU"   "BRUNO MUYE"       
    ## [10] "FRANKLINE KAZUNGU" "ALI"               "BASHIR SAID"      
    ## [13] "GARAMA YERI"       "KITSAO KARISA"     "KARIMA NYINGE"    
    ## [16] "NGALA"             "OMAR ALI"          NA

### Landing\_site

``` r
df$landing_site <- toupper(df$landing_site)
unique(df$landing_site)
```

    ##  [1] "MAYUNGU"         "UYOMBO"          "MAWE YA KATI"    "KANAMAI"        
    ##  [5] "KIJANGWANI"      "KURUWITU"        "KIRKLAND"        "NGOLOKO"        
    ##  [9] "KITANGANI"       "KIVUKONI"        "CHAUREMBO"       "SUN N SAND"     
    ## [13] "MWANAMIA"        "VUMA"            "KIVULINI"        "BURENI"         
    ## [17] "MWENDO WA PANYA" NA

``` r
## Step #3 in protocol to double check this output list is all the correct site names 
```

### BMU

``` r
df$BMU <- toupper(df$BMU)
unique(df$BMU)
```

    ## [1] "MAYUNGU"  "UYOMBO"   "KANAMAI"  "KURUWITU" "TAKAUNGU" NA

``` r
## Step #4 in protocol to double check this output list is all the correct BMU names 
```

### fisher\_phone

``` r
## circle back to checking if these are supposed to be a certain # of digits?
```

### household\_id

The only issue I can detect here is some lower case vs upper case.

``` r
df$household_id <- toupper(df$household_id)
unique(df$household_id)
```

    ##   [1] "SS/MAY/SB/047"  "SS/MAY/SB/045"  "SS/MAY/SB/048"  "SS/MAY/SB/017" 
    ##   [5] "SS/MAY/SB/083"  "SS/MAY/SB/082"  "SS/MAY/SB/028"  "SS/MAY/SB/052" 
    ##   [9] "SS/MAY/SB/029"  "SS/MAY/SB/011"  "SS/MAY/SB/074"  "SS/MAY/SB/062" 
    ##  [13] "SS/MAY/SB/078"  "SS/MAY/SB/004"  "SS/MAY/SB/003"  "SS/MAY/SB/072" 
    ##  [17] "SS/MAY/SB/006"  "SS/MAY/SB/038"  "SS/MAY/SB/034"  "SS/MAY/SB/049" 
    ##  [21] "SS/MAY/SB/081"  "SS/MAY/SB/058"  "SS/MAY/SB/063"  "SS/MAY/SB/064" 
    ##  [25] "SS/MAY/SB/036"  "SS/MAY/SB/014"  "SS/MAY/SB/015"  "SS/MAY/SB/077" 
    ##  [29] "SS/MAY/SB/030"  "SS/MAY/SB/021"  "SS/MAY/SB/018"  "SS/MAY/SB/032" 
    ##  [33] "SS/MAY/SB/010"  "SS/MAY/SB/002"  "SS/MAY/SB/001"  "SS/MAY/SB/013" 
    ##  [37] "SS/MAY/SB/035"  "SS/MAY/SB/033"  "SS/MAY/SB/012"  "SS/MAY/SB/005" 
    ##  [41] "SS/MAY/SB/007"  "SS/MAY/SB/031"  "SS/MAY/SB/076"  "SS/MAY/SB/066" 
    ##  [45] "SS/MAY/SB/067"  "SS/MAY/SB/068"  "SS/MAY/SB/019"  "SS/MAY/SB/041" 
    ##  [49] "SS/UYO/SB/100"  "SS/UYO/SB/099"  "SS/MAY/SB/042"  "SS/MAY/SB/071" 
    ##  [53] "SS/MAY/SB/070"  "SS/UYO/SB/091"  "SS/UYO/SB/090"  "SS/UYO/SB/095" 
    ##  [57] "SS/UYO/SB/085"  "SS/UYO/SB/089"  "SS/UYO/SB/094"  "SS/UYO/SB/086" 
    ##  [61] "SS/UYO/SB/088"  "SS/UYO/SB/096"  "SS/UYO/SB/092"  "SS/UYO/SB/028" 
    ##  [65] "SS/UYO/SB/071"  "SS/UYO/SB/91"   "SS/UYO/SB/079"  "SS/UYO/SB/097" 
    ##  [69] "SS/UYO/SB/098"  "SS/MAY/SB/022"  "SS/UYO/SB/070"  "SS/UYO/SB/002" 
    ##  [73] "SS/UYO/SB/074"  "SS/UYO/SB/003"  "SS/UYO/SB/011"  "SS/UYO/SB/036" 
    ##  [77] "SS/UYO/SB/010"  "SS/UYO/SB/015"  "SS/UYO/SB/001"  "SS/UYO/SB/013" 
    ##  [81] "SS/UYO/SB/030"  "SS/UYO/SB/031"  "SS/UYO/SB/052"  "SS/UYO/SB/017" 
    ##  [85] "SS/UYO/SB/076"  "SS/UYO/SB/082"  "SS/UYO/SB/063"  "SS/UYO/SB/048" 
    ##  [89] "SS/UYO/SB/020"  "SS/UYO/SB/021"  "SS/UYO/SB/018"  "SS/UYO/SB/045" 
    ##  [93] "SS/UYO/SB/049"  "SS/MAY/SB/088"  "SS/MAY/SB/060"  "SS/KAN/CO/140" 
    ##  [97] "SS/KAN/CO/085"  "SS/KAN/CO/076"  "SS/UYO/SB/99"   "SS/UYO/SB/004" 
    ## [101] "SS/UYO/SB/014"  "SS/UYO/SB/038"  "SS/UYO/SB/006"  "SS/MAY/SB/085" 
    ## [105] "SS/KUR/SG/100"  "SS/KUR/SG/077"  "SS/KUR/SG/069"  "SS/KUR/SG/0077"
    ## [109] "SS/KUR/SG/060"  "SS/KUR/SG/018"  "SS/KUR/SG/010"  "SS/KUR/SG/016" 
    ## [113] "SS/KUR/SG/072"  "SS/KUR/SG/071"  "SS/KUR/SG/068"  "SS/KUR/SG/064" 
    ## [117] "SS/KUR/SG/073"  "SS/KUR/SG/061"  "SS/KUR/SG/011"  "SS/KUR/SG/062" 
    ## [121] "SS/KAN/CO/135"  "SS/KAN/CO/031"  "SS/KAN/CO/035"  "SS/KAN/CO/030" 
    ## [125] "SS/KAN/CO/113"  "SS/KAN/CO/036"  "SS/KAN/CO/088"  "SS/KAN/CO/026" 
    ## [129] "SS/KAN/CO/117"  "SS/KAN/CO/023"  "SS/KAN/CO/020"  "SS/KAN/CO/024" 
    ## [133] "SS/KAN/CO/133"  "SS/KAN/CO/078"  "SS/KAN/CO/075"  "SS/KAN/CO/072" 
    ## [137] "SS/KAN/CO/077"  "SS/KAN/CO/051"  "SS/MAY/SB/026"  "SS/KAN/CO/012" 
    ## [141] "SS/KAN/CO/013"  "SS/KAN/CO/014"  "SS/KAN/CO/015"  "SS/TAK/CO/145" 
    ## [145] "SS/TAK/CO/152"  "SS/TAK/CO/146"  "SS/TAK/CO/147"  "SS/TAK/CO/151" 
    ## [149] "SS/TAK/CO/154"  "SS/TAK/CO/165"  "SS/TAK/CO/160"  "SS/KAN/CO/044" 
    ## [153] "SS/KAN/CO/038"  "SS/KAN/CO/043"  "SS/KAN/CO/042"  "SS/KAN/CO/041" 
    ## [157] "SS/KAN/CO/064"  "SS/KAN/CO/O41"  "SS/KAN/CO/034"  "SS/KAN/CO/037" 
    ## [161] "SS/KUR/SG/097"  "SS/KUR/SG/037"  "SS/KUR/SG/038"  "SS/KUR/SG/034" 
    ## [165] "SS/KUR/SG/050"  "SS/KUR/SG/40"   "SS/KUR/SG/035"  "SS/KUR/SG/098" 
    ## [169] "SS/KUR/SG/033"  "SS/KUR/SG/041"  "SS/KUR/SG/046"  "SS/KUR/SG/036" 
    ## [173] "SS/TAK/CO/183"  "SS/TAK/CO/173"  "SS/TAK/CO/172"  "SS/TAK/CO/168" 
    ## [177] "SS/TAK/CO/167"  "SS/TAK/CO/191"  "SS/TAK/CO/182"  "SS/TAK/CO/170" 
    ## [181] "SS/TAK/CO/171"  "SS/TAK/CO/164"  "SS/TAK/CO/077"  "SS/MAY/SB/065" 
    ## [185] "SS/MAY/SB/009"  "SS/MAY/SB/025"  "SS/MAY/SB/023"  "SS/MAY/SB/027" 
    ## [189] "SS/MAT/SB/001"  "SS/MAT/SB/045"  "SS/MAT/SB/072"  "SS/MAT/SB/068" 
    ## [193] "SS/MAT/SB/066"  "SS/MAT/SB/034"  "SS/MAT/SB/033"  "SS/MAT/SB/032" 
    ## [197] "SS/MAT/SB/006"  "SS/MAT/SB/004"  "SS/MAT/SB/031"  "SS/MAT/SB/077" 
    ## [201] "SS/MAT/SB/010"  "SS/MAT/SB/003"  "SS/MAT/SB/011"  "SS/MAT/SB/019" 
    ## [205] "SS/MAT/SB/013"  "SS/MAT/SB/014"  "SS/MAT/SB/036"  "SS/MAT/SB/074" 
    ## [209] "SS/MAT/SB/049"  "SS/MAT/SB/022"  "SS/MAT/SB/018"  "SS/KUR/SG/065" 
    ## [213] "SS/KUR/SG/054"  "SS/KUR/SG/082"  "SS/KUR/SG/099"  "SS/KUR/SG/043" 
    ## [217] "SS/KUR/SG/021"  "SS/KUR/SG/090"  "SS/KUR/SG/029"  "SS/KUR/SG/020" 
    ## [221] "SS/KUR/SG/030"  "SS/KUR/SG/022"  "SS/KUR/SG/081"  "SS/KUR/SG/086" 
    ## [225] "SS/TAK/CO/176"  "SS/TAK/CO/174"  "SS/TAK/CO/144"  "SS/TAK/CO/142" 
    ## [229] "SS/TAK/CO/195"  "SS/TAK/CO/198"  "SS/TAK/CO/179"  "SS/TAK/CO/194" 
    ## [233] "SS/TAK/CO/175"  "SS/TAK/CO/197"  "SS/TAK/CO/178"  "SS/TAK/CO/180" 
    ## [237] "SS/KAN/CO/136"  "SS/KAN/CO/084"  "SS/KAN/CO/083"  "SS/KAN/CO/018" 
    ## [241] "SS/KAN/CO/017"  "SS/KAN/CO/086"  "SS/KAN/CO/082"  "SS/KAN/CO/016" 
    ## [245] "SS/TAK/CO/150"  "SS/TAK/CO/161"  "SS/TAK/CO/192"  "SS/TAK/CO/205" 
    ## [249] "SS/TAK/CO/166"  "SS/KAN/CO/073"  "SS/KAN/CO/102"  "SS/UYO/SB/039" 
    ## [253] "SS/UYO/SB/093"  "SS/UYO/SB/029"  "SS/KAN/CO/106"  "SS/KAN/CO/127" 
    ## [257] "SS/TAK/CO/177"  "SS/TAK/CO/102"  "SS/TAK/CO/072"  "SS/TAK/CO/071" 
    ## [261] "SS/NAT/SB/019"  "SS/NAT/SB/045"  "SS/NAT/SB/001"  "SS/NAT/SB/018" 
    ## [265] "SS/NAT/SB/022"  "SS/NAT/SB/010"  "SS/NAT/SB/072"  "SS/NAT/SB/074" 
    ## [269] "SS/NAT/SB/011"  "SS/NAT/SB/003"  "SS/NAT/SB/031"  "SS/NAT/SB/049" 
    ## [273] "SS/NAT/SB/077"  "SS/NAT/SB/013"  "SS/NAT/SB/006"  "SS/NAT/SB/004" 
    ## [277] "SS/NAT/SB/034"  "SS/NAT/SB/033"  NA

### trap\_type

The only issue I can detect here is some lower case vs upper case.

``` r
df$trap_type <- toupper(df$trap_type)
unique(df$trap_type)
```

    ##  [1] "UNMODIFIED"     NA               "BUNDUKI"        "NYAVU UZI"     
    ##  [5] "MSHIPI"         "NYAVU"          "MODIFIED"       "NETI YA MKANO" 
    ##  [9] "NYAVU YA UZI"   "NYAVU YA MKANO" "SPEAR"          "WAYAA"         
    ## [13] "MKANO"

### total\_traps\_collected

View the values put in the df here to double check all values make
sense.

``` r
total_traps_collected <- df %>% select(total_traps_collected) %>% na.omit()
range(total_traps_collected)
```

    ## [1]  2 24

``` r
## Step #4 of protocol with new df: double check the below range is expected 
```

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

### No. of fishers in crew

``` r
fishermen_no <- df %>% select(`No. of fishers in crew`) %>% na.omit()

## Step #4 from protocol: Double check the below values are in the correct ranges
range(fishermen_no)
```

    ## [1]  1 18

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

### SPECIES

We can pull in the validation\_lists df to double check these spellings.

``` r
# Taking out double spaces in between genus and species 
validation_lists$scientific_name <- gsub("  ", " ", validation_lists$scientific_name)
```

``` r
unique(sort(df$scientific_name)) # view the df we are working with first; can run this function after each modification to check
```

    ##   [1] "Abdefduf sexfasciatus"         "Abudefduf sexfaciatus"        
    ##   [3] "Abudefduf sexfasciatus"        "Acanthopagrus berda"          
    ##   [5] "Acanthrus  dussumieri"         "Acanthrus dussumieri"         
    ##   [7] "Acanthurus blochii"            "acanthurus dussumieri"        
    ##   [9] "Acanthurus dussumieri"         "Acanthurus leucosternon"      
    ##  [11] "Acanthurus lineatus"           "Acanthurus mata"              
    ##  [13] "Acanthurus nigricauda"         "Acanthurus nigrofuscus"       
    ##  [15] "Acanthurus triostegus"         "Acanthurus Triostegus"        
    ##  [17] "Acanthurus xanthopterus"       "Acantrus dussumieri"          
    ##  [19] "Acathurus dussumiera"          "Acronthurus dussumieri"       
    ##  [21] "aethaloperca rogaa"            "Anampses caeruleopanctatus"   
    ##  [23] "Auxis thazard"                 "Balistapus undulatus"         
    ##  [25] "Calotomus carolinus"           "Carangoides ferdau"           
    ##  [27] "Caranx caninus"                "caranx hebei"                 
    ##  [29] "Caranx hippos"                 "Carnx kippos"                 
    ##  [31] "Cephalopholis argus"           "Cephalopholis spiloparaea"    
    ##  [33] "Chaetodon auriga"              "Cheilinus chlorourus"         
    ##  [35] "cheilinus trilobatus"          "Cheilinus trilobatus"         
    ##  [37] "Cheilinus undulatus"           "Cheilio inemis"               
    ##  [39] "Cheilio inermis"               "Cheillinus trilobatus"        
    ##  [41] "Cheilo inermis"                "Chlorurus strongylocephalus"  
    ##  [43] "Cholurururs stronycephalus"    "Cirrhitus pinnulatus"         
    ##  [45] "Coris formosa"                 "Ctenochaetus striatus"        
    ##  [47] "Cynoglossus lachneri"          "Cyrnecranius randoculis"      
    ##  [49] "Diagramma pictum"              "Diodon liturosus"             
    ##  [51] "Epinephelus  melanostigma"     "Epinephelus coioidea"         
    ##  [53] "Epinephelus coioides"          "Epinephelus fasciatus"        
    ##  [55] "epinephelus faveatus"          "Epinephelus fuscoguttatus"    
    ##  [57] "Epinephelus malabaricus"       "Epinephelus malanostigma"     
    ##  [59] "Epinephelus melanostigma"      "Epinephelus merra"            
    ##  [61] "Epinephelus sp."               "Epinephelus spilotoceps"      
    ##  [63] "Fistularia petimba"            "Gerres oyena"                 
    ##  [65] "Gomphosus caeruleus"           "Gymnothorax favagineus"       
    ##  [67] "Gymnothorax flavimarginatus"   "Gymnothorax griseus"          
    ##  [69] "gymnothorax javanicus"         "Gymnothorax javanicus"        
    ##  [71] "Gymnothorax monochrou"         "Gymnothorax monochrous"       
    ##  [73] "Gymonthorax favagineus"        "Gymonthorax monochrous"       
    ##  [75] "Halichoeres hortulanus"        "Himantura gerrardi"           
    ##  [77] "Hipposcarus hard"              "Hipposcarus harid"            
    ##  [79] "Kyphasus bigibbus"             "Kyphasus vaigiensis"          
    ##  [81] "kyphosus bigibbus"             "Kyphosus bigibbus"            
    ##  [83] "Kyphosus cinerascens"          "Kyphosus vaigiensis"          
    ##  [85] "Lenthrinus   harak"            "lenthrinus bornonicus"        
    ##  [87] "lenthrinus conchyliatus"       "Lenthrinus conchyliatus"      
    ##  [89] "lenthrinus nebulous"           "leptoscarus vaigiensis"       
    ##  [91] "Leptoscarus vaigiensis"        "Leptoscarus vaiginsis"        
    ##  [93] "Leptoscarus vaiguensis"        "Leptoscaus vaigiensis"        
    ##  [95] "Lethrinus borbonicus"          "Lethrinus conchyliatus"       
    ##  [97] "Lethrinus erythropterus"       "Lethrinus guttatus"           
    ##  [99] "lethrinus harak"               "Lethrinus harak"              
    ## [101] "Lethrinus lentjan"             "Lethrinus mahsena"            
    ## [103] "Lethrinus nebulosis"           "lethrinus nebulosus"          
    ## [105] "Lethrinus nebulosus"           "Lethrinus obsoletus"          
    ## [107] "Lethrinus vaigiensis"          "Lethrinus variegatus"         
    ## [109] "Leturinus lentjan"             "Lutjanus argentimaculatus"    
    ## [111] "Lutjanus argentmaculatus"      "Lutjanus fluluiflamma"        
    ## [113] "Lutjanus fulviflamma"          "Lutjanus gibbus"              
    ## [115] "Monodactylus argenteus"        "monotaxis grandoculis"        
    ## [117] "Monotaxis grandoculis"         "Monotaxis granoculis"         
    ## [119] "Mugil cephalus"                "Mugil cephalus"               
    ## [121] "Mulloidichthys flavlineathe"   "Mulloidichthys flavolineatus" 
    ## [123] "Mulloidichthys pfluegeri"      "Mulloidichthys vanicolensis"  
    ## [125] "Myripristis berndti"           "Myripristis formosa"          
    ## [127] "Naso annulatus"                "Naso brachycention"           
    ## [129] "Naso brachycentron"            "Naso brevirostris"            
    ## [131] "Naso hexacanthus"              "Naso sp."                     
    ## [133] "Naso unicornis"                "Navaculichthys nitaeniourous" 
    ## [135] "Novaculichthys taeniourous"    "Novaculichthys taeniourus"    
    ## [137] "octopus cyanea"                "Octopus cyanea"               
    ## [139] "ostracion nasus"               "Ostracion nasus"              
    ## [141] "Panulirus versicolor"          "Parupeneous macronema"        
    ## [143] "parupeneus"                    "Parupeneus barberinus"        
    ## [145] "Parupeneus heptacanthus"       "Parupeneus indicus"           
    ## [147] "Parupeneus macronemus"         "parupeneus sp nov."           
    ## [149] "Parupeneus sp."                "Planiliza alata"              
    ## [151] "Platax teira"                  "Platux teira"                 
    ## [153] "Platybelone platyura"          "Platybelone platyyuna"        
    ## [155] "Plectorhinchus flavomaculatus" "Plectorhinchus gaterinus"     
    ## [157] "Plectorhinchus playfair"       "Plectorhinchus sordidus"      
    ## [159] "Plectorhinchus sp."            "plectorhinchus vittatus"      
    ## [161] "Plectorhinchus vittatus"       "Plectorhincus gaterinus"      
    ## [163] "Plectorhnichus gaterinus"      "Plotasus canius"              
    ## [165] "Plotosus canius"               "plotosus lineatus"            
    ## [167] "Plotosus lineatus"             "Pomacanthus imperator"        
    ## [169] "pomacanthus semicirculatus"    "Pomacanthus semicirculatus"   
    ## [171] "Pomacnathus semisulcatus"      "Pomatomus saltatrix"          
    ## [173] "Pomatonus saltatrix"           "Pomatonus semicirculatus"     
    ## [175] "Pseudorhombus arsius"          "Pterois miles"                
    ## [177] "Rhinecanthurus aculeatus"      "Rhinecanthus aculeatus"       
    ## [179] "Sardinella melanura"           "Sargocentron violaceum"       
    ## [181] "Scarus  ghobban"               "Scarus frenatus"              
    ## [183] "scarus ghobban"                "Scarus ghobban"               
    ## [185] "Scarus globiceps"              "Scarus guttatus"              
    ## [187] "Scarus rubroviolaceus"         "Scarus sirubroviolaceus"      
    ## [189] "Scarus vubroviolaceus"         "Scromberomorus commerson"     
    ## [191] "Sepia pharaonis"               "Siganus argenteus"            
    ## [193] "Siganus canaliculatus"         "Siganus canaliculutus"        
    ## [195] "Siganus candiculatus"          "siganus fuscescens"           
    ## [197] "Siganus fuscescens"            "siganus guttatus"             
    ## [199] "Siganus guttatus"              "Siganus luridus"              
    ## [201] "Siganus sp."                   "Siganus stellatus"            
    ## [203] "siganus sutor"                 "Siganus sutor"                
    ## [205] "Siganus Sutor"                 "Sphraena"                     
    ## [207] "Sphyraena acutipinnis"         "Sphyraena leiura"             
    ## [209] "Strongylura leiura"            "Strophidon sathete"           
    ## [211] "Synodus variegatus"            "Taeniura lymma"               
    ## [213] "Taeniura meyeni"               "Taeniura meyeri"              
    ## [215] "Thunnus albacares"             "Thysanophrys chiltonae"       
    ## [217] "Trichiurus lepturus"           "Upeneus japonicus"            
    ## [219] "uroteuthis duvaucelii"         "Uroteuthis duvaucelii"        
    ## [221] "Zanclus cornutus"

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
    ## [39] "Scarus guttatus"             "Scarus sirubroviolaceus"    
    ## [41] "Scarus vubroviolaceus"       "Scromberomorus commerson"   
    ## [43] "Sepia pharaonis"             "Siganus fuscescens"         
    ## [45] "Siganus guttatus"            "Siganus luridus"            
    ## [47] "Sphraena"                    "Sphyraena leiura"           
    ## [49] "Taeniura meyeni"             "Taeniura meyeri"            
    ## [51] "Thunnus albacares"           "Thysanophrys chiltonae"     
    ## [53] "Upeneus japonicus"           "Uroteuthis duvaucelii"

``` r
## left off at #38 Sardinella melanura
```

In catch composition and in fishbase but not on validation list:  
- Acanthopagrus berda  
- Acanthurus blochii  
- Acanthurus mata  
- Aethaloperca rogaa  
- Auxis thazard  
- Caranx caninus  
- Caranx heberi  
- Caranx hippos  
- Epinephelus faveatus  
- Epinephelus malabaricus  
- Epinephelus melanostigma  
- Epinephelus spilotoceps  
- Fistularia petimba  
- Gymnothorax flavimarginatus  
- Gymnothorax monochrous  
- Himantura gerrardi  
- Kyphosus bigibbus  
- Monotaxis grandoculis  
- Mugil cephalus  
- Mulloidichthys pfluegeri  
- Myripristis formosa  
- Planiliza alata - Platybelone platyura - Plectorhinchus playfairi  
- Plotasus canius - Pseudorhombus arsius  
- Rhineacanthus aculeatus  
- - Siganus guttatus  
- Siganus luridus  
- Upeneus japonicus

In catch composition but not in validation list or on fishbase:  
- Cyrnecranius randoculis  
- Lethrinus guttatus  
- Lethrinus vaigiensis  
- Navaculichthys nitaeniourous  
- Ostracion nasus  
- Panulirus versicolor  
- Pomatomus semicirculatus

In validation list but is not on fish base: - Acanthurus vaigiensis

### Final check: any notes from both datasets

``` r
## check for any notes that the data collectors left for analysis 
unique(df$fishing_operation_notes)
```

    ## [1] NA

``` r
unique(df$catch_composition_notes)
```

    ## [1] NA
