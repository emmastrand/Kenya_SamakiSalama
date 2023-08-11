Analysis of Fishing Landings dataset - household groups
================
Author: Emma Strand; <emma_strand@uri.edu>

## Prior Scripts

QC:
<https://github.com/emmastrand/Kenya_SamakiSalama/blob/main/FishLandings/scripts/QC.md>

## <a name="data"></a> **Reading in datafiles**

``` r
library(plyr)
library(dplyr)
library(tidyverse)
library(purrr)
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
library(forcats)
library(ggstatsplot)
library(ggridges)
library(ggbreak)
library(emmeans)
library(cowplot)
```

## Read in the data frame that is the output of the QC script.

Summary information so far: 741 total surveys, 591 unmodified traps, 150
modified traps.

``` r
# read in excel file
## 2,156 rows x 30 variables (this should match the final version of QC script)
data <- read.csv("household groups/alternate_fishing.csv") %>% #read in excel file 
  dplyr::select(-X)

# creating new columns with month year and date
## survey ID is master identifier that is based on `date_collected_dd_mm_yyyy` column 
data <- data %>% 
  separate(date_collected_dd_mm_yyyy, c("year", "month", "day"), remove = FALSE) 
## now 34 columns 

#changing this column to numeric instead of a character (needed for next fxn)
data$month <- as.numeric(data$month) 
#changing numeric months to month names
data$month <- month.abb[data$month] 

# changing levels of month (important for figures later on); only first three letter of month 
data$month <- factor(data$month, levels=c("Jan", "Feb","Mar","Apr","May",
                                          "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

data <- data %>%
  mutate(household_group = case_when(
    BMU == "KANAMAI" ~ "Control",
    BMU == "KURUWITU" ~ "Social marketing",
    BMU == "TAKAUNGU" ~ "Social marketing"))
```

**Control households**  
- KANAMAI (141 surveys): November 2021 and January - May 2022 - Gear
used: Spearguns - Units of data: Per single fisherman / household

**Social marketing households**  
- KURUWITU (382 surveys): July - December 2021 and January - May 2022  
- TAKAUNGU (105 surveys): January - May 2022  
- Gear used: Both used spearguns and TAKAUNGU used monofilament - Units
of data: Per single fisherman / household

### Read in the Group 3

``` r
##737 rows 
group3 <- read.csv("output/pertrap_final_df.csv", header=TRUE) %>% dplyr::select(-X) %>% 
  mutate(total_catch_per_fisherman = (CPUE*total_traps_collected)/`No..of.fishers.in.crew`,
        Total_Biomass_kg_per_fisherman = (calculated_biomass_pertrap*total_traps_collected)/`No..of.fishers.in.crew`,
        Total_value_KES_per_fisherman = (KSHpertrap*total_traps_collected)/`No..of.fishers.in.crew`,
        Calcium_mg_per_fisherman = (pertrap_Calcium*total_traps_collected)/`No..of.fishers.in.crew`,
        Iron_mg_per_fisherman = (pertrap_Iron*total_traps_collected)/`No..of.fishers.in.crew`,
        Omega3_g_per_fisherman = (pertrap_Omega3*total_traps_collected)/`No..of.fishers.in.crew`,
        Protein_g_per_fisherman = (pertrap_Protein*total_traps_collected)/`No..of.fishers.in.crew`,
        VitaminA_ug_per_fisherman = (pertrap_VitaminA*total_traps_collected)/`No..of.fishers.in.crew`,
        Selenium_ug_per_fisherman = (pertrap_Selenium*total_traps_collected)/`No..of.fishers.in.crew`,
        Zinc_mg_per_fisherman = (pertrap_Zinc*total_traps_collected)/`No..of.fishers.in.crew`)

##741 rows
length_data <- read.csv("output/length_persurvey.csv") %>% dplyr::select(survey_id, trap_type, median_length)
## 17,824 rows
length_total <- read.csv("output/length_total.csv") %>% dplyr::select(survey_id, trap_type, median_length)

##737 rows -- should match the initial group3 dataframe 
group3 <- left_join(group3, length_data, by = c("survey_id", "trap_type"))

### will merge these data with group 1 and 2 at the end of this script 
```

## Check ranges and add filters

63 unique species total:  
- 40 of those found in KANAMAI  
- 56 of those found in KURUWITU  
- 32 of those found in TAKAUNGU

``` r
hist(data$total_biomass_kg) ## 0.25 26.00
```

![](Analysis-HouseholdGroups_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
hist(data$take_home_weight_kg, na.rm = TRUE) ## 0-4
```

    ## Warning in plot.window(xlim, ylim, "", ...): "na.rm" is not a graphical
    ## parameter

    ## Warning in title(main = main, sub = sub, xlab = xlab, ylab = ylab, ...):
    ## "na.rm" is not a graphical parameter

    ## Warning in axis(1, ...): "na.rm" is not a graphical parameter

    ## Warning in axis(2, at = yt, ...): "na.rm" is not a graphical parameter

![](Analysis-HouseholdGroups_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

``` r
hist(data$total_value_KES) ## 40 - 4800 
```

![](Analysis-HouseholdGroups_files/figure-gfm/unnamed-chunk-4-3.png)<!-- -->

``` r
hist(data$take_home_value_KES, na.rm = TRUE) ## 0-600 
```

    ## Warning in plot.window(xlim, ylim, "", ...): "na.rm" is not a graphical
    ## parameter

    ## Warning in title(main = main, sub = sub, xlab = xlab, ylab = ylab, ...):
    ## "na.rm" is not a graphical parameter

    ## Warning in axis(1, ...): "na.rm" is not a graphical parameter

    ## Warning in axis(2, at = yt, ...): "na.rm" is not a graphical parameter

![](Analysis-HouseholdGroups_files/figure-gfm/unnamed-chunk-4-4.png)<!-- -->

``` r
range(data$`No..of.fishers.in.crew`) ## 1-3 people 
```

    ## [1] 1 3

``` r
hist(data$number_of_fish) ## 1 - 19 fish 
```

![](Analysis-HouseholdGroups_files/figure-gfm/unnamed-chunk-4-5.png)<!-- -->

``` r
#unique(data$scientific_name) ## 63 fish 
#data %>% subset(BMU == "TAKAUNGU") %>% dplyr::select(scientific_name) %>% distinct()
```

## Calculating median length

``` r
data <- data %>% 
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
    length_corrected == ">90" ~ 100 ### circle back to what value to use here
  )) 

hist(data$median_length) ## majority fall within 10-40 cm 
```

![](Analysis-HouseholdGroups_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## Loading Galligan and FishBase dataset

Most species in the list above have metadata below.

``` r
fishbase_lifehistory <- read_excel("data/fishbase.xlsx", sheet = "life history") %>% #read in excel file 
  select(scientific_name, Lm, Lmax) %>% dplyr::rename(Lmat_fishbase = Lm) %>% dplyr::rename(Lmax_fishbase = Lmax) 

# rows that appear in species_list but not fishbase_lifehistory
## checking which fish I still need Fishbase information for (44 species)
# dplyr::setdiff(species_list, fishbase_lifehistory) %>% write.csv("data/add_fishbase_info.csv")

fishbase_biomass <- read_excel("data/fishbase.xlsx", sheet = "biomass") %>%
  dplyr::select(scientific_name, a, b)

fishbase <- full_join(fishbase_lifehistory, fishbase_biomass, by = "scientific_name")

galligan <- read.csv("data/SpeciesData_GatedTraps_Galligan_edited.csv", header=TRUE, sep = ",") %>%
  dplyr::rename(scientific_name = Species) #%>% dplyr::select(., scientific_name)
# dplyr::setdiff(species_list, galligan) 
### 11 fish found in our dataset that are not found in Galligan dataset 
## these won't be included in the nutrient or revenue analysis
## I think this is OK b/c all species have 113 or lower than 40 total catch

metadata <- full_join(fishbase, galligan, by = "scientific_name")

### PRE MERGE DATA = 2,156 X 34
### POST MERGE DATA = 2,156 X 82 
data2 <- left_join(data, metadata, by = "scientific_name")
```

## Calculating total biomass

W=aL^b

W = Biomass  
L = Length (in our case, median length)  
a = (taken from fishbase) b = (taken from fishbase)

Read in data from fishbase. These units are in cm and grams.

<https://www.fishbase.de/manual/English/FishbaseThe_LENGTH_WEIGHT_table.htm>

``` r
### Switching to data3 which will filter out those fish with no nutrient or a and b values 
data2 <- data2 %>%  
  ## mutating a and b column to be numeric; this will create NAs b/c not all fish had a and b valueus in fishbase
  mutate_at(c('a', 'b'), as.numeric) %>%
  mutate(W_g = (a*(median_length^b))*number_of_fish,
         W_kg = W_g/1000) %>%
  group_by(survey_id) %>%
  filter(!is.na(W_kg)) %>%
  mutate(calculated_total_biomass = sum(W_kg)) %>% 
  ungroup()
```

    ## Warning: There were 2 warnings in `mutate()`.
    ## The first warning was:
    ## ℹ In argument: `a = .Primitive("as.double")(a)`.
    ## Caused by warning:
    ## ! NAs introduced by coercion
    ## ℹ Run `dplyr::last_dplyr_warnings()` to see the 1 remaining warning.

``` r
range(data2$calculated_total_biomass) ## 0.001832111 kg - 18.201910245 kg
```

    ## [1]  0.001832111 18.201910245

### Comparing reported vs. calculated

``` r
data2 %>% 
  select(survey_id, total_biomass_kg, calculated_total_biomass) %>%
  distinct() %>% na.omit() %>%
  mutate(comparison = if_else(total_biomass_kg > calculated_total_biomass, "REPORTED > CALC", "REPORTED < CALC"),
         difference = total_biomass_kg-calculated_total_biomass) %>%
  ggplot(., aes(x=comparison, y=difference)) + theme_bw() +
  geom_jitter(aes(color=comparison), size=1) + xlab("") + ylab("difference in value")
```

![](Analysis-HouseholdGroups_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

## Calculating take home kg/%

``` r
takehome <- data2 %>% dplyr::group_by(survey_id, destination) %>%   
  mutate(kg_perdestination = sum(W_kg)) %>% ungroup() %>% 
  dplyr::select(survey_id, BMU, household_group, destination, kg_perdestination) %>%
  distinct() %>% spread(destination, kg_perdestination) %>%
  #mutate_all(., ~replace_na(.,0)) %>% 
  dplyr::select(survey_id, BMU, household_group, HOME) %>%
  dplyr::rename(TakeHome_kg = HOME)
```

## Calculating trophic level

``` r
data2_trophic_level <- data2 %>% group_by(survey_id) %>%
  tidyr::uncount(., number_of_fish, .remove = TRUE) %>%
  mutate(mean.trophic = mean(TrophLevel, na.rm = TRUE)) %>% 
  dplyr::select(survey_id, mean.trophic) %>% distinct()

data2 <- data2 %>% left_join(., data2_trophic_level, by = "survey_id")
```

## Calculating herbivore/carnivore presence

``` r
diet <- data2 %>% group_by(survey_id, Diet) %>%
  dplyr::mutate(countper_diet = n()) %>% ungroup() %>%
  select(survey_id, BMU, household_group, Diet, countper_diet) %>% distinct() %>%
  group_by(survey_id) %>% spread(Diet, countper_diet) %>% dplyr::select(-`<NA>`) %>%
  mutate_all(., ~replace_na(.,0)) %>%
  mutate(total = sum(c(Carnivorous, `Herbivorous detritivorous`, `Macroalgal herbivorous`,
                        `Mobile Inverts`, `Omnivorous`, `Planktivorous`))) %>%
  mutate(Carnivorous = Carnivorous/total*100,
         `Herbivorous detritivorous` = `Herbivorous detritivorous`/total*100,
         `Macroalgal herbivorous` = `Macroalgal herbivorous`/total*100,
         `Mobile Inverts` = `Mobile Inverts`/total*100,
         `Omnivorous` = `Omnivorous`/total*100,
         `Planktivorous` = `Planktivorous`/total*100,
         Carnivorous_total = Carnivorous+`Mobile Inverts`) %>% 
  dplyr::select(-total, -Carnivorous, -`Mobile Inverts`) %>%
  dplyr::rename(Diet_percent_Herbivorous_Detritivorous = `Herbivorous detritivorous`) %>%
  dplyr::rename(Diet_percent_Herbivorous_Macroalgal = `Macroalgal herbivorous`) %>%
  dplyr::rename(Diet_percent_Omnivorous = Omnivorous) %>%
  dplyr::rename(Diet_percent_Planktivorous = Planktivorous) %>%
  dplyr::rename(Diet_percent_Carnivorous = Carnivorous_total)
```

    ## `mutate_all()` ignored the following grouping variables:
    ## • Column `survey_id`
    ## ℹ Use `mutate_at(df, vars(-group_cols()), myoperation)` to silence the message.

``` r
head(diet)
```

    ## # A tibble: 6 × 8
    ## # Groups:   survey_id [6]
    ##   survey_id  BMU   household_group Diet_percent_Herbivo…¹ Diet_percent_Herbivo…²
    ##   <chr>      <chr> <chr>                            <dbl>                  <dbl>
    ## 1 2021-07-2… KURU… Social marketi…                   66.7                      0
    ## 2 2021-07-2… KURU… Social marketi…                   42.9                      0
    ## 3 2021-07-2… KURU… Social marketi…                    0                        0
    ## 4 2021-07-2… KURU… Social marketi…                   66.7                      0
    ## 5 2021-07-2… KURU… Social marketi…                   20                        0
    ## 6 2021-07-2… KURU… Social marketi…                   50                        0
    ## # ℹ abbreviated names: ¹​Diet_percent_Herbivorous_Detritivorous,
    ## #   ²​Diet_percent_Herbivorous_Macroalgal
    ## # ℹ 3 more variables: Diet_percent_Omnivorous <dbl>,
    ## #   Diet_percent_Planktivorous <dbl>, Diet_percent_Carnivorous <dbl>

## Calculating average length (cm)

``` r
data2_length <- data2 %>% group_by(survey_id) %>%
  tidyr::uncount(., number_of_fish, .remove = TRUE) %>%
  mutate(mean_length = mean(median_length), na.rm=TRUE) %>% 
  dplyr::select(survey_id, mean_length) %>% distinct()

data2 <- data2 %>% left_join(., data2_length, by = "survey_id")
```

## Calculating average value (KES)

``` r
data2 <- data2 %>% 
  mutate(KSHperspp = Price_KSHPerkg*W_kg) %>%
  group_by(survey_id) %>%
  filter(!is.na(W_kg)) %>%
  mutate(calculated_total_value = sum(KSHperspp, na.rm=TRUE)) %>% 
  ungroup()
```

## Calculating average catch

``` r
data2 <- data2 %>%
  group_by(survey_id) %>%
  mutate(total_catch_per_fisherman = sum(number_of_fish)/`No..of.fishers.in.crew`)
```

## Calculating species richness

``` r
data2 <- data2 %>%
  ## still grouped by survey id from above
  mutate(richness = n_distinct(scientific_name))
```

## Calculating nutrient information

``` r
data2 <- data2 %>%
  mutate(spp_Calcium = (W_g/100)*Calcium_mgPer100g,
         spp_Iron = (W_g/100)*Iron_mgPer100g,
         spp_Omega3 = (W_g/100)*Omega3_gPer100g,
         spp_Protein = (W_g/100)*Protein_gPer100g,
         spp_VitaminA = (W_g/100)*VitaminA_ugPer100g,
         spp_Selenium = (W_g/100)*Selenium_ugPer100g,
         spp_Zinc = (W_g/100)*Zinc_mgPer100g) %>%
  ### still grouped by survey id from above 
  mutate(Calcium_mg_per_fisherman = sum(spp_Calcium, na.rm = TRUE)/`No..of.fishers.in.crew`,
         Iron_mg_per_fisherman = sum(spp_Iron, na.rm = TRUE)/`No..of.fishers.in.crew`,
         Omega3_g_per_fisherman = sum(spp_Omega3, na.rm = TRUE)/`No..of.fishers.in.crew`,
         Protein_g_per_fisherman = sum(spp_Protein, na.rm = TRUE)/`No..of.fishers.in.crew`,
         VitaminA_ug_per_fisherman = sum(spp_VitaminA, na.rm = TRUE)/`No..of.fishers.in.crew`,
         Selenium_ug_per_fisherman = sum(spp_Selenium, na.rm = TRUE)/`No..of.fishers.in.crew`,
         Zinc_mg_per_fisherman = sum(spp_Zinc, na.rm = TRUE)/`No..of.fishers.in.crew`) %>% ungroup()
```

## Calculating % under Lmaturity

``` r
data2 <- data2 %>%
  mutate(Lmat_corrected = if_else(is.na(Lmat_cm), Lmat_fishbase, Lmat_cm)) %>%
  ### categorizing mature or immature 
  mutate(maturity = if_else(median_length >= Lmat_corrected, "mature", "immature")) %>%
  ### calculating distance from Lmat
  mutate(length_dist = median_length-Lmat_corrected) %>% 
  group_by(survey_id) %>%
    mutate(mean_lengthdist = mean(length_dist)) %>%
  ungroup()

hist(data2$mean_lengthdist)
```

![](Analysis-HouseholdGroups_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

## Summary and include Group 3

``` r
group_1_2 <- data2 %>% 
  dplyr::select(survey_id, year, month, day, BMU, household_group, calculated_total_biomass, mean.trophic,
                richness, total_catch_per_fisherman, calculated_total_value, mean_length, Calcium_mg_per_fisherman,
                Iron_mg_per_fisherman, Omega3_g_per_fisherman, Protein_g_per_fisherman, 
                VitaminA_ug_per_fisherman, Selenium_ug_per_fisherman, mean_lengthdist,
                Zinc_mg_per_fisherman, `No..of.fishers.in.crew`) %>%
  distinct() %>% 
  dplyr::rename(Total_Biomass_kg_per_fisherman = calculated_total_biomass) %>%
  dplyr::rename(Avg_Trophic_Level = mean.trophic) %>%
  dplyr::rename(Total_value_KES_per_fisherman = calculated_total_value) %>%
  dplyr::rename(Mean_length_cm = mean_length) %>%
  dplyr::rename(No_fishermen = `No..of.fishers.in.crew`) %>%
  left_join(., takehome, c("survey_id", "BMU", "household_group")) %>%
  left_join(., diet, by = c("survey_id", "BMU", "household_group")) %>% ungroup() %>%
  ### subset to months of interest 
  subset(month == "Apr" | month == "May")

head(group_1_2)
```

    ## # A tibble: 6 × 27
    ##   survey_id       year  month day   BMU   household_group Total_Biomass_kg_per…¹
    ##   <chr>           <chr> <fct> <chr> <chr> <chr>                            <dbl>
    ## 1 2022-04-29 23:… 2022  Apr   29    KANA… Control                         1.25  
    ## 2 2022-04-03 12:… 2022  Apr   03    TAKA… Social marketi…                 0.0721
    ## 3 2022-04-03 11:… 2022  Apr   03    TAKA… Social marketi…                 0.939 
    ## 4 2022-05-03 12:… 2022  May   03    TAKA… Social marketi…                 1.23  
    ## 5 2022-05-03 09:… 2022  May   03    TAKA… Social marketi…                 1.57  
    ## 6 2022-04-03 12:… 2022  Apr   03    KANA… Control                         2.24  
    ## # ℹ abbreviated name: ¹​Total_Biomass_kg_per_fisherman
    ## # ℹ 20 more variables: Avg_Trophic_Level <dbl>, richness <int>,
    ## #   total_catch_per_fisherman <dbl>, Total_value_KES_per_fisherman <dbl>,
    ## #   Mean_length_cm <dbl>, Calcium_mg_per_fisherman <dbl>,
    ## #   Iron_mg_per_fisherman <dbl>, Omega3_g_per_fisherman <dbl>,
    ## #   Protein_g_per_fisherman <dbl>, VitaminA_ug_per_fisherman <dbl>,
    ## #   Selenium_ug_per_fisherman <dbl>, mean_lengthdist <dbl>, …

``` r
### 161 surveys total for April and May combined 2022 
## Control = 41 surveys 
## Social marketing = 120 surveys 
```

### Subsetting group 3’s data

``` r
diet_group3 <- read.csv("output/group3_diet.csv", header=TRUE) %>% dplyr::select(-X)
takehome_group3 <- read.csv("output/takehome_kg.csv", header=TRUE) %>% dplyr::select(-X) %>%
  dplyr::rename(TakeHome_kg = HOME)

## PRE FILTER = 737 surveys 
## POST FILTER 228 surveys
group3 <- group3 %>% 
  subset(month == "Apr" | month == "May") %>%
  subset(year == "2022") %>%
  dplyr::select(survey_id, BMU, trap_type, total_traps_collected, year, month, day, `No..of.fishers.in.crew`,
                richness, total_catch_per_fisherman, Total_Biomass_kg_per_fisherman, Total_value_KES_per_fisherman,
                Calcium_mg_per_fisherman, Iron_mg_per_fisherman, Omega3_g_per_fisherman, Protein_g_per_fisherman,
                VitaminA_ug_per_fisherman, Selenium_ug_per_fisherman, Zinc_mg_per_fisherman, mean.trophic, median_length,
                mean_lengthdist) %>%
  mutate(household_group = case_when(
    trap_type == "Control" ~ "Social Marketing + Traps: Control",
    trap_type == "Experimental" ~ "Social Marketing + Traps: Experimental"
  )) %>% 
  dplyr::rename(No_fishermen = `No..of.fishers.in.crew`) %>%
  dplyr::rename(Mean_length_cm = median_length) %>%
  dplyr::rename(Avg_Trophic_Level = mean.trophic) 

group3 <- group3 %>% 
  left_join(., diet_group3, by = c("survey_id", "trap_type", "BMU")) %>%
  left_join(., takehome_group3, by = c("survey_id", "trap_type"))

## 73 surveys 
## 34 control and 39 experimental 
## 41 group 1 control

group3$year <- as.character(group3$year)
group3$day <- as.character(group3$day)

summary <- full_join(group_1_2, group3) %>% filter(Calcium_mg_per_fisherman > 0) %>%
  mutate(household_group_collapsed = case_when(
    household_group == "Control" ~ "Control",
    household_group == "Social marketing" ~ "Social marketing",
    household_group == "Social Marketing + Traps: Experimental" ~ "Social Marketing + Traps",
    household_group == "Social Marketing + Traps: Control" ~ "Social Marketing + Traps"
  )) %>%
  relocate(No_fishermen, .after = last_col())
```

    ## Joining with `by = join_by(survey_id, year, month, day, BMU, household_group,
    ## Total_Biomass_kg_per_fisherman, Avg_Trophic_Level, richness,
    ## total_catch_per_fisherman, Total_value_KES_per_fisherman, Mean_length_cm,
    ## Calcium_mg_per_fisherman, Iron_mg_per_fisherman, Omega3_g_per_fisherman,
    ## Protein_g_per_fisherman, VitaminA_ug_per_fisherman, Selenium_ug_per_fisherman,
    ## mean_lengthdist, Zinc_mg_per_fisherman, No_fishermen, TakeHome_kg,
    ## Diet_percent_Herbivorous_Detritivorous, Diet_percent_Herbivorous_Macroalgal,
    ## Diet_percent_Omnivorous, Diet_percent_Planktivorous, Diet_percent_Carnivorous)`

``` r
## 232 rows (234 before filter fxn) 

summary %>%
 write.csv("household groups/household_groups_finaldf.csv")

### Group 1 = 41 
### Group 2 = 118
### Group 3 = 73; 34 control and 39 experimental 
```

### Data exploration

``` r
summary <- summary %>%
  filter(., mean_lengthdist > -20) %>%
  filter(., total_catch_per_fisherman < 49) %>%
  filter(., Selenium_ug_per_fisherman < 7500) %>% ## Selenium
  filter(., mean_lengthdist > -20) %>% ## Lmat distance
  filter(., VitaminA_ug_per_fisherman < 10000) ## VitaminA
  
summary2 <- summary %>%
  tidyr::gather("measurement", "value", 7:26) %>%
  mutate(measurement = case_when(
    measurement == "Avg_Trophic_Level" ~ "Trophic Level",
    measurement == "Calcium_mg_per_fisherman" ~ "Calcium (mg)",
    measurement == "Diet_percent_Carnivorous" ~ "Carnivorous (%)",
    measurement == "Diet_percent_Herbivorous_Detritivorous" ~ "Herbivorous Detritivorous (%)",
    measurement == "Diet_percent_Herbivorous_Macroalgal" ~ "Herbivorous Macroalgal (%)",
    measurement == "Diet_percent_Omnivorous" ~ "Omnivorous (%)",
    measurement == "Diet_percent_Planktivorous" ~ "Planktivorous (%)",
    measurement == "Iron_mg_per_fisherman" ~ "Iron (mg)",
    measurement == "Mean_length_cm" ~ "Length (cm)",
    measurement == "Omega3_g_per_fisherman" ~ "Omega3 (g)",
    measurement == "Protein_g_per_fisherman" ~ "Protein (g)",
    measurement == "richness" ~ "Species Richness",
    measurement == "Selenium_ug_per_fisherman" ~ "Selenium (ug)",
    measurement == "TakeHome_kg" ~ "Biomass Taken Home (kg)",
    measurement == "Total_Biomass_kg_per_fisherman" ~ "Biomass (kg)",
    measurement == "total_catch_per_fisherman" ~ "CPUE",
    measurement == "Total_value_KES_per_fisherman" ~ "Value (KES)",
    measurement == "VitaminA_ug_per_fisherman" ~ "VitaminA (ug)",
    measurement == "Zinc_mg_per_fisherman" ~ "Zinc (ug)",
    measurement == "mean_lengthdist" ~ "Distance from Lmat (cm)"
  )) %>%
  mutate(household_group_collapsed = case_when(
    household_group_collapsed == "Control" ~ "Control",
    household_group_collapsed == "Social marketing" ~ "SM",
    household_group_collapsed == "Social Marketing + Traps" ~ "SM + Traps"
  )) 
  

summary_stats <- summarySE(summary2, measurevar = c("value"), 
                           groupvars = c("household_group_collapsed", "measurement"), na.rm = TRUE)
```

Individual point range plots

``` r
createPlot <- function(data, y) {
  ## subset to each input (variable)
  data <- data %>% subset(measurement == y)
  ## creating plot
  p <- 
    ggplot(data, aes(x=household_group_collapsed, y=value, color=household_group_collapsed)) +
    #geom_jitter(data=data, size=1, alpha=0.4, width=0.2) +
    geom_errorbar(data=data, aes(x=household_group_collapsed, y=value, ymin=value-se, ymax=value+se), 
                      position=position_dodge(0.3), alpha=0.9, size=0.75, width=0.2) + 
    geom_point(data=data, size=3, position=position_dodge(0.3)) +
    theme_bw() +
    theme(#strip.background = element_blank(),
        #axis.text.x=element_blank(),
        #panel.background = element_rect(margin(1, 1, 1, 1, "cm")),
        #plot.margin = margin(1, 1, 1, 1, "cm"),
        strip.placement='outside',
        legend.position="none",
        strip.text = element_text(face = "bold", size=10),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size=12, face="bold"),
        axis.text.x = element_text(color="black")) +
    scale_color_manual(values = c("#11BF66", "#E78818", "#48B5B7")) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 17)) +
    ylab(y) + xlab("") +
    labs(color = "Household Group")
  ggsave(file = paste0("household groups/figures/", y, ".png"), width = 3, height = 4, units = c("in"))
}

cols <- unique(summary_stats$measurement)

## invisible presents the output printing in this script
invisible(lapply(cols, createPlot, data = summary_stats))
```

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

Create jitter plots for all variables

``` r
for (i in cols){
  summary_subset <- summary_stats %>% filter(measurement==i)
  
  summary2 %>% subset(measurement==i) %>% ### CHANGE VARIABLE HERE
  ggplot(., aes(x=household_group_collapsed, y=value, color=household_group_collapsed)) + 
    geom_boxplot(outlier.shape = NA) +
  #geom_jitter(size=0.6, alpha=0.1, width=0.1, height=0.05) +  ## need to keep the height here for points to be in correct place
    geom_jitter(size=1.5, alpha=0.3, width=0.15, height=0.003) +  ## need to keep the height here for points to be in correct place
  theme_bw() +
  scale_color_manual(values = c("#11BF66", "#E78818", "#48B5B7")) +
  scale_fill_manual(values = c("#11BF66", "#E78818", "#48B5B7")) +
  theme(#strip.background = element_blank(),
        #axis.text.x=element_blank()g,
        #panel.background = element_rect(margin(1, 1, 1, 1, "cm")),
        #plot.margin = margin(1, 1, 1, 1, "cm"),
        strip.placement='outside',
        legend.position="none",
        strip.text = element_text(face = "bold", size=10),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size=12, face="bold"),
        axis.text.x = element_text(color="black")) +
    ylab(i) + xlab("") +
    labs(color = "Household Group")# +
    # geom_errorbar(data=summary_subset, aes(x=household_group_collapsed, y=value, ymin=value-se, ymax=value+se),
    #                   position=position_dodge(0.3), alpha=1, size=0.5, width=0.15) +
    # geom_point(data=summary_subset, aes(x=household_group_collapsed, y=value), size=1.5, position=position_dodge(0.3))
  
  #ggsave(file = paste0("household groups/figures/", i, " jitter.png"), width = 3, height = 4, units = c("in"))
    ggsave(file = paste0("household groups/figures/", i, " boxplot.png"), width = 3, height = 4, units = c("in"))
}
```

    ## Warning: Removed 175 rows containing non-finite values (`stat_boxplot()`).

    ## Warning: Removed 175 rows containing missing values (`geom_point()`).

Length frequency plot

``` r
length_total$household_group <- "SM + Traps"
## 17,824 rows 

length_groups12 <- data %>% 
  dplyr::select(survey_id, median_length, household_group) %>%
  mutate(household_group = case_when(
    household_group == "Social marketing" ~ "SM",
    household_group == "Control" ~ "Control"
  ))
## 2,156 rows

lengthfreq_df <- full_join(length_total, length_groups12)
```

    ## Joining with `by = join_by(survey_id, median_length, household_group)`

``` r
## 19,980 rows

LF <- lengthfreq_df %>%
  ggplot(aes(x=median_length, fill=household_group)) +
  geom_histogram(color="black", alpha=0.8, position = 'identity', binwidth = 5) +
    scale_fill_manual(values = c("#11BF66", "#E78818", "#48B5B7")) + theme_bw() +
  facet_grid(household_group ~ ., scales="free_y") +
  theme(panel.background=element_rect(fill='white', colour='black'),
        strip.background=element_rect(fill='white', colour='black'),
        strip.text = element_text(size = 12, face="bold"),
        legend.position="none",
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size=12, face="bold"),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0), size=12, face="bold")) + 
  ylab("Count") + xlab("Length (cm)")

ggsave(LF, file="household groups/figures/Length_freq.png", width=6, height=7, units=c("in"))
```

Large plot altogether

``` r
# plot2 <- summary_stats %>%
#   ggplot(., aes(x=household_group_collapsed, y=value, color=household_group_collapsed)) + 
#   geom_errorbar(aes(x=household_group_collapsed, y=value, ymin=value-se, ymax=value+se), 
#                 position=position_dodge(0.3), alpha=0.9, size=0.75, width=0.2) + 
#   geom_point(size=3, position=position_dodge(0.3)) +
#   facet_wrap(~factor(measurement, levels=c('Diet_percent_Carnivorous', 
#                                            'Diet_percent_Herbivorous_Detritivorous', 
#                                            'Diet_percent_Herbivorous_Macroalgal',
#                                            'Diet_percent_Omnivorous',
#                                            'Diet_percent_Planktivorous',
#                                            'Iron_mg_per_fisherman',
#                                            'Calcium_mg_per_fisherman',
#                                            'Omega3_g_per_fisherman',
#                                            'Protein_g_per_fisherman',
#                                            'Selenium_ug_per_fisherman',
#                                            'VitaminA_ug_per_fisherman',
#                                            'Zinc_mg_per_fisherman',
#                                            'Avg_Trophic_Level',
#                                            'Mean_length_cm',
#                                            'total_catch_per_fisherman',
#                                            'richness',
#                                            'Total_Biomass_kg_per_fisherman',
#                                            'TakeHome_kg',
#                                            'Total_value_KES_per_fisherman')), 
#              labeller = as_labeller(c(Diet_percent_Carnivorous = "% of catch: Carnivorous",
#                                       Diet_percent_Herbivorous_Detritivorous = "% of catch: Herbivorous Detritivorou",
#                                       Diet_percent_Herbivorous_Macroalgal = "% of catch: Herbivorous Macroalgal",
#                                       Diet_percent_Omnivorous = "% of catch: Omnivorous",
#                                       Diet_percent_Planktivorous = "% of catch: Planktivorous",
#                                       Iron_mg_per_fisherman = "Iron Content (mg) fisherman^-1",
#                                       Calcium_mg_per_fisherman = "Calcium Content (mg) fisherman^-1",
#                                       Omega3_g_per_fisherman = "Omega3 Content (g) fisherman^-1",
#                                       Protein_g_per_fisherman = "Protein Content (g) fisherman^-1",
#                                       Selenium_ug_per_fisherman = "Selenium Content (ug) fisherman^-1",
#                                       VitaminA_ug_per_fisherman = "VitaminA Content (ug) fisherman^-1",
#                                       Zinc_mg_per_fisherman = "Zinc Content (mg) fisherman^-1",
#                                       Avg_Trophic_Level = "Trophic Level",
#                                       Mean_length_cm = "Length (cm)",
#                                       total_catch_per_fisherman = "Catch: # of fish fisherman^-1",
#                                       richness = "# of unique species survey^-1",
#                                       Total_Biomass_kg_per_fisherman = "Biomass (kg) fisherman^-1",
#                                       TakeHome_kg = "Biomass (kg) taken home fisherman^-1",
#                                       Total_value_KES_per_fisherman = "Value (KES) fisherman^-1")),
#              scales = "free", strip.position = 'left') +
#   ylab(NULL) + 
#   xlab("") +
#   theme(strip.background = element_blank(),
#         strip.placement='outside',
#         strip.text = element_text(face = "bold", size=10),
#         axis.text.x=element_blank(),
#         panel.background = element_rect(fill = 'white', color = 'black')) + 
#   #scale_color_manual(values = c("#53C936", "#E78818", "#48B5B7", "#185AE7")) +
#   scale_color_manual(values = c("#11BF66", "#E78818", "#48B5B7"))
#   labs(color = "Household Group")
# 
# ggsave(file="household groups/metrics2_collapsed.png", plot2, width = 17, height = 12, units = c("in"))
```

### Species Richness

``` r
summary %>% 
  mutate(total_catch = total_catch_per_fisherman*No_fishermen) %>%
  ggplot(., aes(x=total_catch, y=richness, fill=household_group_collapsed, color=household_group_collapsed)) + 
  geom_smooth(alpha=0.1) +
  geom_point(size=2, alpha=0.7, shape=21) + 
  scale_fill_manual(values = c("#11BF66", "#E78818", "#48B5B7")) + 
  scale_color_manual(values = c("#11BF66", "#E78818", "#48B5B7")) +
  theme_bw()
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](Analysis-HouseholdGroups_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

``` r
summary %>%
  ggplot(., aes(x=household_group_collapsed, y=richness, fill=household_group_collapsed)) + 
  geom_boxplot(alpha=0.4, width=0.5) + 
  geom_jitter(size=2, alpha=0.8, width=0.15, height=0.1, shape=21) +
  scale_fill_manual(values = c("#11BF66", "#E78818", "#48B5B7")) +
  theme_bw()
```

![](Analysis-HouseholdGroups_files/figure-gfm/unnamed-chunk-25-2.png)<!-- -->

### Statistics

ANOVA for all groups

``` r
for (i in cols){
  summary2_subset <- summary2 %>% subset(measurement == i)
  
  aov <- aov(value~household_group_collapsed, data = summary2_subset)
  ANOVA <- Anova(aov, type = "III")
  TUKEY <- TukeyHSD(aov)
  
  ANOVA_results <- as.data.frame(ANOVA)
  ANOVA_results <- rownames_to_column(ANOVA_results, var = "variable")
  ANOVA_results$measurement <- i
  
  TUKEY_results <- as.data.frame(TUKEY$household_group_collapsed)
  TUKEY_results <- rownames_to_column(TUKEY_results, var = "variable")
  TUKEY_results$measurement <- i
  
  results <- full_join(ANOVA_results, TUKEY_results, by = c("measurement", "variable")) %>%
    ## mutating scientific notation to 6 decimal places 
    mutate_if(is.numeric, round, 6)
  
  write_xlsx(results, paste0("household groups/statistics/", i, "_output.xlsx"))
}
```

    ## Warning in printHypothesis(L, rhs, names(b)): one or more coefficients in the hypothesis include
    ##      arithmetic operators in their names;
    ##   the printed representation of the hypothesis will be omitted

    ## Warning in printHypothesis(L, rhs, names(b)): one or more coefficients in the hypothesis include
    ##      arithmetic operators in their names;
    ##   the printed representation of the hypothesis will be omitted

    ## Warning in printHypothesis(L, rhs, names(b)): one or more coefficients in the hypothesis include
    ##      arithmetic operators in their names;
    ##   the printed representation of the hypothesis will be omitted

    ## Warning in printHypothesis(L, rhs, names(b)): one or more coefficients in the hypothesis include
    ##      arithmetic operators in their names;
    ##   the printed representation of the hypothesis will be omitted

    ## Warning in printHypothesis(L, rhs, names(b)): one or more coefficients in the hypothesis include
    ##      arithmetic operators in their names;
    ##   the printed representation of the hypothesis will be omitted

    ## Warning in printHypothesis(L, rhs, names(b)): one or more coefficients in the hypothesis include
    ##      arithmetic operators in their names;
    ##   the printed representation of the hypothesis will be omitted

    ## Warning in printHypothesis(L, rhs, names(b)): one or more coefficients in the hypothesis include
    ##      arithmetic operators in their names;
    ##   the printed representation of the hypothesis will be omitted

    ## Warning in printHypothesis(L, rhs, names(b)): one or more coefficients in the hypothesis include
    ##      arithmetic operators in their names;
    ##   the printed representation of the hypothesis will be omitted

    ## Warning in printHypothesis(L, rhs, names(b)): one or more coefficients in the hypothesis include
    ##      arithmetic operators in their names;
    ##   the printed representation of the hypothesis will be omitted

    ## Warning in printHypothesis(L, rhs, names(b)): one or more coefficients in the hypothesis include
    ##      arithmetic operators in their names;
    ##   the printed representation of the hypothesis will be omitted

    ## Warning in printHypothesis(L, rhs, names(b)): one or more coefficients in the hypothesis include
    ##      arithmetic operators in their names;
    ##   the printed representation of the hypothesis will be omitted

    ## Warning in printHypothesis(L, rhs, names(b)): one or more coefficients in the hypothesis include
    ##      arithmetic operators in their names;
    ##   the printed representation of the hypothesis will be omitted

    ## Warning in printHypothesis(L, rhs, names(b)): one or more coefficients in the hypothesis include
    ##      arithmetic operators in their names;
    ##   the printed representation of the hypothesis will be omitted

    ## Warning in printHypothesis(L, rhs, names(b)): one or more coefficients in the hypothesis include
    ##      arithmetic operators in their names;
    ##   the printed representation of the hypothesis will be omitted

    ## Warning in printHypothesis(L, rhs, names(b)): one or more coefficients in the hypothesis include
    ##      arithmetic operators in their names;
    ##   the printed representation of the hypothesis will be omitted

    ## Warning in printHypothesis(L, rhs, names(b)): one or more coefficients in the hypothesis include
    ##      arithmetic operators in their names;
    ##   the printed representation of the hypothesis will be omitted

    ## Warning in printHypothesis(L, rhs, names(b)): one or more coefficients in the hypothesis include
    ##      arithmetic operators in their names;
    ##   the printed representation of the hypothesis will be omitted

    ## Warning in printHypothesis(L, rhs, names(b)): one or more coefficients in the hypothesis include
    ##      arithmetic operators in their names;
    ##   the printed representation of the hypothesis will be omitted

    ## Warning in printHypothesis(L, rhs, names(b)): one or more coefficients in the hypothesis include
    ##      arithmetic operators in their names;
    ##   the printed representation of the hypothesis will be omitted

    ## Warning in printHypothesis(L, rhs, names(b)): one or more coefficients in the hypothesis include
    ##      arithmetic operators in their names;
    ##   the printed representation of the hypothesis will be omitted
