CUE, maturity, and length analysis of Fishing Landings dataset
================
Author: Emma Strand; <emma_strand@uri.edu>

## Questions for Austin and Clay

## Calculations Made (all only unmodified and modified traps)

In `modified_trap_df`:  
Grouped by survey id (operation date + fisher ID):  
- `total_catch` (total number of fish in that fishing trip) = sum of
`number_of_fish`  
- `kg_per_trap` (total \# kg per trap – Biomass) =
total\_biomass\_kg/total\_traps\_collected  
- `catch_per_trap` (\# of fish per trap – Abundance) =
total\_catch/total\_traps\_collected

In `species_list` to calculate top 10 species throughout entire
survey:  
Grouped by species and trap type, - `species_catch` = sum of
`number_of_fish`

In `species_df` to calculate top species regardless of trap type:  
Grouped by species,  
- `species_total_catch` = sum of `number_of_fish`

In `species_df` to calculate abundance for each top species:  
Dataset filtered to only top 10 species:  
Grouped by survey id and species,  
- `topspecies_catch` = sum of `number_of_fish` (number of fish in top 10
for each trap, not the same \# of fish value as above calculations)  
- `catch_per_trap` = topspecies\_catch/total\_traps\_collected. *Catch
per trap is the \# of that species / trap \# from that survey* i.e. 25
Siganus sutor individuals / 4 traps collected for that fishing trip =
there are 6.25 Siganus sutor per trap set.

In `maturity` to calculate the abundance of mature and immature fish:  
- `median_length` is the median of the length bin (i.e. 6-10 bin =
median length of 8, 71-80 bin = median length of 75.5)  
Grouped by survey id, species, maturity, trap type:  
- `nofish_pertrap` = number of fish / total traps collected.

## Contents

-   [**Reading in datafiles**](#data)  
-   [**Total catch (grams) per unit effort (trap
    set)**](#catch_effort)  
-   [**Calculate top species caught**](#species)  
-   [**Top species stats per trap**](#species_pertrap)  
-   [**Creating database from Fishbase**](#fishbase)  
-   [**Catch per unit effort for top species by maturity**](#maturity)  
-   [**Catch and length data of mature fish**](#length)  
-   [**Length Frequency plots of top species**](#freq_plots)

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

Read in the data frame that is the output of the QC script.

``` r
# read in excel file
data <- read_excel("data/cleaned-Fishlandings-data- CC-JM-Clay-IW updated 04-09-2022.xlsx") #read in excel file 

data <- data %>% separate(Operation_date, c("year", "month", "day"), remove = FALSE) # creating new columns with month year and date
data$month <- as.numeric(data$month) #changing this column to numeric instead of a character (needed for next fxn)
data$month <- month.abb[data$month] #changing numeric months to month names 

data$month <- factor(data$month, levels=c("Jan", "Feb","Mar","Apr","May","Jun",
                                          "Jul", "Aug", "Sep", "Oct", "Dec", "NA"))

data$total_traps_collected[data$total_traps_collected == 0] <- NA

# creating a survey id
data <- data %>%
  unite(survey_id, Operation_date, fisher_id, sep = " ", remove = FALSE)
```

## <a name="catch_effort"></a> **Total catch (grams) per unit effort (trap set)**

**Total catch per unit effort between modified and traditional traps. It
would be great to see this as grams captured per trap set.**

Grouping by fisher\_id but this might be effective to group by
enumerator once I have correct list of names. There are 3 boat trips
recorded with the exact same fish data that are under 3 different fisher
ID names but all the same enumerator.. come back to this in QC.

Goal: kilograms captured per trap set.

``` r
modified_trap_df <- data %>%
  dplyr::group_by(survey_id) %>% # group by survey id
  mutate(total_catch = sum(number_of_fish), #count the number of fish caught for each survey id
         kg_per_trap = total_biomass_kg/total_traps_collected, #divide total weight for survey id by total traps 
         catch_per_trap = total_catch/total_traps_collected) %>% #divide total catch per survey id by total traps 
  dplyr::ungroup(survey_id) %>% #ungroup by this column  
  subset(trap_type == "MODIFIED" | trap_type == "UNMODIFIED") %>% #subset for only modified and unmodified traps 
  select(survey_id, enumerator, trap_type, total_traps_collected, `No. of fishers in crew`, landing_site, total_catch, month, year, kg_per_trap, catch_per_trap) %>%
  distinct() 
```

### Plotting figures.

Abundance = Catch per trap

``` r
# basic total catch per trap with no other variables 
modified_trap_df %>% filter(catch_per_trap < 1500) %>%
  ggplot(aes(x=trap_type, y=catch_per_trap, color=trap_type)) +
  geom_boxplot(aes(color=trap_type), outlier.size = 0, lwd=0.5) +
    geom_point(aes(fill=trap_type), pch = 21, size=1) +
  theme_bw() + 
  ylab("Abundance (# of individuals per trap collected)") + xlab("Type of trap") +
  theme(axis.text.x = element_text(vjust = 1.1)) #Set the text angle
```

![](CUE-maturity-length-analysis_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
# by fisherman 
modified_trap_df %>% 
  ggplot(aes(x=trap_type, y=catch_per_trap, color=trap_type)) + 
  facet_wrap(~enumerator) +
  geom_boxplot(aes(color=trap_type), outlier.size = 0, lwd=0.5) +
    geom_point(aes(fill=trap_type), pch = 21, size=1) +
  theme_bw() + 
  ylab("Abundance (# of individuals per trap collected)") + xlab("Type of trap") +
  theme(axis.text.x = element_text(vjust = 1.1)) #Set the text angle
```

    ## Warning: Removed 231 rows containing non-finite values (stat_boxplot).

    ## Warning: Removed 231 rows containing missing values (geom_point).

![](CUE-maturity-length-analysis_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

``` r
# by landing site
modified_trap_df %>% 
  ggplot(aes(x=trap_type, y=catch_per_trap, color=trap_type)) + 
  facet_wrap(~landing_site) +
  geom_boxplot(aes(color=trap_type), outlier.size = 0, lwd=0.5) +
    geom_point(aes(fill=trap_type), pch = 21, size=1) +
  theme_bw() + 
  ylab("Abundance (# of individuals per trap collected)") + xlab("Type of trap") +
  theme(axis.text.x = element_text(vjust = 1.1)) #Set the text angle
```

    ## Warning: Removed 231 rows containing non-finite values (stat_boxplot).
    ## Removed 231 rows containing missing values (geom_point).

![](CUE-maturity-length-analysis_files/figure-gfm/unnamed-chunk-4-3.png)<!-- -->

``` r
# by month and year 
modified_trap_df %>% unite(ym, year, month, sep = " ", remove = FALSE) %>% filter(catch_per_trap < 1500) %>%
  ggplot(aes(x=month, y=catch_per_trap, color=trap_type)) + 
  facet_wrap(~year, scales = "free_y") +
  geom_boxplot(aes(color=trap_type), outlier.size = 0, lwd=0.5) +
  #geom_point(aes(x=ym, y=catch_per_trap, fill=trap_type), pch = 21, size=1) +
  theme_classic() + 
  ylab("Abundance (# of individuals per trap collected)") + xlab("Time of year") +
  theme(axis.text.x = element_text(vjust = 1.1, hjust=1.1, angle=60)) #Set the text angle
```

![](CUE-maturity-length-analysis_files/figure-gfm/unnamed-chunk-4-4.png)<!-- -->

Biomass = Kilograms per trap

``` r
# basic grams per trap plot with no other variables 
modified_trap_df %>% 
  ggplot(aes(x=trap_type, y=kg_per_trap, color=trap_type)) + 
  geom_boxplot(aes(color=trap_type), outlier.size = 0, lwd=0.5) +
    geom_point(aes(fill=trap_type), pch = 21, size=1) +
  theme_bw() + 
  ylab("Biomass (kg per trap collected)") + xlab("Type of trap") +
  theme(axis.text.x = element_text(vjust = 1.1)) #Set the text angle
```

    ## Warning: Removed 25 rows containing non-finite values (stat_boxplot).

    ## Warning: Removed 25 rows containing missing values (geom_point).

![](CUE-maturity-length-analysis_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
# visually seeing if this differs by fisherman 
modified_trap_df %>% 
  ggplot(aes(x=trap_type, y=kg_per_trap, color=trap_type)) + 
  facet_wrap(~enumerator) +
  geom_boxplot(aes(color=trap_type), outlier.size = 0, lwd=0.5) +
    geom_point(aes(fill=trap_type), pch = 21, size=1) +
  theme_bw() + 
  ylab("Biomass (kg per trap collected)") + xlab("Type of trap") +
  theme(axis.text.x = element_text(vjust = 1.1)) #Set the text angle
```

    ## Warning: Removed 25 rows containing non-finite values (stat_boxplot).
    ## Removed 25 rows containing missing values (geom_point).

![](CUE-maturity-length-analysis_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

``` r
# visually seeing if this differs by landing site 
modified_trap_df %>% 
  ggplot(aes(x=trap_type, y=kg_per_trap, color=trap_type)) + 
  facet_wrap(~landing_site) +
  geom_boxplot(aes(color=trap_type), outlier.size = 0, lwd=0.5) +
    geom_point(aes(fill=trap_type), pch = 21, size=1) +
  theme_bw() + 
  ylab("Biomass (kg per trap collected)") + xlab("Type of trap") +
  theme(axis.text.x = element_text(vjust = 1.1)) #Set the text angle
```

    ## Warning: Removed 25 rows containing non-finite values (stat_boxplot).
    ## Removed 25 rows containing missing values (geom_point).

![](CUE-maturity-length-analysis_files/figure-gfm/unnamed-chunk-5-3.png)<!-- -->

``` r
# by month and year 
modified_trap_df %>% unite(ym, year, month, sep = " ", remove = FALSE) %>% #filter(catch_per_trap < 1500) %>%
  ggplot(aes(x=month, y=kg_per_trap, color=trap_type)) + 
  facet_wrap(~year) +
  geom_boxplot(aes(color=trap_type), outlier.size = 0, lwd=0.5) +
 # geom_point(aes(x=month, group=trap_type, y=grams_per_trap, fill=trap_type), pch = 21, size=1) +
  theme_classic() + 
  ylab("Biomass (kg per trap collected)") + xlab("Time of year") +
  theme(axis.text.x = element_text(vjust = 1.1, hjust=1.1, angle=60)) #Set the text angle
```

    ## Warning: Removed 25 rows containing non-finite values (stat_boxplot).

![](CUE-maturity-length-analysis_files/figure-gfm/unnamed-chunk-5-4.png)<!-- -->

The relationship between grams per trap and total catch per trap.

``` r
modified_trap_df %>% filter(catch_per_trap < 1500) %>%
  ggplot(aes(x=catch_per_trap, y=kg_per_trap, color=trap_type)) +
  theme_bw() + xlab("Abundance (# of individuals per trap collected)") + ylab("Biomass (kg per trap collected)") +
  geom_smooth(aes(fill=trap_type), method="loess", se=TRUE, fullrange=TRUE, level=0.95, color="black") +
  geom_point(aes(fill=trap_type), pch = 21, size=1)
```

    ## `geom_smooth()` using formula 'y ~ x'

    ## Warning: Removed 3 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 10 rows containing missing values (geom_smooth).

    ## Warning: Removed 3 rows containing missing values (geom_point).

![](CUE-maturity-length-analysis_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

### Statistics on the above.

Default of t.test in R is a Welch t-test which is just an adaptation of
t-test, and it is used when the two samples have possibly unequal
variances. Use var.equal = TRUE or FALSE to specifiy the variances of
the dataset.

You can test equal variances with a Fisher’s F-test. If p &lt; 0.05 then
we include var.equal = FALSE in below ttest. If p &gt; 0.05 then we
include var.equal = TRUE in below ttest.

We are using an unpaired two sample t-test for this dataset.

``` r
UN <- modified_trap_df %>% subset(trap_type == "MODIFIED") %>% filter(!is.na(kg_per_trap)) %>% filter(!is.na(catch_per_trap))
MOD <- modified_trap_df %>% subset(trap_type == "UNMODIFIED") %>% filter(!is.na(kg_per_trap)) %>% filter(!is.na(catch_per_trap))
```

#### Kilograms per trap

``` r
var.test(UN$kg_per_trap, MOD$kg_per_trap)
```

    ## 
    ##  F test to compare two variances
    ## 
    ## data:  UN$kg_per_trap and MOD$kg_per_trap
    ## F = 0.75413, num df = 1508, denom df = 3688, p-value = 1.693e-10
    ## alternative hypothesis: true ratio of variances is not equal to 1
    ## 95 percent confidence interval:
    ##  0.6933801 0.8214534
    ## sample estimates:
    ## ratio of variances 
    ##          0.7541288

``` r
t.test(kg_per_trap~trap_type, data = modified_trap_df, var.equal = FALSE)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  kg_per_trap by trap_type
    ## t = 2.4187, df = 3674.2, p-value = 0.01563
    ## alternative hypothesis: true difference in means between group MODIFIED and group UNMODIFIED is not equal to 0
    ## 95 percent confidence interval:
    ##  0.006639708 0.063477779
    ## sample estimates:
    ##   mean in group MODIFIED mean in group UNMODIFIED 
    ##                0.8317823                0.7967236

#### Total catch per trap

``` r
var.test(UN$catch_per_trap, MOD$catch_per_trap)
```

    ## 
    ##  F test to compare two variances
    ## 
    ## data:  UN$catch_per_trap and MOD$catch_per_trap
    ## F = 5.202, num df = 1508, denom df = 3688, p-value < 2.2e-16
    ## alternative hypothesis: true ratio of variances is not equal to 1
    ## 95 percent confidence interval:
    ##  4.782988 5.666448
    ## sample estimates:
    ## ratio of variances 
    ##           5.202038

``` r
t.test(catch_per_trap~trap_type, data = modified_trap_df, var.equal = FALSE)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  catch_per_trap by trap_type
    ## t = 23.931, df = 1749.7, p-value < 2.2e-16
    ## alternative hypothesis: true difference in means between group MODIFIED and group UNMODIFIED is not equal to 0
    ## 95 percent confidence interval:
    ##  32.92163 38.79981
    ## sample estimates:
    ##   mean in group MODIFIED mean in group UNMODIFIED 
    ##                 49.25425                 13.39353

#### Total catch per trap vs weight in grams per trap.

We use a linear mixed model for this so we can include other variables
like fisherman and landing site.

Grams per trap is log transformed.

I’m not sure yet if this is the best way to do this…

``` r
# unmodified
un_catch_model <- lmer(log(kg_per_trap) ~ catch_per_trap + (1|enumerator) + (1|landing_site), na.action=na.omit, data=UN)
qqPlot(residuals(un_catch_model)) 
```

![](CUE-maturity-length-analysis_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

    ## [1] 980 602

``` r
hist(residuals(un_catch_model))
```

![](CUE-maturity-length-analysis_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->

``` r
# modified
mod_catch_model <- lmer(log(kg_per_trap) ~ catch_per_trap + (1|enumerator) + (1|landing_site), na.action=na.omit, data=MOD)
qqPlot(residuals(mod_catch_model)) 
```

![](CUE-maturity-length-analysis_files/figure-gfm/unnamed-chunk-10-3.png)<!-- -->

    ## [1] 2469  808

``` r
hist(residuals(mod_catch_model))
```

![](CUE-maturity-length-analysis_files/figure-gfm/unnamed-chunk-10-4.png)<!-- -->

``` r
summary(un_catch_model)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: 
    ## log(kg_per_trap) ~ catch_per_trap + (1 | enumerator) + (1 | landing_site)
    ##    Data: UN
    ## 
    ## REML criterion at convergence: 2272.3
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.3022 -0.6687  0.0764  0.7218  3.3657 
    ## 
    ## Random effects:
    ##  Groups       Name        Variance  Std.Dev.
    ##  landing_site (Intercept) 0.0119459 0.10930 
    ##  enumerator   (Intercept) 0.0006427 0.02535 
    ##  Residual                 0.2591703 0.50909 
    ## Number of obs: 1509, groups:  landing_site, 4; enumerator, 3
    ## 
    ## Fixed effects:
    ##                  Estimate Std. Error t value
    ## (Intercept)    -0.3776891  0.0615431  -6.137
    ## catch_per_trap  0.0027048  0.0002426  11.149
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## ctch_pr_trp -0.210

``` r
summary(mod_catch_model)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: 
    ## log(kg_per_trap) ~ catch_per_trap + (1 | enumerator) + (1 | landing_site)
    ##    Data: MOD
    ## 
    ## REML criterion at convergence: 6826.6
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -5.4389 -0.5965  0.0581  0.6998  3.3298 
    ## 
    ## Random effects:
    ##  Groups       Name        Variance Std.Dev.
    ##  landing_site (Intercept) 0.02644  0.1626  
    ##  enumerator   (Intercept) 0.01226  0.1107  
    ##  Residual                 0.36913  0.6076  
    ## Number of obs: 3689, groups:  landing_site, 4; enumerator, 3
    ## 
    ## Fixed effects:
    ##                 Estimate Std. Error t value
    ## (Intercept)    -0.498829   0.105221  -4.741
    ## catch_per_trap  0.007792   0.000409  19.051
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## ctch_pr_trp -0.056

``` r
Anova(un_catch_model, ddf="lme4", type='III')
```

    ## Analysis of Deviance Table (Type III Wald chisquare tests)
    ## 
    ## Response: log(kg_per_trap)
    ##                  Chisq Df Pr(>Chisq)    
    ## (Intercept)     37.663  1   8.41e-10 ***
    ## catch_per_trap 124.294  1  < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
Anova(mod_catch_model, ddf="lme4", type='III')
```

    ## Analysis of Deviance Table (Type III Wald chisquare tests)
    ## 
    ## Response: log(kg_per_trap)
    ##                  Chisq Df Pr(>Chisq)    
    ## (Intercept)     22.475  1  2.129e-06 ***
    ## catch_per_trap 362.935  1  < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## <a name="species"></a> **Calculate top species caught**

**Species catch per unit effort between modified and traditional traps.
Take the top 3-5 species and run \#1 for them separately.**

Calculating which species were the most abundant across the entire
survey.

This might have to be number of fish caught per trap? So that the
difference in \# of traps for modified and unmodified is accounted for?

This is split for modified and unmodified so far.. but can be changed to
combined.. the trend is about the same for most abundant type of fish in
each trap..

``` r
species_list <- data %>% select(scientific_name, number_of_fish, trap_type) %>% 
  filter(!is.na(trap_type)) %>% 
  filter(!is.na(number_of_fish)) %>% 
  subset(trap_type == "MODIFIED" | trap_type == "UNMODIFIED") %>% #subset for only modified and unmodified traps 
  dplyr::group_by(scientific_name, trap_type) %>%
  mutate(species_catch = sum(number_of_fish)) %>% 
  select(-number_of_fish) %>% distinct() %>%
  ungroup()

# above 250 cut off includes top each count for each type of trap 
species_list %>% filter(species_catch > 250) %>%
  ggplot(., aes(x=scientific_name, y=species_catch, group = trap_type, color = trap_type)) + 
  ylab("number of fish caught") + xlab("Genus species") +
  ggtitle("Species with counts > 250") +
  geom_point() + theme_bw() + theme(axis.text.x = element_text(angle = 60, hjust=1)) #Set the text angle
```

![](CUE-maturity-length-analysis_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
# species_list %>% filter(species_catch > 250) %>% 
#   select(scientific_name) %>% 
#   distinct() %>%
#   write_csv("output/top_species.csv")

# print top 5 species from modified traps 
species_list %>% subset(trap_type == "MODIFIED") %>%                                    
  arrange(desc(species_catch)) %>% head(5)
```

    ## # A tibble: 5 × 3
    ##   scientific_name       trap_type species_catch
    ##   <chr>                 <chr>             <dbl>
    ## 1 Siganus sutor         MODIFIED         124369
    ## 2 Lethrinus nebulosus   MODIFIED          17832
    ## 3 Scarus ghobban        MODIFIED          12354
    ## 4 Siganus canaliculatus MODIFIED           6772
    ## 5 Parupeneus indicus    MODIFIED           5449

``` r
# print top 5 species from unmodified traps 
species_list %>% subset(trap_type == "UNMODIFIED") %>%                                    
  arrange(desc(species_catch)) %>% head(5)
```

    ## # A tibble: 5 × 3
    ##   scientific_name        trap_type  species_catch
    ##   <chr>                  <chr>              <dbl>
    ## 1 Siganus sutor          UNMODIFIED        187444
    ## 2 Lethrinus nebulosus    UNMODIFIED         33780
    ## 3 Scarus ghobban         UNMODIFIED         21899
    ## 4 Siganus canaliculatus  UNMODIFIED         16401
    ## 5 Leptoscarus vaigiensis UNMODIFIED         11049

## <a name="species_pertrap"></a> **Top species stats per trap**

Create a subsetted df from the top 10 total species (break this down
into modified and unmodified later?).

``` r
# calculate species total catch to then figure out which ones were most popular regardless of trap type
species_df <- data %>% filter(!is.na(number_of_fish)) %>%
  subset(trap_type == "MODIFIED" | trap_type == "UNMODIFIED") %>%
  group_by(scientific_name) %>%
  mutate(species_total_catch = sum(number_of_fish)) %>% ungroup() #must ungroup for following commands 

# use the above metric to subset to top 10 of those 
species_keep <- species_df %>% select(scientific_name, species_total_catch) %>% 
  distinct() %>% slice_max(species_total_catch, n = 10) 

# filter species df based on the species_keep list 
species_df <- species_df %>% filter(scientific_name %in% species_keep$scientific_name)

# double checking the above command worked - output should be only 5 
unique(sort(species_df$scientific_name))
```

    ##  [1] "Acanthurus dussumieri"  "Chaetodon selene"       "Leptoscarus vaigiensis"
    ##  [4] "Lethrinus nebulosus"    "Parupeneus indicus"     "Scarus ghobban"        
    ##  [7] "Scarus psittacus"       "Scarus rubroviolaceus"  "Siganus canaliculatus" 
    ## [10] "Siganus sutor"

### Abundance

``` r
### species_df is already subsetted to the top 10 species 
# within each survey id, for each species calculate topspecies_catch as the number fish in that species in that survey
# then calculate the catch per trap as the number of fish of that species over the total number of traps collected
species_df <- species_df %>%
  dplyr::group_by(survey_id, scientific_name) %>% # group by survey id
  mutate(topspecies_catch = sum(number_of_fish),
         catch_per_trap = topspecies_catch/total_traps_collected) %>% #divide total catch per survey id by total traps 
  dplyr::ungroup(survey_id, scientific_name) #ungroup by this column

species_df %>% mutate(trap_type = case_when(
    trap_type == "MODIFIED" ~ "MOD",
    trap_type == "UNMODIFIED" ~ "UNMOD")) %>%
  ggplot(aes(x=trap_type, y=catch_per_trap, color=trap_type)) + 
  geom_boxplot(aes(color=trap_type), outlier.size = 0, lwd=0.5) +
  facet_wrap(~scientific_name, scales = "free_y") +
  theme_classic() + ggtitle("Top 10 most abundant species") +
  ylab("Abundance (# of individuals per trap collected)") + xlab("Genus species") +
  theme(axis.text.x = element_text()) #Set the text angle
```

    ## Warning: Removed 203 rows containing non-finite values (stat_boxplot).

![](CUE-maturity-length-analysis_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

### Biomass per trap collected

1.) Expand df by \# of fish so that each row is a fish observation.  
2.) Calculate median length of the bin of each fish (L).  
3.) Find a and b parameters for all fish. Add this to the fishbase and
read in that df.  
4.) Add a and b columns to `species_df` by species Id.  
5.) Mutate to calculate W=aLb.  
6.) Group by species and survey id to calculate biomass per trap
collected.

### Statistics for the top ten species

Left off at trying to solve the following error when running aov:

Error in as.POSIXlt.character(x, tz, …) : character string is not in a
standard unambiguous format

Error in lm.fit(x, y, offset = offset, singular.ok = singular.ok, …) :
NA/NaN/Inf in ‘y’

``` r
Topspp_MOD <- species_df %>% subset(trap_type == "MODIFIED")

Topspp_UNMOD <- species_df %>% subset(trap_type == "UNMODIFIED")  

var.test(Topspp_MOD$catch_per_trap, MOD$catch_per_trap) # output is significant so put false below
```

    ## 
    ##  F test to compare two variances
    ## 
    ## data:  Topspp_MOD$catch_per_trap and MOD$catch_per_trap
    ## F = 3.1806, num df = 22129, denom df = 3688, p-value < 2.2e-16
    ## alternative hypothesis: true ratio of variances is not equal to 1
    ## 95 percent confidence interval:
    ##  3.026261 3.339891
    ## sample estimates:
    ## ratio of variances 
    ##            3.18061

``` r
#t.test(catch_per_trap~trap_type, data = species_df, var.equal = FALSE)

#aov(catch_per_trap ~ trap_type + scientific_name, data = species_df)
```

## <a name="maturity"></a> **Catch per unit effort for top species by maturity**

**Total mature fish catch per unit effort between modified and
traditional traps. This will have to be for the top 3-5 species
separately. Go to Fishbase and find the length at first maturity for
that particular species, then assign each fish a “mature” or “immature”
status in the data and calculate.**

Data pulled from Fishbase and put in the datasheet `fishbase.xlsx`.

``` r
fishbase <- read_excel("data/fishbase.xlsx", sheet = "data") %>% #read in excel file 
  select(scientific_name, Lm, Lm_se_min, Lm_se_max, Lmax)

maturity <- full_join(data, fishbase, by = "scientific_name") %>% # joining maturity reproduction info with main dataframe
  filter(!is.na(Lm)) %>% # taking out observations that don't have an Lm value so only the top species 
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

maturity <- maturity %>% 
  mutate(maturity = if_else(median_length >= Lm, "mature", "immature")) %>%
  subset(trap_type == "MODIFIED" | trap_type == "UNMODIFIED") %>% #subsetting to only the modified and unmodified traps 
  select(survey_id, year, scientific_name, trap_type, maturity, total_traps_collected, 
         length_corrected, median_length, number_of_fish, Lm) %>%
  mutate(nofish_pertrap = number_of_fish/total_traps_collected) 
```

Immaure and mature fish catch comparison

``` r
maturity %>% select(number_of_fish, trap_type, maturity, scientific_name, 
                    total_traps_collected, nofish_pertrap) %>% 
  na.omit() %>% 
  ggplot(., aes(x=scientific_name, y=nofish_pertrap, color=maturity)) +
  geom_boxplot() + theme_classic() + 
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  xlab("Species") +
  ylab("Abundance (number of individuals per trap collected)")
```

![](CUE-maturity-length-analysis_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
maturity %>% select(number_of_fish, trap_type, maturity, scientific_name, total_traps_collected, nofish_pertrap) %>% 
  na.omit() %>%  
  ggplot(., aes(x=maturity, y=nofish_pertrap, color=trap_type)) +
  geom_boxplot() + theme_classic() + 
  facet_wrap(~trap_type, scales = "free_y") +
  #theme(axis.text.x = element_text(angle=60, hjust=1)) +
  xlab("Maturity") +
  ylab("Abundance (number of individuals per trap collected)")
```

![](CUE-maturity-length-analysis_files/figure-gfm/unnamed-chunk-16-2.png)<!-- -->

Statistics on the above…

Circle back to the padj values coming out to “0.000000e+00”.

Double check `trap_type+maturity` vs `trap_type*maturity`.

``` r
nofish_maturity_aov <- aov(number_of_fish ~ trap_type*maturity, data = maturity)
summary(nofish_maturity_aov)
```

    ##                       Df  Sum Sq Mean Sq F value Pr(>F)    
    ## trap_type              1    8312    8312 135.829 <2e-16 ***
    ## maturity               1   18693   18693 305.451 <2e-16 ***
    ## trap_type:maturity     1     326     326   5.332 0.0209 *  
    ## Residuals          65183 3989011      61                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 130 observations deleted due to missingness

``` r
nofish_maturity_tukey <- TukeyHSD(nofish_maturity_aov)
nofish_maturity_tukey$`trap_type:maturity`
```

    ##                                             diff        lwr        upr
    ## UNMODIFIED:immature-MODIFIED:immature -0.9302315 -1.1691878 -0.6912751
    ## MODIFIED:mature-MODIFIED:immature     -1.2644424 -1.5303845 -0.9985004
    ## UNMODIFIED:mature-MODIFIED:immature   -1.8979688 -2.1357075 -1.6602300
    ## MODIFIED:mature-UNMODIFIED:immature   -0.3342109 -0.5632469 -0.1051750
    ## UNMODIFIED:mature-UNMODIFIED:immature -0.9677373 -1.1633185 -0.7721560
    ## UNMODIFIED:mature-MODIFIED:mature     -0.6335263 -0.8612917 -0.4057610
    ##                                              p adj
    ## UNMODIFIED:immature-MODIFIED:immature 3.153033e-14
    ## MODIFIED:mature-MODIFIED:immature     0.000000e+00
    ## UNMODIFIED:mature-MODIFIED:immature   0.000000e+00
    ## MODIFIED:mature-UNMODIFIED:immature   1.020389e-03
    ## UNMODIFIED:mature-UNMODIFIED:immature 0.000000e+00
    ## UNMODIFIED:mature-MODIFIED:mature     5.399681e-12

## <a name="length"></a> **Catch and length data of mature fish**

**Average length of catch versus length at first maturity (Lmat). Take
the difference for each fish in the data against its length at first
maturity and then calculate a weighted value for modified versus
traditional traps where a value above 0 represents a fish above Lmat and
a value below represents a fish below Lmat.**

Transform count value to that number of observations:
<https://stackoverflow.com/questions/70759069/transform-count-column-into-number-of-rows>.

``` r
maturity_dist <- maturity %>% 
  mutate(number_of_fish = if_else(is.na(number_of_fish), 1, number_of_fish)) %>% # replacing NAs with the value of 1
  tidyr::uncount(., number_of_fish, .remove = TRUE) %>% # expanding number of fish to number of observations
  mutate(length_dist = median_length-Lm) %>%
  group_by(scientific_name, trap_type) %>%
  mutate(avg_length_dist = mean(length_dist)) %>% #### a positive value here = mature; a negative value = immature 
  ungroup()

maturity_dist %>% group_by(scientific_name) %>%
  filter(., n_distinct(avg_length_dist) >= 2) %>% # filters out the observations aren't in both categories (takes out species that only have 1 unique mean dist value so only 1 trap type)
  ungroup() %>%
  ggplot(., aes(x=scientific_name, y=avg_length_dist, color = trap_type)) +
  geom_hline(yintercept = 0, lty = "dotted") +
  geom_line(aes(group = scientific_name), color="grey14") +
  geom_point() + theme_bw() + 
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(y="Mean distance from Lm (cm)", x="Genus species", color="Trap Type")
```

    ## Warning: Removed 9985 row(s) containing missing values (geom_path).

    ## Warning: Removed 239234 rows containing missing values (geom_point).

![](CUE-maturity-length-analysis_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

## <a name="freq_plots"></a> **Length Frequency plots of top species**

**Length frequency of top 3-5 species in modified versus traditional
(different colors) with Lmat etc. indicators pulled from Fishbase.**

Check this calculation for each fish not just each survey id???

<http://derekogle.com/fishR/2017-07-28-JoyPlot>

From the `species_keep` df:

Siganus sutor = 311813  
Lethrinus nebulosus = 51612  
Scarus ghobban = 34253  
Siganus canaliculatus = 23173  
Leptoscarus vaigiensis = 15758

``` r
# the column we want to plot: maturity$length_corrected
maturity$length_corrected <- factor(maturity$length_corrected, levels=c("0-10", "11-15","16-20","21-25","26-30",
                                                                        "31-35","36-40","41-45", "46-50", "51-60",
                                                                        "61-70", "71-80", "81-90", ">90"))

maturity <- maturity %>% filter(!is.na(length_corrected)) %>%
  group_by(length_corrected, scientific_name, trap_type) %>%
  mutate(count.per.bin = sum(number_of_fish)) %>%
  ungroup()

maturity2022_topspp <- maturity %>% subset(year=="2022") %>% 
  subset(scientific_name == "Siganus sutor" | scientific_name == "Lethrinus nebulosus" |
           scientific_name == "Scarus ghobban" | scientific_name == "Leptoscarus vaigiensis" |
           scientific_name == "Siganus canaliculatus") %>%
    mutate(Lm_range = case_when(
    Lm >= 0 & Lm <= 10.5 ~ "0-10",
    Lm >= 10.5 & Lm <= 15.4 ~ "11-15",
    Lm >= 15.5 & Lm <= 20.4 ~ "16-20",
    Lm >= 20.5 & Lm <= 25.4 ~ "21-25",
    Lm >= 25.5 & Lm <= 30.4 ~ "26-30",
    Lm >= 30.5 & Lm <= 35.4 ~ "31-35",
    Lm >= 35.5 & Lm <= 40.4 ~ "36-40",
    Lm >= 40.5 & Lm <= 45.4 ~ "41-45",
    Lm >= 45.5 & Lm <= 50.4 ~ "46-50",
    Lm >= 50.5 & Lm <= 60.4 ~ "51-60",
    Lm >= 60.5 & Lm <= 70.4 ~ "61-70",
    Lm >= 70.5 & Lm <= 80.4 ~ "71-80",
    Lm >= 80.5 & Lm <= 90.4 ~ "81-90",
    Lm > 90.5 ~ ">90")) 
  
  
maturity2022_topspp %>% 
  mutate(number_of_fish = if_else(is.na(number_of_fish), 1, number_of_fish)) %>% # replacing NAs with the value of 1
  tidyr::uncount(., number_of_fish, .remove = TRUE) %>% # expanding number of fish to number of observations
  ggplot(., aes(x=length_corrected, fill=trap_type, color=trap_type)) + 
  geom_bar(alpha=0.3) +
  geom_vline(data = maturity2022_topspp, mapping = aes(xintercept = Lm_range), lty = "dotted") +
  theme_bw() + xlab("Length (cm)") + ylab("Frequency") +
  theme(axis.text.x.bottom = element_text(colour = 'black', angle = 60, hjust = 1),
        axis.text.y = element_text(colour = 'black', size = 8, face = 'italic')) + 
  theme(panel.border = element_blank(),
   panel.grid.major = element_blank(),
   panel.grid.minor = element_blank(),
   strip.text.x = element_text(size = 9, color = "black", face = "bold.italic"),
   axis.line = element_line(colour = "black")) +
  theme(axis.title.y = element_text(size = 12, face = "bold", margin = margin(t = 0, r = 10, b = 0, l = 0))) +
  theme(axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  facet_wrap(~scientific_name, scales = "free_y") +
  ggtitle("Siganus canaliculatus Lm range = 0-10, can edit on in other program")
```

![](CUE-maturity-length-analysis_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

## Fishbase R

<https://github.com/ropensci/rfishbase>

Left off at FishBase R having SSL certificate problems. Download a and b
from fishbase for top 5 and make script for this.

``` r
#remotes::install_github("ropensci/rfishbase")
library("rfishbase")
#library(curl)
#install.packages("curl", type = "source")
```

``` r
#FB <- fb_tbl("ecology")
```

Error:

`Error in curl::curl_fetch_memory(file, handle): SSL certificate problem: certificate has expired   Error in curl::curl_fetch_memory(file, handle): SSL certificate problem: certificate has expired   Error in curl::curl_fetch_memory(file, handle): SSL certificate problem: certificate has expired   Error in curl::curl_fetch_memory(file, handle): SSL certificate problem: certificate has expired   Warning in curl::curl_fetch_memory(url, handle = handle) :     SSL certificate problem: certificate has expired   Warning in curl::curl_fetch_memory(url, handle = handle) :     SSL certificate problem: certificate has expired   Error in rbind(deparse.level, ...) :      numbers of columns of arguments do not match`
