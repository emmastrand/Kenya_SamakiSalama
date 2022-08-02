CUE, maturity, and length analysis of Fishing Landings dataset
================
Author: Emma Strand; <emma_strand@uri.edu>

## Questions for Austin and Clay

## Summary of dataset

Modified traps data:  
2022: January, February, March, April, May

Unmodified traps data: 2021: January, June, August, September 2022:
January, February, March, April, May, June

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
data <- read_excel("data/Fishlandings-cleaned-clay-June_updated-IW.xlsx") #read in excel file 

data <- data %>% separate(Operation_date, c("year", "month", "day"), remove = FALSE) # creating new columns with month year and date
data$month <- as.numeric(data$month) #changing this column to numeric instead of a character (needed for next fxn)
data$month <- month.abb[data$month] #changing numeric months to month names 
```

## <a name="catch_effort"></a> **Total catch (grams) per unit effort (trap set)**

**Total catch per unit effort between modified and traditional traps. It
would be great to see this as grams captured per trap set.**

Grouping by fisher\_id but this might be effective to group by
enumerator once I have correct list of names. There are 3 boat trips
recorded with the exact same fish data that are under 3 different fisher
ID names but all the same enumerator.. come back to this in QC.

Goal: grams captured per trap set.

``` r
modified_trap_df <- data %>% unite(survey_id, Operation_date, fisher_id, sep = " ", remove = FALSE) %>%
  dplyr::group_by(survey_id) %>% # group by survey id
  mutate(total_catch = sum(number_of_fish), #count the number of fish caught for each survey id
         grams_per_trap = total_weight_kg/total_traps_collected, #divide total weight for survey id by total traps 
         catch_per_trap = total_catch/total_traps_collected) %>% #divide total catch per survey id by total traps 
  dplyr::ungroup(survey_id) %>% #ungroup by this column  
  subset(trap_type == "MODIFIED" | trap_type == "UNMODIFIED") %>% #subset for only modified and unmodified traps 
  select(survey_id, enumerator, trap_type, `No. of fishers in crew`, landing_site, total_catch, month, year, grams_per_trap, catch_per_trap) %>%
  distinct() 
```

### Plotting figures.

Catch per trap

``` r
# basic total catch per trap with no other variables 
modified_trap_df %>% filter(catch_per_trap < 1500) %>%
  ggplot(aes(x=trap_type, y=catch_per_trap, color=trap_type)) +
  geom_boxplot(aes(color=trap_type), outlier.size = 0, lwd=0.5) +
    geom_point(aes(fill=trap_type), pch = 21, size=1) +
  theme_bw() + 
  ylab("Total catch per trap") + xlab("Type of trap") +
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
  ylab("Total catch per trap") + xlab("Type of trap") +
  theme(axis.text.x = element_text(vjust = 1.1)) #Set the text angle
```

    ## Warning: Removed 210 rows containing non-finite values (stat_boxplot).

    ## Warning: Removed 210 rows containing missing values (geom_point).

![](CUE-maturity-length-analysis_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

``` r
# by landing site
modified_trap_df %>% 
  ggplot(aes(x=trap_type, y=catch_per_trap, color=trap_type)) + 
  facet_wrap(~landing_site) +
  geom_boxplot(aes(color=trap_type), outlier.size = 0, lwd=0.5) +
    geom_point(aes(fill=trap_type), pch = 21, size=1) +
  theme_bw() + 
  ylab("Total catch per trap") + xlab("Type of trap") +
  theme(axis.text.x = element_text(vjust = 1.1)) #Set the text angle
```

    ## Warning: Removed 210 rows containing non-finite values (stat_boxplot).
    ## Removed 210 rows containing missing values (geom_point).

![](CUE-maturity-length-analysis_files/figure-gfm/unnamed-chunk-4-3.png)<!-- -->

``` r
# by month and year 
modified_trap_df %>% unite(ym, year, month, sep = " ", remove = FALSE) %>% filter(catch_per_trap < 1500) %>%
  ggplot(aes(x=month, y=catch_per_trap, color=trap_type)) + 
  facet_wrap(~year, scales = "free_y") +
  geom_boxplot(aes(color=trap_type), outlier.size = 0, lwd=0.5) +
  #geom_point(aes(x=ym, y=catch_per_trap, fill=trap_type), pch = 21, size=1) +
  theme_classic() + 
  ylab("Total catch per trap") + xlab("Time of year") +
  theme(axis.text.x = element_text(vjust = 1.1, hjust=1.1, angle=60)) #Set the text angle
```

![](CUE-maturity-length-analysis_files/figure-gfm/unnamed-chunk-4-4.png)<!-- -->

Grams per trap

``` r
# basic grams per trap plot with no other variables 
modified_trap_df %>% 
  ggplot(aes(x=trap_type, y=grams_per_trap, color=trap_type)) + 
  geom_boxplot(aes(color=trap_type), outlier.size = 0, lwd=0.5) +
    geom_point(aes(fill=trap_type), pch = 21, size=1) +
  theme_bw() + 
  ylab("Grams of fish per trap") + xlab("Type of trap") +
  theme(axis.text.x = element_text(vjust = 1.1)) #Set the text angle
```

    ## Warning: Removed 94 rows containing non-finite values (stat_boxplot).

    ## Warning: Removed 94 rows containing missing values (geom_point).

![](CUE-maturity-length-analysis_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
# visually seeing if this differs by fisherman 
modified_trap_df %>% 
  ggplot(aes(x=trap_type, y=grams_per_trap, color=trap_type)) + 
  facet_wrap(~enumerator) +
  geom_boxplot(aes(color=trap_type), outlier.size = 0, lwd=0.5) +
    geom_point(aes(fill=trap_type), pch = 21, size=1) +
  theme_bw() + 
  ylab("Grams of fish per trap") + xlab("Type of trap") +
  theme(axis.text.x = element_text(vjust = 1.1)) #Set the text angle
```

    ## Warning: Removed 94 rows containing non-finite values (stat_boxplot).
    ## Removed 94 rows containing missing values (geom_point).

![](CUE-maturity-length-analysis_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

``` r
# visually seeing if this differs by landing site 
modified_trap_df %>% 
  ggplot(aes(x=trap_type, y=grams_per_trap, color=trap_type)) + 
  facet_wrap(~landing_site) +
  geom_boxplot(aes(color=trap_type), outlier.size = 0, lwd=0.5) +
    geom_point(aes(fill=trap_type), pch = 21, size=1) +
  theme_bw() + 
  ylab("Grams of fish per trap") + xlab("Type of trap") +
  theme(axis.text.x = element_text(vjust = 1.1)) #Set the text angle
```

    ## Warning: Removed 94 rows containing non-finite values (stat_boxplot).
    ## Removed 94 rows containing missing values (geom_point).

![](CUE-maturity-length-analysis_files/figure-gfm/unnamed-chunk-5-3.png)<!-- -->

``` r
# by month and year 
modified_trap_df %>% unite(ym, year, month, sep = " ", remove = FALSE) %>% #filter(catch_per_trap < 1500) %>%
  ggplot(aes(x=month, y=grams_per_trap, color=trap_type)) + 
  facet_wrap(~year, scales = "free_y") +
  geom_boxplot(aes(color=trap_type), outlier.size = 0, lwd=0.5) +
 # geom_point(aes(x=month, group=trap_type, y=grams_per_trap, fill=trap_type), pch = 21, size=1) +
  theme_classic() + 
  ylab("Grams of fish per trap") + xlab("Time of year") +
  theme(axis.text.x = element_text(vjust = 1.1, hjust=1.1, angle=60)) #Set the text angle
```

    ## Warning: Removed 94 rows containing non-finite values (stat_boxplot).

![](CUE-maturity-length-analysis_files/figure-gfm/unnamed-chunk-5-4.png)<!-- -->

The relationship between grams per trap and total catch per trap.

``` r
modified_trap_df %>% filter(catch_per_trap < 1500) %>%
  ggplot(aes(x=catch_per_trap, y=grams_per_trap, color=trap_type)) +
  theme_bw() + xlab("total catch per trap") + ylab("grams per trap") +
  geom_smooth(aes(fill=trap_type), method="loess", se=TRUE, fullrange=TRUE, level=0.95, color="black") +
  geom_point(aes(fill=trap_type), pch = 21, size=1)
```

    ## `geom_smooth()` using formula 'y ~ x'

    ## Warning: Removed 1 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 30 rows containing missing values (geom_smooth).

    ## Warning: Removed 1 rows containing missing values (geom_point).

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
UN <- modified_trap_df %>% subset(trap_type == "MODIFIED") %>% filter(!is.na(grams_per_trap)) %>% filter(!is.na(catch_per_trap))
MOD <- modified_trap_df %>% subset(trap_type == "UNMODIFIED") %>% filter(!is.na(grams_per_trap)) %>% filter(!is.na(catch_per_trap))
```

#### Grams per trap

``` r
var.test(UN$grams_per_trap, MOD$grams_per_trap)
```

    ## 
    ##  F test to compare two variances
    ## 
    ## data:  UN$grams_per_trap and MOD$grams_per_trap
    ## F = 0.75266, num df = 755, denom df = 1290, p-value = 1.576e-05
    ## alternative hypothesis: true ratio of variances is not equal to 1
    ## 95 percent confidence interval:
    ##  0.6635431 0.8555765
    ## sample estimates:
    ## ratio of variances 
    ##          0.7526603

``` r
t.test(grams_per_trap~trap_type, data = modified_trap_df, var.equal = FALSE)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  grams_per_trap by trap_type
    ## t = -0.99452, df = 1939.3, p-value = 0.3201
    ## alternative hypothesis: true difference in means between group MODIFIED and group UNMODIFIED is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.06561682  0.02145999
    ## sample estimates:
    ##   mean in group MODIFIED mean in group UNMODIFIED 
    ##                0.8929056                0.9149840

#### Total catch per trap

``` r
var.test(UN$catch_per_trap, MOD$catch_per_trap)
```

    ## 
    ##  F test to compare two variances
    ## 
    ## data:  UN$catch_per_trap and MOD$catch_per_trap
    ## F = 0.58716, num df = 755, denom df = 1290, p-value = 1.442e-15
    ## alternative hypothesis: true ratio of variances is not equal to 1
    ## 95 percent confidence interval:
    ##  0.5176356 0.6674425
    ## sample estimates:
    ## ratio of variances 
    ##          0.5871567

``` r
t.test(catch_per_trap~trap_type, data = modified_trap_df, var.equal = FALSE)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  catch_per_trap by trap_type
    ## t = 10.041, df = 1903.5, p-value < 2.2e-16
    ## alternative hypothesis: true difference in means between group MODIFIED and group UNMODIFIED is not equal to 0
    ## 95 percent confidence interval:
    ##  25.27546 37.54562
    ## sample estimates:
    ##   mean in group MODIFIED mean in group UNMODIFIED 
    ##                 60.88043                 29.46989

#### Total catch per trap vs weight in grams per trap.

We use a linear mixed model for this so we can include other variables
like fisherman and landing site.

Grams per trap is log transformed.

I’m not sure yet if this is the best way to do this…

``` r
# unmodified
un_catch_model <- lmer(log(grams_per_trap) ~ catch_per_trap + (1|enumerator) + (1|landing_site), na.action=na.omit, data=UN)
```

    ## boundary (singular) fit: see help('isSingular')

``` r
qqPlot(residuals(un_catch_model)) 
```

![](CUE-maturity-length-analysis_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

    ## [1] 748 234

``` r
hist(residuals(un_catch_model))
```

![](CUE-maturity-length-analysis_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->

``` r
# modified
mod_catch_model <- lmer(log(grams_per_trap) ~ catch_per_trap + (1|enumerator) + (1|landing_site), na.action=na.omit, data=MOD)
```

    ## boundary (singular) fit: see help('isSingular')

``` r
qqPlot(residuals(mod_catch_model)) 
```

![](CUE-maturity-length-analysis_files/figure-gfm/unnamed-chunk-10-3.png)<!-- -->

    ## [1]  396 1263

``` r
hist(residuals(mod_catch_model))
```

![](CUE-maturity-length-analysis_files/figure-gfm/unnamed-chunk-10-4.png)<!-- -->

``` r
summary(un_catch_model)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: log(grams_per_trap) ~ catch_per_trap + (1 | enumerator) + (1 |  
    ##     landing_site)
    ##    Data: UN
    ## 
    ## REML criterion at convergence: 1102.1
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.9314 -0.5140  0.0612  0.6536  3.3642 
    ## 
    ## Random effects:
    ##  Groups       Name        Variance Std.Dev.
    ##  landing_site (Intercept) 0.0143   0.1196  
    ##  enumerator   (Intercept) 0.0000   0.0000  
    ##  Residual                 0.2436   0.4935  
    ## Number of obs: 756, groups:  landing_site, 4; enumerator, 3
    ## 
    ## Fixed effects:
    ##                  Estimate Std. Error t value
    ## (Intercept)    -0.2839420  0.0681428  -4.167
    ## catch_per_trap  0.0021206  0.0002958   7.169
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## ctch_pr_trp -0.276
    ## optimizer (nloptwrap) convergence code: 0 (OK)
    ## boundary (singular) fit: see help('isSingular')

``` r
summary(mod_catch_model)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: log(grams_per_trap) ~ catch_per_trap + (1 | enumerator) + (1 |  
    ##     landing_site)
    ##    Data: MOD
    ## 
    ## REML criterion at convergence: 2285.6
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.2306 -0.6223  0.1676  0.6442  2.9521 
    ## 
    ## Random effects:
    ##  Groups       Name        Variance  Std.Dev. 
    ##  landing_site (Intercept) 1.655e-16 1.286e-08
    ##  enumerator   (Intercept) 3.057e-18 1.748e-09
    ##  Residual                 3.387e-01 5.820e-01
    ## Number of obs: 1291, groups:  landing_site, 4; enumerator, 3
    ## 
    ## Fixed effects:
    ##                  Estimate Std. Error t value
    ## (Intercept)    -0.2785068  0.0172781 -16.119
    ## catch_per_trap  0.0009228  0.0002039   4.525
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## ctch_pr_trp -0.348
    ## optimizer (nloptwrap) convergence code: 0 (OK)
    ## boundary (singular) fit: see help('isSingular')

``` r
Anova(un_catch_model, ddf="lme4", type='III')
```

    ## Analysis of Deviance Table (Type III Wald chisquare tests)
    ## 
    ## Response: log(grams_per_trap)
    ##                 Chisq Df Pr(>Chisq)    
    ## (Intercept)    17.363  1  3.088e-05 ***
    ## catch_per_trap 51.398  1  7.543e-13 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
Anova(mod_catch_model, ddf="lme4", type='III')
```

    ## Analysis of Deviance Table (Type III Wald chisquare tests)
    ## 
    ## Response: log(grams_per_trap)
    ##                  Chisq Df Pr(>Chisq)    
    ## (Intercept)    259.825  1  < 2.2e-16 ***
    ## catch_per_trap  20.476  1  6.037e-06 ***
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
    ## 1 Siganus sutor         MODIFIED         121529
    ## 2 Lethrinus nebulosus   MODIFIED           8717
    ## 3 Scarus ghobban        MODIFIED           7335
    ## 4 Siganus canaliculatus MODIFIED           3221
    ## 5 Acanthurus dussumieri MODIFIED           3201

``` r
# print top 5 species from unmodified traps 
species_list %>% subset(trap_type == "UNMODIFIED") %>%                                    
  arrange(desc(species_catch)) %>% head(5)
```

    ## # A tibble: 5 × 3
    ##   scientific_name        trap_type  species_catch
    ##   <chr>                  <chr>              <dbl>
    ## 1 Siganus sutor          UNMODIFIED        178391
    ## 2 Lethrinus nebulosus    UNMODIFIED         18444
    ## 3 Scarus ghobban         UNMODIFIED         15690
    ## 4 Leptoscarus vaigiensis UNMODIFIED         11393
    ## 5 Parupeneus indicus     UNMODIFIED          6328

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

With just the top 10 species, just plotting catch per trap. I don’t
think I can do weight of just these species by trap because the weight
is for the whole catch.

``` r
### species_df is already subsetted to the top 10 species 
# within each survey id, for each species calculate topspecies_catch as the number fish in that species in that survey
# then calculate the catch per trap as the number of fish of that species over the total number of traps collected
species_df <- species_df %>% unite(survey_id, Operation_date, fisher_id, sep = " ", remove = FALSE) %>%
  dplyr::group_by(survey_id, scientific_name) %>% # group by survey id
  mutate(topspecies_catch = sum(number_of_fish),
         catch_per_trap = topspecies_catch/total_traps_collected) %>% #divide total catch per survey id by total traps 
  dplyr::ungroup(survey_id, scientific_name) #ungroup by this column

# basic grams per trap plot with no other variables 
species_df %>% mutate(trap_type = case_when(
    trap_type == "MODIFIED" ~ "MOD",
    trap_type == "UNMODIFIED" ~ "UNMOD")) %>%
  ggplot(aes(x=trap_type, y=catch_per_trap, color=trap_type)) + 
  geom_boxplot(aes(color=trap_type), outlier.size = 0, lwd=0.5) +
  facet_wrap(~scientific_name, scales = "free_y") +
  theme_classic() + ggtitle("For top 10 most abundant species") +
  ylab("Catch per trap (species catch # / total catch)") + xlab("Genus species") +
  theme(axis.text.x = element_text()) #Set the text angle
```

    ## Warning: Removed 1809 rows containing non-finite values (stat_boxplot).

![](CUE-maturity-length-analysis_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

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
    ## F = 0.38673, num df = 14582, denom df = 1290, p-value < 2.2e-16
    ## alternative hypothesis: true ratio of variances is not equal to 1
    ## 95 percent confidence interval:
    ##  0.3563133 0.4185927
    ## sample estimates:
    ## ratio of variances 
    ##          0.3867316

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

### Creating maturity dataframe based on data pulled from fishbase

``` r
fishbase <- read_excel("data/fishbase.xlsx", sheet = "data") %>% #read in excel file 
  select(scientific_name, Lm, Lm_se_min, Lm_se_max)

maturity <- full_join(data, fishbase, by = "scientific_name") %>% # joining maturity reproduction info with main dataframe
  unite(survey_id, Operation_date, fisher_id, sep = " ", remove = FALSE) %>% #creating a survey id based on date and fisherman
  filter(!is.na(Lm)) %>% # taking out observations that don't have an Lm value so only the top species 
  mutate(Lm_range = case_when( ### making a new column as Lm corrected to then compare to Length_corrected 
    Lm >= 0 & Lm <= 10.5 ~ "0-10",
    Lm >= 10.5 & Lm <= 15.4 ~ "11-15",
    Lm >= 15.5 & Lm <= 20.4 ~ "16-20",
    Lm >= 20.5 & Lm <= 25.4 ~ "21-25",
    Lm >= 25.5 & Lm <= 30.4 ~ "26-30",
    Lm >= 30.5 & Lm <= 35.4 ~ "31-35",
    Lm >= 35.5 & Lm <= 40.4 ~ "36-40",
    Lm >= 40.5 & Lm <= 45.4 ~ "41-45",
    Lm >= 45.5 & Lm <= 50.4 ~ "46-50",
    Lm >= 50.5 & Lm <= 75 ~ ">50",
    Lm > 75 ~ ">75")) %>%
  mutate(Lm_comparison = case_when( ### I can't compare character columns so making a new column to use in comparison
    Lm_range == "0-10" ~ 1,
    Lm_range == "11-15" ~ 11,
    Lm_range == "16-20" ~ 16,
    Lm_range == "21-25" ~ 21,
    Lm_range == "26-30" ~ 26,
    Lm_range == "31-35" ~ 31,
    Lm_range == "36-40" ~ 36,
    Lm_range == "41-45" ~ 41,
    Lm_range == "46-50" ~ 46,
    Lm_range == ">50" ~ 50,
    Lm_range == ">75" ~ 75)) %>%
  mutate(Length_comparison = case_when( ### I can't compare character columns so making a new column to use in comparison
    length_corrected == "0-10" ~ 1,
    length_corrected == "11-15" ~ 11,
    length_corrected == "16-20" ~ 16,
    length_corrected == "21-25" ~ 21,
    length_corrected == "26-30" ~ 26,
    length_corrected == "31-35" ~ 31,
    length_corrected == "36-40" ~ 36,
    length_corrected == "41-45" ~ 41,
    length_corrected == "46-50" ~ 46,
    length_corrected == ">50" ~ 50,
    length_corrected == ">75" ~ 75))
  
### there is probably a more efficient way to do the above this works for now 

maturity$Length_comparison <- as.numeric(maturity$Length_comparison)
maturity$Lm_comparison <- as.numeric(maturity$Lm_comparison)

maturity <- maturity %>% 
  mutate(maturity = if_else(length_corrected >= Lm_comparison, "mature", "immature")) %>%
  subset(trap_type == "MODIFIED" | trap_type == "UNMODIFIED") %>% #subsetting to only the modified and unmodified traps 
  mutate(nofish_pertrap = number_of_fish/total_traps_collected) #per species per survey id
```

Immaure and mature fish catch comparison

``` r
maturity %>% select(number_of_fish, trap_type, maturity, scientific_name, total_traps_collected, nofish_pertrap) %>% 
  na.omit() %>% 
  ggplot(., aes(x=scientific_name, y=nofish_pertrap, color=maturity)) +
  geom_boxplot() + theme_classic() + 
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  xlab("Species") +
  ylab("Number of fish per trap set")
```

    ## Warning: Removed 24 rows containing non-finite values (stat_boxplot).

![](CUE-maturity-length-analysis_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
maturity %>% select(number_of_fish, trap_type, maturity, scientific_name, total_traps_collected, nofish_pertrap) %>% 
  na.omit() %>%  
  ggplot(., aes(x=maturity, y=nofish_pertrap, color=trap_type)) +
  geom_boxplot() + theme_classic() + 
  facet_wrap(~trap_type, scales = "free_y") +
  #theme(axis.text.x = element_text(angle=60, hjust=1)) +
  xlab("Maturity") +
  ylab("Number of fish per trap set")
```

    ## Warning: Removed 24 rows containing non-finite values (stat_boxplot).

![](CUE-maturity-length-analysis_files/figure-gfm/unnamed-chunk-16-2.png)<!-- -->

Statistics on the above…

Circle back to the padj values coming out to “0.000000e+00”.

Double check `trap_type+maturity` vs `trap_type*maturity`.

``` r
nofish_maturity_aov <- aov(number_of_fish ~ trap_type*maturity, data = maturity)
summary(nofish_maturity_aov)
```

    ##                       Df  Sum Sq Mean Sq F value Pr(>F)    
    ## trap_type              1   41359   41359 496.606 <2e-16 ***
    ## maturity               1   24723   24723 296.861 <2e-16 ***
    ## trap_type:maturity     1       2       2   0.022  0.883    
    ## Residuals          44568 3711755      83                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 251 observations deleted due to missingness

``` r
nofish_maturity_tukey <- TukeyHSD(nofish_maturity_aov)
nofish_maturity_tukey$`trap_type:maturity`
```

    ##                                             diff        lwr        upr
    ## UNMODIFIED:immature-MODIFIED:immature -2.1288036 -2.4865221 -1.7710851
    ## MODIFIED:mature-MODIFIED:immature     -1.5164503 -1.9069269 -1.1259738
    ## UNMODIFIED:mature-MODIFIED:immature   -3.6180805 -3.9686493 -3.2675118
    ## MODIFIED:mature-UNMODIFIED:immature    0.6123533  0.2902488  0.9344577
    ## UNMODIFIED:mature-UNMODIFIED:immature -1.4892770 -1.7616330 -1.2169210
    ## UNMODIFIED:mature-MODIFIED:mature     -2.1016302 -2.4157754 -1.7874850
    ##                                              p adj
    ## UNMODIFIED:immature-MODIFIED:immature 0.000000e+00
    ## MODIFIED:mature-MODIFIED:immature     3.297362e-14
    ## UNMODIFIED:mature-MODIFIED:immature   0.000000e+00
    ## MODIFIED:mature-UNMODIFIED:immature   6.193922e-06
    ## UNMODIFIED:mature-UNMODIFIED:immature 0.000000e+00
    ## UNMODIFIED:mature-MODIFIED:mature     0.000000e+00

## <a name="length"></a> **Catch and length data of mature fish**

**Average length of catch versus length at first maturity (Lmat). Take
the difference for each fish in the data against its length at first
maturity and then calculate a weighted value for modified versus
traditional traps where a value above 0 represents a fish above Lmat and
a value below represents a fish below Lmat.**

We only have bins for the length values.. so this might have to be by
5s. i.e. catch = 11-15; Lm 21-25. Can take median values and do that
calculation?

I’m not sure this is the best way to do this…

Transform count value to that number of observations:
<https://stackoverflow.com/questions/70759069/transform-count-column-into-number-of-rows>.

``` r
maturity_dist <- maturity %>% 
  select(survey_id, number_of_fish, scientific_name, trap_type, length_corrected, Lm, Lm_range, Lm_comparison, Length_comparison, maturity) %>%
  mutate(number_of_fish = if_else(is.na(number_of_fish), 1, number_of_fish)) %>% # replacing NAs with the value of 1
  tidyr::uncount(., number_of_fish, .remove = TRUE) %>% # expanding number of fish to number of observations
  #### notes on below fxn --- maybe not the best way to do this? 0-10 is a bigger bin than 11-15 so median value isn't as representative?
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
    length_corrected == ">50" ~ 50,
    length_corrected == ">75" ~ 75)) %>%
  mutate(length_dist = median_length-Lm) %>%
  group_by(scientific_name, trap_type) %>%
  mutate(avg_length_dist = mean(length_dist)) %>% #### a positive value here = mature; a negative value = immature 
  ungroup()

maturity_dist %>% group_by(scientific_name) %>%
  filter(., n_distinct(avg_length_dist) >= 2) %>% # filters out the observations aren't in both categories (takes out species that only have 1 unique mean dist value so only 1 trap type)
  ungroup() %>%
  ggplot(., aes(x=scientific_name, y=avg_length_dist, color = trap_type)) +
  geom_line(aes(group = scientific_name), color="grey14") +
  geom_point() + theme_bw() + 
  geom_hline(yintercept = 0, lty = "dotted") +
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(y="Mean distance from Lm", x="Genus species", color="Trap Type")
```

    ## Warning: Removed 375 row(s) containing missing values (geom_path).

    ## Warning: Removed 198675 rows containing missing values (geom_point).

![](CUE-maturity-length-analysis_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

## <a name="freq_plots"></a> **Length Frequency plots of top species**

**Length frequency of top 3-5 species in modified versus traditional
(different colors) with Lmat etc. indicators pulled from Fishbase.**

Check this calculation for each fish not just each survey id???

<http://derekogle.com/fishR/2017-07-28-JoyPlot>

From the `species_keep` df:

Siganus sutor = 299920  
Lethrinus nebulosus = 27161  
Scarus ghobban = 23025  
Leptoscarus vaigiensis = 14070  
Siganus canaliculatus = 9509

``` r
# the column we want to plot: maturity$length_corrected
maturity$length_corrected <- factor(maturity$length_corrected, levels=c("0-10", "11-15","16-20","21-25","26-30","31-35","36-40",
                                                                        "41-45", "46-50", ">75"))

maturity <- maturity %>% filter(!is.na(length_corrected)) %>%
  group_by(length_corrected, scientific_name, trap_type) %>%
  mutate(count.per.bin = sum(number_of_fish)) %>%
  ungroup()

maturity2022_topspp <- maturity %>% subset(year=="2022") %>% 
  subset(scientific_name == "Siganus sutor" | scientific_name == "Lethrinus nebulosus" |
           scientific_name == "Scarus ghobban" | scientific_name == "Leptoscarus vaigiensis" |
           scientific_name == "Siganus canaliculatus")
  
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
