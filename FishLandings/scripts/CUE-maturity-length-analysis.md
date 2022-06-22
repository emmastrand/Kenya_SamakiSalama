CUE, maturity, and length analysis of Fishing Landings dataset
================
Author: Emma Strand; <emma_strand@uri.edu>

## Questions for Austin and Clay

1.) Modified traps were only recorded in February.. is this a problem?
The month of June for unmodified traps might be influencing some of
these conclusions.. can we make that claim across all months?

**To-Do**:  
- Model for grams per trap vs catch per trap and trape type.  
- ANOVA for top species per trap type.  
- Create datafile from fishbase info.  
- \#3-5 on below aims.  
- Relative abundance plots in other script.  
- Summary points, results, and next steps suggestions finalize

## Aims and Results

**1. Total catch per unit effort between modified and traditional traps.
It would be great to see this as grams captured per trap set.**

Results:  
- Modified traps had a significantly higher total catch per trap set.
This pattern holds for all landing sites and individual fisherman (it
does not appear that either of those variables influence this result).  
- There is no significant difference in weight in grams per trap set.
This pattern holds for all individual fisherman, but Mayungu landing
site might drive this result more than the other sites.  
- *insert statement about grams per trap vs catch per trap and trap
type. finish stats below for this.*

**2. Species catch per unit effort between modified and traditional
traps. Take the top 3-5 species and run \#1 for them separately.** s
Results:  
- Top species caught across all surveys (not in order): *Leptoscarus
vaigiensis*, *Lethrinus nebulosus*, *Scarus ghobban*, *Siganus
canaliculutus*, and *Siganus sutor*.  
- Top species caught in modified traps: 1. *Siganus sutor*, 2.
*Lethrinus nebulosus*, 3. *Scarus ghobban*, 4. *Naso annulatus*, and 5.
*Leptoscarus vaigiensis*.  
- Top species caught in unmodified traps: 1. *Siganus sutor*, 2.
*Leptoscarus vaigiensis*, 3. *Siganus canaliculutus*, 4. *Lethrinus
nebulosus*, and 5. *Scarus ghobban*.

*Come back to running ANOVA on if type of trap affects the catch for
each species.*

**3. Total mature fish catch per unit effort between modified and
traditional traps. This will have to be for the top 3-5 species
separately. Go to Fishbase and find the length at first maturity for
that particular species, then assign each fish a “mature” or “immature”
status in the data and calculate.**

Results:

**4. Average length of catch versus length at first maturity (Lmat).
Take the difference for each fish in the data against its length at
first maturity and then calculate a weighted value for modified versus
traditional traps where a value above 0 represents a fish above Lmat and
a value below represents a fish below Lmat.**

Results:

**5. Length frequency of top 3-5 species in modified versus traditional
(different colors) with Lmat etc. indicators pulled from Fishbase.**

Results:

## Contents

-   [**Reading in datafiles**](#data)  
-   [**Total catch (grams) per unit effort (trap
    set)**](#catch_effort)  
-   [**Calculate top species caught**](#species)  
-   [**Top species stats per trap**](#species_pertrap)  
-   [**Creating database from Fishbase**](#fishbase)  
-   [**Catch per unit effort for top species by maturity**](#maturity)  
-   [**Catch and length data of mature fish**](#length)

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
data <- read_excel("data/Fishlandings-cleaned-21052022-May.xlsx") #read in excel file 

# creating a month column based on operation date column 
data$month <- data$Operation_date #duplicating the operation date column to then modify in next steps 
data$month <- format(data$month, "%m") #extracting only the month in the new column
data$month <- as.numeric(data$month) #changing this column to numeric instead of a character (needed for next fxn)
data$month <- month.abb[data$month] #changing numeric months to month names 
```

## <a name="catch_effort"></a> **Total catch (grams) per unit effort (trap set)**

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
  select(survey_id, enumerator, trap_type, `No. of fishers in crew`, landing_site, total_catch, month, grams_per_trap, catch_per_trap) %>%
  distinct() 
```

### Plotting figures.

Catch per trap

``` r
# basic total catch per trap with no other variables 
modified_trap_df %>% 
  ggplot(aes(x=trap_type, y=catch_per_trap, color=trap_type)) +
  geom_boxplot(aes(color=trap_type), outlier.size = 0, lwd=0.5) +
    geom_point(aes(fill=trap_type), pch = 21, size=1) +
  theme_bw() + 
  ylab("Total catch per trap") + xlab("Type of trap") +
  theme(axis.text.x = element_text(vjust = 1.1)) #Set the text angle
```

    ## Warning: Removed 33 rows containing non-finite values (stat_boxplot).

    ## Warning: Removed 33 rows containing missing values (geom_point).

![](CUE-maturity-length-analysis_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
# by fisherman 
modified_trap_df %>% 
  ggplot(aes(x=trap_type, y=catch_per_trap, color=trap_type)) + 
  facet_wrap(~enumerator) +
  geom_boxplot(aes(color=trap_type), outlier.size = 0, lwd=0.5) +
    geom_point(aes(fill=trap_type), pch = 21, size=1) +
  theme_bw() + 
  ylab("Grams of fish per trap") + xlab("Type of trap") +
  theme(axis.text.x = element_text(vjust = 1.1)) #Set the text angle
```

    ## Warning: Removed 33 rows containing non-finite values (stat_boxplot).
    ## Removed 33 rows containing missing values (geom_point).

![](CUE-maturity-length-analysis_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

``` r
# by landing site
modified_trap_df %>% 
  ggplot(aes(x=trap_type, y=catch_per_trap, color=trap_type)) + 
  facet_wrap(~landing_site) +
  geom_boxplot(aes(color=trap_type), outlier.size = 0, lwd=0.5) +
    geom_point(aes(fill=trap_type), pch = 21, size=1) +
  theme_bw() + 
  ylab("Grams of fish per trap") + xlab("Type of trap") +
  theme(axis.text.x = element_text(vjust = 1.1)) #Set the text angle
```

    ## Warning: Removed 33 rows containing non-finite values (stat_boxplot).
    ## Removed 33 rows containing missing values (geom_point).

![](CUE-maturity-length-analysis_files/figure-gfm/unnamed-chunk-4-3.png)<!-- -->

``` r
# by month
modified_trap_df %>% 
  ggplot(aes(x=trap_type, y=catch_per_trap, color=trap_type)) + 
  facet_wrap(~month) +
  geom_boxplot(aes(color=trap_type), outlier.size = 0, lwd=0.5) +
    geom_point(aes(fill=trap_type), pch = 21, size=1) +
  theme_bw() + 
  ylab("Grams of fish per trap") + xlab("Type of trap") +
  theme(axis.text.x = element_text(vjust = 1.1)) #Set the text angle
```

    ## Warning: Removed 33 rows containing non-finite values (stat_boxplot).
    ## Removed 33 rows containing missing values (geom_point).

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

    ## Warning: Removed 32 rows containing non-finite values (stat_boxplot).

    ## Warning: Removed 32 rows containing missing values (geom_point).

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

    ## Warning: Removed 32 rows containing non-finite values (stat_boxplot).
    ## Removed 32 rows containing missing values (geom_point).

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

    ## Warning: Removed 32 rows containing non-finite values (stat_boxplot).
    ## Removed 32 rows containing missing values (geom_point).

![](CUE-maturity-length-analysis_files/figure-gfm/unnamed-chunk-5-3.png)<!-- -->

``` r
# by month
modified_trap_df %>% 
  ggplot(aes(x=trap_type, y=grams_per_trap, color=trap_type)) + 
  facet_wrap(~month) +
  geom_boxplot(aes(color=trap_type), outlier.size = 0, lwd=0.5) +
    geom_point(aes(fill=trap_type), pch = 21, size=1) +
  theme_bw() + 
  ylab("Grams of fish per trap") + xlab("Type of trap") +
  theme(axis.text.x = element_text(vjust = 1.1)) #Set the text angle
```

    ## Warning: Removed 32 rows containing non-finite values (stat_boxplot).
    ## Removed 32 rows containing missing values (geom_point).

![](CUE-maturity-length-analysis_files/figure-gfm/unnamed-chunk-5-4.png)<!-- -->

The relationship between grams per trap and total catch per trap.

``` r
modified_trap_df %>% 
  ggplot(aes(x=catch_per_trap, y=grams_per_trap, color=trap_type)) +
  theme_bw() + xlab("total catch per trap") + ylab("grams per trap") +
  geom_smooth(aes(fill=trap_type), method="loess", se=TRUE, fullrange=TRUE, level=0.95, color="black") +
  geom_point(aes(fill=trap_type), pch = 21, size=1)
```

    ## `geom_smooth()` using formula 'y ~ x'

    ## Warning: Removed 38 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 51 rows containing missing values (geom_smooth).

    ## Warning: Removed 38 rows containing missing values (geom_point).

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
UN <- modified_trap_df %>% subset(trap_type == "MODIFIED") %>% na.omit()
MOD <- modified_trap_df %>% subset(trap_type == "UNMODIFIED") %>% na.omit()
```

#### Grams per trap

``` r
var.test(UN$grams_per_trap, MOD$grams_per_trap)
```

    ## 
    ##  F test to compare two variances
    ## 
    ## data:  UN$grams_per_trap and MOD$grams_per_trap
    ## F = 0.68826, num df = 149, denom df = 882, p-value = 0.00479
    ## alternative hypothesis: true ratio of variances is not equal to 1
    ## 95 percent confidence interval:
    ##  0.5439213 0.8900902
    ## sample estimates:
    ## ratio of variances 
    ##          0.6882569

``` r
t.test(grams_per_trap~trap_type, data = modified_trap_df, var.equal = FALSE)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  grams_per_trap by trap_type
    ## t = -1.008, df = 236.13, p-value = 0.3145
    ## alternative hypothesis: true difference in means between group MODIFIED and group UNMODIFIED is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.17341431  0.05602353
    ## sample estimates:
    ##   mean in group MODIFIED mean in group UNMODIFIED 
    ##                0.8711472                0.9298426

#### Total catch per trap

``` r
var.test(UN$catch_per_trap, MOD$catch_per_trap)
```

    ## 
    ##  F test to compare two variances
    ## 
    ## data:  UN$catch_per_trap and MOD$catch_per_trap
    ## F = 0.52472, num df = 149, denom df = 882, p-value = 2.203e-06
    ## alternative hypothesis: true ratio of variances is not equal to 1
    ## 95 percent confidence interval:
    ##  0.4146785 0.6785931
    ## sample estimates:
    ## ratio of variances 
    ##           0.524718

``` r
t.test(catch_per_trap~trap_type, data = modified_trap_df, var.equal = FALSE)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  catch_per_trap by trap_type
    ## t = 3.8431, df = 255.32, p-value = 0.0001534
    ## alternative hypothesis: true difference in means between group MODIFIED and group UNMODIFIED is not equal to 0
    ## 95 percent confidence interval:
    ##  2.209387 6.853403
    ## sample estimates:
    ##   mean in group MODIFIED mean in group UNMODIFIED 
    ##                 15.20394                 10.67255

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

    ## [1] 47 28

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

    ## [1] 300 569

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
    ## REML criterion at convergence: 276.2
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.88549 -0.70461  0.00677  0.62022  2.51131 
    ## 
    ## Random effects:
    ##  Groups       Name        Variance  Std.Dev. 
    ##  landing_site (Intercept) 1.715e-01 4.141e-01
    ##  enumerator   (Intercept) 4.652e-17 6.820e-09
    ##  Residual                 3.301e-01 5.745e-01
    ## Number of obs: 150, groups:  landing_site, 4; enumerator, 3
    ## 
    ## Fixed effects:
    ##                 Estimate Std. Error t value
    ## (Intercept)    -1.039056   0.237055  -4.383
    ## catch_per_trap  0.023858   0.003893   6.128
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## ctch_pr_trp -0.230
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
    ## REML criterion at convergence: 1737.9
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.6143 -0.5831  0.1070  0.6265  2.6695 
    ## 
    ## Random effects:
    ##  Groups       Name        Variance  Std.Dev. 
    ##  enumerator   (Intercept) 1.254e-20 1.120e-10
    ##  landing_site (Intercept) 2.503e-01 5.003e-01
    ##  Residual                 4.085e-01 6.391e-01
    ## Number of obs: 883, groups:  enumerator, 4; landing_site, 3
    ## 
    ## Fixed effects:
    ##                 Estimate Std. Error t value
    ## (Intercept)    -0.426486   0.302044  -1.412
    ## catch_per_trap  0.008625   0.001263   6.830
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## ctch_pr_trp -0.033
    ## optimizer (nloptwrap) convergence code: 0 (OK)
    ## boundary (singular) fit: see help('isSingular')

``` r
Anova(un_catch_model, ddf="lme4", type='III')
```

    ## Analysis of Deviance Table (Type III Wald chisquare tests)
    ## 
    ## Response: log(grams_per_trap)
    ##                 Chisq Df Pr(>Chisq)    
    ## (Intercept)    19.212  1  1.170e-05 ***
    ## catch_per_trap 37.553  1  8.896e-10 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
Anova(mod_catch_model, ddf="lme4", type='III')
```

    ## Analysis of Deviance Table (Type III Wald chisquare tests)
    ## 
    ## Response: log(grams_per_trap)
    ##                  Chisq Df Pr(>Chisq)    
    ## (Intercept)     1.9937  1      0.158    
    ## catch_per_trap 46.6555  1  8.463e-12 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## <a name="species"></a> **Calculate top species caught**

Calculating which species were the most abundant across the entire
survey.

This might have to be number of fish caught per trap? So that the
difference in \# of traps for modified and unmodified is accounted for?

This is split for modified and unmodified so far.. but can be changed to
combined.. the trend is about the same for most abundant type of fish in
each trap..

``` r
species_list <- data %>% select(scientific_name, number_of_fish, trap_type) %>% 
  na.omit() %>% 
  subset(trap_type == "MODIFIED" | trap_type == "UNMODIFIED") %>% #subset for only modified and unmodified traps 
  dplyr::group_by(scientific_name, trap_type) %>%
  mutate(species_catch = sum(number_of_fish)) %>% select(-number_of_fish) %>% distinct()

# above 250 cut off includes top each count for each type of trap 
species_list %>% filter(species_catch > 250) %>%
  ggplot(., aes(x=scientific_name, y=species_catch, group = trap_type, color = trap_type)) + 
  ylab("number of fish caught") + xlab("Genus species") +
  geom_point() + theme_bw() + theme(axis.text.x = element_text(angle = 60, hjust=1)) #Set the text angle
```

![](CUE-maturity-length-analysis_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
# print top 5 species from modified traps 
species_list %>% subset(trap_type == "MODIFIED") %>%                                    
  arrange(desc(species_catch)) %>% head(5)
```

    ## # A tibble: 5 × 3
    ## # Groups:   scientific_name, trap_type [5]
    ##   scientific_name        trap_type species_catch
    ##   <chr>                  <chr>             <dbl>
    ## 1 Siganus sutor          MODIFIED           4691
    ## 2 Lethrinus nebulosus    MODIFIED           2293
    ## 3 Scarus ghobban         MODIFIED           1219
    ## 4 Naso annulatus         MODIFIED            552
    ## 5 Leptoscarus vaigiensis MODIFIED            271

``` r
# print top 5 species from unmodified traps 
species_list %>% subset(trap_type == "UNMODIFIED") %>%                                    
  arrange(desc(species_catch)) %>% head(5)
```

    ## # A tibble: 5 × 3
    ## # Groups:   scientific_name, trap_type [5]
    ##   scientific_name        trap_type  species_catch
    ##   <chr>                  <chr>              <dbl>
    ## 1 Siganus sutor          UNMODIFIED         29857
    ## 2 Leptoscarus vaigiensis UNMODIFIED          6644
    ## 3 Siganus canaliculutus  UNMODIFIED          3840
    ## 4 Lethrinus nebulosus    UNMODIFIED          3662
    ## 5 Scarus ghobban         UNMODIFIED          3333

Relative abundance plots? relative of total caught number?

## <a name="species_pertrap"></a> **Top species stats per trap**

Create a subsetted df from the top 5 total species (break this down into
modified and unmodified later?).

``` r
species_df <- data %>% filter(!is.na(number_of_fish)) %>%
  subset(trap_type == "MODIFIED" | trap_type == "UNMODIFIED") %>%
  group_by(scientific_name) %>%
  mutate(species_total_catch = sum(number_of_fish)) %>% ungroup() #must ungroup for following commands 

species_keep <- species_df %>% select(scientific_name, species_total_catch) %>% 
  distinct() %>% slice_max(species_total_catch, n = 5)

# filter species df based on the species_keep list 
species_df <- species_df %>% filter(scientific_name %in% species_keep$scientific_name)

# double checking the above command worked - output should be only 5 
unique(sort(species_df$scientific_name))
```

    ## [1] "Leptoscarus vaigiensis" "Lethrinus nebulosus"    "Scarus ghobban"        
    ## [4] "Siganus canaliculutus"  "Siganus sutor"

With just the top 5 species, do catch per trap. I don’t think I can do
weight of just these species by trap because the weight is for the whole
catch.

``` r
species_df <- species_df %>% unite(survey_id, Operation_date, fisher_id, sep = " ", remove = FALSE) %>%
  dplyr::group_by(survey_id) %>% # group by survey id
  mutate(topspecies_catch = sum(number_of_fish),
         catch_per_trap = topspecies_catch/total_traps_collected) %>% #divide total catch per survey id by total traps 
  dplyr::ungroup(survey_id) #ungroup by this column

# basic grams per trap plot with no other variables 
species_df %>% 
  ggplot(aes(x=scientific_name, y=catch_per_trap, color=trap_type)) + 
  geom_boxplot(aes(color=trap_type), outlier.size = 0, lwd=0.5) +
  theme_bw() + ggtitle("For top 5 most abundant species") +
  ylab("Catch per trap (species catch / total catch)") + xlab("Genus species") +
  theme(axis.text.x = element_text(angle = 60, hjust=1)) #Set the text angle
```

    ## Warning: Removed 7 rows containing non-finite values (stat_boxplot).

![](CUE-maturity-length-analysis_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
# ANOVA on the top 5 species 
```

## <a name="fishbase"></a> **Creating database from Fishbase**

Search on fishbase maturity level and length stats to add to the
spreadsheet ‘maturity.xlsx’ on the github folder ‘data’.

## <a name="maturity"></a> **Catch per unit effort for top species by maturity**

## <a name="length"></a> **Catch and length data of mature fish**
