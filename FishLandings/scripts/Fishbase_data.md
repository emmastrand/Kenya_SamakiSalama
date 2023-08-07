FishBaseR
================

# Testing rfishbase

[Package
info](https://www.r-project.org/nosvn/pandoc/rfishbase.html#:~:text=rfishbase%20is%20available%20on%20CRAN%20and%20can%20be,installed%20using%20the%20devtools%20package%3A%20library%28devtools%29%20install_github%28%22rfishbase%22%2C%20%22ropensci%22%29)

``` r
library(tidyverse)
library(rfishbase)
```

``` r
sp <- c('Siganus sutor')

data <- estimate(sp) %>% data.frame()
```

    ## Joining with `by = join_by(SpecCode)`

``` r
head(data)
```

    ##         Species SpecCode MaxLengthTL TLObserved Troph seTroph TrophObserved
    ## 1 Siganus sutor     4615        54.9         -1  2.25     0.2            -1
    ##   TrophPredicted seTrophPredicted         a sd_log10a    b   sd_b
    ## 1             NA               NA 0.0177828     0.116 2.95 0.0736
    ##                                           Method_ab prior_r lcl_r ucl_r n_r
    ## 1 LWR estimates for this species & Genus-body shape      NA    NA    NA  NA
    ##   Comment_r    K SD_logK Linf SD_logLinf Median_T lcl_T ucl_T n_T   Winf
    ## 1      <NA> 0.78      NA   NA         NA       NA    NA    NA  NA 1603.5
    ##   LengthType ComDepthMin ComDepthMax ComDepMinObserved ComDepMaxObserved
    ## 1       <NA>           1          12                -1                -1
    ##   DepthMin DepthMax DepthMinEstimate DepthMaxEstimate PredPreyRatioMin
    ## 1        1       50                0                0         64.45928
    ##   PredPreyRatioMax    AgeMin   AgeMax TempPrefMin TempPrefMean TempPrefMax
    ## 1         3420.312 0.1161935 3.257537        24.6         27.5        28.9
    ##   nCells FeedingPath FeedingPathLevel  Calcium Calcium_l95 Calcium_u95     Iron
    ## 1    745     benthic          species 31.96948    15.61756    69.91541 0.634428
    ##    Iron_l95 Iron_u95    Omega3 Omega3_l95 Omega3_u95  Protein Protein_l95
    ## 1 0.3154547 1.310469 0.1486481 0.08520559   0.263034 19.29799    18.09703
    ##   Protein_u95 Selenium Selenium_l95 Selenium_u95 VitaminA VitaminA_l95
    ## 1     20.5255 23.10685     10.20274     49.05339 39.03574     12.02034
    ##   VitaminA_u95     Zinc  Zinc_l95 Zinc_u95 LastModified MaxLengthSL KObserved
    ## 1      119.624 1.566569 0.6434847 3.059337   2023-01-31          45        -1

## Extracting Fish Nutrient data

<https://james-robinson.shinyapps.io/FishNutrientsApp/>
