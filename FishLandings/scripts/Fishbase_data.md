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
view(data)
```

## Extracting Fish Nutrient data

<https://james-robinson.shinyapps.io/FishNutrientsApp/>
