
<!-- README.md is generated from README.Rmd. Please edit that file -->
camiller
========

`camiller` is a set of convenience functions, functions for working with ACS data via `tidycensus`, and a `ggplot` theme.

Installation
------------

Install from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("camille-s/camiller")
```

Example
-------

This is a basic example of two of the major functions in this package, `add_grps` and `calc_shares`:

``` r
library(tidyverse)
library(camiller)

unique(race_pops$variable)
#> [1] "total"  "white"  "black"  "asian"  "latino"

race_grps <- list(
  total = 1,
  black_latino = c(3, 5),
  poc = 3:5
)

race_pops %>%
  group_by(region, name) %>%
  add_grps(grp_list = race_grps, group = variable, estimate = estimate, moe = moe) %>%
  calc_shares(region, name, group = variable, denom = "total", moe = moe)
#> # A tibble: 39 x 7
#>    region     name       variable     estimate   moe  share sharemoe
#>    <fct>      <chr>      <fct>           <dbl> <dbl>  <dbl>    <dbl>
#>  1 New Haven  New Haven  total          130405    60 NA       NA    
#>  2 New Haven  New Haven  black_latino    80201  2182  0.615    0.017
#>  3 New Haven  New Haven  poc             86243  2291  0.661    0.018
#>  4 Inner Ring East Haven total           29015    24 NA       NA    
#>  5 Inner Ring East Haven black_latino     4513   756  0.156    0.026
#>  6 Inner Ring East Haven poc              5636   962  0.194    0.033
#>  7 Inner Ring Hamden     total           61476    31 NA       NA    
#>  8 Inner Ring Hamden     black_latino    19659  1330  0.32     0.022
#>  9 Inner Ring Hamden     poc             22944  1435  0.373    0.023
#> 10 Inner Ring West Haven total           54972    42 NA       NA    
#> # ... with 29 more rows
```
