# comprep


<!-- README.md is generated from README.qmd. Please edit that file -->

Helper package for the project “Computational Reproducibility in Sports
Science”.

## Installation

You can install the development version of comprep using:

``` r
# install.packages("remotes")
remotes::install_github("comprep-sport/comprep")
```

## Example

Comparing a numerical value to a reported value to check the numerical
correctness of reproduced results:

``` r
library(comprep)
comp(recalculated = 7.2, reported = "7.15")
#> $recalculated
#> [1] 7.2
#> 
#> $margin
#>   lower   upper 
#> 7.07355 7.22655 
#> 
#> $correct
#> [1] TRUE
```
