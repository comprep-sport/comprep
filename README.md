# comprep

Helper package for the project "Computational Reproducibility in Sports Science".

## Installation

You can install the development version of comprep like so:

``` r
# install.packages("remotes")
remotes::install_github("comprep-sport/comprep")
```

## Example

Comparing a numerical value to a reported value to check the numerical correctness of reproduced results:

``` r
library(comprep)
comp(recalculated = 7.2, reported = "7.15")
```

