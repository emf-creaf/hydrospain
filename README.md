
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cehAforos

<!-- badges: start -->
<!-- badges: end -->

The goal of cehAforos is to allow users to easily download datasets for
gauging stations in Spanish rivers. The datasets are available at [the
CEDEX web site](https://ceh.cedex.es/anuarioaforos/demarcaciones.asp)

## Installation

You can install the development version of cehAforos from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("emf-creaf/cehAforos")
#> ℹ Loading metadata database✔ Loading metadata database ... done
#>  
#> ℹ No downloads are needed
#> ✔ 1 pkg + 35 deps: kept 31 [9.1s]
```

## Example

This is a basic example which shows you how to download daily data from
river gauging stations for the Ebro basin:

``` r
library(cehAforos)
# Use verbose = TRUE (default) if you want to track progress.
# x <- get_ceh_data(table = "afliq", basin = "ebro", verbose = FALSE)
```

<br>

We can retrieve as easily the same datasets for all stations and from
all the basins by omitting the $\tt{basin}$ parameter.

``` r
# x <- read_cedex_basin(table = "afliq", verbose = FALSE)
```
