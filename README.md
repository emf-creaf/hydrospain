
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Overview

<!-- badges: start -->
<!-- badges: end -->

The goal of the $\tt{cehAforos}$ R package is to allow users to easily
download historical data from for gauging stations in Spanish rivers.
Those datasets are available at the [CEDEX web
site](https://ceh.cedex.es/anuarioaforos/demarcaciones.asp). Rather that
downloading oneself, $\texttt{cehAforos}$ allows the automatic retrieval
of any of those files, appending UTM30 coordinates to the datasets.

## Installation

You can install the development version of cehAforos from
[GitHub](https://github.com/) with:

``` r
# install.packages("githubinstall")
# githubinstall::gh_install_packages("cehAforos", "devel")
library(cehAforos)
```

## Example

This is a basic example which shows you how to download daily data from
river gauging stations for the Ebro basin:

``` r
# Use verbose = TRUE (default) if you want to track progress.
# x <- get_ceh_data(file_name = "afliq", basin_nam = "ebro", verbose = FALSE)
```

<br>

We can retrieve as easily the same datasets for all stations and from
all the basins by omitting the $\tt{basin}$ parameter.

``` r
# x <- get_ceh_data(file_name = "afliq", verbose = FALSE)
```
