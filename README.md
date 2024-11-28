
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Overview

<!-- badges: start -->
<!-- badges: end -->

The goal of the `hydrospain` R package is to allow users to easily
download historical data from for gauging stations in Spanish rivers.
Those datasets are available at the [CEDEX
web](https://ceh.cedex.es/anuarioaforos/demarcaciones.asp) archive.
Although files can be downloaded by hand one by one, $\tt{hydrospain}$
allows the automatic retrieval of any of those files, appending WGS84
UTM30 (EPSG:32630) coordinates to the datasets. In addition, date fields
are formatted accordingly.

## Installation

You can install the development version of hydrospain from
[GitHub](https://github.com/) with:

``` r
# install.packages("githubinstall")
# githubinstall::gh_install_packages("hydrospain", "devel")
library(hydrospain)
```

## Example

This is a basic example which shows you how to download daily data from
river gauging stations for the Ebro basin:

``` r
# Use verbose = TRUE (default) if you want to track progress.
x <- hydrospain(file_name = "afliq", basin_nam = "ebro", verbose = FALSE)
```

<br>

We can retrieve as easily the same datasets for all stations and from
all the basins by omitting the $\tt{basin}$ parameter.

``` r
x <- hydrospain(file_name = "afliq", verbose = FALSE)
```

<br>

Notice that the retrieval can be done for all basin stations at once,
but files must be retrieved one by one. For a exhaustive list of files
to download, go to the CEDEX web.
