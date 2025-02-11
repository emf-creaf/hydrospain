
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Overview

<!-- badges: start -->

<!-- badges: end -->

The goal of the `hydrospain` R package is to allow users to easily
download historical data from for gauging stations in Spanish rivers.
Those datasets are available at the
[CEDEX](https://ceh.cedex.es/anuarioaforos/demarcaciones.asp) web
archive. Although files can be downloaded by hand one by one,
`hydrospain` allows the automatic retrieval of any of those files,
appending WGS84 UTM30 (EPSG:32630) coordinates to the datasets and.
optionally, creating a spatial
[sf](https://CRAN.R-project.org/package=sf) object. In addition, date
fields are formatted accordingly.

## Installation

You can install the development version of `hydrospain` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
# devtools::install_github("emf-creaf/hydrospain")
library(hydrospain)
```

<br>

Available basin names, as of Nov. 2024, are the following:

| **basin_nam** argument | **CEDEX full basin name**           |
|------------------------|-------------------------------------|
| galicia                | AUGAS DE GALICIA – XUNTA DE GALICIA |
| cantabrico             | C.H. CANTABRICO                     |
| duero                  | C.H. DUERO                          |
| ebro                   | C.H. EBRO                           |
| guadalquivir           | C.H. GUADALQUIVIR                   |
| guadiana               | C.H. GUADIANA                       |
| jucar                  | C.H. JUCAR                          |
| miño                   | C.H. MIÑO-SIL                       |
| segura                 | C.H. SEGURA                         |
| tajo                   | C.H. TAJO                           |

<br>

Files that can be retrieved from the
[CEDEX](https://ceh.cedex.es/anuarioaforos/demarcaciones.asp) web site
correspond to data for gauging stations located at rivers, channels,
reservoirs and evaporation stations. For a full list of files check the
[CEDEX](https://ceh.cedex.es/anuarioaforos/demarcaciones.asp) web.

## Example

This is a basic example which shows you how to download daily data from
river gauging stations for the Ebro basin:

``` r
# Use verbose = TRUE (default) if you want to track progress.
x <- hydrospain(file_name = "afliq", basin_nam = "ebro", verbose = FALSE)
```

<br>

We can retrieve as easily the same file for all stations and from all
the basins by omitting the $\textit{basin}$ parameter.

``` r
x <- hydrospain(file_name = "afliq", verbose = FALSE)
```

<br>

Notice that the retrieval can be done for all basin stations at once,
but files must be retrieved one by one. For a exhaustive list of files
to download, go to the CEDEX web.
