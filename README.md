# IndiaTS

The goal of **IndiaTS** is to provide tools for cleaning, transforming,
and standardising Indian statistical datasets into a consistent
long-format time series structure. It includes utilities for working
with datasets from sources such as eSankhyiki and helps convert raw
tabular statistical data into analysis-ready time series objects.

## Installation

You can install the development version of IndiaTS from GitHub with:

``` r
# install.packages("pak")
pak::pak("bedantaguru/IndiaTS")
```

## Example

This is a basic example which shows how to convert raw GVA data into a
standardised time series:

``` r
library(IndiaTS)

# convert GVA dataset
ts <- es_convert_gva(dat)

ts
```

GitHub repository:\
https://github.com/bedantaguru/IndiaTS
