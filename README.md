
<!-- README.md is generated from README.Rmd. Please edit that file -->

# AgreenaIPCC

<!-- badges: start -->
<!-- badges: end -->

The goal of AgreenaIPCC is to make the IPCC calculation for N2O
emissions from soil and CO2 emissions due to fuel consumption and lime
application.

## Installation

You can install the development version of AgreenaIPCC from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Agreena-ApS/AgreenaIPCC")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(AgreenaIPCC)
#setwd("Your_Work_Path")
data <- example_data
harvest_year <- 2023
databases <- databases_2023
result <- AgreenaIPCC(data, harvest_year, databases)
```
