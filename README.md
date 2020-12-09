
<!-- README.md is generated from README.Rmd. Please edit that file -->

# insectcleanr

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

The goal of insectcleanr is to provide functions for building cleaned
data tables of insect data. This code package was developed for internal
use by a SESYNC pursuit team and SESYNC data science staff.

## Installation

You can install the latest version of insectcleanr from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("reblake/insectcleanr")
```

## Example

Example data in this package is courtesy of Morimoto, N., Kiritani, K.,
Yamamura, K., & Yamanaka, T. (2019). Finding indications of lag time,
saturation and trading inflow in the emergence record of exotic
agricultural insect pests in Japan. Applied Entomology and Zoology,
54(4), 437-450.

This is a basic example which shows you how to get accepted taxonomies
for insect taxa from GBIF.

``` r
library(insectcleanr)
#> Loading required package: dplyr
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
#> Loading required package: magrittr
#> Loading required package: readxl
#> Loading required package: purrr
#> 
#> Attaching package: 'purrr'
#> The following object is masked from 'package:magrittr':
#> 
#>     set_names

# list the path(s) to your raw data files (your path will look different than this)
# file_list <- system.file("extdata", "Japan_taxa.csv", package = "insectcleanr")

# read in raw data and separate out taxonomic information
# taxa_list <- lapply(file_list, separate_taxonomy) %>% 
#              purrr::reduce(full_join) %>%  # join list of dataframes into one dataframe
#              distinct(genus_species) %>%  # get unique taxa names
#              select(genus_species) %>%  # select only the column with taxa names
#              unlist(., use.names = FALSE)  # make taxa names into a vector   
              
# get accepted taxonomic information from GBIF
# taxa_accepted <- lapply(taxa_list, get_accepted_taxonomy)
```

More details are in the vignette for making the taxonomy table.

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
#summary(cars)
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub\!
