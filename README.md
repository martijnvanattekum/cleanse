
# cleanse

## Overview

The SummarizedExperiment (se) class offers a useful way to store multiple row and column data along with the values from an experiment and is widely used in computational biology.  
Although subsetting se's is possible with base R notation (ie using ```[]```), se's cannot be manipulated using grammar from the tidyverse. This package contains a number of wrapper functions to apply tidyverse functions to se's.

## Supported dplyr functions

  - `mutate()` adds new variables that are functions of existing
    variables
  - `select()` picks columns/samples based on the values in the se's colData.
  - `filter()` picks rows/genes based on the values in the se's rowData.
  - `arrange()` changes the ordering of the rows.
  - `sample_n()` picks n random cols/rows from the se.
  - `sample_frac()` picks a random fractions of cols/rows from the se.

Usage information can be found by reading the vignettes: `vignette("cleanse")`, and `vignette("dplyr")`.

## Installation
``` r
# install.packages("devtools")
devtools::install_github("martijnvanattekum/cleanse")
```


## Usage
``` r
library(cleanse)
```

## Getting help

If you encounter a clear bug, please file a minimal reproducible example
on [github](https://github.com/martijnvanattekum/cleanse).
