
# cleanse

## Overview

The SummarizedExperiment (se) class offers a useful way to store multiple row and column metadata along with the values from an experiment and is widely used in computational biology.  
Although subsetting se's is possible with base R notation (ie using `[]`), se's cannot be manipulated using grammar from the tidyverse. This package contains a number of wrapper functions to extend the usage of se's:
- dplyr functions: to use dplyr's grammar of data manipulation
- arithmetic functions: to perform arithmetic on 2 se's
- write functions: to print the options of a se and to write se's to delimited files

Usage information can be found by reading the vignettes: `browseVignettes("cleanse")`.

## Supported dplyr functions

_Functions that subset the se based on the rowData or colData_
  - `filter()` picks rows/cols based on the se's attached rowData/colData
  - `slice()` picks rows/cols by position
  - `arrange()` changes the ordering of the rows
  - `sample_n_<row/col>()` picks n random rows or cols from the se.
  - `sample_frac_<row/col>()` picks a random fractions of rows or cols from the se.

_Functions that change the se's rowData or colData_
- `select()` selects variables
- `rename()` renames variables
- `mutate()` adds new variables that are functions of existing
  variables
- `drop_metadata()` drops all rowData and colData having only 1 unique value

## Supported arithmetic functions

  - `-` subtracts values from the assays in 2 se's
  - `+` adds values from the assays in 2 se's
  - `/` divides values from the assays in 2 se's
  - `*` multiplies values from the assays in 2 se's
  - `round` rounds the assay values of a se

## Supported write functions

  - `write_csv()` writes a se to csv
  - `write_tsv()` writes a se to tsv
  - `write_delim()` writes a se to a delimited file

## Installation
``` r
# install.packages("devtools")
devtools::install_github("martijnvanattekum/cleanse",
build_opts = c("--no-resave-data", "--no-manual"))
```

## Usage
``` r
library(cleanse)

# -- An example se called seq_se is provided

# Example pipe
seq_se %>%
  filter(row, gene_group == "NOTCH") %>%
  filter(col, site %in% c("brain", "skin")) %>%
  arrange(col, patient) %>%
  round(3) %>%
  write_csv("expression", "out.csv")

# Example sampling
seq_se %>% sample_n(row, 5)

# Example arithmetic subtracting the expression values at T=0 from T=4
(select(seq_se, time == 4)) - (select(seq_se, time == 0))
```

## Getting help

If you encounter a clear bug, please file a minimal reproducible example
on [github](https://github.com/martijnvanattekum/cleanse/issues).
