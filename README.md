
# cleanse

## Overview

The SummarizedExperiment (se) class offers a useful way to store multiple row 
and column metadata along with the values from an experiment and is widely used 
in computational biology.  
Although subsetting se's is possible with base R notation (ie using `[]`), 
se's cannot be manipulated using grammar from the **tidyverse**. As a 
consequence, it is not possible to manipulate se's in pipelines using the
**pipe operator**.

This package contains a number of wrapper functions to extend the usage of se's:
- dplyr functions: to use dplyr's grammar of data manipulation
- arithmetic functions: to perform arithmetic on 2 se's
- write functions: to print the options of a se and to write se's to delimited 
files

As an example, compare how cleanse is used to subset columns for timepoint == 4 of a se:
| Using native syntax                                                               | Using cleanse                          |
|:----------------------------------------------------------------------------------|:---------------------------------------|
| <pre> coldata <- colData(se) <br> indices <- which(coldata$time == 4) <br> se[,indices] </pre> | <pre> se %>% <br>     filter(col, time == 4) </pre> |

Usage information can be found by reading the vignettes: `browseVignettes("cleanse")`.

## Supported dplyr functions

_Functions that subset the se based on the rowData or colData_
  - `filter()` picks rows/cols based on the se's attached rowData/colData
  - `slice()` picks rows/cols by position
  - `arrange()` changes the ordering of the rows
  - `sample_slice())` picks a random portion of rows or cols from the se.

_Functions that change the se's rowData or colData_
- `select()` selects variables
- `rename()` renames variables
- `mutate()` adds new variables that are functions of existing variables
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
data(seq_se)
seq_se %>%
  filter(row, gene_group == "NOTCH") %>%
  filter(col, site %in% c("brain", "skin")) %>%
  arrange(col, patient) %>%
  round(3)

# Example sampling
data(seq_se)
seq_se %>% slice_sample(row, prop=.2)

# Example arithmetic subtracting the expression values at T=0 from T=4
data(seq_se)
(filter(seq_se, col, time == 4)) - (filter(seq_se, col, time == 0))
```

## Getting help

If you encounter a clear bug, please file a minimal reproducible example
on [github](https://github.com/martijnvanattekum/cleanse/issues).
