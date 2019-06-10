## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(dplyr)
library(DESeq2)
library(cleanse)
write_se_to_csv(seq_se, "expression", "out.csv")

