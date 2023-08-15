## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message=FALSE------------------------------------------------------------
library(dplyr)
library(SummarizedExperiment)
library(cleanse)

## ----eval=FALSE---------------------------------------------------------------
#  data(seq_se)
#  seq_se %>% round(3) %>% write_csv("out.csv", "expression")

## -----------------------------------------------------------------------------
sessionInfo()

