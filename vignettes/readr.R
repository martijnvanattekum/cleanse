## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message=FALSE-------------------------------------------------------
library(dplyr)
library(SummarizedExperiment)
library(cleanse)

## ----eval=FALSE----------------------------------------------------------
#  seq_se %>% round(3) %>% write_csv("expression", "out.csv")

