## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message=FALSE------------------------------------------------------------
library(dplyr)
library(SummarizedExperiment)
library(cleanse)

## -----------------------------------------------------------------------------
data(seq_se)
seq_se_t0 <- seq_se %>% filter(col, time == 0) %>% 
  select(col, -time) %>% 
  arrange(col, patient, site, treatment)
assay(seq_se_t0)[1:5, 1:5]
seq_se_t4 <- seq_se %>% filter(col, time == 4) %>% 
  select(col, -time) %>% 
  arrange(col, patient, site, treatment)
assay(seq_se_t4)[1:5, 1:5]
seq_se_diff <- seq_se_t4 - seq_se_t0
assay(seq_se_diff)[1:5, 1:5]

## -----------------------------------------------------------------------------
seq_se_rounded <- seq_se %>% round(3)
assay(seq_se)[1:5, 1:5]
assay(seq_se_rounded)[1:5, 1:5]

## -----------------------------------------------------------------------------
sessionInfo()

