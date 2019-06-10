## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(dplyr)
library(DESeq2)
library(cleanse)
seq_se_t0 <- seq_se %>% select(time == 0) %>% arrange(col, patient, site, treatment)
seq_se_t4 <- seq_se %>% select(time == 4) %>% arrange(col, patient, site, treatment)
seq_se_diff <- seq_se_t4 - seq_se_t0

