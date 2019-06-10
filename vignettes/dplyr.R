## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(dplyr)
library(DESeq2)
library(cleanse)
print_options(seq_se)

## ------------------------------------------------------------------------
seq_se %>% filter(gene_group == "IL")
seq_se %>% select(treatment == "B", site %in% c("brain", "skin"))

## ------------------------------------------------------------------------
seq_se %>% arrange(row, gene_group, gene_name)

## ------------------------------------------------------------------------
seq_se %>% sample_n(col, size = 4, replace = TRUE)
seq_se %>% sample_frac(row, size = .5)

## ------------------------------------------------------------------------
seq_se %>% mutate(col, time = (time * 60))

## ------------------------------------------------------------------------
seq_se %>% mutate(row, group_and_name = paste(gene_group, gene_name, sep = "_"))

