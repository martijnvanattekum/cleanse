## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message=FALSE-------------------------------------------------------
#library(dplyr)
library(SummarizedExperiment)
library(cleanse)

## ------------------------------------------------------------------------
print_options(seq_se)

## ------------------------------------------------------------------------
genes_subset_se <- seq_se %>% filter(row, gene_group == "IL")
print_options(genes_subset_se)  #note the change in available gene_groups
dim(seq_se)
dim(genes_subset_se)

sample_subset_se <- seq_se %>% filter(col, treatment == "B", site %in% c("brain", "skin"))
print_options(sample_subset_se) #note the change in available treatments and sites
dim(seq_se)
dim(sample_subset_se)

