## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message=FALSE-------------------------------------------------------
library(dplyr)
library(SummarizedExperiment)
library(cleanse)

## ------------------------------------------------------------------------
print_options(seq_se)

## ------------------------------------------------------------------------
genes_subset_se <- seq_se %>% filter(gene_group == "IL")
print_options(genes_subset_se)  #note the change in available gene_groups

sample_subset_se <- seq_se %>% select(treatment == "B", site %in% c("brain", "skin"))
print_options(sample_subset_se) #note the change in available treatment and site

## ------------------------------------------------------------------------
seq_se_reordered <- seq_se %>% arrange(row, gene_name, gene_group)
rowData(seq_se)
rowData(seq_se_reordered)

## ------------------------------------------------------------------------
seq_se %>% sample_n(col, size = 4, replace = TRUE) #note the change in dim

seq_se %>% sample_frac(row, size = .5) #note the change in dim

## ------------------------------------------------------------------------
seq_se_mins <- seq_se %>% mutate(col, time = (time * 60))
seq_se$time
seq_se_mins$time

## ------------------------------------------------------------------------
seq_se_gene_comb <- seq_se %>% mutate(row, group_and_name = paste(gene_group, gene_name, sep = "_"))
rowData(seq_se_gene_comb)$group_and_name

