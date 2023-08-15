## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message=FALSE------------------------------------------------------------
library(cleanse)

## -----------------------------------------------------------------------------
data(seq_se)
print_options(seq_se)

## -----------------------------------------------------------------------------
genes_subset_se <- seq_se %>% filter(row, gene_group == "IL")
print_options(genes_subset_se)  #note the change in available gene_groups
dim(seq_se)
dim(genes_subset_se)

sample_subset_se <- seq_se %>% filter(col, treatment == "B", site %in% c("brain", "skin"))
print_options(sample_subset_se) #note the change in available treatments and sites
dim(seq_se)
dim(sample_subset_se)

## -----------------------------------------------------------------------------
seq_se %>% slice(axis=col, 1:10) #select the first 10 columns

## -----------------------------------------------------------------------------
seq_se_reordered <- seq_se %>% arrange(row, gene_name, gene_group)

## -----------------------------------------------------------------------------
slice_sample(seq_se, col, n=3)#note the change in dim

slice_sample(seq_se, row, prop=.2) #note the change in dim

## -----------------------------------------------------------------------------
# remove the time variable after filtering for time == 0
seq_se_min_time <- seq_se %>% filter(col, time == 0) %>% select(col, -time)
print_options(seq_se_min_time)  #note the time variable has disappeared from the colData

# rename the time variable after changing it to minutes
seq_se_ren_time <- seq_se %>% mutate(col, time = (time * 60)) %>% rename(col, time_mins = time)
print_options(seq_se_ren_time)  #note the time variable is now called time_mins

## -----------------------------------------------------------------------------
seq_se_mins <- seq_se %>% mutate(col, time = (time * 60))
seq_se$time
seq_se_mins$time

## -----------------------------------------------------------------------------
seq_se_gene_comb <- seq_se %>% 
  mutate(row, group_and_name = paste(gene_group, gene_name, sep = "_"))

## -----------------------------------------------------------------------------
seq_se_dropped <- seq_se %>% 
  filter(col, time == 4) %>% 
  drop_metadata()
print_options(seq_se_dropped) #note the time variable from colData is dropped as all values were 4

## -----------------------------------------------------------------------------
sessionInfo()

