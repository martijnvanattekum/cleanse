## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message=FALSE------------------------------------------------------------
library(cleanse)

## -----------------------------------------------------------------------------
data(seq_se)
# Get the coldata as a tibble
get_col_data(seq_se)

# generate new rowdata and create a new se with the rowdata set
new_rd <- get_row_data(seq_se) %>% 
  tidyr::unite(gene_group_name, c(gene_group, gene_name))
se_new_rowdata <- set_row_data(seq_se, new_rd)

## -----------------------------------------------------------------------------
options_from_coldata(seq_se, "site")

