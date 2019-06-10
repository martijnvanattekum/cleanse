# data were downloaded from https://depmap.org/portal/download/. The processing below of each data source
# should yield similar data structures, with variable "value" representing the final data returned to the user
# data are in long format; a sample data frame can be found in the file sample.csv
library(devtools)
library(tidyverse)
library(DESeq2)

#####################################################################
######################### INTERNAL DATA #############################
#####################################################################
#save internal data
#usethis::use_data(#, internal = TRUE, overwrite = TRUE)


#####################################################################
########################### USER DATA ###############################
#####################################################################

#example se
set.seed(15)
expr <- matrix(runif(480,0,25), ncol = 48)
cn <- matrix(runif(480,1,5), ncol = 48)
genes <- data.frame(gene_group = rep(c("IL", "NOTCH", "TLR"), c(5,3,2)), 
                    gene_name = c("IL1R1", "IL1R2", "IL2RA", "IL2RB", "IL2RG", "DLL1", "DLL3", "JAG1", "TLR1", "TLR2"))
samples <- expand.grid(patient = 1:4, 
                       site = c("skin", "brain", "liver"), 
                       time = c(0,4), 
                       treatment = c("A", "B"))

seq_se <- SummarizedExperiment(assays = list(expression = expr, copy_number = cn),
                               rowData = genes,
                               colData = samples)

# SAVE USER DATA ----------------------------------------------------------
usethis::use_data(seq_se, overwrite = TRUE)
