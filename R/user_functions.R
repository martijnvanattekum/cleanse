## quiets concerns of R CMD check about dplyr variables
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

#####################################################################
###################### dplyr functions ##############################
#####################################################################

#' Returns rows or cols of se with matching conditions
#' 
#' Use filter() to choose rows/genes where conditions are true. Uses attached metadata
#' to find matches. 
#' @param .data SummarizedExperiment to subset
#' @param axis The axis to perform the operation on. Either row or col.
#' @param ... Logical predicates defined in terms of the variables in .data. 
#' Multiple conditions are combined with & or ,. Only rows where the condition evaluates to TRUE are kept.
#' @examples
#' #filter the rows of the example se with genes from the IL group
#' seq_se %>% filter(row, gene_group == "IL")
#' @importFrom dplyr filter
#' @export
filter.SummarizedExperiment <- function(.data, axis, ...){
  subset_se(.data, deparse(substitute(axis)), dplyr::filter, ...)
}

#' Choose rows by position
#' 
#' Choose rows by their ordinal position in the se
#' @param .data SummarizedExperiment to subset
#' @param axis The axis to perform the operation on. Either row or col.
#' @param ... Integer row values. Provide either positive values to keep, or 
#' negative values to drop. The values provided must be either all positive or all 
#' negative. Indices beyond the number of rows in the input are silently ignored.
#'
#' The arguments in ... are automatically quoted and evaluated in the context of 
#' the data frame. They support unquoting and splicing. See vignette("programming") 
#' for an introduction to these concepts.
#' @examples
#' #subset the first 10 cols of the se
#' seq_se %>% slice(col, 1:10)
#' @importFrom dplyr slice
#' @export
slice.SummarizedExperiment <- function(.data, axis, ...){
  subset_se(.data, deparse(substitute(axis)), dplyr::slice, ...)
}

#' Arrange by variables
#' 
#' Order either rows or cols from se by an expression involving its variables.
#' @param .data SummarizedExperiment to arrange
#' @param axis The axis to perform the operation on. Either row or col.
#' @param ... Comma separated list of unquoted variable names, 
#' or expressions involving variable names. 
#' Use desc() to sort a variable in descending order
#' @examples
#' # Arrange seq_se by gene_group and then gene_name
#' seq_se %>% arrange(row, gene_group, gene_name)
#' @importFrom dplyr arrange
#' @export
arrange.SummarizedExperiment <- function(.data, axis, ...){
  subset_se(.data, deparse(substitute(axis)), dplyr::arrange, ...)
}

#' Sample n rows from a se
#' 
#' Selects random rows
#' @param tbl SummarizedExperiment to sample
#' @param size The number of rows to select
#' @param replace Sample with or without replacement?
#' @param weight Sampling weights. This must evaluate to a vector of non-negative 
#' numbers the same length as the input. Weights are automatically standardised to 
#' sum to 1.
#'
#' This argument is automatically quoted and later evaluated in the context of the 
#' data frame. It supports unquoting. See vignette("programming") for an introduction 
#' to these concepts.
#' @param ... ignored
#' @examples
#' #Sample 4 rows from seq_se with replacement
#' seq_se %>% sample_n_row(size = 4, replace = TRUE)
#' @export
sample_n_row <- function(tbl, size, replace = FALSE, weight = NULL, ...){
  UseMethod("sample_n_row")}

#' @export
sample_n_row.SummarizedExperiment <- function(tbl, size, replace = FALSE, weight = NULL, ...){
  tbl[sample.int(nrow(tbl), size = size, replace = replace, prob = weight),]}
  
#' Sample n cols from a se
#' 
#' Selects random cols
#' @param tbl SummarizedExperiment to sample
#' @param size The number of cols to select
#' @param replace Sample with or without replacement?
#' @param weight Sampling weights. This must evaluate to a vector of non-negative 
#' numbers the same length as the input. Weights are automatically standardised to 
#' sum to 1.
#'
#' This argument is automatically quoted and later evaluated in the context of the 
#' data frame. It supports unquoting. See vignette("programming") for an introduction 
#' to these concepts.
#' @param ... ignored
#' @examples
#' #Sample 4 columns from seq_se with replacement
#' seq_se %>% sample_n_col(4, replace = TRUE)
#' @export
sample_n_col <- function(tbl, size, replace = FALSE, weight = NULL, ...){
  UseMethod("sample_n_col")}

#' @export
sample_n_col.SummarizedExperiment <- function(tbl, size, replace = FALSE, weight = NULL, ...){
  tbl[,sample.int(ncol(tbl), size = size, replace = replace, prob = weight)]}

#' Sample a fraction of rows from a se
#' 
#' Selects a random fraction of rows
#' @param tbl SummarizedExperiment to sample
#' @param size The fraction of rows to select
#' @param replace Sample with or without replacement?
#' @param weight Sampling weights. This must evaluate to a vector of non-negative 
#' numbers the same length as the input. Weights are automatically standardised to 
#' sum to 1.
#'
#' This argument is automatically quoted and later evaluated in the context of the 
#' data frame. It supports unquoting. See vignette("programming") for an introduction 
#' to these concepts.
#' @param ... ignored
#' @examples
#' #Sample half of the rows
#' seq_se %>% sample_frac_row(size = .5)
#' @export
sample_frac_row <- function(tbl, size, replace = FALSE, weight = NULL, ...){
  UseMethod("sample_frac_row")}

#' @export
sample_frac_row.SummarizedExperiment <- function(tbl, size, replace = FALSE, weight = NULL, ...){
  tbl[sample.int(nrow(tbl), size = size, replace = replace, prob = weight),]}

#' Sample a fraction of cols from a se
#' 
#' Selects a random fraction of cols
#' @param tbl SummarizedExperiment to sample
#' @param size The fraction of cols to select
#' @param replace Sample with or without replacement?
#' @param weight Sampling weights. This must evaluate to a vector of non-negative 
#' numbers the same length as the input. Weights are automatically standardised to 
#' sum to 1.
#'
#' This argument is automatically quoted and later evaluated in the context of the 
#' data frame. It supports unquoting. See vignette("programming") for an introduction 
#' to these concepts.
#' @param ... ignored
#' @examples
#' #Sample half of the columns
#' seq_se %>% sample_frac_col(size = .5)
#' @export
sample_frac_col <- function(tbl, size, replace = FALSE, weight = NULL, ...){
  UseMethod("sample_frac_col")}

#' @export
sample_frac_col.SummarizedExperiment <- function(tbl, size, replace = FALSE, weight = NULL, ...){
  tbl[,sample.int(ncol(tbl), size = size, replace = replace, prob = weight)]}

#' Select variables by name for rowData or colData
#' 
#' Choose variables from the rowData or colData that you want to keep. Select drops all other variables.
#' @param .data SummarizedExperiment to subset
#' @param axis The axis to perform the operation on. Either row or col.
#' @param ... One or more unquoted expressions separated by commas. You can treat variable names like they are positions, so you can use expressions like x:y to select ranges of variables.
#' Positive values select variables; negative values drop variables. If the first expression is negative, select() will automatically start with all variables.  
#' 
#' Use named arguments, e.g. new_name = old_name, to rename selected variables.  
#' 
#' The arguments in ... are automatically quoted and evaluated in a context where column names represent column positions. They also support unquoting and splicing. See vignette("programming") for an introduction to these concepts.  
#' 
#' See select helpers for more details and examples about tidyselect helpers such as starts_with(), everything(), ...
#' @examples
#' # remove the time variable after filtering for time == 0
#' seq_se %>% filter(col, time == 0) %>% select(col, -time)
#' @importFrom dplyr select
#' @export
select.SummarizedExperiment <- function(.data, axis, ...){
  update_metadata_se(.data, deparse(substitute(axis)), dplyr::select, ...)
} 

#' Rename variables by name for rowData or colData
#' 
#' Choose variables from the rowData or colData that you want to rename. Rename keeps all other variables.
#' @param .data SummarizedExperiment to subset
#' @param axis The axis to perform the operation on. Either row or col.
#' @param ... One or more unquoted expressions separated by commas. You can treat variable names like they are positions, so you can use expressions like x:y to select ranges of variables.
#' 
#' Positive values select variables; negative values drop variables. If the first expression is negative, select() will automatically start with all variables.  
#' 
#' Use named arguments, e.g. new_name = old_name, to rename selected variables.  
#' 
#' The arguments in ... are automatically quoted and evaluated in a context where column names represent column positions. They also support unquoting and splicing. See vignette("programming") for an introduction to these concepts. 
#'  
#' See select helpers for more details and examples about tidyselect helpers such as starts_with(), everything(), ...
#' @examples
#' # rename the time variable after changing it to minutes
#' seq_se %>% mutate(col, time = (time * 60)) %>% rename(col, time_mins = time)
#' @importFrom dplyr rename
#' @export
rename.SummarizedExperiment <- function(.data, axis, ...){
  update_metadata_se(.data, deparse(substitute(axis)), dplyr::rename, ...)
} 

#' Create or transform variables
#' 
#' mutate() adds new variables and preserves existing ones; 
#' it preserves the number of rows/cols of the input. 
#' New variables overwrite existing variables of the same name.
#' @param .data SummarizedExperiment to subset
#' @param axis The axis to perform the operation on. Either row or col.
#' @param ... Name-value pairs of expressions, each with length 1 or the same 
#' length as the number of rows/cols in row- or colData. The name of each argument will 
#' be the name of a new variable, and the value will be its corresponding value. 
#' Use a NULL value in mutate to drop a variable. New variables overwrite existing variables of the same name.
#' The arguments in ... are automatically quoted and evaluated in the context of the data frame. 
#' They support unquoting and splicing. See vignette("programming") for an introduction to these concepts.
#' @examples
#' #Change the treatment time from hours to minutes
#' seq_se %>% mutate(col, time = (time * 60))
#' @importFrom dplyr mutate
#' @export
mutate.SummarizedExperiment <- function(.data, axis, ...){
  update_metadata_se(.data, deparse(substitute(axis)), dplyr::mutate, ...)
} 

#####################################################################
#################### Arithmetic operations  #########################
#####################################################################

#' Subtract two SummarizedExperiments
#' 
#' Subtracts the values from each of the underlying assay values for 2 se's. 
#' @param se1,se2 SummarizedExperiments for which se2 values will be subtracted from se1 values.
#' Need to have the same rowData and colData
#' @examples
#' #create subset se's for time == 0 and time == 4. Then subtract their values.
#' #remove time to make colData the same
#' t0 <- seq_se %>% filter(col, time == 0) %>% select(col, -time)
#' t4 <- seq_se %>% filter(col, time == 4) %>% select(col, -time)
#' t4 - t0
#' @export
`-.SummarizedExperiment` <- function(se1, se2){
  arith_se(se1, se2, `-`)
}

#' Add two SummarizedExperiments
#' 
#' Adds the values from each of the underlying assay values for 2 se's. 
#' @param se1,se2 SummarizedExperiments for which se1 and se2 values will be added
#' Need to have the same rowData and colData
#' @examples
#' #create subset se's for time == 0 and time == 4. Then add their values.
#' #remove time to make colData the same
#' t0 <- seq_se %>% filter(col, time == 0) %>% select(col, -time)
#' t4 <- seq_se %>% filter(col, time == 4) %>% select(col, -time)
#' t4 + t0
#' @export
`+.SummarizedExperiment` <- function(se1, se2){
  arith_se(se1, se2, `+`)
}

#' Divide two SummarizedExperiments
#' 
#' Divides the values from each of the underlying assay values for 2 se's. 
#' @param se1,se2 SummarizedExperiments for which se1 values will be divided by se2 values.
#' Need to have the same rowData and colData
#' @examples
#' #create subset se's for time == 0 and time == 4. Then subtract their values.
#' #remove time to make colData the same
#' t0 <- seq_se %>% filter(col, time == 0) %>% select(col, -time)
#' t4 <- seq_se %>% filter(col, time == 4) %>% select(col, -time)
#' t4 / t0
#' @export
`/.SummarizedExperiment` <- function(se1, se2){
  arith_se(se1, se2, `/`)
}

#' Multiply two SummarizedExperiments
#' 
#' Multiplies the values from each of the underlying assay values for 2 se's. 
#' @param se1,se2 SummarizedExperiments for which se1 and se2 values will be multiplied.
#' Need to have the same rowData and colData
#' @examples
#' #create subset se's for time == 0 and time == 4. Then add their values.
#' #remove time to make colData the same
#' t0 <- seq_se %>% filter(col, time == 0) %>% select(col, -time)
#' t4 <- seq_se %>% filter(col, time == 4) %>% select(col, -time)
#' t4 * t0
#' @export
`*.SummarizedExperiment` <- function(se1, se2){
  arith_se(se1, se2, `*`)
}

#' Round the values of a SummarizedExperiments
#' 
#' Rounds the values in all of the underlying assay matrices to the given number of digits
#' @param x SummarizedExperiment to round
#' @param digits integer indicating the number of decimal places
#' @examples 
#' # round the example se to 2 digits
#' seq_se %>% round(2)
#' @export
round.SummarizedExperiment <- function(x, digits = 0){
  update_assays_se(x, lapply(SummarizedExperiment::assays(x), function(mat)base::round(mat, digits)))
}

#####################################################################
################## Output functions (readr)  ########################
#####################################################################

#' Prints available options for the se 
#' 
#' Creates a print of all possible assays, rowData and colData
#' @param se SummarizedExperiment to show information for
#' @examples
#' #show available options for the example se
#' print_options(seq_se)
#' @export
print_options <- function(se) {
  cat(paste("** ASSAY OPTIONS FOR", deparse(substitute(se)), "** \n"))
  cat(paste(SummarizedExperiment::assayNames(se), collapse = ", "))
  cat(paste("\n\n** COLDATA OPTIONS FOR", deparse(substitute(se)), "** \n"))
  printdata(SummarizedExperiment::colData(se))
  cat(paste("\n** ROWDATA OPTIONS FOR", deparse(substitute(se)), "** \n"))
  printdata(SummarizedExperiment::rowData(se))
}

#' Write a se to csv format
#' 
#' @param se SummarizedExperiment to round
#' @param path Path or connection to write to
#' @param assay_name Name of the assay contained in se to write. Not required when 
#' only 1 assay is contained in the se
#' @examples
#' # write the example se to csv
#' seq_se %>% round(3) %>% write_csv("out.csv", "expression")
#' @importFrom readr write_csv 
#' @export
write_csv <- function(se, path, assay_name = NULL){ 
  if (is.null(assay_name) & length(SummarizedExperiment::assayNames(se)) == 1){
    assay_name <- SummarizedExperiment::assayNames(se)[[1]]}
  if (!assay_name %in% SummarizedExperiment::assayNames(se))stop(paste0("Assay '", assay_name, "' does not exist in the supplied se."))
    readr::write_csv(get_delim_df(se, assay_name), path, col_names = FALSE)
}

#' Write a se to tsv format
#' 
#' Rounds the values in all of the underlying assay matrices to the given number of digits
#' @param se SummarizedExperiment to round
#' @param path Path or connection to write to
#' @param assay_name Name of the assay contained in se to write. Not required when 
#' only 1 assay is contained in the se
#' @examples
#' # write the example se to tsv
#' seq_se %>% round(3) %>% write_tsv("out.tsv", "expression")
#' @importFrom readr write_tsv
#' @export
write_tsv <- function(se, path, assay_name = NULL){ 
  if (is.null(assay_name) & length(SummarizedExperiment::assayNames(se)) == 1){
    assay_name <- SummarizedExperiment::assayNames(se)[[1]]}
  if (!assay_name %in% SummarizedExperiment::assayNames(se))stop(paste0("Assay '", assay_name, "' does not exist in the supplied se."))
  readr::write_tsv(get_delim_df(se, assay_name), path, col_names = FALSE)
}

#' Write a se to delim format
#' 
#' Rounds the values in all of the underlying assay matrices to the given number of digits
#' @param se SummarizedExperiment to round
#' @param path Path or connection to write to
#' @param delim Delimiter used to separate values
#' @param assay_name Name of the assay contained in se to write. Not required when 
#' only 1 assay is contained in the se
#' @examples
#' # write the example se to delim file, defaulting to " " as delimiter
#' seq_se %>% round(3) %>% write_delim("out.txt", assay_name = "expression")
#' @importFrom readr write_delim
#' @export
write_delim <- function(se, path, delim = " ", assay_name = NULL){
  if (is.null(assay_name) & length(SummarizedExperiment::assayNames(se)) == 1){
    assay_name <- SummarizedExperiment::assayNames(se)[[1]]}
  if (!assay_name %in% SummarizedExperiment::assayNames(se))stop(paste0("Assay '", assay_name, "' does not exist in the supplied se."))
  readr::write_delim(get_delim_df(se, assay_name), path, col_names = FALSE)
}

#####################################################################
########################### OTHER functions  ########################
#####################################################################
#' Removes non-informative metadata
#' 
#' For each of colData and rowData, removes the variables that have only 1 unique value
#' @param se SummarizedExperiment to drop metadata for
#' @examples
#' # as the se only contains time == 4 data, the time variable can be dropped
#' seq_se %>% filter(col, time == 4) %>% drop_metadata 
#' @export
drop_metadata <- function(se){
  coldt <- SummarizedExperiment::colData(se) %>% as.data.frame() %>% .[, sapply(., function(col)length(unique(col)) > 1)]
  rowdt <- SummarizedExperiment::rowData(se) %>% as.data.frame() %>% .[, sapply(., function(col)length(unique(col)) > 1)]
  SummarizedExperiment::SummarizedExperiment(assays = SummarizedExperiment::assays(se), 
                                             colData = coldt, 
                                             rowData = rowdt)
}