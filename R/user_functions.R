## quiets concerns of R CMD check about dplyr variables
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

#####################################################################
###################### dplyr functions ##############################
#####################################################################

#' Returns row of se with matching conditions
#' 
#' Use filter() to choose rows/genes where conditions are true. Uses attached rowData
#' to find matches. 
#' @param se SummarizedExperiment to subset
#' @param ... Logical predicates defined in terms of the variables in .data. 
#' Multiple conditions are combined with & or ,. Only rows where the condition evaluates to TRUE are kept.
#' @examples
#' #filter the rows of the example se with genes from the IL group
#' seq_se %>% filter(gene_group == "IL")
#' @export
filter <- function(se, ...){UseMethod("filter")}

#' @rdname filter 
#' @export
filter.SummarizedExperiment <- function(se, ...){
  se[getidx(SummarizedExperiment::rowData(se), dplyr::filter, ...), ]
}

#' Returns columns of se with matching conditions
#' 
#' Use select() to choose columns/samples where conditions are true. Uses attached colData
#' to find matches. Note that dplyr's filter syntax is used.
#' @param se SummarizedExperiment to subset
#' @param ... Logical predicates defined in terms of the variables in .data. 
#' Multiple conditions are combined with & or ,. Only columns where the condition 
#' evaluates to TRUE are kept.
#' @examples
#' #select the columns/samples of patient 1
#' seq_se %>% select(patient == 1)
#' #select the columns/samples where treatment is B and the site is either brain or skin
#' #' seq_se %>% select(treatment == "B", site %in% c("brain", "skin"))
#' @export
select <- function(se, ...){UseMethod("select")}

#' @rdname select 
#' @export
select.SummarizedExperiment <- function(se, ...){
  se[, getidx(SummarizedExperiment::colData(se), dplyr::filter, ...)]
} 

#' Arrange by variables
#' 
#' Order either rows or cols from se by an expression involving its variables.
#' @param se SummarizedExperiment to arrange
#' @param axis The axis to perform the operation on. Either row or col.
#' @param ... Comma separated list of unquoted variable names, 
#' or expressions involving variable names. 
#' Use desc() to sort a variable in descending order
#' @examples
#' # Arrange seq_se by gene_group and then gene_name
#' seq_se %>% cleanse::arrange(row, gene_group, gene_name)
#' @export
arrange <- function(se, axis, ...){UseMethod("arrange")}

#' @rdname arrange
#' @export
arrange.SummarizedExperiment <- function(se, axis, ...){
  axis <- deparse(substitute(axis))
  axis <- match.arg(axis, c("col", "row"))
  if (axis == "row") return(se[getidx(SummarizedExperiment::rowData(se), dplyr::arrange, ...),])
  se[, getidx(SummarizedExperiment::colData(se), dplyr::arrange, ...)]
}

#' Sample n rows or cols from a se
#' 
#' Selects random rows or columns
#' @param se SummarizedExperiment to sample
#' @param axis The axis to perform the operation on. Either row or col.
#' @param ... Additional arguments to sample_n
#' @examples
#' #Sample 4 columns from seq_se with replacement
#' seq_se %>% sample_n(col, size = 4, replace = TRUE)
#' @export
sample_n <- function(se, axis, ...){UseMethod("sample_n")}

#' @rdname sample_n
#' @export
sample_n.SummarizedExperiment <- function(se, axis, ...){
  axis <- deparse(substitute(axis))
  axis <- match.arg(axis, c("col", "row"))
  if (axis == "row") return(se[getidx(SummarizedExperiment::rowData(se), dplyr::sample_n, ...),])
  se[,getidx(SummarizedExperiment::colData(se), dplyr::sample_n, ...)]
}

#' Sample a fraction of total rows or cols from a se
#' 
#' Selects random fraction of all rows or columns
#' @param se SummarizedExperiment to sample
#' @param axis The axis to perform the operation on. Either row or col.
#' @param ... Additional arguments to sample_frac
#' @examples
#' #Sample half of the genes from seq_se
#' seq_se %>% sample_frac(row, size = .5)
#' @export
sample_frac <- function(se, axis,...){UseMethod("sample_frac")}

#' @rdname sample_frac
#' @export
sample_frac.SummarizedExperiment <- function(se, axis, ...){
  axis <- deparse(substitute(axis))
  axis <- match.arg(axis, c("col", "row"))
  if (axis == "row") return(se[getidx(SummarizedExperiment::rowData(se), dplyr::sample_frac, ...),])
  se[,getidx(SummarizedExperiment::colData(se), dplyr::sample_frac, ...)]
}

#' Create or transform variables
#' 
#' mutate() adds new variables and preserves existing ones; 
#' it preserves the number of rows/cols of the input. 
#' New variables overwrite existing variables of the same name.
#' @param se SummarizedExperiment to subset
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
#' @export mutate
mutate <- function(se, axis, ...){UseMethod("mutate")}

#' @rdname mutate
#' @export
mutate.SummarizedExperiment <- function(se, axis, ...){
  axis <- deparse(substitute(axis))
  axis <- match.arg(axis, c("col", "row"))
  if (axis == "row") return(update_se(se, rowData = dplyr::mutate(as.data.frame(SummarizedExperiment::rowData(se)), ...)))
  update_se(se, colData = dplyr::mutate(as.data.frame(SummarizedExperiment::colData(se)), ...))
}

#####################################################################
#################### Arithmetic operations  #########################
#####################################################################

#' Subtract two SummarizedExperiments
#' 
#' For 2 equal sized se's containing the same assays, subtracts the values from
#' each of the underlying assay values. The returned se is of the same size, and contains
#' the row and column data from the original first se argument.
#' @param se1,se2 SummarizedExperiments for which se2 will be subtracted from se1
#' @examples
#' #create subset dfs of time == 0 and 4h, and subtract their values
#' (select(seq_se, time == 0)) - (select(seq_se, time == 04))
#' @export
`-.SummarizedExperiment` <- function(se1, se2){
  arith_se(se1, se2, `-`)
}

#' Add two SummarizedExperiments
#' 
#' For 2 equal sized se's containing the same assays, adds the values from
#' each of the underlying assay values. The returned se is of the same size, and contains
#' the row and column data from the original first se argument.
#' @param se1,se2 SummarizedExperiments to add
#' @examples
#' #create subset dfs of time == 0 and 4h, and add their values
#' (select(seq_se, time == 0)) + (select(seq_se, time == 04))
#' @export
`+.SummarizedExperiment` <- function(se1, se2){
  arith_se(se1, se2, `+`)
}

#' Divide two SummarizedExperiments
#' 
#' For 2 equal sized se's containing the same assays, divides the values from
#' each of the underlying assay values. The returned se is of the same size, and contains
#' the row and column data from the original first se argument.
#' @param se1,se2 SummarizedExperiments for which se1 will be divided by se2
#' @examples
#' #create subset dfs of time == 0 and 4h, and divide their values
#' (select(seq_se, time == 0)) / (select(seq_se, time == 04))
#' @export
`/.SummarizedExperiment` <- function(se1, se2){
  arith_se(se1, se2, `/`)
}

#' Multiply two SummarizedExperiments
#' 
#' For 2 equal sized se's containing the same assays, multiplies the values from
#' each of the underlying assay values. The returned se is of the same size, and contains
#' the row and column data from the original first se argument.
#' @param se1,se2 SummarizedExperiments to multiply
#' @examples
#' #create subset dfs of time == 0 and 4h, and multiply their values
#' (select(seq_se, time == 0)) * (select(seq_se, time == 04))
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
  update_se(x, assays = lapply(SummarizedExperiment::assays(x), function(mat)round(mat, digits)))
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
#' Rounds the values in all of the underlying assay matrices to the given number of digits
#' @param se SummarizedExperiment to round
#' @param assayname Name of the assay contained in se to write
#' @param path Path or connection to write to
#' @examples
#' # write the example se to csv
#' seq_se %>% round(3) %>% write_csv("expression", "out.csv")
#' @export
write_csv <- function(se, assayname, path){UseMethod("write_csv")}

#' @rdname write_csv
#' @export
write_csv <- function(se, assayname, path){ #for each assay
  if (!assayname %in% SummarizedExperiment::assayNames(se))stop(paste0("Assay ", assayname, " does not exist in the supplied se."))
  readr::write_csv(get_delim_df(se, assayname), path, col_names = FALSE)
}

#' Write a se to tsv format
#' 
#' Rounds the values in all of the underlying assay matrices to the given number of digits
#' @param se SummarizedExperiment to round
#' @param assayname Name of the assay contained in se to write
#' @param path Path or connection to write to
#' @examples
#' # write the example se to tsv
#' seq_se %>% round(3) %>% write_tsv("expression", "out.tsv")
#' @export
write_tsv <- function(se, assayname, path){UseMethod("write_tsv")}

#' @rdname write_tsv
#' @export
write_tsv <- function(se, assayname, path){ #for each assay
  if (!assayname %in% SummarizedExperiment::assayNames(se))stop(paste0("Assay ", assayname, " does not exist in the supplied se."))
  readr::write_tsv(get_delim_df(se, assayname), path, col_names = FALSE)
}

#' Write a se to delim format
#' 
#' Rounds the values in all of the underlying assay matrices to the given number of digits
#' @param se SummarizedExperiment to round
#' @param assayname Name of the assay contained in se to write
#' @param path Path or connection to write to
#' @param delim Delimiter used to separate values
#' @examples
#' # write the example se to delim file, defaulting to " " as delimiter
#' seq_se %>% round(3) %>% write_delim("expression", "out.txt")
#' @export
write_delim <- function(se, assayname, path, delim){UseMethod("write_delim")}

#' @rdname write_delim
#' @export
write_delim <- function(se, assayname, path, delim = " "){ #for each assay
  if (!assayname %in% SummarizedExperiment::assayNames(se))stop(paste0("Assay ", assayname, " does not exist in the supplied se."))
  readr::write_delim(get_delim_df(se, assayname), path, col_names = FALSE)
}