## quiets concerns of R CMD check about dplyr variables
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

#' Prints available options for the se 
#' 
#' Creates a print of all possible assays, rowData and colData
#' @param se SummarizedExperiment to show information for
#' @returns The input SummarizedExperiment
#' @examples
#' #show available options for the example se
#' data(seq_se)
#' print_options(seq_se)
#' @export
print_options <- function(se) {
  cat(paste("** ASSAY OPTIONS FOR", deparse(substitute(se)), "** \n"))
  cat(paste(SummarizedExperiment::assayNames(se), collapse = ", "))
  cat(paste("\n\n** COLDATA OPTIONS FOR", deparse(substitute(se)), "** \n"))
  printdata(get_col_data(se))
  cat(paste("\n** ROWDATA OPTIONS FOR", deparse(substitute(se)), "** \n"))
  printdata(get_row_data(se))
  se
}

#' Write a se to csv format
#' 
#' @param se SummarizedExperiment to round
#' @param path Path or connection to write to
#' @param assay_name Name of the assay contained in se to write. Not required 
#' when only 1 assay is contained in the se
#' @returns The path to where the SummarizedExperiment was written
#' @examples
#' # write the example se to csv
#' data(seq_se)
#' seq_se %>% round(3) %>% write_csv("out.csv", "expression")
#' @importFrom readr write_csv 
#' @export
write_csv <- function(se, path, assay_name = NULL){ 
  if (is.null(assay_name) & length(SummarizedExperiment::assayNames(se)) == 1){
    assay_name <- SummarizedExperiment::assayNames(se)[[1]]}
  if (!assay_name %in% SummarizedExperiment::assayNames(se)){
    stop(paste0("Assay '", assay_name, "' does not exist in the supplied se."))}
  readr::write_csv(get_delim_df(se, assay_name), path, col_names = FALSE)
  path
}

#' Write a se to tsv format
#' 
#' Rounds the values in all of the underlying assay matrices to the given 
#' number of digits
#' @param se SummarizedExperiment to round
#' @param path Path or connection to write to
#' @param assay_name Name of the assay contained in se to write. Not required 
#' when only 1 assay is contained in the se
#' @returns The path to where the SummarizedExperiment was written
#' @examples
#' # write the example se to tsv
#' data(seq_se)
#' seq_se %>% round(3) %>% write_tsv("out.tsv", "expression")
#' @importFrom readr write_tsv
#' @export
write_tsv <- function(se, path, assay_name = NULL){ 
  if (is.null(assay_name) & length(SummarizedExperiment::assayNames(se)) == 1){
    assay_name <- SummarizedExperiment::assayNames(se)[[1]]}
  if (!assay_name %in% SummarizedExperiment::assayNames(se)){
    stop(paste0("Assay '", assay_name, "' does not exist in the supplied se."))}
  readr::write_tsv(get_delim_df(se, assay_name), path, col_names = FALSE)
  path
}

#' Write a se to delim format
#' 
#' Rounds the values in all of the underlying assay matrices to the given 
#' number of digits
#' @param se SummarizedExperiment to round
#' @param path Path or connection to write to
#' @param delim Delimiter used to separate values
#' @param assay_name Name of the assay contained in se to write. Not required 
#' when only 1 assay is contained in the se
#' @returns The path to where the SummarizedExperiment was written
#' @examples
#' # write the example se to delim file, defaulting to " " as delimiter
#' data(seq_se)
#' seq_se %>% round(3) %>% write_delim("out.txt", assay_name = "expression")
#' @importFrom readr write_delim
#' @export
write_delim <- function(se, path, delim = " ", assay_name = NULL){
  if (is.null(assay_name) & length(SummarizedExperiment::assayNames(se)) == 1){
    assay_name <- SummarizedExperiment::assayNames(se)[[1]]}
  if (!assay_name %in% SummarizedExperiment::assayNames(se)){
    stop(paste0("Assay '", assay_name, "' does not exist in the supplied se."))}
  readr::write_delim(get_delim_df(se, assay_name), path, col_names = FALSE)
  path
}