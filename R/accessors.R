## quiets concerns of R CMD check about dplyr variables
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

#' Gets column data from an se as a tibble
#' 
#' @param se SummarizedExperiment
#' @returns A tibble containing the coldata of the SummarizedExperiment
#' @examples
#' data(seq_se)
#' get_col_data(seq_se)
#' @importFrom tibble tibble
#' @export
get_col_data <- function(se) {

  se %>% 
    SummarizedExperiment::colData() %>% 
    data.frame %>% 
    tibble::tibble()
    
}

#' Gets row data from an se as a tibble
#' 
#' @param se SummarizedExperiment
#' @returns A tibble containing the rowdata of the SummarizedExperiment
#' @examples
#' data(seq_se)
#' get_row_data(seq_se)
#' @importFrom tibble tibble
#' @export
get_row_data <- function(se) {
  
  se %>% 
    SummarizedExperiment::rowData() %>% 
    data.frame %>% 
    tibble::tibble()
  
}

#' Sets column data from an se
#' 
#' As this does not require inplace operations, this can typically be used
#' in a pipe
#' 
#' @param se SummarizedExperiment
#' @param coldata data frame or tibble containing the coldata
#' @returns A SummarizedExperiment with the new coldata set
#' @examples
#' data(seq_se)
#' new_cd <- get_col_data(seq_se) %>% 
#' tidyr::unite(time_treatment, c(time, treatment))
#' se_new_coldata <- set_col_data(seq_se, new_cd)
#' @importFrom SummarizedExperiment colData
#' @importFrom S4Vectors DataFrame
#' @importFrom tidyr unite
#' @export
set_col_data <- function(se, coldata) {
  
  se_copy <- se
  coldata_for_se <- coldata %>% 
    data.frame() %>% 
    S4Vectors::DataFrame()
  
  SummarizedExperiment::colData(se_copy) <- coldata_for_se
  se_copy
  
}

#' Sets row data from an se
#' 
#' As this does not require inplace operations, this can typically be used
#' in a pipe
#' 
#' @param se SummarizedExperiment
#' @param rowdata data frame or tibble containing the coldata
#' @returns A SummarizedExperiment with the new rowdata set
#' @examples
#' data(seq_se)
#' new_rd <- get_row_data(seq_se) %>% 
#' tidyr::unite(gene_group_name, c(gene_group, gene_name))
#' se_new_rowdata <- set_row_data(seq_se, new_rd)
#' @importFrom SummarizedExperiment rowData
#' @importFrom S4Vectors DataFrame
#' @export
set_row_data <- function(se, rowdata) {
  
  se_copy <- se
  rowdata_for_se <- rowdata %>% 
    data.frame() %>% 
    S4Vectors::DataFrame()
  
  SummarizedExperiment::rowData(se_copy) <- rowdata_for_se
  se_copy
  
}

#' Gets available options for a particular column of the coldata of an se
#' 
#' @param se SummarizedExperiment
#' @param colname the column name (character) to extract options for
#' @returns A vector containing the unique options for a coldata column
#' @examples
#' data(seq_se)
#' options_from_coldata(seq_se, "site")
#' @importFrom dplyr pull
#' @importFrom stats na.omit
#' @export
options_from_coldata <- function(se, colname) {
  
  se %>% 
    get_col_data() %>% 
    dplyr::pull(!!colname) %>% 
    unique %>% 
    stats::na.omit() %>% 
    as.vector
  
}

