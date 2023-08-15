#' Removes non-informative metadata
#' 
#' For each of colData and rowData, removes the variables that have only 1 
#' unique value
#' @param se SummarizedExperiment to drop metadata for
#' @returns A SummarizedExperiment after redundant metadata has been dropped
#' @examples
#' # as the se only contains time == 4 data, the time variable can be dropped
#' data(seq_se)
#' seq_se %>% filter(col, time == 4) %>% drop_metadata 
#' @export
drop_metadata <- function(se){
  
  coldt <- get_col_data(se) %>% dplyr::select_if(\(col) length(unique(col)) > 1)
  rowdt <- get_row_data(se) %>% dplyr::select_if(\(col) length(unique(col)) > 1)
  SummarizedExperiment::SummarizedExperiment(assays = 
                                               SummarizedExperiment::assays(se), 
                                             colData = coldt, 
                                             rowData = rowdt)
}