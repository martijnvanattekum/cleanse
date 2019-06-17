## quiets concerns of R CMD check about dplyr variables
if(getRversion() >= "2.15.1") utils::globalVariables(c(".", "idx"))

#print function
printdata <- function(.data){
  linelen <- 100
  df <- .data %>% 
    as.data.frame
  critnames <- names(df)
  maxlen <- critnames %>% nchar %>% max()
  for (critname in critnames){
    line <- paste(stringr::str_pad(critname, maxlen), ":", (df[,critname] %>% unlist %>% unique %>% sort %>% paste(collapse = ", ")), "\n")
    if (nchar(line) > linelen) line <- paste0(substr(line, 1, linelen), "...\n")
    cat(line)
  }
} 

#returns subset based on metadata
subset_se <- function(se, axis, fun, ...){
  if (!axis %in% c("row", "col")) stop("Argument axis needs to be either row or col")
  
  idx <- {if (axis == "row") SummarizedExperiment::rowData(se) else (SummarizedExperiment::colData(se))} %>% 
    as.data.frame %>% 
    dplyr::mutate(idx = 1:nrow(.)) %>% 
    fun(...) %>% 
    dplyr::pull(idx)

  return(if (axis == "row") se[idx, ] else se[, idx])
}

#changes metadata
update_metadata_se <- function(se, axis, fun, ...) {
#  axis <- deparse(axis)
#  print(paste("axis", axis))
  if (!axis %in% c("row", "col")) stop("Argument axis needs to be either row or col")
  coldt <- SummarizedExperiment::colData(se) %>% {if (axis == "row") . else fun(as.data.frame(.), ...)}
  rowdt <- SummarizedExperiment::rowData(se) %>% {if (axis == "col") . else fun(as.data.frame(.), ...)}
  SummarizedExperiment::SummarizedExperiment(assays = SummarizedExperiment::assays(se), 
                                             colData = coldt, 
                                             rowData = rowdt)
}

update_metadata_se(se, "row", dplyr::select_if, fun)



#changes assays
update_assays_se <- function(se, assays) {
  SummarizedExperiment::SummarizedExperiment(assays = assays, 
                                             colData = SummarizedExperiment::colData(se), 
                                             rowData = SummarizedExperiment::rowData(se))
}

#perform arithmic to 2 equal-sized se's
arith_se <- function(se1, se2, fun) {
  if (!identical(SummarizedExperiment::assayNames(se1), SummarizedExperiment::assayNames(se2)))stop("Assay names of se1 and se2 need to be the same to proceed")
  if (!identical(SummarizedExperiment::colData(se1), SummarizedExperiment::colData(se2)))stop("The colData of se1 and se2 need to be the same to proceed") 
  if (!identical(SummarizedExperiment::rowData(se1), SummarizedExperiment::rowData(se2)))stop("The rowData of se1 and se2 need to be the same to proceed") 
  assays <- lapply(SummarizedExperiment::assayNames(se1), 
                   function(name) fun(SummarizedExperiment::assays(se1)[[name]], 
                                      SummarizedExperiment::assays(se2)[[name]])) %>% 
    `names<-`(SummarizedExperiment::assayNames(se1))
  update_assays_se(se1, assays)
}

#create a df with all char cols from a df, list or matrix. Helper function for get_delim_df
as.char.df <- function(.data) {
  .data %>% 
    data.frame(stringsAsFactors = FALSE) %>% 
    lapply(as.character) %>% 
    data.frame(stringsAsFactors = FALSE)
}

#returns a writable df based on each assay in the se
get_delim_df <- function(se, assay_name) {
  
  if (is.null(assay_name) & length(SummarizedExperiment::assayNames(se)) == 1){
    assay_name <- SummarizedExperiment::assayNames(se)[[1]]}
  if (!assay_name %in% SummarizedExperiment::assayNames(se))stop(paste0("Assay '", assay_name, "' does not exist in the supplied se."))
  
  coldt <- SummarizedExperiment::colData(se) %>% as.char.df()
  rowdt <- SummarizedExperiment::rowData(se) %>% as.char.df()
  assay <- SummarizedExperiment::assays(se)[[assay_name]] %>% as.char.df() %>% 
    `colnames<-`(paste0("X", 1:ncol(.))) %>% #conforms with names from the header df
    `rownames<-`(NULL)
  
  leftcol <- data.frame(col0 = c(toupper(assay_name), names(coldt), rep("", nrow(assay))))
  middlecols <- data.frame(lapply(names(rowdt), function(name)
    c(name, rep("", ncol(coldt)), rowdt[[name]]))) %>% 
    `colnames<-`(paste0("col", 1:ncol(rowdt)))
  rightcols <- dplyr::bind_rows(dplyr::bind_cols(data.frame(rep("", nrow(coldt))), #header
                                   coldt) %>% t %>% data.frame(stringsAsFactors = FALSE),
                         assay) #values
  
  dplyr::bind_cols(leftcol, middlecols, rightcols)
}
