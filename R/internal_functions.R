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

#returns indices that can be used to subset the se
getidx <- function(.data, fun, ...){
  .data %>% 
    as.data.frame %>% 
    dplyr::mutate(idx = 1:nrow(.)) %>% 
    fun(...) %>% 
    dplyr::pull(idx)
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
  update_se(se1, assays = assays)
}

#changes one or more of assays, colData, or rowData for an se
update_se <- function(se, assays = NULL, colData = NULL, rowData = NULL) {
  if (all(is.null(assays), is.null(colData), is.null(rowData)))return(se)
  if (is.null(assays))  assays  <- SummarizedExperiment::assays(se)
  if (is.null(colData)) colData <- SummarizedExperiment::colData(se) %>% as.data.frame()
  if (is.null(rowData)) rowData <- SummarizedExperiment::rowData(se) %>% as.data.frame()
  SummarizedExperiment::SummarizedExperiment(assays = assays, colData = colData, rowData = rowData)
}

#create a df with all char cols from a df, list or matrix. Helper function for get_delim_df
as.char.df <- function(.data) {
  .data %>% 
    data.frame(stringsAsFactors = FALSE) %>% 
    lapply(as.character) %>% 
    data.frame(stringsAsFactors = FALSE)
}

#returns a writable df based on each assay in the se
get_delim_df <- function(se, assayname) {

  coldt <- SummarizedExperiment::colData(se) %>% as.char.df()
  rowdt <- SummarizedExperiment::rowData(se) %>% as.char.df()
  assay <- SummarizedExperiment::assays(se)[[assayname]] %>% as.char.df()
  
  leftcol <- data.frame(col0 = c(toupper(assayname), names(coldt), rep("", nrow(assay))))
  middlecols <- data.frame(lapply(names(rowdt), function(name)
    c(name, rep("", ncol(coldt)), rowdt[[name]]))) %>% 
    `colnames<-`(paste0("col", 1:ncol(rowdt)))
  rightcols <- dplyr::bind_rows(dplyr::bind_cols(data.frame(rep("", nrow(coldt))), #header
                                   coldt) %>% t %>% data.frame(stringsAsFactors = FALSE),
                         assay) #values
  
  dplyr::bind_cols(leftcol, middlecols, rightcols)
}
