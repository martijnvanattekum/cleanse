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
    line <- paste(stringr::str_pad(critname, maxlen), ":", 
                  (df[,critname] %>% 
                     unlist %>% 
                     unique %>% 
                     sort %>% 
                     paste(collapse = ", ")), "\n")
    if (nchar(line) > linelen) line <- paste0(substr(line, 1, linelen), "...\n")
    cat(line)
  }
} 

#returns subset based on metadata
subset_se <- function(se, axis, fun, ...){
  if (!axis %in% c("row", "col")) {
    stop("Argument axis needs to be either row or col")}
  
  .extract_fun <- if (axis == "row") get_row_data else get_col_data
  
  idx <- se %>% 
    .extract_fun() %>% 
    dplyr::mutate(idx = seq_len(nrow(.))) %>% 
    fun(...) %>% 
    dplyr::pull(idx)

  return(if (axis == "row") se[idx, ] else se[, idx])
}

#changes metadata
update_metadata_se <- function(se, axis, fun, ...) {
  
  if (!axis %in% c("row", "col")) {
    stop("Argument axis needs to be either row or col")}
  
  se_copy <- se
  coldt <- get_col_data(se_copy) %>% {if (axis == "row") . else fun(., ...)}
  rowdt <- get_row_data(se_copy) %>% {if (axis == "col") . else fun(., ...)}
  se_copy %>% 
    set_col_data(coldt) %>% 
    set_row_data(rowdt)

  }

#changes assays
update_assays_se <- function(se, assays) {

  SummarizedExperiment::SummarizedExperiment(
    assays = assays, 
    colData = SummarizedExperiment::colData(se), 
    rowData = SummarizedExperiment::rowData(se))
}

#perform arithmic to 2 equal-sized se's
arith_se <- function(se1, se2, fun) {
  
  if (!identical(SummarizedExperiment::assayNames(se1), 
                 SummarizedExperiment::assayNames(se2))) {
    warning("The assay names of se1 and se2 are not the same")}
  if (!identical(get_col_data(se1), get_col_data(se2))) {
    warning("The colData of se1 and se2 is not the same")}
  if (!identical(get_row_data(se1), get_row_data(se2))) {
    warning("The rowData of se1 and se2 is not the same")}
  assays <- lapply(SummarizedExperiment::assayNames(se1), 
                   function(name) fun(SummarizedExperiment::assays(se1)[[name]], 
                                      SummarizedExperiment::assays(se2)[[name]])
                   ) %>% 
    `names<-`(SummarizedExperiment::assayNames(se1))
  update_assays_se(se1, assays)
  
}

#create a df with all char cols from a df, list or matrix. Helper function for 
# get_delim_df
as.char.df <- function(.data) {
  .data %>% 
    data.frame(stringsAsFactors = FALSE) %>% 
    lapply(as.character) %>% 
    data.frame(stringsAsFactors = FALSE)
}

#returns a writable df based on each assay in the se
get_delim_df <- function(se, assay_name = NULL) {

  coldt <- get_col_data(se) %>% as.char.df()
  rowdt <- get_row_data(se) %>% as.char.df()
  assay <- SummarizedExperiment::assays(se)[[assay_name]] %>% as.char.df() %>% 
    #conforms with names from the header df
    `colnames<-`(paste0("X", seq_len(ncol(.)))) %>% 
    `rownames<-`(NULL)
  
  leftcol <- data.frame(col0 = c(toupper(assay_name), names(coldt), 
                                 rep("", nrow(assay))))
  middlecols <- data.frame(lapply(names(rowdt), function(name)
    c(name, rep("", ncol(coldt)), rowdt[[name]]))) %>% 
    `colnames<-`(paste0("col", seq_len(ncol(rowdt))))
  rightcols <- dplyr::bind_rows(dplyr::bind_cols(
    data.frame(rep("", nrow(coldt))), #header
    coldt) %>% 
      t %>% 
      data.frame(stringsAsFactors = FALSE),
    assay) #values
  
  dplyr::bind_cols(leftcol, middlecols, rightcols)
}
