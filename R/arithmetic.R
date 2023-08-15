## quiets concerns of R CMD check about dplyr variables
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

#' Subtract two SummarizedExperiments
#' 
#' Subtracts the values from each of the underlying assay values for 2 se's. 
#' @param se1,se2 SummarizedExperiments for which se2 values will be subtracted 
#' from se1 values.
#' Need to have the same rowData and colData
#' @returns A SummarizedExperiment after the subtract operation
#' @examples
#' #create subset se's for time == 0 and time == 4. Then subtract their values.
#' #remove time to make colData the same
#' data(seq_se)
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
#' @param se1,se2 SummarizedExperiments for which se1 and se2 values will be 
#' added
#' Need to have the same rowData and colData
#' @returns A SummarizedExperiment after the add operation
#' @examples
#' #create subset se's for time == 0 and time == 4. Then add their values.
#' #remove time to make colData the same
#' data(seq_se)
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
#' @param se1,se2 SummarizedExperiments for which se1 values will be divided by 
#' se2 values.
#' Need to have the same rowData and colData
#' @returns A SummarizedExperiment after the division operation
#' @examples
#' #create subset se's for time == 0 and time == 4. Then subtract their values.
#' #remove time to make colData the same
#' data(seq_se)
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
#' @param se1,se2 SummarizedExperiments for which se1 and se2 values will be 
#' multiplied.
#' Need to have the same rowData and colData
#' @returns A SummarizedExperiment after the multiplication operation
#' @examples
#' #create subset se's for time == 0 and time == 4. Then add their values.
#' #remove time to make colData the same
#' data(seq_se)
#' t0 <- seq_se %>% filter(col, time == 0) %>% select(col, -time)
#' t4 <- seq_se %>% filter(col, time == 4) %>% select(col, -time)
#' t4 * t0
#' @export
`*.SummarizedExperiment` <- function(se1, se2){
  arith_se(se1, se2, `*`)
}

#' Round the values of a SummarizedExperiments
#' 
#' Rounds the values in all of the underlying assay matrices to the given 
#' number of digits
#' @param x SummarizedExperiment to round
#' @param digits integer indicating the number of decimal places
#' @returns A SummarizedExperiment after the round operation
#' @examples 
#' # round the example se to 2 digits
#' data(seq_se)
#' seq_se %>% round(2)
#' @export
round.SummarizedExperiment <- function(x, digits = 0){
  update_assays_se(x, lapply(SummarizedExperiment::assays(x), 
                             \(mat)base::round(mat, digits)))
}