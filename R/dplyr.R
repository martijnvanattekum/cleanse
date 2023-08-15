## quiets concerns of R CMD check about dplyr variables
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

#' Returns rows or cols of se with matching conditions
#' 
#' Use filter() to choose rows/genes where conditions are true. Uses attached 
#' metadata
#' to find matches. 
#' @param .data SummarizedExperiment to subset
#' @param axis The axis to perform the operation on. Either row or col.
#' @param ... Logical predicates defined in terms of the variables in .data. 
#' Multiple conditions are combined with & or ,. Only rows where the condition 
#' evaluates to TRUE are kept.
#' @returns A SummarizedExperiment after the filter operation
#' @examples
#' #filter the rows of the example se with genes from the IL group
#' data(seq_se)
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
#' negative values to drop. The values provided must be either all positive or 
#' all negative. Indices beyond the number of rows in the input are silently 
#' ignored.
#'
#' The arguments in ... are automatically quoted and evaluated in the context of 
#' the data frame. They support unquoting and splicing. See 
#' vignette("programming") 
#' for an introduction to these concepts.
#' @returns A SummarizedExperiment after the slice operation
#' @examples
#' #subset the first 10 cols of the se
#' data(seq_se)
#' seq_se %>% slice(col, 1:10)
#' @importFrom dplyr slice
#' @export
slice.SummarizedExperiment <- function(.data, axis, ...){
  subset_se(.data, deparse(substitute(axis)), dplyr::slice, ...)
}

#' Randomly selects rows or columns
#' 
#' Uses a slice operation to subset
#' @param .data SummarizedExperiment to subset
#' @param axis The axis to perform the operation on. Either row or col.
#' @param ... Arguments passed to dplyr::slice_sample
#' @param n  See prop
#' @param prop Provide either n, the number of rows, or prop, the proportion of 
#' rows to select. If neither are supplied, n = 1 will be used. If n is greater 
#' than the number of rows in the group (or prop > 1), the result will be 
#' silently truncated to the group size. prop will be rounded towards zero to 
#' generate an integer number of rows. A negative value of n or prop will be 
#' subtracted from the group size. For example, n = -2 with a group of 5 rows 
#' will select 5 - 2 = 3 rows; prop = -0.25 with 8 rows will select 
#' 8 * (1 - 0.25) = 6 rows.
#' @returns A SummarizedExperiment after the sample operation
#' @examples
#' # subset 5 random columns
#' data(seq_se)
#' seq_se %>% slice_sample(col, n=5)
#' # randomly subset 20% of rows
#' seq_se %>% slice_sample(row, prop=.2)
#' @importFrom dplyr slice
#' @export
slice_sample.SummarizedExperiment <- function(.data, axis, ..., n, prop){
  subset_se(.data, deparse(substitute(axis)), dplyr::slice_sample, ..., n=n, 
            prop=prop)
}

#' Arrange by variables
#' 
#' Order either rows or cols from se by an expression involving its variables.
#' @param .data SummarizedExperiment to arrange
#' @param axis The axis to perform the operation on. Either row or col.
#' @param ... Comma separated list of unquoted variable names, 
#' or expressions involving variable names. 
#' Use desc() to sort a variable in descending order
#' @returns A SummarizedExperiment after the filter operation
#' @examples
#' # Arrange seq_se by gene_group and then gene_name
#' data(seq_se)
#' seq_se %>% arrange(row, gene_group, gene_name)
#' @importFrom dplyr arrange
#' @export
arrange.SummarizedExperiment <- function(.data, axis, ...){
  subset_se(.data, deparse(substitute(axis)), dplyr::arrange, ...)
}

#' Select variables by name for rowData or colData
#' 
#' Choose variables from the rowData or colData that you want to keep. Select 
#' drops all other variables.
#' @param .data SummarizedExperiment to subset
#' @param axis The axis to perform the operation on. Either row or col.
#' @param ... One or more unquoted expressions separated by commas. You can 
#' treat variable names like they are positions, so you can use expressions 
#' like x:y to select ranges of variables.
#' Positive values select variables; negative values drop variables. If the 
#' first expression is negative, select() will automatically start with all 
#' variables.  
#' 
#' Use named arguments, e.g. new_name = old_name, to rename selected variables.  
#' 
#' The arguments in ... are automatically quoted and evaluated in a context 
#' where column names represent column positions. They also support unquoting 
#' and splicing. See vignette("programming") for an introduction to these 
#' concepts.  
#' 
#' See select helpers for more details and examples about tidyselect helpers 
#' such as starts_with(), everything(), ...
#' @returns A SummarizedExperiment after the select operation
#' @examples
#' # remove the time variable after filtering for time == 0
#' data(seq_se)
#' seq_se %>% filter(col, time == 0) %>% select(col, -time)
#' @importFrom dplyr select
#' @export
select.SummarizedExperiment <- function(.data, axis, ...){
  update_metadata_se(.data, deparse(substitute(axis)), dplyr::select, ...)
} 

#' Rename variables by name for rowData or colData
#' 
#' Choose variables from the rowData or colData that you want to rename. Rename 
#' keeps all other variables.
#' @param .data SummarizedExperiment to subset
#' @param axis The axis to perform the operation on. Either row or col.
#' @param ... One or more unquoted expressions separated by commas. You can 
#' treat variable names like they are positions, so you can use expressions 
#' like x:y to select ranges of variables.
#' 
#' Positive values select variables; negative values drop variables. If the 
#' first expression is negative, select() will automatically start with all 
#' variables.  
#' 
#' Use named arguments, e.g. new_name = old_name, to rename selected variables.  
#' 
#' The arguments in ... are automatically quoted and evaluated in a context 
#' where column names represent column positions. They also support unquoting 
#' and splicing. See vignette("programming") for an introduction to these 
#' concepts. 
#'  
#' See select helpers for more details and examples about tidyselect helpers 
#' such as starts_with(), everything(), ...
#' @returns A SummarizedExperiment after the rename operation
#' @examples
#' # rename the time variable after changing it to minutes
#' data(seq_se)
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
#' length as the number of rows/cols in row- or colData. The name of each 
#' argument will 
#' be the name of a new variable, and the value will be its corresponding value. 
#' Use a NULL value in mutate to drop a variable. New variables overwrite 
#' existing variables of the same name.
#' The arguments in ... are automatically quoted and evaluated in the context 
#' of the data frame. 
#' They support unquoting and splicing. See vignette("programming") for an 
#' introduction to these concepts.
#' @returns A SummarizedExperiment after the mutate operation
#' @examples
#' #Change the treatment time from hours to minutes
#' data(seq_se)
#' seq_se %>% mutate(col, time = (time * 60))
#' @importFrom dplyr mutate
#' @export
mutate.SummarizedExperiment <- function(.data, axis, ...){
  update_metadata_se(.data, deparse(substitute(axis)), dplyr::mutate, ...)
} 