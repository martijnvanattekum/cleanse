% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/accessors.R
\name{set_row_data}
\alias{set_row_data}
\title{Sets row data from an se}
\usage{
set_row_data(se, rowdata)
}
\arguments{
\item{se}{SummarizedExperiment}

\item{rowdata}{data frame or tibble containing the coldata}
}
\value{
A SummarizedExperiment with the new rowdata set
}
\description{
As this does not require inplace operations, this can typically be used
in a pipe
}
\examples{
data(seq_se)
new_rd <- get_row_data(seq_se) \%>\% 
tidyr::unite(gene_group_name, c(gene_group, gene_name))
se_new_rowdata <- set_row_data(seq_se, new_rd)
}
