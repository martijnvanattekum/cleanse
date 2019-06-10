% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/user_functions.R
\name{*.SummarizedExperiment}
\alias{*.SummarizedExperiment}
\title{Multiply two SummarizedExperiments}
\usage{
\method{*}{SummarizedExperiment}(se1, se2)
}
\arguments{
\item{se1, se2}{SummarizedExperiments to multiply}
}
\description{
For 2 equal sized se's containing the same assays, multiplies the values from
each of the underlying assay values. The returned se is of the same size, and contains
the row and column data from the original first se argument.
}
\examples{
#create subset dfs of time == 0 and 4h, and multiply their values
(select(seq_se, time == 0)) * (select(seq_se, time == 04))
}