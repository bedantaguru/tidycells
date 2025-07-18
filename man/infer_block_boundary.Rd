% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/infer_helpers_analysis.R
\name{infer_block_boundary}
\alias{infer_block_boundary}
\title{Weighted Percentile Boundary of Grid Cells (blocks)}
\usage{
infer_block_boundary(df, coverage = 0.95, exact = FALSE)
}
\arguments{
\item{df}{Data frame with 'row' and 'col' columns. (Optionally grouped)}

\item{coverage}{Proportion to cover, default is 0.95 (95\% block).}

\item{exact}{If \code{TRUE}, uses exact min/max instead of percentiles. Default
is \code{FALSE}.}
}
\value{
tibble: r_min, r_max, c_min, c_max. (group vars if \code{df} is grouped)
}
\description{
Calculates the bounding box covering a specified proportion of filled cells,
using weighted percentiles of cell counts.
}
\keyword{internal}
