% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/core_classes_generics.R
\name{print.sheets}
\alias{print.sheets}
\title{Print a Sheets Object}
\usage{
\method{print}{sheets}(x, ignore_validation = FALSE, ...)
}
\arguments{
\item{x}{A Sheets object.}

\item{ignore_validation}{Logical. If TRUE, skips validation of the object.
Default is FALSE.}

\item{...}{Further arguments passed to methods (currently unused).}
}
\description{
Prints a summary and details of a Sheets object, including the number of
sheets and their sizes. Optionally validates the object before printing.
}
\keyword{internal}
