% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/value_attribute_classifiers.R
\name{numeric_values_classifier}
\alias{numeric_values_classifier}
\alias{numeric_values_classifier.default}
\alias{numeric_values_classifier.cell_df}
\title{Value/Attribute Classifier which considers number like cells as values}
\usage{
numeric_values_classifier(x, ..., allow_chars, verbose = FALSE)

\method{numeric_values_classifier}{default}(x, ..., allow_chars = NULL, verbose = FALSE)

\method{numeric_values_classifier}{cell_df}(x, ..., allow_chars, verbose = FALSE)
}
\arguments{
\item{x}{Either a \code{\link[=cell_df-class]{cell_df}} or
leave blank for function output (required by \code{\link[=value_attribute_classify]{value_attribute_classify}})}

\item{allow_chars}{(Optional) Allow characters together with numbers. (a character vector)}

\item{verbose}{Logical scalar for printing discovered cells (which gets new type by this rule)}
}
\description{
Value/Attribute Classifier which considers number like cells as values
}
\seealso{
\link[=value_attribute_classify]{value_attribute_classify}
}
\keyword{internal}
