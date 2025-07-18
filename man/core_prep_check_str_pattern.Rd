% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/core_preprocess.R
\name{core_prep_check_str_pattern}
\alias{core_prep_check_str_pattern}
\title{Core function to check a string against patterns.}
\usage{
core_prep_check_str_pattern(
  x,
  lookup_regex = list(target = "^[\\\\d.,+-]+$", exception = NULL),
  pattern_class = list(target = "num", exception = "non_num", blank = "blank", default =
    "non_num"),
  allowed_strings = NULL
)
}
\arguments{
\item{x}{A character vector to be checked.}

\item{lookup_regex}{A named list of regex patterns, must contain \code{target} and
\code{exception}.}

\item{pattern_class}{A named list of classification strings, must contain
\code{target}, \code{exception}, \code{blank}, and \code{default}.}

\item{allowed_strings}{A character vector of substrings to be removed from the
input before pattern matching. (These are ignored to allow the underlying
pattern to be matched).}
}
\value{
A character vector with classification results.
}
\description{
This is an internal helper function that provides a generic way to classify
strings based on a set of regex patterns and cleaning rules.
}
\keyword{internal}
