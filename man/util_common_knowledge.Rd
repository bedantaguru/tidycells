% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/core_utils.R
\name{util_common_knowledge}
\alias{util_common_knowledge}
\title{Common Knowledge Store/Get for this package environment (memoization)}
\usage{
util_common_knowledge(
  ...,
  common_knowledge_head_name = "common_knowledge",
  clean = FALSE,
  append = TRUE,
  exists = FALSE
)
}
\arguments{
\item{...}{Named arguments to set (key = value); unnamed character vectors to
get keys.}

\item{common_knowledge_head_name}{Name for the main list in the environment (default "common_knowledge").}

\item{clean}{If TRUE, clears all stored values.}

\item{append}{If TRUE, appends to existing keys (data.frames merged, vectors
concatenated).}

\item{exists}{If TRUE, returns TRUE if all keys exist (for gets).}
}
\value{
Value(s) for get
}
\description{
Stores or retrieves named values in the package-wide internal environment for
cross-module sharing.
}
\keyword{internal}
