% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/file_operations.R
\name{file_op_detect_and_read}
\alias{file_op_detect_and_read}
\title{Detect and Read File Type and Content}
\usage{
file_op_detect_and_read(fn, omit = NULL)
}
\arguments{
\item{fn}{Character string. Path to the file to be read.}

\item{omit}{(Optional) Character vector. A vector of file types to skip during
detection. Supported types include: \code{"html"}, \code{"csv"}, \code{"tsv"}, \code{"xls"},
\code{"doc"}, \code{"xlsx"}, \code{"docx"}, and \code{"pdf"}. Defaults to \code{NULL} (no types are
skipped).}
}
\value{
A data structure suitable for downstream use: either a \code{data.frame},
\code{matrix}, or a list of such objects. If a list is returned, it may be
homogeneous or heterogeneous depending on the file content.
}
\description{
Automatically detects the file type and attempts to read tabular
content from various file formats, including CSV, HTML, XLS, DOC, XLSX,
DOCX, and PDF. Detection is based on file extensions, magic numbers (file
signatures), and trial-and-error attempts using available R packages. The
function is resilient to incorrect file extensions and adapts dynamically
based on available packages.

This function acts as an automated table reader, focusing on best-effort
content extraction. If reading fails for any format, it will \strong{fail
silently} — without displaying errors or warnings. Users should use this
function cautiously, as it intentionally suppresses detailed error reporting
to ensure smooth fallback behavior.
}
\section{External Dependencies}{
 Reading \code{.doc} files may require external
system dependencies such as
\strong{LibreOffice}, which enables content extraction via back-end system calls.
While this functionality is available, reading DOC files is generally not
recommended unless necessary, due to their less structured nature.
}

\keyword{internal}
