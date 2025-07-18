% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/core_preprocess_public.R
\name{value_attribute_classify}
\alias{value_attribute_classify}
\title{Value-Attribute Classification for \code{cells} Objects}
\usage{
value_attribute_classify(
  x,
  method = c("probabilistic", "heuristic_type_1", "heuristic_type_2")
)
}
\arguments{
\item{x}{A valid \code{cells} object (see \linkS4class{cells}).}

\item{method}{Character, one of \code{"probabilistic"}, \code{"heuristic_type_1"}, or
\code{"heuristic_type_2"}. Controls the classification logic:
\describe{
\item{\code{"probabilistic"}}{Uses probabilistic type inference, allowing ambiguous types
(such as \code{"logical"}, \code{"categorical"}, \code{"date"}, and \code{"time"}) to be classified
as either attribute or value based on conditional probability distributions. This is
more resource-intensive but usually gives better result.}
\item{\code{"heuristic_type_1"}}{Assumes that \code{"logical"}, \code{"categorical"}, \code{"date"}, and \code{"time"}
types are almost always attributes (e.g., headers), assigning them \code{PoA = 1}, \code{PoV = 0}.
(default)}
\item{\code{"heuristic_type_2"}}{Assumes ambiguous types (\code{"logical"}, \code{"categorical"}, \code{"date"}, \code{"time"})
may be either attribute or value, assigning both \code{PoA = 0.5}, \code{PoV = 0.5}.}
}}
}
\value{
The input \code{cells} object with updated or added columns: \code{type},
\code{PoA}, and \code{PoV}.
}
\description{
Classifies each cell in a \code{cells} object as more likely to be an "attribute"
(e.g., header or label) or a "value" (e.g., data entry), based on its content
type and configurable heuristics. This function adds or updates the columns
\code{type}, \code{PoA} (Probability of Attribute), and \code{PoV} (Probability of Value) in
the supplied \code{cells} object. This supports tidy and semantic analysis of
tabular data.
}
\details{
This function is central to table structure learning and semantic
table analysis. It uses robust heuristics and probabilistic methods
(depending on \code{method}), but users may further adjust the resulting
probabilities for custom logic. For details on the \code{cells} object structure
and recognized formats, see \linkS4class{cells}.
}
\section{Value-Attribute Classification}{
 The classification process uses the
content of each cell (column \code{value}) and its initially read type (column
\code{data_type}) to determine:
\itemize{
\item \code{type}: The refined content type, one of \code{"numeric"}, \code{"character"},
\code{"logical"}, \code{"categorical"}, \code{"date"}, \code{"time"}, or \code{"blank"}.
\item \code{PoA}: Probability (in \verb{[0,1]}) that the cell is an attribute.
\item \code{PoV}: Probability (in \verb{[0,1]}) that the cell is a value.
}
The constraint \code{PoA + PoV <= 1} holds for all cells.
}

\seealso{
\linkS4class{cells}
}
