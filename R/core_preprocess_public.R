#' Value-Attribute Classification for `cells` Objects
#'
#' Classifies each cell in a `cells` object as more likely to be an "attribute"
#' (e.g., header or label) or a "value" (e.g., data entry), based on its content
#' type and configurable heuristics. This function adds or updates the columns
#' `type`, `PoA` (Probability of Attribute), and `PoV` (Probability of Value) in
#' the supplied `cells` object. This supports tidy and semantic analysis of
#' tabular data.
#'
#' @section Value-Attribute Classification: The classification process uses the
#'   content of each cell (column `value`) and its initially read type (column
#'   `data_type`) to determine:
#' \itemize{
#'   \item `type`: The refined content type, one of `"numeric"`, `"character"`,
#'   `"logical"`, `"categorical"`, `"date"`, `"time"`, or `"blank"`.
#'   \item `PoA`: Probability (in `[0,1]`) that the cell is an attribute.
#'   \item `PoV`: Probability (in `[0,1]`) that the cell is a value.
#' }
#'   The constraint `PoA + PoV <= 1` holds for all cells.
#'
#' @param x A valid `cells` object (see [cells-class]).
#' @param method Character, one of `"probabilistic"`, `"heuristic_type_1"`, or
#'   `"heuristic_type_2"`. Controls the classification logic:
#'   \describe{
#'     \item{`"probabilistic"`}{Uses probabilistic type inference, allowing ambiguous types
#'     (such as `"logical"`, `"categorical"`, `"date"`, and `"time"`) to be classified
#'     as either attribute or value based on conditional probability distributions. This is
#'     more resource-intensive but usually gives better result.}
#'     \item{`"heuristic_type_1"`}{Assumes that `"logical"`, `"categorical"`, `"date"`, and `"time"`
#'     types are almost always attributes (e.g., headers), assigning them `PoA = 1`, `PoV = 0`.
#'     (default)}
#'     \item{`"heuristic_type_2"`}{Assumes ambiguous types (`"logical"`, `"categorical"`, `"date"`, `"time"`)
#'     may be either attribute or value, assigning both `PoA = 0.5`, `PoV = 0.5`.}
#'   }
#'
#' @return The input `cells` object with updated or added columns: `type`,
#'   `PoA`, and `PoV`.
#'
#' @details This function is central to table structure learning and semantic
#'   table analysis. It uses robust heuristics and probabilistic methods
#'   (depending on `method`), but users may further adjust the resulting
#'   probabilities for custom logic. For details on the `cells` object structure
#'   and recognized formats, see [cells-class].
#'
#' @seealso [cells-class]
#' @export
value_attribute_classify <- function(
    x,
    method = c("probabilistic", "heuristic_type_1", "heuristic_type_2")) {

  # Check and halt if the input is not a valid cells object. Otherwise, proceed
  # with the classification
  if(isFALSE(core_cells_validation_end_use(x))){
    rlang::abort("Malformed Cells - Halted Execution! Please fix the issues.")
  }

  method <- match.arg(method)

  if(method == "probabilistic") {
    x <- core_prep_va_heuristic_probabilistic(x)
  } else if(method == "heuristic_type_1") {
    x <- core_prep_va_heuristic_type_1(x)
  } else if(method == "heuristic_type_2") {
    x <- core_prep_va_heuristic_type_2(x)
  }

  return(x)
}
