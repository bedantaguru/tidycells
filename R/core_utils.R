
util_convertible_to_processable <- function(x){
  # either data frame or list of data frame or matrix or list matrix list of
  # matrix and data frame (mix is also allowed)
  if (is.data.frame(x) || is.matrix(x)) {
    return(TRUE)
  }

  if (is.list(x)) {
    if (all(purrr::map_lgl(x, ~ is.data.frame(.x) || is.matrix(.x)))) {
      return(TRUE)
    }
  }

  return(FALSE)
}


util_utilize_namespace_object <- function(pkg, what, ...){
  e <- asNamespace(pkg)
  if(exists(what, envir = e, inherits = FALSE)) {
    if (is.function(e[[what]])) {
      return(e[[what]](...))
    }
  }
}

# Internal environment for package-wide use
util_internal_env_of_this_pkg <- new.env()

#' Common Knowledge Store/Get for this package environment (memoization)
#'
#' Stores or retrieves named values in the package-wide internal environment for
#' cross-module sharing.
#'
#' @param ... Named arguments to set (key = value); unnamed character vectors to
#'   get keys.
#' @param common_knowledge_head_name Name for the main list in the environment (default "common_knowledge").
#' @param clean If TRUE, clears all stored values.
#' @param append If TRUE, appends to existing keys (data.frames merged, vectors
#'   concatenated).
#' @param exists If TRUE, returns TRUE if all keys exist (for gets).
#' @return Value(s) for get
#' @keywords internal
util_common_knowledge <- function(
    ...,
    common_knowledge_head_name = "common_knowledge",
    clean = FALSE, append = TRUE, exists = FALSE) {

  env <- util_internal_env_of_this_pkg

  if (clean) {
    env[[common_knowledge_head_name]] <- list()
    return(invisible(NULL))
  }

  dots <- list(...)
  nds <- names(dots)
  ck <- env[[common_knowledge_head_name]]
  if (is.null(ck)) ck <- list()

  # SET: named args
  if (!is.null(nds) && any(nzchar(nds))) {
    for (nm in nds[nzchar(nds)]) {
      val <- dots[[nm]]
      if (append && !is.null(ck[[nm]])) {
        # If both are data.frames, merge and remove duplicates
        if (is.data.frame(val) && is.data.frame(ck[[nm]])) {
          ck[[nm]] <- dplyr::bind_rows(ck[[nm]], val) %>% dplyr::distinct()
        } else {
          # Otherwise, concatenate and de-duplicate
          ck[[nm]] <- unique(c(ck[[nm]], val))
        }
      } else {
        ck[[nm]] <- val
      }
    }
    env[[common_knowledge_head_name]] <- ck
    return(invisible(NULL))
  }

  # GET: unnamed args (must be character keys)
  if (length(dots) > 0) {
    keys <- unlist(dots)
    vals <- ck[keys]
    if (exists) return(all(!vapply(vals, is.null, logical(1))))
    if (length(vals) == 1) return(vals[[1]])
    return(vals)
  }
  invisible(NULL)
}


#' Optimally apply a function to a vector by handling duplicates
#'
#' Efficiently applies a (potentially expensive) function to a vector by
#' avoiding redundant computations for duplicate values. If the input vector is
#' large and contains many repeated values, the function is applied only to the
#' unique values, and results are mapped back to the original ordering. This
#' optimization is triggered only when both the input length and the proportion
#' of unique values cross user-specified thresholds.
#'
#' This function is only safe for "value-based" functions: those for which
#' `f(x)[i]` depends only on the value of `x[i]`, not on its position in `x` or
#' any global state. The output of `f(x)` must be the same length as `x`.
#'
#' If the function also requires other arguments that should be subset in sync
#' with `x`, their names may be supplied in `paired_arguments`.
#'
#' @param x Input vector.
#' @param f A vectorized function to apply to `x`. Must be "value-based" (see
#'   Details).
#' @param ratio_threshold If (`n_distinct / n_total`) is below this, the
#'   optimization is triggered. Defaults to 0.7.
#' @param length_threshold If `length(x)` is below this, `f` is applied
#'   directly. Defaults to 10000.
#' @param paired_arguments Optional character vector of additional argument
#'   names that, if supplied, should be subset in parallel with unique values of
#'   `x`.
#' @param ... Additional arguments passed to `f`.
#'
#' @return A vector with the result of `f(x, ...)`.
#' @keywords internal
util_vector_operation_optim <- function(
    x, f,
    ratio_threshold = 0.7,
    length_threshold = 10000,
    paired_arguments = NULL,
    ...
) {

  n_total <- length(x)
  if (n_total == 0) return(x)
  if (n_total < length_threshold) return(f(x, ...))

  # Identify unique values (preserve order of first appearance)
  unique_idx <- !duplicated(x)
  x_unique <- x[unique_idx]
  n_distinct <- length(x_unique)
  if ((n_distinct / n_total) < ratio_threshold) {

    # Collect all arguments to pass to f
    arg_list <- c(list(x = x_unique), list(...))
    if (!is.null(paired_arguments)) {
      # Subset paired arguments in sync with x_unique
      arg_list[paired_arguments] <- lapply(
        arg_list[paired_arguments],
        function(arg) if (!is.null(arg)) arg[unique_idx] else arg
      )
    }

    results_unique <- do.call(f, arg_list)
    # Map results back to the original vector
    return(results_unique[match(x, x_unique)])
  } else {
    return(f(x, ...))
  }
}




#' Infer the most frequent character in a vector
#'
#' This function takes a character vector and returns the most frequently occurring
#' character. If there are multiple characters with the same maximum frequency,
#' it returns the first one in alphabetical order. If the input vector is empty,
#' it returns `NA_character_`.
#'
#' @param x A character vector.
#'
#' @return The most frequent character in the vector, or `NA_character_` if the
#' vector is empty.
#'
#' @keywords internal
util_most_frequent <- function(x) {
  if (length(x) == 0) {
    return(NA_character_)
  }
  counts <- table(x)
  most_frequent <- names(counts[counts == max(counts)])

  return(max(most_frequent))
}



