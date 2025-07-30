

#' Collate Columns Across a List of Data Frames or Cells Composition Object
#'
#' Collates ("stacks") columns from a list of data frames or cells composition
#' object by merging/stacking columns with similar content, regardless of column
#' names. The function is "content-aware": it analyzes column values and
#' intelligently merges columns that are highly similar, minimizing redundant
#' columns and maximizing within-column similarity.
#'
#' This function generalizes the behavior of `dplyr::bind_rows` (or `rbind`),
#' but rather than simply aligning columns by name or order, it matches columns
#' based on their data content. If `similarity_threshold = 1`, the function
#' behaves like `bind_rows`, keeping columns separate unless their contents are
#' identical. If `similarity_threshold = 0`, the function aggressively merges
#' columns with any overlap, reducing the number of columns as much as possible.
#' The function does not use or preserve non-fixed column names—output columns
#' are renamed (e.g., `collated_1`, `collated_2`, etc.) and only the values
#' matter for matching.
#'
#' @param x A list of data frames or a cells composition object (which is also a
#'   list of data frames).
#' @param ... Reserved for future use or passed to methods.
#' @param fixed_columns Character vector of column names that must be preserved
#'   and not merged. If `NULL`, these are inferred as the intersection of column
#'   names across all data frames.
#' @param similarity_threshold Numeric between 0 and 1. Columns with similarity
#'   above this threshold are considered for merging. Default is `0` (maximum
#'   merging/stacking).
#'
#' @return A data frame with fixed columns and a set of collated columns, where
#'   similar columns from the input list have been merged based on content
#'   similarity.
#'
#' @details
#' - Fixed columns are always preserved and appear as-is in the result.
#' - Non-fixed columns are merged based on a similarity measure computed from their values, not their names.
#' - Output column names (other than fixed columns) are automatically generated.
#' - Designed for use with data extracted from complex or heterogeneous tables, especially when column names are not trustworthy or consistent.
#'
#' @export
collate_columns <- function(x,
                            ...,
                            fixed_columns = NULL,
                            similarity_threshold = 0) {
  UseMethod("collate_columns")
}


#' @noRd
#' @export
collate_columns.list <- function(x,
                                 ...,
                                 fixed_columns = NULL,
                                 similarity_threshold = 0) {
  collate_columns_lst(
    list_of_data = x,
    fixed_columns = fixed_columns,
    similarity_threshold = similarity_threshold
  )
}

#' Collate columns of a cells composition object
#'
#' This function collates columns of a cells composition object, which is a list
#' of data frames with columns representing different attributes of cells. It
#' merges the columns based on their similarity, allowing for flexible handling
#' of data.
#'
#' @param x A cells composition object, which is a list of data frames.
#' @param ... Additional arguments passed to the function.
#' @param fixed_columns A character vector of column names that should not be
#'   altered during the collation process. Default is `c("row", "col",
#'   "data_gid", "value", "sheet_name")`.
#' @param similarity_threshold A numeric value indicating the minimum similarity
#'   score required for two columns to be considered similar. Default is `0`. (0
#'   means it will try to reduce columns as much as possible)
#' @param dicard_cell_address A logical value indicating whether to discard cell
#'   address columns (like `row`, `col`, `data_gid`) from the output. Default is
#'   `TRUE`.
#'
#' @return A data frame with collated columns, where similar columns are merged
#'   based on their similarity scores.
#'
#' @keywords internal
#' @export
collate_columns.cells_composition <- function(
    x,
    ...,
    fixed_columns = c("row", "col", "data_gid", "value","sheet_name"),
    similarity_threshold = 0,
    dicard_cell_address = TRUE) {
  # If the input is a cells_composition object, use specialized arguments
  if (dicard_cell_address) {
    # Remove these columns from all nodes and all columns with ^cellAddress_
    rem_columns <- c("row", "col", "data_gid")
    x <- x %>%
      purrr::map(function(df) {
        df %>%
          dplyr::select(-dplyr::any_of(rem_columns),
                        # May induce cellAddress_ columns from composition
                        -dplyr::starts_with("cellAddress_"))
      })

    # Also remove these from fixed_columns
    fixed_columns <- setdiff(fixed_columns, rem_columns)
  }

  dout <- collate_columns_lst(
    list_of_data = x,
    fixed_columns = fixed_columns,
    similarity_threshold = similarity_threshold
  )

  # Minor column ordering:
  # Keep the value at the end and sort rest columns
  rest_col <- setdiff(colnames(dout), "value")
  dout[c(rest_col[stringr::str_order(rest_col, numeric = TRUE)], "value")]
}


#' Collate columns of a cells_analysis object
#'
#' This function collates columns of a cells_analysis object, which is a
#' specialized data structure for analyzing cell compositions. It merges similar
#' columns based on a similarity threshold, allowing for flexible handling of
#' data.
#'
#' @param x A cells_analysis object, which is a specialized data structure for
#'   analyzing cell compositions.
#' @param ... Additional arguments passed to the function.
#' @param fixed_columns A character vector of column names that should not be
#'   altered during the collation process. Default is `c("row", "col",
#'   "data_gid", "value")`.
#' @param similarity_threshold A numeric value indicating the minimum similarity
#'   score required for two columns to be considered similar. Default is `0`. (0
#'   means it will try to reduce columns as much as possible)
#' @param dicard_cell_address A logical value indicating whether to discard cell
#'   address columns (like `row`, `col`, `data_gid`) from the output. Default is
#'   `TRUE`.
#'
#' @return A data frame with collated columns, where similar columns are merged
#'   based on their similarity scores.
#' @keywords internal
#' @export
collate_columns.cells_analysis <- function(x,
                                           ...,
                                           fixed_columns = c("row", "col", "data_gid", "value"),
                                           similarity_threshold = 0,
                                           dicard_cell_address = TRUE) {
  # If the input is a cells_analysis object, use specialized arguments

  # First, compose the cells to get a list of data frames
  xc <- compose(
    x,
    trace_composition = FALSE,
    simplify = FALSE)

  # Now, call the collate_columns function on the list of data frames
  collate_columns.cells_composition(
    x = xc,
    ...,
    fixed_columns = fixed_columns,
    similarity_threshold = similarity_threshold,
    dicard_cell_address = dicard_cell_address
  )
}

collate_columns_lst <- function(list_of_data,
                                fixed_columns = NULL,
                                similarity_threshold = 0) {

  # Check if list_of_data is a list
  if (!is.list(list_of_data)) {
    rlang::abort("`list_of_data` must be a list of data frames.")
  }

  # Check if all elements in the list are data frames
  if (!all(purrr::map_lgl(list_of_data, is.data.frame))) {
    rlang::abort("All elements in `list_of_data` must be data frames.")
  }

  # If only one data frame, return it
  if (length(list_of_data) == 1) {
    return(list_of_data[[1]])
  }

  # Reorder list_of_data based on NCOLs and keep the maximum number of columns
  # in first node etc.
  list_of_data_nc <- list_of_data %>%
    purrr::map_int(NCOL)

  list_of_data <- list_of_data[order(list_of_data_nc, decreasing = TRUE)]

  # Reduce the list of data frames by collating columns
  purrr::reduce(
    list_of_data,
    collate_col_reduce_two_df,
    fixed_columns = fixed_columns,
    similarity_threshold = similarity_threshold
  ) %>% dplyr::distinct()

}


collate_col_reduce_two_df <- function(df1, df2,
                                      fixed_columns = NULL,
                                      similarity_threshold = 0) {

  # Step 0: Pre-processing

  # Rename preexisting ^collated_ and ^uncollated_ columns to avoid conflicts
  colnames(df1) <- colnames(df1) %>%
    stringr::str_replace("^collated_", "old_col_") %>%
    stringr::str_replace("^uncollated_", "old_uncol_")

  colnames(df2) <- colnames(df2) %>%
    stringr::str_replace("^collated_", "old_col_") %>%
    stringr::str_replace("^uncollated_", "old_uncol_")

  # Ensure that df1 and df2 have unique column names
  colnames(df1)[!(colnames(df1) %in% fixed_columns)] <-
    util_make_unique_minimal(colnames(df1)[!(colnames(df1) %in% fixed_columns)])
  colnames(df2)[!(colnames(df2) %in% fixed_columns)] <-
    util_make_unique_minimal(colnames(df2)[!(colnames(df2) %in% fixed_columns)])

  # Step 1: Get column representatives
  cr1 <- collate_col_get_col_representative(df1, except_cols = fixed_columns)
  cr2 <- collate_col_get_col_representative(df2, except_cols = fixed_columns)

  # Early return if both any empty
  # if either of cr1 or cr2 is empty simply rbind and return
  if (length(cr1) * length(cr2) == 0) {
    return(dplyr::bind_rows(df1, df2))
  }

  # Step 2: Compute the similarity of column representatives
  all_pairs <- expand.grid(
    n1 = names(cr1),
    n2 = names(cr2), stringsAsFactors = FALSE)

  all_pairs$similarity_score <- seq(NROW(all_pairs)) %>%
    purrr::map_dbl(
      function(idx){
        collate_col_similarity_score(
          cr1[[all_pairs$n1[idx]]],
          cr2[[all_pairs$n2[idx]]])
      }
    )

  # Step 3 : Now find out the best matches
  all_pairs_iter <- all_pairs

  col_map <- tibble::tibble()

  while (nrow(all_pairs_iter) > 0) {
    # Find rows with the maximum similarity_score
    col_map_this <- all_pairs_iter %>%
      dplyr::slice_max(order_by = .data$similarity_score, with_ties = TRUE)

    # If nothing found, exit
    if (nrow(col_map_this) == 0) break

    # Collect the matches
    col_map <- dplyr::bind_rows(col_map, col_map_this)

    # Remove used pairs from pool
    all_pairs_iter <- all_pairs_iter %>%
      dplyr::filter(!(.data$n1 %in% col_map_this$n1),
                    !(.data$n2 %in% col_map_this$n2))
  }

  # Step 4: Now we have the column map, we can stack the data frames

  collate_possible_pairs <- col_map %>%
    dplyr::filter(.data$similarity_score >= similarity_threshold) %>%
    dplyr::mutate(
      new_col_name = paste0(
        "collated_", util_hierarchical_rank(.data$n1, .data$n2)),
    )

  # Step 5: change col-names suitably

  # Since names are unique, we can use them directly as key-value pair
  cname_1 <- colnames(df1)
  names(cname_1) <- cname_1
  cname_2 <- colnames(df2)
  names(cname_2) <- cname_2

  # Base Name
  all_names <- c(cname_1, cname_2) %>%
    setdiff(fixed_columns) %>%
    unique()


  # Rename the columns in df1 and df2 which are collate-able
  if (NROW(collate_possible_pairs) > 0) {
    cname_1[collate_possible_pairs$n1] <-
      collate_possible_pairs$new_col_name
    cname_2[collate_possible_pairs$n2] <-
      collate_possible_pairs$new_col_name

    all_names <- c(
      setdiff(cname_1, collate_possible_pairs$new_col_name),
      setdiff(cname_2, collate_possible_pairs$new_col_name)
    ) %>%
      setdiff(fixed_columns) %>%
      unique()
  }

  # Find the columns which are not collate-able
  d_base_name <- tibble::tibble(
    old_col_name = all_names) %>%
    # Name them as un-collated_1,2,3 etc.
    dplyr::mutate(
      new_col_name = paste0(
        "uncollated_",
        util_hierarchical_rank(.data$old_col_name))
    ) %>%
    dplyr::mutate(
      is_one = .data$old_col_name %in% cname_1,
      is_two = .data$old_col_name %in% cname_2
    )

  # Rename the columns in df1 and df2 which are not collate-able (if any)
  bn1 <- d_base_name %>%
    dplyr::filter(.data$is_one)
  bn1 <- stats::setNames(bn1$new_col_name, bn1$old_col_name)
  cname_1[names(bn1)] <- bn1

  bn2 <- d_base_name %>%
    dplyr::filter(.data$is_two)
  bn2 <- stats::setNames(bn2$new_col_name, bn2$old_col_name)
  cname_2[names(bn2)] <- bn2


  # Finally, set the column names
  colnames(df1) <- cname_1
  colnames(df2) <- cname_2

  # Step 6: Bind the data frames
  dplyr::bind_rows(df1, df2)

}

collate_col_get_col_representative <- function(df, except_cols = NULL) {

  cols <- as.list(df[setdiff(colnames(df),  except_cols)])

  cols %>%
    purrr::map(function(col) {
      # Get the non-NA values in the column
      non_na <- col[!is.na(col)]
      non_na <- unique(tolower(non_na))
      non_na <- non_na[nzchar(non_na)]
      if(length(non_na) > 500) {
        # If more than 500 unique non-NA values, return a sample of 10
        sample(sort(non_na), 10)
      }else{
        non_na
      }
    })
}


collate_col_similarity_score <- function(x, y) {

  # Calculate similarity score between two sets of values. Multiple scores are
  # used.

  # Convert to character to ensure consistent comparison
  x <- as.character(x)
  y <- as.character(y)

  score <- list()

  score$base <- length(intersect(x, y)) / length(union(x, y))

  # word based score
  words_x <- stringr::str_split(x, "\\s+") %>% unlist() %>% unique()
  words_y <- stringr::str_split(y, "\\s+") %>% unlist() %>% unique()
  common_words <- intersect(words_x, words_y)
  union_words <- union(words_x, words_y)
  # Word based measure 1
  score$word_based1 <- length(common_words) / length(union_words)
  # Word based measure 2 (nchar weighted score)
  score$word_based2 <- sum(nchar(common_words)) / sum(nchar(union_words))

  # character based score
  chars_x <- unlist(stringr::str_split(x, ""))
  chars_x <- unique(chars_x[nzchar(chars_x)])
  chars_y <- unlist(stringr::str_split(y, ""))
  chars_y <- unique(chars_y[nzchar(chars_y)])
  score$char_based <- length(intersect(chars_x, chars_y)) /
    length(union(chars_x, chars_y))

  # Levenshtein distance based
  similarity_matrix <- 1 - utils::adist(x, y) / outer(nchar(x), nchar(y), pmax)
  score$levenshtein <- mean(similarity_matrix, na.rm = TRUE)


  # If any of score is NA then set it to 0
  score <- purrr::map(score, ~ ifelse(is.na(.x), 0, .x))

  # Finally combine the scores using weighted average
  score$combined <-
    (score$base * 0.4 + score$word_based1 * 0.3 +
       score$word_based2 * 0.2 + score$char_based * 0.05 +
       score$levenshtein * 0.05)

  score$combined

}
