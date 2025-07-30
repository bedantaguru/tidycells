

#' Print a Cells Analysis Object
#'
#' @param x A Output of cells/sheet analysis.
#' @param ... Further arguments passed to methods (currently unused).
#' @keywords internal
#' @export
print.cells_analysis <- function(x, ...) {
  # Print a summary of the Cells Analysis object
  msg <- c(
    paste0("A ", cli_bold_blue("Cells Analysis"), " Object - with"),
    paste0(cli_green(cli_bullet()),"No. of data/value-blocks(s): ",
           cli_blue(dplyr::n_distinct(x$attr_data_map$data_gid)))
  )

  cat(paste0(msg, collapse = "\n"))

  return(invisible(x))
}


#' Print a Cells Composition Object
#'
#' Prints a summary of the Cells Composition object, including the number of
#' node/data/value-blocks.
#'
#' @param x A Cells Composition object.
#' @param ... Further arguments passed to methods (currently unused).
#'
#' @return Invisibly returns the Cells Composition object.
#' @keywords internal
#' @export
print.cells_composition <- function(x, ...) {
  # Print a summary of the Cells Composition object
  msg <- c(
    paste0("A ", cli_bold_blue("Cells Composition"), " Object - with"),
    paste0(cli_green(cli_bullet()),"No. of node/data/value-blocks(s): ",
           cli_blue(length(x)))
  )

  cat(paste0(msg, collapse = "\n"))

  return(invisible(x))
}

#' Print a Sheets Object
#'
#' Prints a summary and details of a Sheets object, including the number of
#' sheets and their sizes. Optionally validates the object before printing.
#'
#' @param x A Sheets object.
#' @param ignore_validation Logical. If TRUE, skips validation of the object.
#'   Default is FALSE.
#' @param ... Further arguments passed to methods (currently unused).
#' @keywords internal
#' @export
print.sheets <- function(x, ignore_validation = FALSE, ...) {
  # If ignore_validation is TRUE, skip validation
  if (ignore_validation) {
    chk <- TRUE
  }else{
    # Validate the sheets object
    chk <- core_validate_sheets(x)
  }

  if(isFALSE(chk)){
    msg <- cli_bold_red(paste0("Malformed ",cli_blue("Sheets")," Object!"))
    msg <- c(
      msg,
      paste0(cli_red(cli_bullet()),"Errors:"),
      paste0(cli_red(cli_cross())," ", (attr(chk, "msg")))
    )
    cat(paste0(paste0(msg, collapse = "\n"), "\n"))
    return(invisible(x))
  }

  # Print the summary message
  msg <- paste0(
    "A ",cli_bold_blue("Sheets")," Object - with ",
    cli_blue(length(x)), " sheet(s). "
  )

  cat(paste0(msg, "\n"))

  purrr::iwalk(
    x,
    function(sheet, name) {
      # Print each sheet
      msg <- paste0(
        cli_bullet(),
        cli_bold_blue("Sheet: "), cli_blue(name), " - ",
        cli_blue(NROW(sheet)), " cell(s). ",
        ifelse(
          NCOL(sheet)*NROW(sheet) > 0,
          yes = paste0(
            "Size ",cli_blue(max(sheet$row)), " x ", cli_blue(max(sheet$col))
          ),
          no = "<Empty Cells Object>"
        )
      )

      cat(paste0(msg, "\n"))

    }
  )

  return(invisible(x))
}

#' Print Summary for Cells Object
#'
#' Prints a summary of a Cells object, including the number of cells and their
#' dimensions. Optionally glances at the data in a styled view and validates the
#' object before printing.
#'
#' @param x A Cells object.
#' @param ignore_validation Logical. If TRUE, skips validation of the object.
#'   Default is FALSE.
#' @param xl_view Logical. If TRUE (default), displays a Excel - styled glimpse
#'   of the cell data (if required packages are available).
#' @param ... Further arguments passed to methods (currently unused).
#' @keywords internal
#' @export
print.cells <- function(x, ignore_validation = FALSE, xl_view = TRUE, ...) {
  # If ignore_validation is TRUE, skip validation
  if (ignore_validation) {
    chk <- TRUE
  }else{
    chk <- core_cells_validation_end_use(x)
  }


  # If validation fails, then it's Malformed Cells object
  if(isFALSE(chk)){
    return(invisible(x))
  }

  # If validation passes, then proceed further
  msg <- c(
    paste0(
      "A ",cli_bold_blue("Cells")," Object - with ",
      cli_blue(NROW(x)), " cell(s), ",
      ifelse(
        NCOL(x)*NROW(x) > 0,
        yes = paste0(
          "Size ",cli_blue(max(x$row)), " x ", cli_blue(max(x$col))
        ),
        no = "<Empty Cells Object>"
      )
    )
  )

  if (utils::hasName(x, "type")) {
    msg <- c(
      msg,
      paste0(
        cli_bullet(),
        "With ",
        cli_blue("Value/Attribute Classification")
      )
    )
  }

  # print msgs
  cat(paste0(paste0(msg, collapse = "\n"), "\n"))

  if(xl_view && cli_is_available()) {
    cat(
      paste0(
        cli_bullet(), cli_blue("Glimpse of The Data Content:\n"),
        cli_bullet(), "(Note: Blank rows/columns are not considered):\n"
      )
    )

    # Print a glimpse of the data content
    cli_print_excel_styled_matrix(
      mat = as.matrix.cells(x),
      col_width = 8,
      max_cols = 10,
      max_rows = 10
    )
  }

  return(invisible(x))

}

#' Convert Cells Object to Matrix
#'
#' Converts a valid Cells object to a matrix. Ensures structure is appropriate
#' for conversion and arranges data accordingly.
#'
#' @param x A Cells object.
#' @return A matrix representing the cell values.
#' @keywords internal
#' @export
as.matrix.cells<- function(x, ...) {

  convertible <- FALSE

  if(utils::hasName(x, "row") && utils::hasName(x, "col") && utils::hasName(x, "value")) {
    if(is.integer(x$row) && is.integer(x$col) && is.character(x$value)) {
      if(all(x$row > 0) && all(x$col > 0)) {
        # Check if the values are convertible to a matrix
        convertible <- TRUE
      }
    }

  }

  if(!convertible) {
    rlang::abort(
      "The input object is not a valid 'cells' object for conversion to matrix."
    )
  }

  # restore the class to 'data.frame'
  class(x) <- "data.frame"

  # note this method assumes data is free from sparse nature
  # i.e. empty rows and columns will be removed
  m <- x[c("row", "col", "value")] %>%
    dplyr::arrange(.data$row,.data$col) %>%
    tidyr::spread(.data$col, .data$value) %>%
    dplyr::select(-"row") %>%
    as.matrix()

  colnames(m) <- NULL
  row.names(m) <- NULL
  m

}




#' Plot a Cells Object as a Grid
#'
#' Visualizes a \code{Cells} object as a grid using \code{ggplot2}, where each
#' cell is represented by a tile. The fill color and text labels can be
#' customized.
#'
#' @param x A \code{Cells} object to be plotted.
#' @param ... Additional arguments passed. (currently unused)
#' @param fill Character. Name of the column in \code{x} to use for tile fill
#'   colors. If missing, uses "type" if present, otherwise "data_type".
#' @param ignore_validation Logical. If \code{TRUE}, skips validation of the
#'   object before plotting. Default is \code{FALSE}.
#' @param no_fill Logical. If \code{TRUE}, disables fill coloring for tiles.
#'   Default is \code{FALSE}.
#' @param txt_size Numeric. Text size for cell labels.
#' @param txt_alpha Numeric. Alpha transparency for cell labels.
#' @param no_txt Logical. If \code{TRUE}, disables text labels on the grid.
#'   Default is \code{FALSE}.
#' @param max_txt_len Integer. Maximum length of text displayed in each cell
#'   before truncation. Default is \code{10}.
#' @param no_plot Logical. If \code{TRUE}, returns the ggplot object without
#'   displaying / plotting it. Default is \code{FALSE}.
#' @param boundaries Data frame containing boundary rectangles to be drawn on
#'   the grid. Should have columns `c_min`, `c_max`, `r_min`, `r_max` for
#'   coordinates, and optionally a grouping variable.
#' @param add_cell_address Logical. If \code{TRUE}, adds cell addresses as text
#'   labels on the grid. Default is \code{FALSE}.
#' @param allow_rc_df Logical. If \code{TRUE}, allows the input to be any rc_df.
#'   This is designed for mainly testing and development purposes. Default is
#'   \code{FALSE}.
#'
#'
#' @details
#' - Requires the \code{ggplot2} package.
#' - Validates the \code{Cells} object unless \code{ignore_validation} is \code{TRUE}.
#' - Warns and does not do anything if the object has more than 200 rows or columns.
#' - If the specified \code{fill} column is not found in \code{x}, the function aborts with an error.
#' - Text in cells longer than \code{max_txt_len} is truncated and appended with "...".
#' - The function returns (invisibly) the \code{ggplot} object.
#'
#' @return Invisibly returns the \code{ggplot} object for further customization
#'   or display.
#'
#' @keywords internal
#' @export
plot.cells <- function(
    x, ...,
    fill,
    ignore_validation = FALSE,
    no_fill = FALSE,
    txt_size = 3, txt_alpha = 1, no_txt = FALSE, max_txt_len = 10,
    no_plot = FALSE,
    boundaries = NULL,
    add_cell_address = FALSE,
    # This is mainly for testing and development purposes
    allow_rc_df = FALSE) {

  # Check if ggplot2 is available
  if(!pkg_is_available("ggplot2")) {
    rlang::warn(
      "The 'ggplot2' package is required for plotting cells objects."
    )
    return(invisible(NULL))
  }

  # This is mainly to assist in testing and development
  if(allow_rc_df){
    ignore_validation <- TRUE
    if(!utils::hasName(x, "value")) {
      x$value <- ""
    }
    if(missing(fill)) {
      fill <- "value"
    }
  }

  # If ignore_validation is TRUE, skip validation
  if (ignore_validation) {
    chk <- TRUE
  }else{
    chk <- core_cells_validation_end_use(x)
  }

  # If validation fails, then it's Malformed Cells object
  if(isFALSE(chk)){
    return(invisible(NULL))
  }

  if(max(x$row)-min(x$row)>200 || max(x$col)-min(x$col)>200){
    rlang::warn(
      "Plot is not suitable for cells with more than 200 rows or columns."
    )
    return(invisible(NULL))
  }


  # If validation passes, then proceed further

  full_grid <- expand.grid(row = seq(min(x$row), max(x$row)),
                           col = seq(min(x$col), max(x$col)),
                           stringsAsFactors = FALSE)

  if (missing(fill)) {
    if (utils::hasName(x, "type")) {
      fill <- "type"
    } else {
      fill <- "data_type"
    }
  }

  # Fill can be either a character string or a symbol
  fill <-  rlang::as_name(rlang::enexpr(fill))

  if (!(fill %in% colnames(x))) {
    rlang::abort("`fill` should be either of column names.")
  }

  # truncate text values if they exceed max_txt_len
  x$value <- ifelse(
    nchar(x$value) > max_txt_len,
    paste0(substr(x$value, 1, max_txt_len), "..."),
    x$value
  )

  if (no_fill) {
    g <- ggplot2::ggplot(x, ggplot2::aes(.data$col, -.data$row, label = .data$value))+
      ggplot2::geom_tile(color = "#00000046", alpha = 0.1, na.rm = TRUE, width = 1, height = 1)
  } else {
    g <- ggplot2::ggplot(x, ggplot2::aes(.data$col, -.data$row, fill = get(fill), label = .data$value)) +
      ggplot2::labs(fill = tools::toTitleCase(stringr::str_replace_all(fill,"_"," ")))

    g <- g +
      ggplot2::geom_tile(color = "#00000046", na.rm = TRUE, width = 1, height = 1)
  }

  g <- g +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )

  if(is.numeric(x[[fill]])){
    # If fill is numeric, use a gradient scale
    g <- g +
      ggplot2::scale_fill_gradient(low = "#00BFC4", high = "#F8766D")
  }

  # Add the full grid to ensure all cells are represented

  g <- g +
    ggplot2::geom_tile(
      data = full_grid,
      ggplot2::aes(x = .data$col, y = -.data$row),
      fill = NA, color = "#00000015", alpha = 0.1, width = 1, height = 1, inherit.aes = FALSE
    )

  if(add_cell_address) {
    # Add cell addresses as text labels
    g <- g +
      ggplot2::geom_text(
        data = full_grid,
        ggplot2::aes(x = .data$col-0.2, y = -.data$row, label = paste0(.data$row, ", ", .data$col)),
        size = 2, alpha = 0.3, color = "#00000099", na.rm = TRUE,
        inherit.aes = FALSE
      )
  }

  # If boundaries are provided, add them to the plot
  if(!is.null(boundaries) && is.data.frame(boundaries)) {

    # Check if boundaries have the a grouping var
    group_var <- setdiff(
      colnames(boundaries),
      c("c_min", "c_max", "r_min", "r_max"))

    # if no grouping var, then use create a dummy one
    if(length(group_var) == 0) {
      boundaries$gid <- ""
      group_var <- "gid"
    }

    # In case multiple grouping vars are present, then use the first one
    group_var <- group_var[1]

    g <- g +
      ggplot2::geom_rect(
        data = boundaries,
        ggplot2::aes(
          xmin = .data$c_min-0.5, xmax = .data$c_max+0.5, ymin = -.data$r_min+0.5, ymax = -.data$r_max-0.5,
          group = .data[[group_var]]
        ),
        color = "red", inherit.aes = FALSE, alpha = 0.1, size = 1.5, lty = 2, lwd = 0.7, fill = "#8a5f5c",
        na.rm = TRUE
      )

    # Add text labels for boundaries
    if(!no_txt){
      g <- g +
        ggplot2::geom_text(
          data = boundaries,
          ggplot2::aes(
            x = (.data$c_min + .data$c_max) / 2, y = -(.data$r_min + .data$r_max) / 2,
            label = .data[[group_var]]
          ),
          inherit.aes = FALSE, color = "#4f1e1a", size = 5, na.rm = TRUE
        )
    }
  }

  if (!no_txt) {
    g <- g + ggplot2::geom_text(size = txt_size, alpha = txt_alpha, na.rm = TRUE)
  }



  if (!no_plot) {
    plot(g, ...)
  }

  return(invisible(g))
}





#' Visualize Cell Data with Combined Attribute Information
#'
#' This function creates a visualization of cell data, combining attribute
#' columns and displaying them alongside data blocks.
#'
#' @param x A `cells_analysis`-object
#' @param ... Additional arguments passed to plot.cells
#' @param attr_cols Column names from attr_data_map to combine with ":"
#'   separator
#' @param max_txt_len Maximum length of text displayed in each cell before
#'   truncation. Default is 18.
#' @param txt_size Numeric. Text size for cell labels. Default is 2.5.
#'
#' @return Invisibly returns the \code{ggplot} object for further customization
#'   or display.
#'
#' @keywords internal
#' @export
plot.cells_analysis <- function(
    x, ...,
    attr_cols = c("data_gid", "nice_header_name", "header_orientation_tag"),
    max_txt_len = 18, txt_size = 2.5) {

  # Step 1: Process the attribute data map
  # First extract the attribute data
  attr_data <- x$attr_data_map

  # Select specified columns and combine them with ":" separator
  # (avoiding the dot notation)
  selected_columns <- dplyr::select(attr_data, dplyr::all_of(attr_cols))
  combined_values <- apply(selected_columns, 1, paste, collapse = ":")

  # Create attribute data with combined values
  attr_data_with_combined_values <- attr_data %>%
    dplyr::mutate(val = combined_values) %>%
    # Extract only the necessary columns for the plot
    dplyr::distinct(.data$row, .data$col, value = .data$val, type = "attr")

  # Step 2: Prepare the data blocks
  # Extract the relevant columns from data_blocks
  data_blocks_simplified <- x$data_blocks %>%
    dplyr::distinct(.data$row, .data$col, value = .data$data_gid, type = "data")

  # Step 3: Combine both datasets
  combined_data <- dplyr::bind_rows(
    attr_data_with_combined_values,
    data_blocks_simplified
  )

  # Step 4: Generate the cell plot
  # Display the combined data with the plot.cells function
  combined_data %>%
    plot.cells(allow_rc_df = TRUE, fill = "type",
               max_txt_len = max_txt_len, txt_size = txt_size, ...)
}
