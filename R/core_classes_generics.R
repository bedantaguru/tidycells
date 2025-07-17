

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
    dplyr::select(-.data$row) %>%
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
#' @param allow_rc_df Logical. If \code{TRUE}, allows the input to be any rc_df.
#'   This is designed for mainly testing and development purposes. Default is
#'   \code{FALSE}.
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
    allow_rc_df = FALSE) {

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
    g <- ggplot2::ggplot(x, ggplot2::aes(.data$col, -.data$row, fill = get(.data$fill), label = .data$value)) +
      ggplot2::labs(fill = tools::toTitleCase(stringr::str_replace_all(.data$fill,"_"," ")))

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

  if (!no_txt) {
    g <- g + ggplot2::geom_text(size = txt_size, alpha = txt_alpha, na.rm = TRUE)
  }

  if (!no_plot) {
    plot(g, ...)
  }

  return(invisible(g))
}
