
##
## Infer Helper Functions
##

# Section: Table Blocks ----


#' Connected-Component Grouping for 4-Connected Grid Cells
#'
#' Identifies all 4-connected groups (components) in a data frame of grid cells.
#' Two cells are considered part of the same group if they are connected via a
#' path of horizontal or vertical (not diagonal) adjacency. Each group receives
#' a unique group id (`gid`).
#'
#' The algorithm applies an iterative label-propagation approach, a variant of
#' the classic Connected-Component Labeling (CCL) algorithm from image
#' processing and graph theory. Labels are propagated among adjacent cells via
#' repeated group-wise minimum assignments until all connected cells share the
#' same id.
#'
#' ## Underlying Theory This approach is a form of Connected-Component Labeling
#' (CCL), widely used in image analysis to find and label contiguous regions
#' (components) in an array or grid. The algorithm used here is an iterative
#' label-propagation method, which merges labels for horizontally and vertically
#' adjacent runs until each component is uniquely identified. See:
#' - https://en.wikipedia.org/wiki/Connected-component_labeling
#' - Gonzalez, R. C., & Woods, R. E. (2008). Digital Image Processing (3rd Ed.), Section 2.5.3.
#'
#' ## Steps Overview
#'
#' | Step      | What it does                                                      | Why                                                           |
#' |-----------|-------------------------------------------------------------------|---------------------------------------------------------------|
#' | Row runs  | Finds horizontally connected contiguous cells; assigns `cid`      | To group cells horizontally                                   |
#' | Col runs  | Finds vertically connected contiguous cells; assigns `rid`        | To group cells vertically                                     |
#' | Propagate | Iteratively spreads minimum label among connected runs            | To ensure all 4-connected cells are merged into one component |
#' | Stop      | Ends when no further label changes                                | At this point, grouping is complete                           |
#'
#' @param dat A `data.frame` or `tibble` with integer columns `'row'` and
#'   `'col'`.
#' @param retain_cells_info If `TRUE`, retains the original `cells` class and
#'   structure, allowing for further processing. If `FALSE`, returns a simple
#'   `data.frame` with 'row', 'col', and 'gid' columns only.
#' @param group_id_tag A string prefix for the group id (`gid`) column. Default
#'   is `"Gr_"` if `retain_cells_info` is `TRUE`. This is useful for
#'   distinguishing group ids from other ids in the data frame.
#'
#' @return The input data frame with an added character column 'gid' indicating
#'   component membership.
#' @keywords internal
infer_table_blocks <- function(
    dat, retain_cells_info = FALSE, group_id_tag = NULL) {


  if(!retain_cells_info) {
    # Remove `cells`-class
    dat <- tibble::as_tibble(unclass(dat))

    dat <- dat[, c("row", "col")]
  }

  # 1. Find the number of digits to safely combine row/col into unique keys
  max_val <- max(dat$row, dat$col)
  digit_sep <- nchar(as.character(max_val)) + 1

  # 2. For each row: assign a 'cid' to each contiguous run of columns
  dat <- dat %>%
    dplyr::arrange(.data$row, .data$col) %>%
    dplyr::group_by(.data$row) %>%
    dplyr::mutate(col_run = c(0, cumsum(diff(.data$col) != 1))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(cid = .data$col_run + .data$row * 10^digit_sep)

  # 3. For each column: assign a 'rid' to each contiguous run of rows
  dat <- dat %>%
    dplyr::arrange(.data$col, .data$row) %>%
    dplyr::group_by(.data$col) %>%
    dplyr::mutate(row_run = c(0, cumsum(diff(.data$row) != 1))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(rid = .data$row_run + .data$col * 10^digit_sep)

  # 4. Initialize group id as rid.
  dat$gid <- dat$rid

  # 5. Iteratively propagate the minimum gid over all connected cells
  repeat {
    old_gid <- dat$gid
    # Propagate min gid within each 'cid' (row-wise contiguous segments)
    dat <- dat %>%
      dplyr::group_by(.data$cid) %>%
      dplyr::mutate(gid = min(.data$gid)) %>%
      dplyr::ungroup()
    # Propagate min gid within each 'rid' (col-wise contiguous segments)
    dat <- dat %>%
      dplyr::group_by(.data$rid) %>%
      dplyr::mutate(gid = min(.data$gid)) %>%
      dplyr::ungroup()
    # Stop if all gids have stabilized (no more merges)
    if (all(dat$gid == old_gid)) break
  }

  if(retain_cells_info && is.null(group_id_tag)) {
    # Default group id tag if not provided in case of `retain_cells_info = TRUE`
    group_id_tag <- "Gr_"
  }

  if(is.null(group_id_tag)){
    dat$gid <- as.character(dat$gid)
  }else{
    # Add group id tag to gid
    dat$gid <- paste0(
      group_id_tag,
      # Convert gid to factor and then numeric for sequence
      as.numeric(as.factor(as.character(dat$gid)))
    )
  }

  # Return
  if(!retain_cells_info){
    dplyr::distinct(dat[, c("row", "col", "gid")])
  }else{
    dplyr::select(
      dat,
      -c(.data$cid, .data$rid, .data$col_run, .data$row_run)) %>%
      # Ensure the class is 'cells'
      core_ensure_cells_format()
  }
}




#' Weighted Percentile Boundary of Grid Cells (blocks)
#'
#' Calculates the bounding box covering a specified proportion of filled cells,
#' using weighted percentiles of cell counts.
#'
#' @param df Data frame with 'row' and 'col' columns. (Optionally grouped)
#' @param coverage Proportion to cover, default is 0.95 (95% block).
#' @param exact If `TRUE`, uses exact min/max instead of percentiles. Default
#'   is `FALSE`.
#'
#' @return tibble: r_min, r_max, c_min, c_max. (group vars if `df` is grouped)
#' @keywords internal
infer_block_boundary <- function(df, coverage = 0.95, exact = FALSE) {

  if (exact) {
    dplyr::summarise(
      df,
      r_min = min(.data$row),
      c_min = min(.data$col),
      r_max = max(.data$row),
      c_max = max(.data$col),
      .groups = "drop"
    )
  } else {

    # Approximate boundary calculation for odd shaped data blocks

    # Helper for weighted quantile
    wtd_quantile <- function(x, w, probs) {
      ord <- order(x)
      x <- x[ord]
      w <- w[ord]
      cum_w <- cumsum(w) / sum(w)
      sapply(probs, function(p) {
        idx <- which(cum_w >= p)[1]
        x[idx]
      })
    }

    # Boundary calculation for a chunk of data
    boundary_fn <- function(rows, cols, coverage) {
      row_counts <- as.data.frame(table(rows), stringsAsFactors = FALSE)
      col_counts <- as.data.frame(table(cols), stringsAsFactors = FALSE)
      half_gap <- (1 - coverage)/2
      probs <- c(half_gap, 1 - half_gap)
      rp <- wtd_quantile(as.integer(row_counts[[1]]), row_counts$Freq, probs)
      cp <- wtd_quantile(as.integer(col_counts[[1]]), col_counts$Freq, probs)
      tibble::tibble(r_min = rp[1], r_max = rp[2], c_min = cp[1], c_max = cp[2])
    }

    dplyr::summarise(
      df,
      boundary = list(boundary_fn(row, col, coverage)),
      .groups = "drop"
    ) %>%
      tidyr::unnest_wider(.data$boundary, names_repair = "minimal", simplify = TRUE)
  }
}





# Section: Major Direction Stats Helpers ----

infer_calc_intermediate_attr_gids_dimwise <- function(
    d_ad, d_att, wise = c("col", "row")) {
  wise <- match.arg(wise)
  # Set axis and interval variables dynamically
  axis_var <- if (wise == "col") "col" else "row"
  interval_var <- if (wise == "col") "row" else "col"

  # Step 1: Pre-compute attr_gid ranges per axis
  attr_ranges <- d_att %>%
    dplyr::group_by(.data[[axis_var]], .data$attr_gid) %>%
    dplyr::summarise(
      attr_min = min(.data[[interval_var]]),
      attr_max = max(.data[[interval_var]]),
      .groups = "drop"
    )

  # Step 2: Add row identifiers and compute search bounds
  d_query <- d_ad %>%
    dplyr::mutate(
      row_id = dplyr::row_number(),
      # Taking pairwise minimum as attr can be on either side of data (N or S, E or W)
      search_min = pmin(.data[[paste0(interval_var, "_dat")]], .data[[paste0(interval_var, "_att")]]),
      search_max = pmax(.data[[paste0(interval_var, "_dat")]], .data[[paste0(interval_var, "_att")]])
    )

  # Step 3: Cartesian join on axis, then filter for overlaps
  overlaps <- d_query %>%
    dplyr::inner_join(
      attr_ranges, by = axis_var,
      relationship = "many-to-many", suffix = c("", "_ar")
    ) %>%
    dplyr::filter(
      .data$attr_gid != .data$attr_gid_ar,  # Exclude same attr_gid
      # Below is equivalent of pmax(.data$search_min, .data$attr_min) < pmin(.data$search_max, .data$attr_max)
      .data$attr_max > .data$search_min,     # Interval overlap condition 1
      .data$attr_min < .data$search_max      # Interval overlap condition 2
    ) %>%
    dplyr::group_by(.data$row_id) %>%
    dplyr::summarise(
      # Count distinct attr_gids that has intersection with the search interval
      no_of_attr_gids_between = dplyr::n_distinct(.data$attr_gid_ar),
      .groups = "drop"
    )

  # Step 4: Join back to original
  result <- d_query %>%
    dplyr::left_join(overlaps, by = "row_id") %>%
    dplyr::mutate(
      no_of_attr_gids_between = dplyr::coalesce(.data$no_of_attr_gids_between, 0L)
    )

  # Only keep original columns + new result
  result <- result[c(colnames(d_ad), "no_of_attr_gids_between")]

  return(result)
}

infer_calc_stats_for_admap_dimwise <- function(
    d_dat, d_att,
    wise = c("col", "row"), penalty_coeff = 0.5) {


  # If no data_gid and attr_gid given, return empty tibble as further processing
  # is not possible.
  if(NROW(d_dat)==0 || NROW(d_att)==0) return(tibble::tibble())

  wise <- match.arg(wise)
  # Set grouping, direction, and axis variables
  axis_var <- if (wise == "col") "col" else "row"
  opp_var  <- if (wise == "col") "row" else "col"
  direction_group_tag <- if (wise == "col") "NS" else "WE"
  direction_tag <- if (wise == "col") list(low = "S", high = "N") else list(low = "E", high = "W")

  # 1. Join data and attributes on axis
  d_ad <- d_dat %>%
    dplyr::left_join(d_att, by = axis_var, suffix = c("_dat", "_att"))

  # 2. Add direction, direction_group, and distance
  d_ad <- d_ad %>%
    dplyr::mutate(
      direction = ifelse(
        .data[[paste0(opp_var, "_dat")]] < .data[[paste0(opp_var, "_att")]],
        direction_tag$low, direction_tag$high),
      direction_group = direction_group_tag,
      dist = abs(.data[[paste0(opp_var, "_dat")]] - .data[[paste0(opp_var, "_att")]])
    )

  # 3. Find smallest distance within axis and retain only that touch point
  d_ad_smallest_dist <- d_ad %>%
    dplyr::group_by(
      .data$data_gid, .data$attr_gid, .data$direction, .data$direction_group,
      .data[[axis_var]]
    ) %>%
    dplyr::filter(.data$dist == min(.data$dist)) %>%
    dplyr::ungroup()

  # If no data_gid and attr_gid pairs found, return empty tibble
  if(NROW(d_ad_smallest_dist)==0) return(tibble::tibble())

  # 4. Calculate intermediate gids between data and attribute gids
  d_ad_stats <- infer_calc_intermediate_attr_gids_dimwise(
    d_ad_smallest_dist, d_att, wise = wise
  )


  # 5. Aggregate for each data_gid, attr_gid, direction, and direction_group
  d_ad_agg <- d_ad_stats %>%
    dplyr::group_by(.data$data_gid, .data$attr_gid,
                    .data$direction, .data$direction_group) %>%
    # This is required to calculate a continuous version of dimension length
    # which is based on distance penalized sum of axis_var
    dplyr::mutate(inv_far = 1/(1+penalty_coeff*(.data$dist - min(.data$dist)))) %>%
    dplyr::summarise(
      # Overall distance between a data_gid and attr_gid is the smallest
      # distance among all nearest connected cells
      dist = min(.data$dist),
      # Dimension length is indication of full info in each side
      dim_len = dplyr::n_distinct(.data[[axis_var]]),
      # Somewhat continuous version of dimension length may be required in case
      # `dim_len` alone is inconclusive
      dim_len_cont = sum(.data$inv_far),
      # Number of intermediate gids between data_gid and attr_gid
      attr_gids_between = max(.data$no_of_attr_gids_between),
      .groups = "drop"
    )

  # 6. Remove incomplete cases (NA removal)
  d_ad_agg <- d_ad_agg[stats::complete.cases(d_ad_agg), ]

  return(d_ad_agg)
}

infer_calc_eval_measure_for_admap <- function(
    d_ad_stg_1, penalty_coeff = 0.5){

  if(dplyr::is_grouped_df(d_ad_stg_1)) {
    # If already grouped use the same group
    d_ad_stg_1_g <- d_ad_stg_1
    retain_group <- TRUE
  } else {
    # Group by data_gid and direction_group if not already grouped
    d_ad_stg_1_g <- d_ad_stg_1 %>%
      dplyr::group_by(.data$data_gid, .data$direction_group)
    retain_group <- FALSE
  }

  # Define main measures
  res <- d_ad_stg_1_g %>%
    dplyr::mutate(
      # Calculate normalized distance
      inv_far = 1/(1+penalty_coeff*(.data$dist - min(.data$dist))),
      # Normalize dimension length
      dim_len_norm = 1/(1 + penalty_coeff*
                          (max(.data$dim_len_cont)-.data$dim_len_cont)),
      # Calculate a measure inversely proportional to (number of intermediate
      # gids between data_gid and attr_gid)
      inv_attr_gids_between = 1/(1+penalty_coeff*
                                   (.data$attr_gids_between -
                                      min(.data$attr_gids_between))),
      # Evaluation measure is a combination of all three
      eval_measure = .data$inv_far + .data$dim_len_norm +
        .data$inv_attr_gids_between
    )

  # Return
  if(retain_group) {
    # If already grouped, return the same grouped only calculated DF
    # That means retains group
    res
  } else {
    # Un-group if grouped by this function
    dplyr::ungroup(res)
  }
}




# Section: Data-Block Merging ----

infer_group_connected_blocks <- function(df) {
  # Extract columns as vectors
  col1_vals <- df[[1]]
  col2_vals <- df[[2]]

  # Get all unique nodes
  all_nodes <- unique(c(col1_vals, col2_vals))
  n_nodes <- length(all_nodes)

  # Create node to index mapping for fast lookup
  node_to_idx <- stats::setNames(seq_len(n_nodes), all_nodes)

  # Create adjacency matrix using vectorized operations
  idx1 <- node_to_idx[col1_vals]
  idx2 <- node_to_idx[col2_vals]

  # Initialize adjacency matrix
  adj_matrix <- matrix(FALSE, n_nodes, n_nodes)
  adj_matrix[cbind(idx1, idx2)] <- TRUE
  adj_matrix[cbind(idx2, idx1)] <- TRUE  # Make symmetric

  # Find connected components using matrix powers (transitive closure)
  # This is the key optimization - no explicit loops for traversal
  reach_matrix <- adj_matrix
  prev_reach <- matrix(FALSE, n_nodes, n_nodes)

  # Iterate until no new connections found (usually converges quickly)
  while (!identical(reach_matrix, prev_reach)) {
    prev_reach <- reach_matrix
    reach_matrix <- reach_matrix | (reach_matrix %*% adj_matrix > 0)
  }

  # Add self-connections
  diag(reach_matrix) <- TRUE

  # Extract unique connected components
  visited <- logical(n_nodes)
  groups <- list()

  # Single loop to extract groups
  for (i in seq_len(n_nodes)) {
    if (!visited[i]) {
      component_indices <- which(reach_matrix[i, ])
      groups <- append(groups, list(all_nodes[component_indices]))
      visited[component_indices] <- TRUE
    }
  }

  groups
}

# This will be slower but more explicit version of the above function. Kept for
# deletion later. Maybe it will be used for testing purposes.
infer_group_connected_blocks_slow <- function(df){
  groups <- list()
  for(i in seq_len(nrow(df))) {
    this_row <- as.character(df[i,])
    chk <- purrr::map_lgl(groups, ~ length(intersect(.x,this_row))>0)
    if(!any(chk)){
      # If no group found, create a new group
      groups <- append(groups, list(this_row))
    } else {
      # If a group is found, merge this row with that group
      idx <- which(chk)[1]
      groups[[idx]] <- unique(c(groups[[idx]], this_row))
    }
  }
  groups
}

infer_implied_surrounding_blocks <- function(ad_map, d_dat, d_att){
  # This function is used to generate the implied surrounding-block of each data
  # block. "implied surrounding block" = exact cells containing data-block cells
  # and connected attr-blocks cells. This (`implied_surrounding_blocks_single`)
  # is for single data-block.
  implied_surrounding_blocks_single <- function(data_block){
    # For each data block, find the corresponding attribute blocks through
    # mapping (ad_map)
    this <- ad_map[ad_map$data_gid == data_block,]

    # Combine data cells and attribute cells for this data block (and connect
    # attributes)
    this_cells <- d_dat[d_dat$data_gid == data_block,] %>%
      # Mark as data cells
      dplyr::mutate(ad_type= "data") %>%
      # Combine with attr cells
      dplyr::bind_rows(
        d_att[d_att$attr_gid %in% this$attr_gid,] %>%
          # Mark as attr cells
          dplyr::mutate(ad_type= "attr")
      ) %>%
      dplyr::distinct(.data$row, .data$col, .data$ad_type)

    # This is new gid for this data block + attribute block : (info_gid) Info
    # block
    this_cells$gid <- data_block

    this_cells
  }

  # Calculate implied surrounding-blocks for each data block
  implied_surrounding_blocks <- unique(ad_map$data_gid) %>%
    purrr::map_dfr(implied_surrounding_blocks_single)

  implied_surrounding_blocks
}

infer_data_block_merging <- function(ad_map, d_dat, d_att){

  # Calculate implied surroundings for each data block

  # This function is used to calculate the implied surroundings of each data
  # block. "implied surroundings" = exact cells boundary (containing rectangle)
  # of data-block cells and connected attr-blocks cells.
  implied_surroundings <- infer_implied_surrounding_blocks(
    ad_map, d_dat, d_att) %>%
    dplyr::group_by(.data$gid) %>%
    # Calculate the block boundaries for each data_gid
    infer_block_boundary(exact = TRUE) %>%
    # Rename data_gid from gid
    dplyr::rename(data_gid = .data$gid)

  # Self-join "implied surroundings" to find overlaps among combinations of two
  # data blocks
  implied_surroundings_overlap <- implied_surroundings %>%
    dplyr::inner_join(
      implied_surroundings,
      # Cartesian join
      by = character(0)  ,
      suffix = c("", "_2")
    ) %>%
    # Only combination of two gid is required so discard others
    dplyr::filter(.data$data_gid > .data$data_gid_2)

  # Compute overlap between two rectangles (blocks)
  implied_surroundings_overlap <- implied_surroundings_overlap %>%
    dplyr::mutate(
      # Check if the two rectangles (blocks) are exactly the same
      is_exact = (.data$r_min == .data$r_min_2) & (.data$r_max == .data$r_max_2) &
        (.data$c_min == .data$c_min_2) & (.data$c_max == .data$c_max_2),
      # Calculate row overlap length (if any); zero if no overlap
      r_overlap = pmax(0, pmin(.data$r_max, .data$r_max_2) - pmax(.data$r_min, .data$r_min_2) + 1),
      # Calculate column overlap length (if any); zero if no overlap
      c_overlap = pmax(0, pmin(.data$c_max, .data$c_max_2) - pmax(.data$c_min, .data$c_min_2) + 1),
      # Compute area of intersection between the two rectangles (overlap area)
      intersection_area = .data$r_overlap * .data$c_overlap,
      # Set block_divergence to zero if rectangles are equal, else use intersection area
      block_divergence = ifelse(.data$is_exact, 0, .data$intersection_area)
    )

  # These are the data gids that can be combined directly (It is a map between
  # two join-able data-gids as stacked data.frame)
  combine_data_blocks <- implied_surroundings_overlap %>%
    dplyr::filter(.data$is_exact) %>%
    dplyr::distinct(.data$data_gid, .data$data_gid_2)

  # These are the data gids that have block divergence (overlap area)
  issue_data_blocks <- implied_surroundings_overlap %>%
    dplyr::filter(.data$block_divergence > 0) %>%
    dplyr::distinct(.data$data_gid) %>%
    dplyr::pull()

  # Note apart from the above, we also may have situations where block_divergence == 0 and is_exact is FALSE.
  # Those are single data blocks that are not connected to any other data block. For those combination is not required.
  # By simple NA handling those will be taken care of in the next step.

  # Resolution of issue data blocks (if any)
  if(length(issue_data_blocks)>0){

    # We take portion of d_dat that has issue data blocks
    d_dat_issue <- d_dat[d_dat$data_gid %in%
                           issue_data_blocks,]
    # We calculate the exact block boundaries for those issue data blocks
    d_dat_issue_bd <- d_dat_issue %>%
      # This group by data_gid is required for calculating block boundaries.
      # Otherwise it will give boundaries for all data.
      dplyr::group_by(.data$data_gid) %>%
      infer_block_boundary(exact = TRUE)


    implied_surroundings_overlap_issue <- implied_surroundings %>%
      # Take only those implied_surroundings corresponding to data gids that
      # have not issue (those will be benchmark-ed against issue data blocks)
      dplyr::filter(!(.data$data_gid %in% issue_data_blocks)) %>%
      # Join with issue data blocks to find overlaps
      dplyr::inner_join(
        d_dat_issue_bd,
        # Cartesian join
        by = character(0)  ,
        suffix = c("", "_2")
      )

    implied_surroundings_overlap_issue <- implied_surroundings_overlap_issue %>%
      dplyr::group_by(.data$data_gid_2) %>%
      dplyr::mutate(
        # Calculate row and column overlap lengths (if any)
        r_overlap = pmax(0, pmin(.data$r_max, .data$r_max_2) -
                           pmax(.data$r_min, .data$r_min_2) + 1),
        c_overlap = pmax(0, pmin(.data$c_max, .data$c_max_2) -
                           pmax(.data$c_min, .data$c_min_2) + 1),
        # Area of intersection between rectangles
        intersection_area = .data$r_overlap * .data$c_overlap,
        # Area of second rectangle
        area_2 = (.data$r_max_2 - .data$r_min_2 + 1) * (.data$c_max_2 - .data$c_min_2 + 1),
        # Fraction of area of second rectangle covered by intersection
        fraction_of_area_covered = .data$intersection_area / .data$area_2
      ) %>%
      dplyr::ungroup()

    best_solution <- implied_surroundings_overlap_issue %>%
      dplyr::group_by(.data$data_gid_2) %>%
      # For each data_gid_2, find the one with maximum fraction of area covered
      dplyr::filter(.data$fraction_of_area_covered==max(.data$fraction_of_area_covered)) %>%
      dplyr::ungroup()

    combine_data_blocks_issue <- best_solution %>%
      dplyr::group_by(.data$data_gid_2) %>%
      # In case of multiple solutions for a data_gid_2, take any one
      dplyr::summarise(data_gid = .data$data_gid[1], .groups = "drop")

  } else {
    # If no issue data blocks, set empty tibble
    combine_data_blocks_issue <- tibble::tibble()
  }


  # Finally the data gids that can be combined are given by following
  combine_data_blocks_final <- dplyr::bind_rows(
    combine_data_blocks,
    combine_data_blocks_issue
  )

  # Use connected components to find groups of data gids that can be combined.
  # It is a list of vectors where each node of list is character vector
  # depicting data-gids which are to be merged.
  data_gid_join_map <- infer_group_connected_blocks(combine_data_blocks_final)

  # Convert it to a mappable format data.frame
  data_gid_join_map <- data_gid_join_map %>%
    purrr::map_dfr(~ tibble::tibble(
      data_gid = .x,
      # New data_gid is the minimum of all data gids in the group
      data_gid_to = min(.x))
    )

  # Form new d_dat equivalent with data_gid replaced by data_gid_to (wherever
  # applicable)
  d_dat_joined <- d_dat %>%
    # Join with data_gid_join_map to get data_gid_to
    dplyr::left_join(data_gid_join_map, by = "data_gid") %>%
    dplyr::mutate(
      # This is where we take care of (block_divergence == 0 and is_exact is
      # FALSE) situations
      data_gid = ifelse(is.na(.data$data_gid_to), .data$data_gid, .data$data_gid_to)
    ) %>%
    dplyr::select(-.data$data_gid_to) %>%
    dplyr::distinct()

  # Since ad_map is a mapping between data_gid and attr_gid, we need to replace
  # data_gid in ad_map with data_gid_to from data_gid_join_map also so that new
  # d_dat_joined and d_att are in sync. We follow same pattern.
  ad_map_joined <- ad_map %>%
    dplyr::left_join(data_gid_join_map, by = "data_gid") %>%
    dplyr::mutate(
      data_gid = ifelse(is.na(.data$data_gid_to), .data$data_gid, .data$data_gid_to)
    ) %>%
    dplyr::select(-.data$data_gid_to) %>%
    dplyr::distinct()

  # However, we still need to ensure that for each data_gid and direction_group,
  # we have single attr_gid. For which we use slightly different method as
  # compared to infer_major_direction_stats. Before employing that method, we
  # need to ensure that we have distinct data_gid, attr_gid, direction, and
  # direction_group combinations. This is because we have combined data gids and
  # thus we may have duplicate entries in ad_map for same data_gid, attr_gid,
  # direction, and direction_group combinations. We need to summarize those
  # combinations.
  ad_map_joined <- ad_map_joined %>%
    # Since we combined data gids, we should have duplicate entries in data_gid,
    # attr_gid, direction, direction_group. We first calculate the summaries on
    # each such distinct combinations.
    dplyr::group_by(
      .data$data_gid, .data$attr_gid,
      .data$direction, .data$direction_group) %>%
    # Calculate proper summaries for each data_gid and attr_gid pair (direction,
    # direction_group) is probably fixed (Due to combination criteria).
    dplyr::summarise(
      # These measures will be best represented by maximum
      dim_len = max(.data$dim_len),
      dim_len_cont = max(.data$dim_len_cont),
      attr_gids_between = max(.data$attr_gids_between),
      # Minimum distance for this data_gid and attr_gid pair
      dist = min(.data$dist),
      .groups = "drop"
    )

  # Filtering started for any potential duplicates.
  ad_map_joined <- ad_map_joined %>%
    # Regroup by data_gid and direction_group.
    dplyr::group_by(.data$data_gid, .data$direction_group) %>%
    # For each data_gid and direction_group, find the one with maximum dimension length
    dplyr::filter(.data$dim_len_cont==max(.data$dim_len_cont)) %>%
    # Further filter the one with minimum distance
    dplyr::filter(.data$dist==min(.data$dist)) %>%
    # Still if there are multiple rows, take the first one
    dplyr::slice(1) %>%
    # Ungroup to remove grouping
    dplyr::ungroup()

  # Finally return the joined data and ad_map

  list(
    d_dat = d_dat_joined,
    ad_map = ad_map_joined
  )
}



# Section: Attr Focused Direction DF ----


# Infer Directional Orientation for Attributes Relative to Data Block(s)
# Main: Get direction, direction_group, and dist for all pairs
infer_get_direction_df <- function(d_dat_bd, d_att, d_dat, fix_inside = TRUE) {
  # Cartesian product: all attribute blocks vs all data blocks
  adnn <- tidyr::crossing(d_att, d_dat_bd)

  # Compute masks for each direction
  N  <- adnn$row < adnn$r_min & dplyr::between(adnn$col, adnn$c_min, adnn$c_max)
  S  <- adnn$row > adnn$r_max & dplyr::between(adnn$col, adnn$c_min, adnn$c_max)
  W  <- adnn$col < adnn$c_min & dplyr::between(adnn$row, adnn$r_min, adnn$r_max)
  E  <- adnn$col > adnn$c_max & dplyr::between(adnn$row, adnn$r_min, adnn$r_max)
  NW <- adnn$row < adnn$r_min & adnn$col < adnn$c_min
  NE <- adnn$row < adnn$r_min & adnn$col > adnn$c_max
  SE <- adnn$row > adnn$r_max & adnn$col > adnn$c_max
  SW <- adnn$row > adnn$r_max & adnn$col < adnn$c_min
  INSIDE <- adnn$row >= adnn$r_min & adnn$row <= adnn$r_max &
    adnn$col >= adnn$c_min & adnn$col <= adnn$c_max

  # Vectorized assignment
  direction = dplyr::case_when(
    N  ~ "N",  S  ~ "S",  W  ~ "W",  E  ~ "E",
    NW ~ "NW", NE ~ "NE", SE ~ "SE", SW ~ "SW",
    INSIDE ~ "INSIDE"
  )

  direction_group = dplyr::case_when(
    N | S ~ "NS",
    W | E ~ "WE",
    NW | NE | SE | SW ~ "corner",
    INSIDE ~ "INSIDE"
  )

  dist = dplyr::case_when(
    N  ~ adnn$r_min - adnn$row,
    S  ~ adnn$row - adnn$r_max,
    W  ~ adnn$c_min - adnn$col,
    E  ~ adnn$col - adnn$c_max,
    NW ~ sqrt((adnn$r_min - adnn$row)^2 + (adnn$c_min - adnn$col)^2),
    NE ~ sqrt((adnn$r_min - adnn$row)^2 + (adnn$c_max - adnn$col)^2),
    SE ~ sqrt((adnn$r_max - adnn$row)^2 + (adnn$c_max - adnn$col)^2),
    SW ~ sqrt((adnn$r_max - adnn$row)^2 + (adnn$c_min - adnn$col)^2),
    INSIDE ~ 0
  )

  out <- dplyr::bind_cols(adnn, tibble::tibble(direction, direction_group, dist)) %>%
    dplyr::filter(!is.na(.data$direction)) %>%
    dplyr::select(-.data$r_min, -.data$r_max, -.data$c_min, -.data$c_max)

  # INSIDE fix (optional, applied per data_gid group)
  if(fix_inside) {
    out <- out %>%
      split(out$data_gid) %>%
      purrr::map_dfr(~infer_fix_inside_direction(.x, d_dat_bd, d_dat)) %>%
      dplyr::ungroup()
  }
  out
}

# Fix INSIDE direction_group/direction for each data_gid if required
infer_fix_inside_direction <- function(df, d_dat_bd, d_dat) {

  inside_rows <- which(df$direction_group == "INSIDE")

  # If there are no INSIDE cases, then we can return as it is
  if(length(inside_rows) == 0) return(df)

  not_inside <- df$direction_group != "INSIDE"
  # If more than 80% are inside, invoke probabilistic fix
  if(mean(not_inside)<0.2) {
    # It is the only place where d_dat is used
    return(infer_fix_inside_direction_probabilistic(df, d_dat))
  }

  # Take sub-boundaries of data block for this data_gid
  d_dat_bd_sub <- d_dat_bd[d_dat_bd$data_gid == df$data_gid[1], ]

  # See which direction_groups are present (excluding INSIDE)
  present_groups <- unique(df$direction_group[not_inside])
  remaining_direction_group <- setdiff(c("NS", "WE"), present_groups)

  # If only one group present, assign that to INSIDE. If both NS and WE, assign
  # the one with the smaller observed dimension fraction
  group_to_assign <- if(length(remaining_direction_group) > 0) {
    remaining_direction_group[1]
  } else {
    # In this case we have both NS and WE direction groups in adm_ok

    # We find best direction group based on the bounding box of data blocks and
    # other parameters

    # Compute observed lengths for NS (unique col) and WE (unique row)
    ns_dim <- dplyr::n_distinct(df$col[df$direction_group == "NS"])
    we_dim <- dplyr::n_distinct(df$row[df$direction_group == "WE"])
    # Compute bounding box lengths
    ns_full <- d_dat_bd_sub$c_max - d_dat_bd_sub$c_min + 1
    we_full <- d_dat_bd_sub$r_max - d_dat_bd_sub$r_min + 1
    # Fraction filled
    ns_frac <- ns_dim / ns_full
    we_frac <- we_dim / we_full
    # Assign the direction to INSIDE cases based on the minimum dimension
    # fraction as that is where the INSIDE cases are likely to be. (more filling
    # required)
    if(ns_frac < we_frac) "NS" else "WE"
  }
  # Assign direction_group for INSIDE
  df$direction_group[inside_rows] <- group_to_assign

  # Compute block center and mean position of INSIDE cells
  center_row <- (d_dat_bd_sub$r_max + d_dat_bd_sub$r_min) / 2
  center_col <- (d_dat_bd_sub$c_max + d_dat_bd_sub$c_min) / 2
  mean_row <- mean(df$row[inside_rows])
  mean_col <- mean(df$col[inside_rows])

  # Assign direction: for NS use N or S, for WE use W or E, based on majority position
  if(group_to_assign == "NS") {
    df$direction[inside_rows] <- ifelse(mean_row < center_row, "N", "S")
  } else if(group_to_assign == "WE") {
    df$direction[inside_rows] <- ifelse(mean_col < center_col, "W", "E")
  }

  df
}

# More resource intensive but probabilistic version of INSIDE fix. This may be
# the only hope for complex situations.
infer_fix_inside_direction_probabilistic <- function(df, d_dat){

  inside_rows <- which(df$direction_group == "INSIDE")

  # If there are no INSIDE cases, then we can return as it is
  if(length(inside_rows) == 0) return(df)

  # Take this data_gid's block only as new d_dat
  d_dat_this <- d_dat[d_dat$data_gid == df$data_gid[1], ]

  # Take distinct attr_gid, row, col combinations as new d_att
  d_att_this <- df %>%
    dplyr::distinct(.data$attr_gid, .data$row, .data$col)

  # Set a few probs for different coverage
  probs <- seq(0.55, 0.95, by = 0.05)

  do_for_a_prob <- function(p){
    # For each such coverage probabilities get direction_df
    infer_get_direction_df(
      # This is where Approximate boundaries are varied for different coverage
      d_dat_bd = infer_block_boundary(d_dat_this, exact = FALSE, coverage = p),
      d_att = d_att_this,
      d_dat = d_dat_this,
      # This is very important otherwise it will fall in infinite recursion
      fix_inside = FALSE
    )
  }

  # Collect all output
  all_probs_out <- purrr::map_dfr(probs, do_for_a_prob)

  # Safe minimum function to handle empty cases. So that warning does not occur
  min_safe <- function(x){
    if(length(x) == 0) return(NA_real_)
    min(x, na.rm = TRUE)
  }

  # Generate result
  res <- all_probs_out %>%
    # For each attr_gid (row, col)
    dplyr::group_by(.data$attr_gid, .data$row, .data$col) %>%
    dplyr::summarise(
      # Get min distance among all outputs for different coverage
      dist = min_safe(.data$dist[.data$direction!="INSIDE"]),
      # Get most frequently occurring direction among all outputs from varies
      # coverage-based boundaries of same data-block
      direction = util_most_frequent(.data$direction[.data$direction!="INSIDE"]),
      .groups = "drop"
    )

  # Fill NA if any
  res$dist[is.na(res$dist)] <- min_safe(res$dist[!is.na(res$dist)])
  res$direction[is.na(res$direction)] <- util_most_frequent(
    res$direction[!is.na(res$direction)]
  )

  # Derive direction_group
  res <- res %>%
    dplyr::mutate(
      direction_group = dplyr::case_when(
        .data$direction %in% c("N", "S") ~ "NS",
        direction %in% c("E", "W") ~ "WE",
        direction %in% c("NE", "NW", "SE", "SW") ~ "corner",
        direction == "INSIDE" ~ "INSIDE",
        TRUE ~ NA_character_
      )
    )

  # Fill NA for direction_group (if any)
  res$direction_group[is.na(res$direction_group)] <- util_most_frequent(
    res$direction_group[!is.na(res$direction_group)]
  )

  # Restore data_gid for compatibility
  res %>%
    dplyr::mutate(data_gid = df$data_gid[1])

}



# Section: Infer Direction Stats ----

# Main function responsible for computing major direction statistics
infer_major_direction_stats <- function(
    d_dat, d_att, stage_1_only = FALSE) {

  # Stage 1: Calculate Directional Statistics
  d_ad_stage1 <- dplyr::bind_rows(
    infer_calc_stats_for_admap_dimwise(d_dat, d_att, wise = "col"),
    infer_calc_stats_for_admap_dimwise(d_dat, d_att, wise = "row")
  )

  # Stage 1 is complete, if stage_1_only is TRUE, return the result
  if(stage_1_only){
    # This is for other modules
    return(d_ad_stage1)
  }

  # Early checks for d_ad_stage1
  if(NROW(d_ad_stage1) == 0) {
    # If no data_gid and attr_gid pairs found, return empty tibble as further
    # porcessing is not possible.
    return(
      list(
        d_dat = d_dat,
        # Empty ad_map
        ad_map = tibble::tibble()
      )
    )
  }

  # This is part 1 of stage 2
  d_ad_stage2_1 <- d_ad_stage1 %>%
    # In stage 2 part 1, we select only those data_gid and direction_group pairs
    # that have no intermediate gids between data_gid and attr_gid
    dplyr::filter(.data$attr_gids_between == 0) %>%
    dplyr::group_by(.data$data_gid, .data$direction_group) %>%
    # For each data_gid and direction_group, find the one with maximum dimension length
    dplyr::filter(.data$dim_len_cont==max(.data$dim_len_cont)) %>%
    # Further filter the one with minimum distance
    dplyr::filter(.data$dist==min(.data$dist)) %>%
    dplyr::ungroup()

  # Ideally we should have one row per data_gid and direction_group
  all_expected <- expand.grid(
    data_gid = unique(d_ad_stage2_1$data_gid),
    direction_group = c("NS", "WE"),
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )

  # Those data_gid and direction_group pairs that are not in d_ad_stage2_1
  remaining <- dplyr::anti_join(
    all_expected, d_ad_stage2_1,
    by = c("data_gid", "direction_group")
  )

  # If there remaining pairs, that means `attr_gids_between == 0` may have
  # filtered them out And thus we need slightly different approach for remaining
  # data_gid and direction_group pairs

  if(NROW(remaining)>0) {

    # Stage 2 part 2: For remaining data_gid and direction_group pairs.
    # Continuous mode of selection for remaining data_gid and direction_group.

    d_ad_stage2_2 <- d_ad_stage1 %>%
      # Take the remaining data_gid and direction_group from d_ad_stage1
      dplyr::inner_join(
        remaining, by = c("data_gid", "direction_group"),
        suffix = c("", "_rem")) %>%
      dplyr::group_by(.data$data_gid, .data$direction_group) %>%
      # See calculated eval_measure(s) in `infer_calc_eval_measure_for_admap`
      infer_calc_eval_measure_for_admap() %>%
      dplyr::filter(.data$eval_measure == max(.data$eval_measure)) %>%
      dplyr::ungroup()

    # Ensure that the columns in d_ad_stage2_2 match those in d_ad_stage2_1
    d_ad_stage2_2 <- d_ad_stage2_2[colnames(d_ad_stage2_1)]
    # Combine both parts of stage 2
    d_ad_stage2 <- dplyr::bind_rows(d_ad_stage2_1, d_ad_stage2_2)
  }else{
    # If there are no remaining pairs, we can use the first part of stage 2
    d_ad_stage2 <- d_ad_stage2_1
  }

  # Stage 3: Joining Data Blocks Based on Implied Surroundings
  #
  # For each data block, we take both NS (North-South) and WE (West-East) side
  # attribute blocks and merge them to form a single block containing data and
  # attributes on both sides. We then identify enclosing boundaries for each
  # data_gid-induced block.
  #
  # Next, we calculate a metric known as "block divergence." If a block is fully
  # contained within another, its block divergence is zero. Otherwise, we
  # compute the overlap cells (even if empty) as block divergence. This measures
  # how well a data block is surrounded.
  #
  # Data blocks with no block divergence can be grouped with other blocks having
  # the same implied surroundings.
  #
  # For blocks with block divergence, we use `d_ad_stage1` to find suitable NS
  # and WE mappings to eliminate block divergence.
  #
  # There are two possibilities:
  #
  # 1. If the data block is contained within the implied surroundings of another
  # block, we simply use the NS and WE mapping of that block.
  #
  # 2. Otherwise, we use `d_ad_stage1` to find a match that minimizes overall
  # block divergence as an optimization measure.
  #
  # Note: This step is crucial as it enables the merging of data blocks.

  # Data Block Merges (if any)
  data_block_merging <- infer_data_block_merging(
    d_ad_stage2, d_dat, d_att
  )

  list(
    d_dat = data_block_merging$d_dat,
    ad_map = data_block_merging$ad_map
  )
}


infer_minor_direction_stats <- function(d_dat, d_att, admap){

  # Check Remaining attributes that are not mapped in major direction
  remaining_attrs <- d_att %>%
    dplyr::anti_join(
      admap, by = "attr_gid"
    )

  if(NROW(remaining_attrs)==0){
    # If no remaining attributes, we can return empty tibble as nothing to map
    return(tibble::tibble())
  }

  # Since major attributes are already mapped, we can use the implied
  # surrounding blocks of data gids to find the minor direction stats. This
  # recreates the "implied surrounding blocks" of data gids considering mapped
  # major attributes also part of data blocks.
  d_implied_surrounding_blocks <- infer_implied_surrounding_blocks(
    admap, d_dat, d_att) %>%
    dplyr::distinct(.data$row, .data$col, .data$gid) %>%
    # Rename gid to data_gid for consistency
    dplyr::rename(data_gid = .data$gid)


  # Initialize admap_minor_part1 with empty tibble
  # If no attributes are mapped in major direction, we can take empty tibble
  admap_minor_part1 <- tibble::tibble()

  # VH is Vertical and Horizontal
  VH_map <- infer_major_direction_stats(
    d_dat = d_implied_surrounding_blocks,
    d_att = remaining_attrs,
    stage_1_only = TRUE
  )


  if(NROW(VH_map)>0){
    # It means some attributes are mapped in major direction either vertically
    # or horizontally. We need to see what are left. And add to final results.
    # Unlike in case of data gids here we are only interested to get connected
    # to any data gid and thus we can take the first one with maximum dimension
    # length and minimum distance in major direction.


    # But it may connect far way attr_blocks also to avoid that we do another
    # mixing with attr based all direction map and fine-tune VH_map result

    # All Direction Attr Focus Map for Completeness

    # However this may be time-consuming if large number of small attrs and
    # data_gid is pesent.
    num_d_blocks <- d_implied_surrounding_blocks$data_gid %>%
      unique() %>% length()
    num_remaining_attrs <- remaining_attrs$attr_gid %>%
      unique() %>% length()

    size_chk <- num_d_blocks * num_remaining_attrs

    # If the size of the cross product is small enough, we can use the attr focused
    # method as there is a expand.grid call which searches all possible
    # data-attr combinations.
    if(size_chk < 10^4){
      # All direction Attr Focus Map
      all_dir_map  <- infer_get_direction_df(
        d_dat_bd = d_implied_surrounding_blocks %>%
          dplyr::group_by(.data$data_gid) %>%
          infer_block_boundary(),
        d_att = remaining_attrs,
        d_dat = d_implied_surrounding_blocks,
        fix_inside = TRUE)

      # The output of `infer_get_direction_df` is a data frame with columns:
      # data_gid, attr_gid, direction, direction_group, dist but for each cells
      # of attr_gid i.e. (row, col) present. So we need to find data-gid attr-gid
      # pairwise stats.
      all_dir_map <- all_dir_map %>%
        dplyr::group_by(.data$attr_gid) %>%
        # Typical Selection Criteria:
        dplyr::filter(.data$dist == min(.data$dist)) %>%
        # If still multiple rows, take the first one
        dplyr::slice(1) %>%
        dplyr::ungroup() %>%
        dplyr::select(-.data$row,-.data$col)
    }else{
      # Otherwise initialize to empty tibble
      all_dir_map <- tibble::tibble()
    }

    # Derive VH_map final connected results
    admap_VH <- VH_map %>%
      dplyr::group_by(.data$attr_gid) %>%
      # Typical Selection Criteria:
      dplyr::filter(.data$dim_len_cont == max(.data$dim_len_cont)) %>%
      dplyr::filter(.data$dist == min(.data$dist)) %>%
      # If still multiple rows, take the first one
      dplyr::slice(1) %>%
      dplyr::ungroup()

    # Combine best of both method to get finer results
    combined_map <- dplyr::bind_rows(
      admap_VH,
      all_dir_map
    )

    admap_minor_part1 <- combined_map %>%
      dplyr::group_by(.data$attr_gid) %>%
      # Dist is only common measure among VH and all_dir map
      dplyr::filter(.data$dist == min(.data$dist)) %>%
      # If still multiple rows, take the first one
      dplyr::slice(1) %>%
      dplyr::ungroup()

    # Update `remaining_attrs` for those attributes that are not yet mapped
    # These are likely to be corner attributes or attributes
    remaining_attrs <- remaining_attrs %>%
      dplyr::anti_join(
        admap_minor_part1, by = "attr_gid"
      )

  }

  # Initialize admap_minor_part2 with empty tibble
  admap_minor_part2 <- tibble::tibble()

  if(NROW(remaining_attrs)>0){

    # num_d_blocks Probably calculated earlier
    num_d_blocks <- d_implied_surrounding_blocks$data_gid %>%
      unique() %>% length()

    # num_remaining_attrs was calculated earlier but remaining_attrs may change
    num_remaining_attrs <- remaining_attrs$attr_gid %>%
      unique() %>% length()

    size_chk <- num_d_blocks * num_remaining_attrs

    # If the size of the cross product is small enough, we can use the old
    # method as there is a expand.grid call which searches all possible
    # data-attr combinations. If size does not meet then we leave these attr to
    # be connected.
    if(size_chk < 10^4){
      # Last resort is to use attr_based method

      # Compute the boundaries of implied surrounding blocks
      d_dat_impl <- d_implied_surrounding_blocks %>%
        # This grouping is required to calculate block boundaries otherwise it
        # `infer_block_boundary` will give boundaries for all data.
        dplyr::group_by(.data$data_gid)

      admap_minor_part2 <- infer_get_direction_df(
        d_dat_impl %>%
          # Note that this is approximate block boundaries (not exact)
          infer_block_boundary(coverage = 0.95, exact = FALSE),
        d_att = remaining_attrs, d_dat = d_dat_impl)

      # If admap_minor_part2 is empty, we may not need to do anything further
      if(NROW(admap_minor_part2)>0){

        # As indicated earlier we need to find data-gid attr-gid pairwise stats.
        admap_minor_part2 <- admap_minor_part2 %>% dplyr::group_by(.data$attr_gid) %>%
          # Typical Selection Criteria:
          dplyr::filter(.data$dist == min(.data$dist)) %>%
          # If still multiple rows, take the first one
          dplyr::slice(1) %>%
          dplyr::ungroup()

      }
    }
  }

  admap_minor <- dplyr::bind_rows(
    admap_minor_part1,
    admap_minor_part2
  )  %>%
    dplyr::select(.data$data_gid, .data$attr_gid, .data$direction, .data$direction_group, .data$dist)

  admap_minor$link_type <- "minor"

  admap_minor

}



# Section: Split Attribute Blocks ----

infer_attr_split <- function(info_blocks) {
  # This function is used to infer the attribute split based on the provided
  # information blocks.

  d_dat <- info_blocks[info_blocks$ad_type == "data", ]
  d_att <- info_blocks[info_blocks$ad_type == "attr", ]
  d_att$ad_type <- NULL

  d_dat <- d_dat %>%
    dplyr::rename(
      # Here retain the gid column as data_gid
      data_gid = .data$gid
    ) %>%
    # Remove the gid column (and others) as it is not needed
    dplyr::distinct(.data$data_gid, .data$row, .data$col)

  d_dat_bd <- d_dat %>%
    dplyr::group_by(.data$data_gid) %>%
    infer_block_boundary(exact = TRUE)

  for_each_info_block <- function(d_blk) {
    # This function is used to process each block of information.

    d_dat_bd_sub <- d_dat_bd %>%
      dplyr::filter(.data$data_gid == d_blk)

    # Attr subsection
    # This is a trick to use attr focused - get_direction_df to re-classify each
    # attributes into direction groups
    d_att_sub <- d_att %>%
      dplyr::filter(.data$gid == d_blk) %>%
      dplyr::mutate(
        # Create a dummy attr_gid "a"+dblk for attribute. It essentially means that
        # all attributes are grouped together.
        attr_gid = paste0("a", d_blk)
      ) %>%
      # Remove the gid column (and others) as it is not needed
      dplyr::distinct(.data$attr_gid, .data$row, .data$col)

    # Get the direction for this block and sub-attributes
    infer_get_direction_df(d_dat_bd_sub, d_att = d_att_sub, d_dat = d_dat)
  }

  admap_for_attr_split <- unique(d_dat_bd$data_gid) %>%
    purrr::map_dfr(for_each_info_block)


  admap_for_attr_split <- admap_for_attr_split %>%
    dplyr::group_by(.data$data_gid, .data$direction) %>%
    dplyr::mutate(
      # Assign an order to each unique distance within each group
      dist_order = as.integer(as.factor(.data$dist)),

      # Create split_tag_part based on direction and position
      split_tag_part = dplyr::case_when(
        .data$direction == "N" ~ paste0(max(.data$row) - .data$row + 1, ":0"),
        .data$direction == "S" ~ paste0(.data$row, ":0"),
        .data$direction == "W" ~ paste0("0:", max(.data$col) - .data$col + 1),
        .data$direction == "E" ~ paste0("0:", .data$col),
        .data$direction_group == "corner" ~ paste0(.data$row, ":", .data$col),
        TRUE ~ "0:0"
      ),

      # Create a unique split_tag for each group/order/position
      split_tag = as.integer(as.factor(
        paste0(.data$dist_order, "_", .data$split_tag_part)
      )),

      # Construct a micro attribute gid for each split
      micro_attr_gids = paste0(
        "a", .data$data_gid, "_", .data$direction, "_", .data$split_tag
      )
    )

  # Final Touch for Creation of attr_split
  attr_split <- admap_for_attr_split %>%
    dplyr::ungroup() %>%
    dplyr::select(
      .data$micro_attr_gids, .data$data_gid,
      .data$direction, .data$direction_group,
      .data$row, .data$col,
      .data$dist
    ) %>%
    dplyr::distinct()

  return(attr_split)
}


