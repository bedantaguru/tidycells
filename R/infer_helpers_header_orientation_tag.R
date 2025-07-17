

#' Infer HOT (header_orientation_tag) metric
#'
#' Computes a scaled fraction denoting the coverage (1 means full coverage) for the supplied direction.
#'
#' @param data_cells Data frame of data block cells (with data_gid)
#' @param header_cells Data frame of header/attribute cells (with attr_gid)
#' @param direction Direction name (should match one of those from infer_get_valid_header_orientation_tags())
#' @return A numeric value between 0 and 1: fraction of data_cells covered by the header in the given direction.
#' @keywords internal
infer_get_HOT_metric <- function(data_cells, header_cells, direction) {
  cov_count <- tryCatch(
    infer_get_HOT_metric_base(data_cells, header_cells, direction),
    error = function(e) 0
  )
  if (length(cov_count) != 1 || is.na(cov_count)) cov_count <- 0
  cov_count / NROW(data_cells)
}

# Coverage count for HOT metric (internal)
infer_get_HOT_metric_base <- function(data_cells, header_cells, direction) {
  # If header cell is a single cell or empty, treat all data cells as covered by
  # that header
  if (NROW(header_cells) <= 1) {
    return(NROW(data_cells))
  }
  # Otherwise, use bind_header to attach headers and count how many data cells are assigned a header in this direction
  attach_header(data_cells, header_cells, direction) %>%
    dplyr::filter(!is.na(.data$attr_gid)) %>%
    NROW()
}



# Adds each attribute-blocks a flag whether it is a full dimension or not.
# This is required in attaching header_orientation_tags
infer_dimension_analysis_details <- function(admap_cellwise) {
  # Step 1: Compute dimensions for each data block (number of unique rows and columns)
  data_dims <- admap_cellwise %>%
    dplyr::group_by(.data$data_gid) %>%
    dplyr::summarise(
      r_dim_data = dplyr::n_distinct(.data$row_d),  # Number of unique rows in data block
      c_dim_data = dplyr::n_distinct(.data$col_d),  # Number of unique columns in data block
      .groups = "drop"
    )

  # Step 2: For each (attr_gid, data_gid) pair, compute intersection dimensions and determine direction group
  attr_data_dim <- admap_cellwise %>%
    dplyr::group_by(.data$attr_gid, .data$data_gid) %>%
    dplyr::summarise(
      # Number of overlapping rows and columns between attribute and data cells
      r_dim = length(intersect(.data$row_d, .data$row_a)),
      c_dim = length(intersect(.data$col_d, .data$col_a)),
      # Take direction_group from the first entry (should be same for the group)
      direction_group = .data$direction_group[1],
      .groups = "drop"
    ) %>%
    # Attach the data block dimensions for each pair
    dplyr::inner_join(data_dims, by = "data_gid") %>%
    # Step 3: Calculate relative dimension coverage for NS, WE, and corner direction groups
    dplyr::mutate(
      rel_dim = dplyr::case_when(
        .data$direction_group == "NS" ~ .data$c_dim / .data$c_dim_data,   # NS checks columns coverage
        .data$direction_group == "WE" ~ .data$r_dim / .data$r_dim_data,   # WE checks rows coverage
        .data$direction_group == "corner" ~ 0,                            # corner not considered full
        TRUE ~ 0
      ),
      # full_dim is TRUE if relative dimension coverage >= 1
      full_dim = .data$rel_dim >= 1
    )

  # Add "is_full_dim" to the admap_cellwise
  out <- attr_data_dim %>%
    dplyr::distinct(.data$attr_gid, .data$data_gid, is_full_dim = .data$full_dim) %>%
    dplyr::right_join(admap_cellwise, by = c("attr_gid", "data_gid"))

  return(out)
}

infer_get_valid_header_orientation_tags <- function() {
  # This function returns the valid header orientation tags.
  # direction order
  # "v" "vl" "vr" "vm"
  # "h" "hu" "hd" "hm"
  # "v" "vl" "vr" "vm"
  # "h" "hu" "hd" "hm"

  list(
    N = c("v", "vl", "vr", "vm"),
    W = c("h", "hu", "hd", "hm"),
    S = c("v", "vl", "vr", "vm"),
    E = c("h", "hu", "hd", "hm"),
    NE = "vr",
    NW = "vl",
    SE = "vr",
    SW = "vl"
  )

}

infer_get_header_orientation_tag <- function(admap_cellwise_with_dim) {
  # 1. Get valid header orientation tag mappings
  header_tag_map <- infer_get_valid_header_orientation_tags()

  # 2. Select pairs with full dimension coverage in major directions (NS/WE)
  full_dim_pairs <- admap_cellwise_with_dim %>%
    dplyr::filter(.data$is_full_dim, .data$direction_group %in% c("NS", "WE"))

  # 3. Remaining pairs are not full coverage
  partial_dim_pairs <- admap_cellwise_with_dim %>%
    dplyr::anti_join(full_dim_pairs, by = c("data_gid", "attr_gid"))

  # 4. Assign tags for full coverage (N, W, S, E) using first listed tag in map
  cardinal_tags <- purrr::map_chr(header_tag_map[c("N", "W", "S", "E")], 1)
  full_dim_pairs$header_orientation_tag <- cardinal_tags[full_dim_pairs$direction]

  # 5. For ambiguous (partial) cases, modify direction order (keeping v, h at
  # the last)
  header_tag_map_mod <- purrr::map(header_tag_map, function(tags) {
    if(length(tags) > 1) c(tags[-1], tags[1]) else tags
  })

  # 6. Prepare representative cell positions for data blocks and attribute groups
  data_block_reps <- partial_dim_pairs %>%
    dplyr::distinct(.data$data_gid, row = .data$row_d, col = .data$col_d)

  # Here the data_gid representative (It reduces the data_gid cells to single cell per
  # row and col) gets computed
  data_block_reps <- dplyr::bind_rows(
    data_block_reps %>% dplyr::group_by(.data$row, .data$data_gid) %>%
      dplyr::summarise(col = min(.data$col), .groups = "drop"),
    data_block_reps %>% dplyr::group_by(.data$col, .data$data_gid) %>%
      dplyr::summarise(row = min(.data$row), .groups = "drop")
  )

  # Split data_block_reps by data_gid for easier access later
  data_block_reps <- split(data_block_reps, data_block_reps$data_gid)

  # It not like data_block_reps, but it is like actual attribute groups
  # retrieved again. Name may be misleading here.
  attr_group_reps <- partial_dim_pairs %>%
    dplyr::distinct(.data$attr_gid, row = .data$row_a, col = .data$col_a)
  # Split by attr_gid for easier access later
  attr_group_reps <-  split(attr_group_reps, attr_group_reps$attr_gid)

  # 7. Assign header_orientation_tag to each ambiguous group
  partial_dim_pairs_split <- split(
    partial_dim_pairs,
    # Not using dplyr::group_split as it is experimental as reported by dplyr
    # itself
    list(partial_dim_pairs$data_gid, partial_dim_pairs$attr_gid),
    drop = TRUE
  )
  partial_dim_pairs_annotated  <- partial_dim_pairs_split %>%
    purrr::map_dfr(function(group) {
      group %>%
        dplyr::mutate(
          header_orientation_tag = infer_header_orientation_tag_for_pair(
            data = group,
            data_block_reps = data_block_reps,
            attr_group_reps = attr_group_reps,
            direction_map = header_tag_map_mod
          )
        )
    })

  # 8. Combine and return
  out <- dplyr::bind_rows(full_dim_pairs, partial_dim_pairs_annotated)
  return(out)
}

# Helper: assign tag for a (data_gid, attr_gid) group
infer_header_orientation_tag_for_pair <- function(
    data, data_block_reps, attr_group_reps, direction_map) {
  direction <- data$direction[1]
  attr_id <- data$attr_gid[1]
  data_id <- data$data_gid[1]
  dirs <- direction_map[[direction]]
  attr_rep <- attr_group_reps[[attr_id]]

  # If attribute group is a single cell, use "direct"
  if(NROW(attr_rep) == 1) return("direct")

  # If multiple directions, pick best by HOT metric, else use only available
  # direction
  if(length(dirs) > 1) {
    data_rep <- data_block_reps[[data_id]]
    scores <- purrr::map_dbl(dirs, ~infer_get_HOT_metric(data_rep, attr_rep, .x))
    dirs[which.max(scores)][1]
  } else {
    # Use only available direction
    dirs[1]
  }
}

# Main function to infer header orientation tags
infer_attach_header_orientation_tag <- function(admap_cellwise) {

  # This function is used to attach header orientation tag to the attribute split.
  admap_cellwise <- dplyr::distinct(admap_cellwise)

  # Single Cell Attribute Handling
  admap_cellwise <- admap_cellwise %>%
    dplyr::group_by(.data$attr_gid) %>%
    # Count distinct row_a and col_a
    dplyr::mutate(n_rc = dplyr::n_distinct(.data$row_a, .data$col_a)) %>%
    dplyr::ungroup()

  # For single cell attributes, we can directly assign the header orientation tag
  out1 <- admap_cellwise %>%
    dplyr::filter(.data$n_rc == 1) %>%
    dplyr::mutate(
      header_orientation_tag = "direct",
      is_full_dim = FALSE  # Single cell attributes are not full dimensions
    ) %>%
    dplyr::select(-.data$n_rc)

  # For multi-cell attributes, we need to analyze further
  out2_part1 <- admap_cellwise %>%
    dplyr::filter(.data$n_rc != 1) %>%
    dplyr::select(-.data$n_rc)

  out2_part2 <- infer_dimension_analysis_details(out2_part1)

  out2 <- infer_get_header_orientation_tag(out2_part2)

  out <- out1 %>% dplyr::bind_rows(out2)

  return(out)
}
