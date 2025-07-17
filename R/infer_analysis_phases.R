


infer_analysis_phase_0_preprocess <- function(d) {

  # Check and halt if the input is not a valid cells object. Otherwise, proceed
  # with the classification
  if(isFALSE(core_cells_validation_end_use(d))){
    rlang::abort("Malformed Cells - Halted Execution! Please fix the issues.")
  }

  # Check PoA and PoV columns are present
  if (!utils::hasName(d, "PoA") || !utils::hasName(d, "PoV")) {
    rlang::abort(
      paste0(
        "The Cells-Values needs to be classified for value/attribute. ",
        "Please use `value_attribute_classify()` to classify the Cells first."
      )
    )
  }

  # Remove any NA cases
  d <- d[stats::complete.cases(d), ]

  # Probability of being Empty
  d$PoE <- 1 - (d$PoA + d$PoV)
  # Discard sure empty cells
  d <- d[d$PoE < 1 , ]

  # The list to return for next phase
  l <- list(orig = d)

  # Note: the term 'data' and 'value' are interchangeably used going forward.

  # Note: At a later stage, dynamic value–attribute classification can
  # potentially be introduced. This would involve, first, adjusting the
  # classification threshold; next, applying a complex method for
  # value–attribute classification with a dynamic thresholding approach; and
  # finally, using pattern-based clustering of blocks and direct cell-level
  # inspection to revise the PoA and PoV.
  l$data <- d[d$PoV >= 0.5, ] %>% core_as_rc_df()
  l$attr <- d[d$PoA >= 0.5, ] %>% core_as_rc_df()

  if (NROW(l$data) == 0) {
    rlang::abort("No `value` cells found! Nothing to do!")
  }

  if (NROW(l$attr) == 0) {
    rlang::abort("No `attribute` cells found! Nothing to do!")
  }

  return(l)
}


infer_analysis_phase_1_admap <- function(l0) {
  # Phase 1: Attribute Data Map (AdMap) Creation
  # This phase creates a mapping of attributes to their respective data blocks.

  # Rename the data and attribute groups for clarity
  d_dat <- l0$data %>% infer_table_blocks(group_id_tag = "d") %>%
    dplyr::rename(data_gid = .data$gid)

  d_att <- l0$attr %>% infer_table_blocks(group_id_tag = "a") %>%
    dplyr::rename(attr_gid = .data$gid)

  # Create the AdMap by matching attributes to data cells
  stage_1 <- infer_major_direction_stats(d_dat, d_att)

  d_dat <- stage_1$d_dat
  admap1 <- stage_1$ad_map

  # Remaining attributes that are not connected to any data
  admap1_2 <- infer_minor_direction_stats(d_dat, d_att, admap1)
  admap1$link_type <- "major"
  admap2 <- dplyr::bind_rows(admap1, admap1_2) %>% dplyr::distinct()

  # Return the final AdMap and the data/attribute blocks for next phases
  return(
    list(
      admap = admap2,
      d_dat = d_dat,
      d_att = d_att
    )
  )

}


infer_analysis_phase_2_attr_cluster <- function(l1) {

  # Here on-wards we take out implied_surrounding_blocks or info_blocks and
  # since that has both mapping and data attribute cells address (row, col) we
  # can use that to infer the clusters of attributes and data blocks.
  # impl_blocks or implied_surrounding_blocks or info_blocks are used
  # interchangeably.
  info_blocks <- infer_implied_surrounding_blocks(l1$admap, l1$d_dat, l1$d_att)

  attr_split <- infer_attr_split(info_blocks)

  # Create De-Normalized self contained map
  data_attr_cell_wise_map <- attr_split %>%
    dplyr::rename(
      attr_gid = .data$micro_attr_gids,
      row_a = .data$row,
      col_a = .data$col
    ) %>%
    dplyr::inner_join(
      l1$d_dat %>%
        dplyr::rename(
          row_d = .data$row,
          col_d = .data$col
        ),
      by = "data_gid")

  return(
    list(
      admap_cellwise = data_attr_cell_wise_map
    )
  )

}


infer_analysis_phase_3_header_orientation_tag <- function(l2){

  # HOT: Header Orientation Tag
  # This step adds two more columns "header_orientation_tag" "is_full_dim"
  with_HOT <- infer_attach_header_orientation_tag(l2$admap_cellwise)

  list(tagged = with_HOT)
}
