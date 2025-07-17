

analyze_cells <- function(d) {

  # Pre-processing
  l0 <- infer_analysis_phase_0_preprocess(d)

  # Fundamental Attr-Data Map
  l1 <- infer_analysis_phase_1_admap(l0)

  # Split/Regroup of Form Cluster of Attributes
  l2 <- infer_analysis_phase_2_attr_cluster(l1)

  # Attach Header Orientation Tags
  l3 <- infer_analysis_phase_3_header_orientation_tag(l2)

  l3$tagged
}
