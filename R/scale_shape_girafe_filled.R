#' Filled shapes for ggiraph interactive plots
#'
#' @description
#' Generates a custom ggplot2 shape scale, as used in ord_explore's ordination.
#' Uses filled shapes, therefore fill aesthetic must be set, in addition to
#' colour, to have filled shapes.
#' Points with NA values for the shape variable are shown as hollow circles.
#'
#' @details
#' Composite shapes e.g. number 7 "square cross" cause ggiraph interactive
#' plots to fail when a variable shape and tooltip is set.
#'
#' Shapes used are, in order: "circle filled", "triangle filled",
#' "square filled", "diamond filled", and "triangle down filled"
#'
#' @return ggplot2 Scale object
#' @export
#'
#' @examples
#' corncob::ibd_phylo %>%
#'   tax_fix() %>%
#'   phyloseq_validate() %>%
#'   tax_transform(rank = "Genus", trans = "clr") %>%
#'   ord_calc(
#'     method = "PCA"
#'   ) %>%
#'   ord_plot(
#'     axes = c(1, 2),
#'     plot_taxa = 1:6,
#'     colour = "DiseaseState", fill = "DiseaseState",
#'     shape = "circle", alpha = 0.5,
#'     size = 3
#'   ) +
#'   scale_shape_girafe_filled()
scale_shape_girafe_filled <- function() {
  ggplot2::scale_shape_manual(
    values = filled_shapes(),
    na.translate = TRUE, na.value = "circle open"
  )
}

# helper for scale_shape_girafe_filled and also taxatree_plot_sig()
filled_shapes <- function() {
  c(
    "circle filled", "triangle filled", "square filled",
    "diamond filled", "triangle down filled"
  )
}
