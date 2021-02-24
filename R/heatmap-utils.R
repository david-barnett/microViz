#' make heatmap from a matrix and annotations
#'
#' Used inside cor_heatmap, comp_heatmap & tax_model_heatmap (will be)
#'
#' @param mat matrix for ComplexHeatmap
#' @param colors output of heat_colors() to set heatmap fill color scheme
#' @param numbers output of heat_numbers() to draw numbers on heatmap cells
#' @param seriation_method method to order the rows (in seriation::seriate)
#' @param seriation_dist distance to use in seriation_method (if needed)
#' @param seriation_method_col method to order the columns (in seriation::seriate)
#' @param seriation_dist_col distance to use in seriation_method_col (if needed)
#' @param right_annotation heatmap annotation for right hand side, e.g. taxAnnotation
#' @param ... extra args passed to ComplexHeatmap::Heatmap()
#' @noRd
viz_heatmap <- function(mat,
                        colors = heat_colors(),
                        numbers = heat_numbers(),
                        seriation_method = "OLO_ward",
                        seriation_dist = "euclidean",
                        seriation_method_col = seriation_method, seriation_dist_col = seriation_dist,
                        right_annotation = NULL,
                        ...) {
  dots <- list(...)
  # order matrix
  ser <- mat_seriate(mat = mat, method = seriation_method, dist = seriation_dist)

  # work in progress handling
  if (identical(numbers, NULL)){
    cell_fun = NULL
  } else if (inherits(numbers, "function")){
    cell_fun = numbers
    # set closure env's parent env to current env so cell_fun can find object mat!
    # ref https://bookdown.org/rdpeng/rprogdatascience/scoping-rules-of-r.html
    # ref https://adv-r.hadley.nz/function-factories.html
    parent.env(environment(cell_fun)) <- environment()
  } else {
    cell_fun <- switch(
      EXPR = numbers,
      "values" = {
        function(j, i, x, y, width, height, fill) {
          val <- mat[i, j]
          grid::grid.text(label = sprintf("%.1f", val), x = x, y = y, gp = grid::gpar(fontsize = 7))
        }
      }
      # TODO support compositions by transforming mat
    )
  }
  args <- list(
    matrix = mat, name = "Value", col = colors,
    right_annotation = right_annotation,
    row_order = ser$row_order,
    cluster_rows = ser$row_tree,
    column_order = ser$col_order,
    cluster_columns = ser$col_tree,
    column_names_rot = 45,
    column_dend_side = "bottom",
    column_names_side = "top",
    row_names_gp = grid::gpar(fontsize = 8),
    column_names_gp = grid::gpar(fontsize = 8),
    rect_gp = grid::gpar(col = "white", lwd = 0.75),
    cell_fun = cell_fun
  )
  args[names(dots)] <- dots

  p <- do.call(ComplexHeatmap::Heatmap, args = args)
  return(p)
}


#' @param data phyloseq or ps_Extra
#' @param taxa selection vector of taxa (names, numbers or logical)
#' @param undetected the value above which taxa are classed as detected/present in a sample
#
#' @return named vector of taxa prevalence values
#' @noRd
prev_calc <- function(data, taxa, undetected = 0) {
  ps <- ps_get(data)
  otu <- otu_get(data)
  otu <- otu[, taxa, drop = FALSE]
  prevalence <- apply(otu, MARGIN = 2, function(x) sum(x > undetected, na.rm = TRUE)) / phyloseq::nsamples(ps)
  return(prevalence)
}

#' @param data phyloseq or ps_Extra
#' @param taxa selection vector of taxa (names, numbers or logical)
#' @param undetected the value above which taxa are classed as detected/present in a sample
#
#' @return matrix of tax abundance values (and NaNs)
#' @noRd
abund_calc <- function(data, taxa, undetected = 0) {
  ps <- ps_get(data)
  totals <- phyloseq::sample_sums(ps)
  prop_threshold <- undetected / totals
  otu <- otu_get(data)[, taxa, drop = FALSE]
  props <- apply(otu, MARGIN = 2, function(x) x / totals)
  props <- apply(props, MARGIN = 2, function(x) ifelse(test = x > prop_threshold, yes = x, no = NaN))
  return(props)
}



