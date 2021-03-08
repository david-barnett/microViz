#' make heatmap from a matrix and annotations
#'
#' Used inside cor_heatmap, comp_heatmap & tax_model_heatmap (will be)
#'
#' @param mat matrix for ComplexHeatmap
#' @param colors output of heat_palette() to set heatmap fill color scheme
#' @param numbers output of heat_numbers() to draw numbers on heatmap cells
#' @param seriation_method method to order the rows (in seriation::seriate)
#' @param seriation_dist distance to use in seriation_method (if needed)
#' @param seriation_method_col method to order the columns (in seriation::seriate)
#' @param seriation_dist_col distance to use in seriation_method_col (if needed)
#' @param right_annotation heatmap annotation for right hand side, e.g. taxAnnotation
#' @param ... extra args passed to ComplexHeatmap::Heatmap()
#' @noRd
viz_heatmap <- function(mat, # used for seriation and colours
                        numbers_mat, # used only for numbers
                        name = "value",
                        colors = heat_palette(),
                        numbers = heat_numbers(),
                        seriation_method = "OLO_ward",
                        seriation_dist = "euclidean",
                        seriation_method_col = seriation_method,
                        seriation_dist_col = seriation_dist,
                        right_annotation = NULL,
                        ...) {
  stopifnot(inherits(mat, "matrix") && storage.mode(mat) %in% c("double", "integer"))
  stopifnot(inherits(numbers_mat, "matrix") && storage.mode(numbers_mat) %in% c("double", "integer"))

  dots <- list(...)
  # order matrix
  ser <- mat_seriate(mat = mat, method = seriation_method, dist = seriation_dist)

  # work in progress handling
  if (identical(numbers, NULL)) {
    cell_fun <- NULL
  } else if (inherits(numbers, "function")) {
    cell_fun <- numbers
    # set closure env's parent env to current env so cell_fun can find object mat!
    # ref https://bookdown.org/rdpeng/rprogdatascience/scoping-rules-of-r.html
    # ref https://adv-r.hadley.nz/function-factories.html
    parent.env(environment(cell_fun)) <- environment()
  } else if (inherits(numbers, "list")) {
    cell_fun <- function(j, i, x, y, width, height, fill) {
      val <- numbers_mat[i, j]
      if (!is.na(val)) grid::grid.text(label = sprintf(numbers[["fmt"]], val), x = x, y = y, gp = numbers[["gp"]])
    }
  }

  # getting colour range from data if necessary
  if (inherits(colors, "function")) colors <- colors(range = range(mat, na.rm = TRUE, finite = TRUE))

  args <- list(
    matrix = mat,
    name = name,
    col = colors,
    right_annotation = right_annotation,
    row_order = ser$row_order,
    cluster_rows = ser$row_tree,
    column_order = ser$col_order,
    cluster_columns = ser$col_tree,
    cell_fun = cell_fun,
    column_names_gp = grid::gpar(fontsize = 7),
    row_names_gp = grid::gpar(fontsize = 7)
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
