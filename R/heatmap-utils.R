#' make heatmap from a matrix and annotations
#'
#' Used inside cor_heatmap and comp_heatmap & will be inside tax_model_heatmap
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
  # compute order and any h clustering trees for matrix rows and columns
  ser <- mat_seriate(
    mat = mat, method = seriation_method, dist = seriation_dist,
    col_method = seriation_method_col, col_dist = seriation_dist_col
  )

  # remove taxa_are_rows attr. which can remain from comp_heatmap otu_table mat
  # (caused warning when Heatmap sets class(mat))
  mat <- methods::as(mat, Class = "matrix")
  numbers_mat <- methods::as(numbers_mat, Class = "matrix")

  # cell_fun function for drawing numbers on heatmap cells
  if (identical(numbers, NULL)) {
    cell_fun <- NULL
  } else if (inherits(numbers, "function")) {
    cell_fun <- numbers
    parent.env(environment(cell_fun)) <- environment()
    # set closure env's parent env to current env so cell_fun can find object mat!
    # ref https://bookdown.org/rdpeng/rprogdatascience/scoping-rules-of-r.html
    # ref https://adv-r.hadley.nz/function-factories.html
  } else if (inherits(numbers, "list")) {
    cell_fun <- function(j, i, x, y, width, height, fill) {
      val <- numbers_mat[i, j]
      if (!is.na(val)) {
        grid::grid.text(
          label = sprintf(numbers[["fmt"]], val),
          x = x, y = y, gp = numbers[["gp"]]
        )
      }
    }
  }

  # getting colour range from data if necessary
  if ("range" %in% methods::formalArgs(def = colors)) {
    colors <- colors(range = range(mat, na.rm = TRUE, finite = TRUE))
  }

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

# trans_fun is a function to be applied (columns) to matrix before returning (if not NA)
# used inside cor_heatmap, where var_fun arg only allows character naming a function
df_to_numeric_matrix <- function(df, vars = NA, trans_fun = NA) {
  if (inherits(df, "matrix")) {
    stopifnot(storage.mode(df) %in% c("double", "integer", "logical"))
    mat <- df
  } else {
    df <- df[, sapply(df, function(x) is.numeric(x) | is.logical(x) | is.integer(x)), drop = FALSE]
    mat <- as.matrix.data.frame(df)
  }
  num_vars <- colnames(mat)
  if (length(num_vars) == 0) stop("no numeric/integer/logical variables found")
  if (!identical(vars, NA)) {
    stopifnot(is.character(vars))
    if (all(vars %in% num_vars)) {
      mat <- mat[, vars, drop = FALSE]
    } else {
      stop(
        paste(vars[!vars %in% num_vars], collapse = " "),
        " is/are not valid variable names in the (sample_) data\n",
        "Possible numeric/integer/logical variables include:\n",
        paste(utils::head(x = num_vars, n = 10), collapse = " ")
      )
    }
  }
  # apply transformation function to matrix columns?
  if (!identical(trans_fun, NA)) {
    if (inherits(trans_fun, "function")) mat <- apply(mat, MARGIN = 2, FUN = trans_fun)
    if (inherits(trans_fun, "character")) mat <- apply(mat, MARGIN = 2, FUN = function(x) do.call(what = trans_fun, args = list(x)))
  }
  return(mat)
}
