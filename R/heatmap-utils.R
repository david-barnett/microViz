# helper for heatmap functions for number-drawing cell function
heatmapMakeCellFun <- function(numbers, numbers_mat) {
  mat <- numbers_mat
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
      val <- mat[i, j]
      if (!is.na(val)) {
        grid::grid.text(
          label = sprintf(numbers[["fmt"]], val),
          x = x, y = y, gp = numbers[["gp"]]
        )
      }
    }
  }
  return(cell_fun)
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
  prevalence <- apply(X = otu, MARGIN = 2, FUN = prev, undetected = undetected)
  return(prevalence)
}
#
#' Calculate prevalence from numeric vector
#'
#' Useful as helper for taxon prevalence calculation
#'
#' @param x numeric vector (of taxon counts or proportions)
#' @param undetected value above which a taxon is considered present or detected
#'
#' @return numeric value
#' @export
#'
#' @examples
#' prev(c(0, 0, 1, 2, 4))
#' prev(c(0, 0, 1, 2, 4), undetected = 1.5)
prev <- function(x, undetected = 0) {
  sum(x > undetected) / length(x)
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
    nm <- sapply(df, function(x) is.numeric(x) | is.logical(x) | is.integer(x))
    df <- df[, nm, drop = FALSE]
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
    if (inherits(trans_fun, "function")) {
      mat <- apply(mat, MARGIN = 2, FUN = trans_fun)
    } else if (inherits(trans_fun, "character")) {
      mat <- apply(
        X = mat, MARGIN = 2,
        FUN = function(x) do.call(what = trans_fun, args = list(x))
      )
    } else {
      stop("var transformation must be specified as a function or name of one")
    }
  }
  return(mat)
}
