#' @param mat numeric matrix
#' @param method method for row seriation
#' @param dist distance for row seriation method
#' @param col_method method for column seriation
#' @param col_dist distance for column seriation method
#'
#' @return list with numeric vectors of row and column orders and trees if appropriate (or FALSEs)
#' @noRd
mat_seriate <- function(mat, method, dist, col_method = method, col_dist = dist) {
  if (identical(method, col_method) && method %in% seriation::list_seriation_methods(kind = "matrix")) {
    ser <- seriation::seriate(mat, method = method)
    row_order <- seriation::get_order(ser, dim = 1)
    col_order <- seriation::get_order(ser, dim = 2)
    row_tree <- col_tree <- FALSE
  } else if (method %in% seriation::list_seriation_methods(kind = "dist")) {
    row_ser <- mat_ser_dist(mat, method = method, dist = dist)
    col_ser <- mat_ser_dist(t(mat), method = method, dist = dist)
    row_order <- seriation::get_order(row_ser)
    col_order <- seriation::get_order(col_ser)
    row_tree <- if (inherits(row_ser[[1]], "hclust")) {
      stats::as.dendrogram(row_ser[[1]])
    } else {
      FALSE
    }
    col_tree <- if (inherits(col_ser[[1]], "hclust")) {
      stats::as.dendrogram(col_ser[[1]])
    } else {
      FALSE
    }
  } else {
    stop(
      method, " is not a valid method in seriation::seriate! See seriation::list_seriation_methods()\n",
      "Nearest match is: ",
      agrep(method, seriation::list_seriation_methods(), value = TRUE, ignore.case = TRUE)[[1]]
    )
  }

  return(list(row_order = row_order, row_tree = row_tree, col_order = col_order, col_tree = col_tree))
}

#' @param mat numeric matrix
#' @param method method in seriation::list_seriation_methods(kind = "dist")
#' @param dist distance method in stats dist or
#' @param ... passed to stats::dist or phyloseq::distance
#'
#' @return seriation object
#' @noRd
mat_ser_dist <- function(mat, method, dist, ...) {
  stopifnot(method %in% seriation::list_seriation_methods(kind = "dist"))

  if (dist %in% c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")) {
    dists <- stats::dist(mat, method = dist, ...)
  } else if (inherits(mat, "otu_table") && dist %in% phyloseq::distanceMethodList) {
    dists <- phyloseq::distance(physeq = mat, method = dist, type = "samples", ...)
  }
  ser <- seriation::seriate(dists, method = method)
  return(ser)
}
