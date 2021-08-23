#' @param mat numeric matrix
#' @param method method for row seriation
#' @param dist distance for row seriation method
#' @param col_method method for column seriation
#' @param col_dist distance for column seriation method
#'
#' @return list with numeric vectors of row and column orders and trees if appropriate (or FALSEs)
#' @noRd
mat_seriate <- function(mat,
                        method,
                        dist,
                        col_method = method,
                        col_dist = dist) {
  rowInfo <- rowSeriationInfo(mat, method = method, dist = dist)
  colInfo <- rowSeriationInfo(t(mat), method = col_method, dist = col_dist)

  return(list(
    row_order = rowInfo$order, row_tree = rowInfo$tree,
    col_order = colInfo$order, col_tree = colInfo$tree
  ))
}

# get list including seriation object, order from that object, and any tree
rowSeriationInfo <- function(mat, method, dist) {

  # get lists of possible methods
  matrixMethods <- seriation::list_seriation_methods(kind = "matrix")
  distanceMethods <- seriation::list_seriation_methods(kind = "dist")
  allMethods <- union(x = matrixMethods, y = distanceMethods)

  # assumes no hclust tree unless later overwritten
  tree <- FALSE

  if (method %in% matrixMethods) {
    ser <- seriation::seriate(mat, method = method)
    order <- seriation::get_order(ser, dim = 1)
  } else if (method %in% distanceMethods) {
    ser <- mat_seriate_dist(mat, method = method, dist = dist)
    order <- seriation::get_order(ser)
    # get hclust tree if present
    if (inherits(ser[[1]], "hclust")) tree <- stats::as.dendrogram(ser[[1]])
  } else {
    stop(
      call. = FALSE,
      method, " method is not in `seriation::list_seriation_methods()`",
      "\nNearest match is: ",
      agrep(method, x = allMethods, value = TRUE, ignore.case = TRUE)[[1]]
    )
  }
  return(list(ser = ser, order = order, tree = tree))
}

#' Distance-based method seriation of rows in a numeric matrix
#'
#' @param mat numeric matrix
#' @param method method in seriation::list_seriation_methods(kind = "dist")
#' @param dist distance method in stats::dist or phyloseq::distance
#' @param ... passed to stats::dist or phyloseq::distance
#'
#' @return seriation object
#' @noRd
mat_seriate_dist <- function(mat, method, dist, ...) {
  if (dist %in% c(
    "euclidean", "maximum", "manhattan",
    "canberra", "binary", "minkowski"
  )) {
    dists <- stats::dist(mat, method = dist, ...)
  } else if (dist %in% unlist(phyloseq::distanceMethodList)) {
    if (inherits(mat, "otu_table")) {
      dists <- phyloseq::distance(
        physeq = mat, method = dist, type = "samples", ...
      )
    } else {
      stop(
        call. = FALSE,
        "matrix must be an otu_table class object, because\n'",
        dist, "' is a distance in phyloseq::distanceMethodList",
        "\nmatrix is class: ", paste(class(mat), collapse = " ")
      )
    }
  } else {
    stop(
      call. = FALSE,
      "distance must be valid for stats::dist or phyloseq::distance",
      "\n- distance you requested was: ", dist
    )
  }

  # seriate and return seriation object
  ser <- seriation::seriate(dists, method = method)
  return(ser)
}
