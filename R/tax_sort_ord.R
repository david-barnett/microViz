#' @title
#' Order taxa in phyloseq by their loading vectors
#'
#' @description
#' `tax_sort_ord` reorders taxa in a phyloseq object based on the relative
#' length of their taxa scores / "loading" vector lengths on 1 or 2 ordination axes.
#'
#' `ord_order_taxa` gets the taxa names in order from the ordination
#' contained in a psExtra object. This is used internally by `tax_sort_ord`.
#'
#' @seealso
#' - These functions were created to support ordering of taxa bars on `ord_plot_iris`
#' - `ps_sort_ord` for ordering samples in phyloseq by ordination
#'
#' @export
#' @rdname ordination-sorting-taxa
#' @inheritParams ps_sort_ord
#' @inheritParams ord_plot
tax_sort_ord <- function(ps, ord, axes = 1:2, scaling = 2) {

  # get taxa order character vector
  taxaInOrder <- ord_order_taxa(ord = ord, axes = axes, scaling = scaling)

  # check taxa names same in ps and ord (e.g. not wrong rank used)
  stopifnot(all(taxaInOrder %in% phyloseq::taxa_names(ps)))

  # sort taxa in phyloseq in this order
  ps <- tax_reorder(ps = ps, tax_order = taxaInOrder, tree_warn = TRUE)

  return(ps)
}

#' @rdname ordination-sorting-taxa
ord_order_taxa <- function(ord, axes = 1:2, scaling = 2) {
  ord <- ord_get(ord)

  # get species scores
  scores <- vegan::scores(ord, choices = axes, scaling = scaling, display = "sp")

  # get length (euclidean norm or "2-norm") of each species score vector
  # ref: https://en.wikipedia.org/wiki/Norm_(mathematics) # Euclidean norm
  vecLengths <- apply(scores, MARGIN = 1, FUN = norm, type = "2")

  # get desired order
  taxaInOrder <- names(sort(vecLengths, decreasing = TRUE))

  return(taxaInOrder)
}
