#' Print names of "top" n taxa
#'
#' Simple wrapper function that:
#' 1. optionally aggregates taxa at `rank`
#' 2. sorts the aggregated taxa according to `by`
#' 3. returns the top `n` number of taxa names
#'
#' @seealso \code{\link{tax_agg}} for more info on taxonomic aggregation
#' @seealso \code{\link{tax_sort}} for more info on sorting taxa
#'
#' @param data phyloseq object or ps_extra
#' @param n how many taxa names to return, or NA for all
#' (can return fewer than n values, if there are fewer to return)
#' @param by how to sort taxa (see `?tax_sort()`),
#' defaults to `sum` which sorts by total abundance across all samples
#' @param rank taxonomic rank to aggregate at before calculating
#' ("unique" = no aggregation)
#' @param ... extra optional args passed to tax_sort
#'
#' @return vector of taxa names at chosen rank
#' @export
#'
#' @examples
#' data("dietswap", package = "microbiome")
#' tax_top(dietswap)
#' tax_top(dietswap, n = 4, by = "prev", rank = "Phylum", undetected = 30)
tax_top <- function(data, n = 10, by = sum, rank = "unique", ...) {
  ps <- ps_get(data)
  ps <- tax_agg(ps, rank = rank)[["ps"]]
  ps <- tax_sort(ps, by = by, ...)
  taxnames <- phyloseq::taxa_names(physeq = ps)
  if (identical(n, NA)) {
    return(taxnames)
  } else {
    return(utils::head(x = taxnames, n = n))
  }
}