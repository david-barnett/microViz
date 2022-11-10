#' Get names of "top" n taxa
#'
#' Simple wrapper around tax_sort that:
#' 1. optionally aggregates taxa at `rank`
#' 2. sorts the aggregated taxa according to `by`
#' 3. returns the top `n` number of taxa names
#'
#' @seealso \code{\link{tax_agg}} for more info on taxonomic aggregation
#' @seealso \code{\link{tax_sort}} for more info on sorting taxa
#'
#' @param data phyloseq object or psExtra
#' @param n how many taxa names to return, or NA for all
#' (can return fewer than n values, if there are fewer to return)
#' @param by how to sort taxa (see `?tax_sort()`),
#' defaults to `sum` which sorts by total abundance across all samples
#' @param rank taxonomic rank to aggregate at before calculating
#' ("unique" = no aggregation)
#'
#' @inheritParams tax_sort
#' @inheritDotParams tax_sort verbose trans
#'
#' @return vector of taxa names at chosen rank
#' @export
#'
#' @examples
#' data("dietswap", package = "microbiome")
#' tax_top(dietswap)
#' tax_top(dietswap, n = 4, by = "prev", rank = "Phylum", undetected = 30)
tax_top <- function(data,
                    n = 10,
                    by = sum,
                    rank = "unique",
                    use_counts = FALSE,
                    ...) {
  if (!rlang::is_na(n) && (!rlang::is_scalar_integerish(n) || n < 1)) {
    stop("`n` must be a single number, greater than zero")
  }
  ps <- ps_get(data, counts = use_counts, warn = "error")
  ps <- tax_agg(ps, rank = rank) %>% ps_get()
  sortArgs <- list(
    data = ps, use_counts = use_counts, by = by, at = "names", tree_warn = FALSE
  )
  ps <- tax_sort(
    data = ps, use_counts = use_counts,
    by = by, at = "names", tree_warn = FALSE, ...
  )
  taxnames <- phyloseq::taxa_names(physeq = ps)
  if (is.na(n) || n > length(taxnames)) n <- length(taxnames)
  return(taxnames[seq_len(n)])
}
