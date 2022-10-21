#' Add rank prefixes to phyloseq tax_table values
#'
#' Prepend the start of rank names to each taxon at each rank
#' (useful particularly in case of duplicated taxa names across ranks, e.g. dietswap dataset)
#'
#' @param ps phyloseq object
#' @param sep characters to paste in between rank initial and taxon name
#' @param nchar number of characters to use from start of rank_names
#'
#' @return phyloseq
#' @export
#'
#' @seealso \code{\link{tax_fix}} for fixing other tax_table problems
#'
#' @examples
#' data("dietswap", package = "microbiome")
#' phyloseq::tax_table(dietswap) %>% head()
#' dietswap %>%
#'   tax_prepend_ranks() %>%
#'   phyloseq::tax_table() %>%
#'   head()
tax_prepend_ranks <- function(ps, sep = ": ", nchar = 1) {
  ps <- ps_get(ps)
  ntax <- phyloseq::ntaxa(ps)
  tt <- unclass(phyloseq::tax_table(ps))
  tt <- vapply(
    X = phyloseq::rank_names(ps),
    FUN.VALUE = character(length = ntax),
    FUN = function(r) {
      taxa <- tt[, r]
      rank_stub <- substr(r, start = 1, stop = nchar)
      paste0(rank_stub, sep, taxa)
    }
  )
  rownames(tt) <- phyloseq::taxa_names(ps)
  phyloseq::tax_table(ps) <- tt
  return(ps)
}
