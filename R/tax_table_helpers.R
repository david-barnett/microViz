#' @name tax_table_helpers
#' @title Fixing duplication problems in phyloseq tax_table
#' @description
#' - `tax_fill_unknowns` replaces any specified unknown names with a fully unique name made by pasting together all the higher taxonomic levels and the current rank
#' - `taxon_fill_unknowns` is an internal helper function for tax_fill_unknowns (so don't use this directly!)
#' - `prepend_ranks` is a helper function to prepend the start of rank name to each taxon at each rank (useful particularly in case of duplicated taxa names across ranks, e.g. dietswap dataset)
#'
#' @details
#' `tax_fill_unknowns` supports parallel processing with future package, which might be useful if tax_table(ps) is massive (1000s rows)
#' e.g. future::plan("future::multisession", workers = 3)
#'
#' @param ps phyloseq object
#' @param unknowns character vector of names that should be 'fixed' e.g. c("", "unknown")
#' @param taxon_row a row of a phyloseq tax_table object
#' @param sep for prepend_ranks (what to paste in between rank initial and taxon name)
#' @param nchar number of characters to use from rank_names
#'
#' @return a modified phyloseq (or a 1-row matrix for `taxon_fill_unknowns`)
#'
#' @examples
#' library(dplyr)
#' library(microbiome)
#' data(dietswap)
#' ps <- dietswap
#'
#' phyloseq::tax_table(ps) %>% head()
#'
#' # test non-unique genera problem handling
#' phyloseq::tax_table(ps)[c(3, 4, 56), "Genus"] <- "unknown"
#' phyloseq::tax_table(ps) %>% head()
#'
#' # supports parallel processing with future package
#' # useful if tax_table(ps) is massive (1000s rows)
#' # e.g. future::plan("future::multisession", workers = 3)
#'
#' ps <- tax_fill_unknowns(ps, unknowns = "unknown")
#'
#' # don't forget to close any parallel sessions
#' # future::plan("future::sequential")
#'
#' phyloseq::tax_table(ps) %>% head()
#'
#' ps %>%
#'   prepend_ranks() %>%
#'   tax_table() %>%
#'   head()
#'
#' # end examples
#' @rdname tax_table_helpers
#' @export
tax_fill_unknowns <- function(ps, unknowns) {
  taxtab <- phyloseq::tax_table(ps)
  l <- furrr::future_map(
    .x = 1:nrow(taxtab),
    .f = function(taxon) {
      taxon_fill_unknowns(
        taxon_row = taxtab[taxon, ],
        unknowns = unknowns
      )
    },
    .options = furrr::furrr_options(packages = "phyloseq")
  )
  mat <- purrr::reduce(l, rbind)
  rownames(mat) <- rownames(taxtab)
  colnames(mat) <- colnames(taxtab)
  phyloseq::tax_table(ps) <- mat
  return(ps)
}

#' @rdname tax_table_helpers
# internal helper function for each taxon (as a row)
taxon_fill_unknowns <- function(taxon_row, unknowns) {
  t_vec <- purrr::pmap_chr(list(
    nam = rev(as.vector(taxon_row)),
    num = rev(seq_along(taxon_row)),
    rank = rev(colnames(taxon_row))
  ),
  .f = function(nam, num, rank) {
    if (is.na(nam) || nam %in% unknowns) {
      prepend <- paste(as.vector(taxon_row)[1:(num - 1)], collapse = ".")
      long_nam <- paste(prepend, rank, sep = ".")
      return(long_nam)
    } else {
      return(nam)
    }
  }
  )
  matrix(rev(t_vec), nrow = 1)
}

#' @rdname tax_table_helpers
#' @export
prepend_ranks <- function(ps, sep = ": ", nchar = 1) {
  tt <- sapply(phyloseq::rank_names(ps), function(r) {
    taxa <- as.vector(phyloseq::tax_table(ps)[, r])
    rank_stub <- substr(r, start = 1, stop = nchar)
    paste0(rank_stub, sep, taxa)
  })
  rownames(tt) <- phyloseq::taxa_names(ps)
  phyloseq::tax_table(ps) <- tt
  return(ps)
}
