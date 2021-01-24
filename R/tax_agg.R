#' Aggregate taxa if requested
#'
#' This function is a simple wrapper around microbiome::aggregate_taxa.
#' tax_agg allows you to pass 'none', 'asv', or 'given', to the level argument which will NOT aggregate the taxa.
#' If you pass any valid rank name it will aggregate taxa with microbiome::aggregate_taxa.
#'
#' @param ps phyloseq object
#' @param agg_level "none", "asv", "given" or name of valid taxonomic rank (try phyloseq::rank_names(ps))
#' @param return choose which parts of list object to return
#'
#' @return list including phyloseq object and (tax_)level argument value
#' @export
#'
#' @examples
#' library(microbiome)
#' data("dietswap", package = "microbiome")
#' tax_agg(ps = dietswap, agg_level = "none")
#' tax_agg(ps = dietswap, agg_level = "Phylum")
tax_agg <- function(ps, agg_level, return = "all") {
  if (!agg_level %in% c("none", "asv", "given")) {
    ps <- microbiome::aggregate_taxa(x = ps, level = agg_level)
  }

  # return list output
  info <- list(tax_level = agg_level)

  out <- list(ps = ps, info = info)
  if (identical(return, "all")) {
    return(out)
  } else if (length(return) == 1) {
    return(out[[return]])
  } else {
    return(out[return])
  }
}
