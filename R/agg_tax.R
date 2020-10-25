#' Aggregate taxa if requested
#'
#' This function is a simple wrapper around microbiome::aggregate_taxa.
#' agg_tax allows you to pass NULL, 'given' or 'asv', to the level argument which will NOT aggregate the taxa.
#' If you pass any valid rank name it will aggregate taxa with microbiome::aggregate_taxa.
#'
#' @param ps phyloseq object
#' @param level NULL, 'given' or name of valid taxonomic rank (try phyloseq::rank_names(ps))
#' @param return choose which parts of list object to return
#'
#' @return list including phyloseq object and level argument value
#' @export
#'
#' @examples
#' library(microbiome)
#' data("dietswap", package = "microbiome")
#' agg_tax(ps = dietswap, level = NULL)
#' agg_tax(ps = dietswap, level = "Phylum")
agg_tax <- function(ps, level, return = 'all') {
  if (!is.null(level) && !level %in% c("asv", "given")) {
    ps <- microbiome::aggregate_taxa(x = ps, level = level)
  }

  # return list output
  out <- list(ps = ps, tax_level = level)
  if (return == "all") {
    return(out)
  } else if (length(return) == 1) {
    return(out[[return]])
  } else {
    return(out[return])
  }
}
