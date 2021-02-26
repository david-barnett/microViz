#' Transform taxa in phyloseq object and record transformation
#'
#' Pipe the results of tax_agg into this function to transform the (aggregated) taxa features. (Or pass it a phyloseq object).
#' This function is a simple wrapper around microbiome::transform() that can perform all the same transformations but returns a ps_extra list
#' containing ps (the transformed phyloseq object) and extra info: tax_transform (a string recording the transformation), and tax_agg
#' (a string recording the taxonomic aggregation level if specified in a preceding tax_agg function call).
#'
#' @param data ps_extra list output from tax_agg, or a phyloseq object
#' @param transformation any valid taxa transformation from microbiome::transform
#' @param ... any extra arguments passed to microbiome::transform()
#'
#' @return ps_extra list including phyloseq object and info
#' @export
#'
#' @examples
#' library(microbiome)
#' data("dietswap", package = "microbiome")
#'
#' # aggregate taxa at Phylum level and perform the center log ratio transform on the phyla counts
#' tax_agg(ps = dietswap, agg_level = "Phylum") %>% tax_transform("clr")
#'
#' # do nothing except record tax_agg as "none" and tax_transform as "identity" in ps_extra info
#' tax_agg(ps = dietswap, agg_level = "none") %>% tax_transform("identity")
tax_transform <- function(data, transformation, ...) {

  ps <- ps_get(data)

  # check input data object class
  if (inherits(data, "ps_extra")) {
    info <- info_get(data)
    if (!is.na(info[["tax_transform"]])) warning("data were already transformed: ", info[["tax_transform"]])
    info[["tax_transform"]] <- transformation
  } else if (methods::is(data, "phyloseq")) {
    info <- new_ps_extra_info(tax_transform = transformation)
  } else {
    stop("data is wrong class, should be ps_extra output of tax_agg, or a phyloseq")
  }

  # transform phyloseq with microbiome::transform
  ps <- microbiome::transform(x = ps, transform = transformation, target = "OTU", ...)
  new_ps_extra(ps = ps, info = info)
}
