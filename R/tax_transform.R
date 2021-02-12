#' Transform taxa in phyloseq object and record transformation
#'
#' Pipe the results of tax_agg into this function to transform the (aggregated) taxa features. (Or pass it a phyloseq object).
#' This function is a simple wrapper around microbiome::transform can perform all the same transformations but returns a list, containing ps (the transformed phyloseq object), tax_transform (a string recording the transformation), and tax_level (a string recording the taxonomic aggregation level if specified in a preceding tax_agg function call.
#'
#' @param data list output from tax_agg, or a phyloseq object
#' @param transformation any valid taxa transformation from microbiome::transform
#' @param ... any extra arguments passed to microbiome::transform()
#'
#' @return list including phyloseq object and level argument value
#' @export
#'
#' @examples
#' library(microbiome)
#' data("dietswap", package = "microbiome")
#' tax_agg(ps = dietswap, agg_level = "none") %>% tax_transform("identity")
#' tax_agg(ps = dietswap, agg_level = "Phylum") %>% tax_transform("clr")
tax_transform <- function(data, transformation, ...) {

  # check input data object class
  if (inherits(data, "ps_extra")) {
    ps <- ps_get(data)
    info <- info_get(data)
    info[["tax_transform"]] <- transformation
  } else if (methods::is(data, "phyloseq")) {
    ps <- data
    info <- new_ps_extra_info(tax_transform = transformation)
  } else {
    stop("data is wrong class, should be ps_extra output of tax_agg, or a phyloseq")
  }

  # transform phyloseq with microbiome::transform
  ps <- microbiome::transform(x = ps, transform = transformation, target = "OTU", ...)
  new_ps_extra(ps = ps, info = info)

}
