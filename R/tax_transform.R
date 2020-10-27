#' Transform taxa in phyloseq object and record transformation
#'
#' Pipe the results of tax_agg into this function to transform the (aggregated) taxa features. (Or pass it a phyloseq object).
#' This function is a simple wrapper around microbiome::transform can perform all the same transformations but returns a list, containing ps (the transformed phyloseq object), tax_transform (a string recording the transformation), and tax_level (a string recording the taxonomic aggregation level if specified in a preceding tax_agg function call.
#'
#' @param data list output from tax_agg, or a phyloseq object
#' @param transformation any valid taxa transformation from microbiome::transform
#' @param return choose which parts of list object to return
#'
#' @return list including phyloseq object and level argument value
#' @export
#'
#' @examples
#' library(microbiome)
#' data("dietswap", package = "microbiome")
#' tax_agg(ps = dietswap, agg_level = "none") %>% tax_transform("identity")
#' tax_agg(ps = dietswap, agg_level = "Phylum") %>% tax_transform("clr")
tax_transform <- function(data, transformation, return = "all") {

  # check input data object class
  if (inherits(data, "list")) {
    ps <- data[["ps"]]
    info <- data[["info"]]
  } else if (inherits(data, "phyloseq")) {
    ps <- data
    info <- list(tax_level = "not specified")
  } else {
    stop("data is wrong class, should be list output of tax_agg, or a phyloseq")
  }

  # transform phyloseq with microbiome::transform
  ps <- microbiome::transform(x = ps, transform = transformation, target = "OTU", scale = 1, shift = 0)

  # return list output
  info[["tax_transform"]] <- transformation
  out <- list(ps = ps, info = info)

  if (return == "all") {
    return(out)
  } else if (length(return) == 1) {
    return(out[[return]])
  } else {
    return(out[return])
  }
}
