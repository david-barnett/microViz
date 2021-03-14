#' Transform taxa in phyloseq object and record transformation
#'
#' Transform taxa features, and optionally aggregate at specified taxonomic rank beforehand.
#' You can pipe the results of tax_agg into tax_transform, or equivalently set the rank argument in tax_transform.
#' This function is a wrapper around microbiome::transform() that can perform all the same transformations but returns a ps_extra list
#' containing ps (the transformed phyloseq object) and extra info: tax_transform (a string recording the transformation), and tax_agg
#' (a string recording the taxonomic aggregation rank if specified here or earlier in tax_agg).
#'
#' @param data ps_extra list output from tax_agg, or a phyloseq object
#' @param transformation any valid taxa transformation from microbiome::transform
#' @param rank If data is phyloseq data are aggregated at this rank before transforming. If NA, runs tax_agg(data, rank = NA). However if rank is NA and data is already ps_extra, this does nothing.
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
#' tax_agg(ps = dietswap, rank = "Phylum") %>% tax_transform("clr")
#' # equivalent shortcut
#' tax_transform(dietswap, transformation = "clr", rank = "Phylum")
#'
#' # do nothing except record tax_agg as "unique" and tax_transform as "identity" in ps_extra info
#' dietswap %>% tax_transform("identity", rank = NA)
tax_transform <- function(data, transformation, rank = NA, ...) {

  # check input data object class and aggregate and set ps_extra info
  if (inherits(data, "ps_extra")) {
    if (!identical(rank, NA)){
      data <- tax_agg(ps = ps_get(data), rank = rank)
    }
    ps <- ps_get(data)
    info <- info_get(data)
    if (!is.na(info[["tax_transform"]])){
      warning("data were already transformed by: ", info[["tax_transform"]])
    }
    info[["tax_transform"]] <- transformation
  } else if (methods::is(data, "phyloseq")) {
    if (identical(rank, NA)) rank <- "unique"
    ps <- ps_get(tax_agg(data, rank = rank))
    info <- new_ps_extra_info(tax_transform = transformation, tax_agg = rank)
  } else {
    stop("data is wrong class, should be ps_extra from tax_agg, or a phyloseq")
  }

  # transform phyloseq with microbiome::transform
  ps <- microbiome::transform(
    x = ps, transform = transformation, target = "OTU", ...
  )
  new_ps_extra(ps = ps, info = info)
}
