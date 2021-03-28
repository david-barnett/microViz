#' Transform taxa in phyloseq object and record transformation
#'
#' Transform taxa features, and optionally aggregate at specified taxonomic rank beforehand.
#' You can pipe the results of tax_agg into tax_transform, or equivalently set the rank argument in tax_transform.
#' This function uses microbiome::transform() internally and can perform the same transformations but returns a ps_extra list
#' containing ps (the transformed phyloseq object) and extra info used for annotating ord_plot() plots:
#' tax_transform (a string recording the transformation), and tax_agg
#' (a string recording the taxonomic aggregation rank if specified here or earlier in tax_agg).
#' See details for notes about some of the available transformations.
#'
#' @details
#' - transformation = "clr" performs the centered log ratio transformation using microbiome::transform(), after adding a small pseudocount.
#' - transformation = "compositional" converts the data into proportions, from 0 to 1.
#' - transformation = "binary" can be used to transform tax abundances into presence/abundance data.
#'
#' By default, otu_table values of 0 are kept as 0, and all positive values are converted to 1.
#' You can set a different threshold, by passing e.g. undetected = 10, for example, in which case all abundances of 10 or below would be converted to 0s.
#' All abundances above 10 would be converted to 1s. Use any numeric value.
#'
#' @param data ps_extra list output from tax_agg, or a phyloseq object
#' @param transformation any valid taxa transformation from microbiome::transform
#' @param rank If data is phyloseq: data are aggregated at this rank before transforming. If NA, runs tax_agg(data, rank = NA). However if rank is NA and data is already ps_extra, this does nothing.
#' @param ... any extra arguments passed to microbiome::transform() or pass undetected = a number when using transformation = "binary"
#'
#' @return ps_extra list including phyloseq object and info
#' @export
#' @seealso \code{microbiome::\link[microbiome]{transform}} for info on other transformations available with tax_transform
#' @seealso \code{\link{tax_agg}}
#' @examples
#' library(microbiome)
#' data("dietswap", package = "microbiome")
#'
#' # aggregate taxa at Phylum level and perform the center log ratio transform on the phyla counts
#' tax_transform(dietswap, transformation = "clr", rank = "Phylum")
#' # equivalent to old, two-step method
#' tax_agg(dietswap, rank = "Phylum") %>% tax_transform("clr")
#'
#' # do nothing except record tax_agg as "unique" and tax_transform as "identity" in ps_extra info
#' dietswap %>% tax_transform("identity", rank = NA)
#'
#' # binary transformation (convert abundances to presence/absence or detected/undetected)
#' tax_transform(dietswap, transformation = "binary")
#' # change detection threshold by setting undetected argument (default is 0)
#' tax_transform(dietswap, transformation = "binary", undetected = 50) %>%
#'   otu_get() %>%
#'   .[1:6, 1:4]
tax_transform <- function(data, transformation, rank = NA, ...) {

  # check input data object class and aggregate and set ps_extra info
  if (inherits(data, "ps_extra")) {
    if (!identical(rank, NA)) {
      data <- tax_agg(ps = ps_get(data), rank = rank)
    }
    ps <- ps_get(data)
    info <- info_get(data)
    if (!is.na(info[["tax_transform"]])) {
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

  # perform special binary transformation if requested
  if (identical(transformation, "binary")) {
    dots <- list(...)
    if ("undetected" %in% names(dots)) {
      undetected <- dots[["undetected"]]
    } else {
      undetected <- 0
    }
    otu <- unclass(otu_get(ps))
    otu <- otu > undetected
    storage.mode(otu) <- "double"
    # return otu table in original orientation
    tax_rows <- phyloseq::taxa_are_rows(ps)
    if (tax_rows) otu <- t(otu)
    phyloseq::otu_table(ps) <- phyloseq::otu_table(
      object = otu, taxa_are_rows = tax_rows
    )
  } else {
    # transform phyloseq with microbiome::transform
    ps <- microbiome::transform(
      x = ps, transform = transformation, target = "OTU", ...
    )
  }
  data <- new_ps_extra(ps = ps, info = info)
  return(data)
}
