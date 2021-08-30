#' Transform taxa in phyloseq object and record transformation
#'
#' @description
#' Transform taxa features, and optionally aggregate at specified taxonomic rank beforehand.
#' You can pipe the results of `tax_agg` into `tax_transform`,
#' or equivalently set the rank argument in `tax_transform`.
#'
#' @details
#' This function often uses `microbiome::transform` internally and can perform the
#' same transformations, including many from `vegan::decostand` (where the default MARGIN = 2).
#' See below for notes about some of the available transformations.
#'
#' `tax_transform` returns a `ps_extra` list containing the transformed phyloseq object and
#' extra info (used for annotating `ord_plot` ordinations):
#'
#' - tax_transform (a string recording the transformation),
#' - tax_agg (a string recording the taxonomic aggregation rank if specified here or earlier in `tax_agg`).
#'
#' A few commonly used transformations:
#'
#' - "clr" performs the centered log ratio transformation using `microbiome::transform`,
#' which adds a small pseudocount of min(relative abundance)/2.
#' - "compositional" converts the data into proportions, from 0 to 1.
#' - "identity" does not transform the data, and records this choice for `ord_plot`
#' - "binary" can be used to transform tax abundances into presence/abundance data.
#'
#' Binary transform notes:
#'
#' By default, otu_table values of 0 are kept as 0, and all positive values are converted to 1 (like `decostand(method = "pa")`).
#' You can set a different threshold, by passing e.g. undetected = 10, for example, in which case all abundances of 10 or below would be converted to 0s.
#' All abundances above 10 would be converted to 1s. Use any numeric value.
#'
#' @param data `ps_extra` list output from `tax_agg`, or a phyloseq object
#' @param transformation any valid taxa transformation (e.g. from `microbiome::transform`)
#' @param rank
#' If data is phyloseq: data are aggregated at this rank before transforming.
#' If NA, runs tax_agg(data, rank = NA).
#' If rank is NA and data is already ps_extra, any preceding aggregation is left as is.
#' @param keep_counts if TRUE, store the pre-transformation count data in ps_extra counts slot
#' @param ... any extra arguments passed to `microbiome::transform` or pass undetected = a number when using transformation = "binary"
#'
#' @return `ps_extra` list including phyloseq object and info
#' @export
#' @seealso \code{microbiome::\link[microbiome]{transform}} for some more info on available transformations
#' @seealso \code{vegan::\link[vegan]{decostand}} for even more transformation options
#' @seealso \code{\link{tax_agg}}
#'
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
tax_transform <- function(data,
                          transformation,
                          rank = NA,
                          keep_counts = TRUE,
                          ...) {

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

  # store otu table prior to transformation (for if keep_counts == TRUE)
  counts_otu <- otu_get(ps)

  # perform special binary transformation if requested
  if (identical(transformation, "binary")) {
    ps <- tax_transformBinary(ps, ...)
  } else {
    # transformations other than the "binary" transform
    # transform phyloseq with microbiome::transform
    ps <- microbiome::transform(
      x = ps, transform = transformation, target = "OTU", ...
    )
  }
  data <- new_ps_extra(ps = ps, info = info)
  if (isTRUE(keep_counts) && !identical(transformation, "identity")) {
    data[["counts"]] <- counts_otu
  }
  return(data)
}

# binary transformation helper
tax_transformBinary <- function(ps, ...){
  # retrieve or create "undetected" argument
  dots <- list(...)
  if ("undetected" %in% names(dots)) {
    undetected <- dots[["undetected"]]
  } else {
    undetected <- 0
  }
  # get and transform otu_table
  otu <- unclass(otu_get(ps))
  otu <- otu > undetected
  # cast from logical to double
  storage.mode(otu) <- "double"
  # return otu table in original orientation
  tax_rows <- phyloseq::taxa_are_rows(ps)
  if (tax_rows) otu <- t(otu)
  phyloseq::otu_table(ps) <- phyloseq::otu_table(
    object = otu, taxa_are_rows = tax_rows
  )
  return(ps)
}

