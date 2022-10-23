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
#' `tax_transform` returns a `psExtra` containing the transformed phyloseq object and
#' extra info (used for annotating `ord_plot` ordinations):
#'
#' - tax_transform (a string recording the transformation),
#' - tax_agg (a string recording the taxonomic aggregation rank if specified here or earlier in `tax_agg`).
#'
#' A few commonly used transformations:
#'
#' - "clr", or "rclr", perform the centered log ratio transformation, or the robust clr, using `microbiome::transform`
#' - "compositional" converts the data into proportions, from 0 to 1.
#' - "identity" does not transform the data, and records this choice for `ord_plot`
#' - "binary" can be used to transform tax abundances into presence/abundance data.
#' - "log2" which performs a log base 2 transformation
#' (don't forget to set zero_replace if there are any zeros in your data)
#'
#' @section (r)clr transformation note:
#'
#' If any values are zero, the clr transform routine first adds a small
#' pseudocount of min(relative abundance)/2 to all values. To avoid this, you
#' can replace any zeros in advance by setting zero_replace to a number > 0.
#'
#' The rclr transform does not replace zeros. Instead, only non-zero features
#' are transformed, using the geometric mean of non-zero features as denominator.
#'
#' @section Binary transformation notes:
#'
#' By default, otu_table values of 0 are kept as 0, and all positive values
#' are converted to 1 (like `decostand(method = "pa")`).
#' You can set a different threshold, by passing e.g. undetected = 10, for
#' example, in which case all abundances of 10 or below would be converted to 0.
#' All abundances above 10 would be converted to 1s.
#'
#' Beware that the choice of detection threshold is not tracked in the psExtra.
#'
#' @param data a phyloseq object or psExtra output from `tax_agg`
#' @param trans
#' any valid taxa transformation (e.g. from `microbiome::transform`)
#' @param rank
#' If data is phyloseq: data are aggregated at this rank before transforming.
#' If NA, runs tax_agg(data, rank = NA).
#' If rank is NA and data is already psExtra, any preceding aggregation is left as is.
#' @param keep_counts
#' if TRUE, store the pre-transformation count data in psExtra counts slot
#' @param transformation deprecated, use `trans` instead!
#' @param ...
#' any extra arguments passed to `microbiome::transform` or pass
#' undetected = `a number` when using trans = "binary"
#' @param chain
#' if TRUE, transforming again is possible when data are already transformed
#' i.e. multiple transformations can be chained with multiple tax_transform calls
#' @param zero_replace
#' Replace any zeros with this value before transforming. Either a numeric, or
#' "halfmin" which replaces zeros with half of the smallest value across the
#' entire dataset.
#' Beware: the choice of zero replacement is not tracked in the psExtra output.
#' @param add
#' Add this value to the otu_table before transforming. If `add` != 0,
#' `zero_replace` does nothing. Either a numeric, or "halfmin".
#' Beware: this choice is not tracked in the psExtra output.
#'
#' @return `psExtra` object including phyloseq and extra info
#' @export
#' @seealso \code{microbiome::\link[microbiome]{transform}} for some more info on available transformations
#' @seealso \code{vegan::\link[vegan]{decostand}} for even more transformation options
#' @seealso \code{\link{tax_agg}}
#'
#' @examples
#' data("dietswap", package = "microbiome")
#'
#' # aggregate taxa at Phylum level and center log ratio transform the phyla counts
#' tax_transform(dietswap, trans = "clr", rank = "Phylum")
#'
#' # this is equivalent to the two-step method (agg then transform)
#' tax_agg(dietswap, rank = "Phylum") %>% tax_transform("clr")
#'
#' # does nothing except record tax_agg as "unique" and tax_transform as "identity" in psExtra info
#' dietswap %>% tax_transform("identity", rank = NA)
#'
#' # binary transformation (convert abundances to presence/absence or detected/undetected)
#' tax_transform(dietswap, trans = "binary", rank = "Genus")
#' # change detection threshold by setting undetected argument (default is 0)
#' tax_transform(dietswap, trans = "binary", rank = "Genus", undetected = 50) %>%
#'   otu_get() %>%
#'   .[1:6, 1:4]
#'
#' # log2 transformation after replacing all zeros with a pseudocount of 1
#' tax_transform(dietswap, trans = "log2", rank = "Family", zero_replace = 1)
#'
#' # log2 transformation after replacing all zeros with a pseudocount of half
#' # the minimum non-zero count value in the aggregated dataset
#' tax_transform(dietswap, trans = "log2", rank = "Family", zero_replace = "halfmin")
tax_transform <- function(data,
                          trans,
                          rank = NA,
                          keep_counts = TRUE,
                          chain = FALSE,
                          zero_replace = 0,
                          add = 0,
                          transformation = NULL,
                          ...) {
  if (!identical(transformation, NULL)) {
    warning("`transformation` argument deprecated, use `trans` instead.")
    trans <- transformation
  }
  if (!rlang::is_na(rank) && !rlang::is_string(rank)) {
    stop("`rank` must be NA or a character string")
  }
  if (!rlang::is_string(trans)) stop("`trans` must be a character string")
  if (!rlang::is_scalar_double(add)) stop("`add` must be a double, length 1")
  if (!rlang::is_scalar_double(zero_replace)) {
    if (!identical(zero_replace, "halfmin")) {
      stop("`zero_replace` must be a number, or 'halfmin'")
    }
  }
  stopifnot(rlang::is_bool(keep_counts))
  stopifnot(rlang::is_bool(chain))

  # check input data object class, validate options, and record psExtra info
  check_is_phyloseq(data, argName = "data")

  if (is(data, "psExtra")) {
    info <- tax_transformInfoUpdate(
      info = info_get(data), trans = trans, rank = rank, chain = chain
    )
  } else {
    if (rlang::is_na(rank)) rank <- "unique"
    info <- new_psExtraInfo(tax_trans = trans, tax_agg = rank)
  }

  # aggregate data if rank now available
  if (!is.na(rank)) data <- tax_agg(ps = ps_get(data), rank = rank)

  # store otu table prior to transformation (for if keep_counts == TRUE)
  counts_otu <- otu_get(ps_counts(data = data))

  # get plain phyloseq from aggregated psExtra data
  ps <- ps_get(data)
  # extract otu
  otu <- otu_get(ps)

  # add constant to all otu table values
  otu <- otuAddConstant(otu, add = add)

  # add pseudocount to zeros if desired
  otu <- otuZeroReplace(otu, zero_replace = zero_replace)

  # perform one of several transformations #
  otu <- otuTransform(otu = otu, trans = trans, ...)

  # return otu table in original orientation
  if (phyloseq::taxa_are_rows(ps)) otu <- phyloseq::t(otu)
  phyloseq::otu_table(ps) <- phyloseq::otu_table(
    object = otu, taxa_are_rows = phyloseq::taxa_are_rows(ps)
  )

  # assemble psExtra for return
  data <- psExtra(ps = ps, info = info)
  if (isTRUE(keep_counts) && !identical(trans, "identity")) {
    data@counts <- counts_otu
  }
  return(data)
}

# internal helper: checks and updates psExtra info
tax_transformInfoUpdate <- function(info, trans, chain, rank) {
  # check if already transformed
  if (length(info$tax_trans) == 0) {
    # log name of (1st) transformation in psExtra info
    info$tax_trans <- trans
  } else {
    if (!isTRUE(chain)) {
      # disallow further transformation by default
      rlang::abort(call = rlang::caller_env(), message = c(
        paste("data were already transformed by:", info$tax_trans),
        ">" = "set argument chain = TRUE if you want to chain another transform"
      ))
    }
    if (!identical(rank, NA) && !identical(rank, "unique")) {
      rlang::abort(call = rlang::caller_env(), message = c(
        "rank must be NA or 'unique' when chaining another transformation!"
      ))
    }
    # append name of this transformation onto psExtra info
    info$tax_trans <- paste(info$tax_trans, trans, sep = "&")
  }
  return(info)
}

# internal helper actually performs transformations on otu table as
# returned by otu_get() (taxa as columns!)
# trans is same as tax_transform trans
otuTransform <- function(otu, trans, ...) {
  # perform one of several transformations #
  if (identical(trans, "binary")) { # perform special binary transformation
    dots <- list(...)
    # retrieve or create "undetected" argument
    if (!"undetected" %in% names(dots)) dots[["undetected"]] <- 0
    otu <- otuTransformBinary(otu, undetected = dots[["undetected"]])
  } else if (identical(trans, "log2")) { # perform log2 transformation
    if (any(otu == 0)) {
      stop(
        "\n- ", sum(otu == 0, na.rm = TRUE), " zeros detected in otu_table",
        "\n- log2 transformation cannot handle zeros\n",
        "- set zero_replace to > 0, to replace zeros before transformation"
      )
    }
    otu <- log2(otu)
  } else {
    # transform phyloseq with microbiome::transform
    otu <- phyloseq::t(otu)
    otu <- microbiome::transform(otu, transform = trans, target = "OTU", ...)
    otu <- phyloseq::t(otu) # microbiome::transform uses/returns taxa as rows
  }
  return(otu)
}


# binary transformation helper
# otu is from otu_get(ps)
otuTransformBinary <- function(otu, undetected = 0) {
  # get and transform otu_table
  otu <- unclass(otu)
  otu <- otu > undetected
  # cast from logical to double
  storage.mode(otu) <- "double"
  return(otu)
}

# otu table zero replacement helper
otuZeroReplace <- function(otu, zero_replace) {
  if (!identical(zero_replace, "halfmin") && !is.numeric(zero_replace)) {
    stop(call. = FALSE, "zero_replace argument must be 'halfmin' or a number")
  }

  # calculate zero replacement number if halfmin option given
  if (identical(zero_replace, "halfmin")) zero_replace <- otuHalfMin(otu)

  # replace zeros with zero_replace number
  if (!identical(zero_replace, 0)) otu[otu == 0] <- zero_replace
  return(otu)
}

# otu table helper adds a constant value to ALL entries in otuTable
otuAddConstant <- function(otu, add) {
  if (!identical(add, "halfmin") && !is.numeric(add) || length(add) != 1) {
    stop(call. = FALSE, "`add` argument must be 'halfmin' or a number")
  }

  # calculate constant number if halfmin option given
  if (identical(add, "halfmin")) add <- otuHalfMin(otu)

  if (!identical(add, 0)) otu <- otu + add
  return(otu)
}

# otu table helper finds half of the global minimum
otuHalfMin <- function(otu) {
  if (any(otu < 0)) {
    stop("'halfmin' is not valid when some otu_table values are negative")
  }
  otu <- methods::as(otu, "matrix")
  halfmin <- min(otu[otu > 0], na.rm = TRUE) / 2
  return(halfmin)
}
