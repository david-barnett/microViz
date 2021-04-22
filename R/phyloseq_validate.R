# Previous version used to also do this:
# - Ensures the storage mode of a phyloseq object's otu table is "double" instead of e.g. "integer", which fixes some compatibility issues (mostly with microbiome).
# This is not completely deleted yet, in case this bug isn't actually gone

#' Check for (and fix) common problems with phyloseq objects
#'
#' - It checks for, and messages about, common uninformative entries in the tax_table, which often cause unwanted results
#' - If there is no sample_data, it creates a sample_data dataframe with the sample_names (as "SAMPLE" variable)
#' - If there is no tax_table, it creates a 1-column tax_table matrix with the taxa_names, and calls the rank "unique"
#' - If remove_undetected = TRUE, it removes taxa where `phyloseq::taxa_sums()` is equal to zero, with a warning
#'
#' @param ps phyloseq object
#' @param remove_undetected if TRUE, removes taxa that sum to zero across all samples
#' @param min_tax_length minimum number of characters to not consider a tax_table entry suspiciously short
#' @param verbose print informative messages if true
#'
#' @return possibly modified phyloseq object
#' @export
#'
#' @examples
#' data(dietswap, package = "microbiome")
#'
#' # expect warning about taxa summing to zero
#' phyloseq_validate(dietswap, remove_undetected = TRUE, verbose = TRUE)
#'
#' # verbose = FALSE will suppress messages and warnings but still:
#' # replace NULL sample_data and remove taxa that sum to 0 across all samples
#' # (if remove_undetected = TRUE)
#' phyloseq_validate(dietswap, verbose = FALSE)
#'
#' # Sometimes you might have a phyloseq with no sample_data
#' # This isn't compatible with some microViz functions, like comp_barplot
#' # So some functions internally use phyloseq_validate to fix this
#' dietswap@sam_data <- NULL
#' phyloseq_validate(dietswap)
#'
#' # Sometimes you might have a phyloseq with no tax_table
#' # This isn't compatible with some microViz functions, like tax_top,
#' # so this is another reason to start your analyses with phyloseq_validate!
#' data("soilrep", package = "phyloseq")
#' soilrep # has NULL tax_table
#' phyloseq_validate(soilrep)
#'
#' # If no messages or warnings are emitted,
#' # this means no problems were detected, and nothing was changed
#' # (but only if verbose = TRUE)
phyloseq_validate <- function(ps,
                              remove_undetected = FALSE,
                              min_tax_length = 4,
                              verbose = TRUE) {
  silencing_advice <-
    "Try `ps <- phyloseq_validate(ps, verbose = FALSE)` to avoid this message"

  # # check and fix storage mode
  # old_store_mode <- storage.mode(phyloseq::otu_table(ps))
  # if (old_store_mode != "double") {
  #   if (verbose) {
  #     message(
  #       "Note: changing OTU table's storage.mode from '",
  #       old_store_mode, "' to 'double'.\n", silencing_advice
  #     )
  #   }
  #   storage.mode(phyloseq::otu_table(ps)) <- "double"
  # }

  # check for NULL sample data
  if (identical(phyloseq::access(ps, "sam_data"), NULL)) {
    if (isTRUE(verbose)) {
      message(
        "Note: Replacing missing sample_data with a dataframe ",
        "of only sample_names.\n", silencing_advice
      )
    }
    phyloseq::sample_data(ps) <- samdat_init(ps)
  }

  # check for NULL tax_table
  if (identical(phyloseq::access(ps, "tax_table"), NULL)) {
    if (isTRUE(verbose)) {
      message(
        "Note: Replacing missing tax_table with a 1-column table ",
        "of only taxa_names.\n", silencing_advice
      )
    }

    taxons <- phyloseq::taxa_names(ps)
    phyloseq::tax_table(ps) <-
      matrix(data = taxons, ncol = 1, dimnames = list(taxons, "unique"))
  }

  if (isTRUE(remove_undetected)) {
    # check for taxa with no counts at all
    # (or other entries summing to exactly zero, which must be suspicious?)
    tax_sums <- phyloseq::taxa_sums(ps)
    zero_sums <- tax_sums == 0
    if (any(zero_sums, na.rm = TRUE)) {
      if (isTRUE(verbose)) {
        warning(
          "Some taxa_sums were zero, removing the following taxa:\n\t",
          paste(names(tax_sums)[zero_sums], collapse = " \n\t"),
          "\nThis may be caused by using `subset_samples()`.",
          "\nTry using `ps_filter()` instead, with .keep_all_taxa = FALSE.",
          "\nOtherwise, to avoid this warning,",
          " try filtering out taxa summing to zero with `tax_filter()`.",
          "\nIf you have already transformed and/or scaled your taxa, ",
          "e.g. with a log transformation or scale,",
          "\nseeing this warning is possible, but very unlikely ",
          "and possibly a bug. Please report this."
        )
      }
      ps <- phyloseq::prune_taxa(taxa = !zero_sums, x = ps)
    }
  }

  if (isTRUE(verbose)) {
    # check tax_table for uninformative entries
    suspicious_names <- tax_common_unknowns(min_length = min_tax_length)

    taxfillmessage <-
      "Consider using tax_fix() to make taxa uniquely identifiable"
    if (anyNA(phyloseq::tax_table(ps))) {
      message("NAs detected in phyloseq tax_table:\n", taxfillmessage)
    } else if (any(nchar(phyloseq::tax_table(ps)) < min_tax_length)) {
      message(
        "Short values detected in phyloseq tax_table (nchar<",
        min_tax_length, ") :\n", taxfillmessage
      )
    } else if (any(phyloseq::tax_table(ps) %in% suspicious_names)) {
      message(
        "Suspicious values detected in phyloseq tax_table:\n", taxfillmessage
      )
      bad <- unclass(tt_get(ps))[tt_get(ps) %in% suspicious_names]
      message("Detected: '", paste(unique(bad), collapse = "', '"), "'\n")
    }
  }
  return(ps)
}

# helper function used in phyloseq_validate and in tax_sort
samdat_init <- function(ps) {
  samples <- phyloseq::sample_names(ps)
  samdat <- phyloseq::sample_data(
    data.frame(SAMPLE = samples, row.names = samples, check.names = FALSE)
  )
  return(samdat)
}
