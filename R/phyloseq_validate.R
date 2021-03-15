#' Check for (and fix) common problems with phyloseq objects
#'
#' - Ensures the storage mode of a phyloseq object's otu table is "double" instead of e.g. "integer".
#' - Checks for, and messages about, common uninformative entries in the tax_table, which often cause unwanted results
#' - Replaces missing sample_data with a dataframe including only sample_names (as "SAMPLE" variable)
#' - Removes taxa where phyloseq::taxa_sums() is equal to zero, with a warning, if remove_undetected = TRUE
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
#' # fix storage.mode, replace NULL sample_data, and
#' # remove taxa that sum to 0 across all samples (if remove_undetected = TRUE)
#' phyloseq_validate(dietswap, verbose = FALSE)
#'
#' # Sometimes you might have a phyloseq with no sample_data
#' # This isn't compatible with some microViz functions, like comp_barplot
#' # So some functions internally use phyloseq_validate to fix this
#' dietswap@sam_data <- NULL
#' phyloseq_validate(dietswap)
#'
#' # If no messages or warnings are emitted,
#' # this means no problems were detected, and nothing was changed
#' # (but only if verbose = TRUE)
#'
phyloseq_validate <- function(ps,
                              remove_undetected = FALSE,
                              min_tax_length = 4,
                              verbose = TRUE) {
  silencing_advice <-
    "try `ps <- phyloseq_validate(ps, verbose = FALSE)` to avoid this message"

  # check and fix storage mode
  if (storage.mode(phyloseq::otu_table(ps)) != "double") {
    if (verbose) {
      message(
        "Note: changing OTU table's storage.mode to 'double',\n",
        silencing_advice
      )
    }
    storage.mode(phyloseq::otu_table(ps)) <- "double"
  }

  # check for NULL sample data
  if (identical(phyloseq::access(ps, "sam_data"), NULL)) {
    message(
      "Note: Replacing missing sample_data with a dataframe ",
      "of only sample_names,\n", silencing_advice
    )
    samples <- phyloseq::sample_names(ps)
    phyloseq::sample_data(ps) <-
      data.frame(SAMPLE = samples, row.names = samples)
  }

  if (isTRUE(remove_undetected)) {
    # check for taxa with no counts at all
    # (or other entries summing to exactly zero, which must be suspicious?)
    tax_sums <- phyloseq::taxa_sums(ps)
    zero_sums <- tax_sums == 0
    if (any(zero_sums, na.rm = TRUE)) {
      if (verbose) {
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

  if (verbose) {
    # check tax_table for uninformative entries
    suspicious_names <- c("unknown", "Unknown")
    # include shorter names if min_tax_length means nchar won't catch them
    if (min_tax_length <= 1) suspicious_names <- c(suspicious_names, " ", "")
    if (min_tax_length <= 2) suspicious_names <- c(suspicious_names, "NA")
    if (min_tax_length <= 3) {
      rank_letters <- c("k", "p", "c", "o", "f", "g", "s")
      rank_letters <- c(rank_letters, toupper(rank_letters))
      suspicious_names <-
        c(suspicious_names, paste0(rank_letters, "__"), "NaN")
    }
    taxfillmessage <-
      "Consider using tax_fill_unknowns() to make taxa uniquely identifiable"
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
