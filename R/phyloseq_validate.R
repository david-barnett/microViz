#' Check for (and fix) common problems with phyloseq objects
#'
#' - Ensures the storage mode of a phyloseq object's otu table is "double" instead of e.g. "integer".
#' - Checks for, and warns about, common uninformative entries in the tax_table which often cause unwanted results in.
#' - Replaces missing sample_data with a dataframe including only sample_names (as "SAMPLE" variable)
#'
#' @param ps phyloseq object
#' @param min_tax_length minimum number of characters to not consider a tax_table entry suspiciously short
#' @param verbose print informative messages if true
#'
#' @return possibly modified phyloseq object
#' @export
#'
#' @examples
#' data(dietswap, package = "microbiome")
#'
#' phyloseq_validate(dietswap, verbose = TRUE)
#' # no messages means no problems detected
phyloseq_validate <- function(ps, min_tax_length = 4, verbose = TRUE) {
  silencing_advice <- "try `ps <- phyloseq_validate(ps, verbose = FALSE)` to avoid this message"

  # check and fix storage mode
  if (storage.mode(phyloseq::otu_table(ps)) != "double") {
    if (verbose) {
      message("Note: changing OTU table's storage.mode to 'double',\n", silencing_advice)
    }
    storage.mode(phyloseq::otu_table(ps)) <- "double"
  }

  # check for NULL sample data
  if (identical(phyloseq::access(ps, "sam_data"), NULL)) {
    message("Note: Replacing missing sample_data with a dataframe including only sample_names,\n", silencing_advice)
    phyloseq::sample_data(ps) <- data.frame(SAMPLE = phyloseq::sample_names(ps))
  }

  # check tax_table for uninformative entries
  suspicious_names <- c("g__", "f__", "unknown", "Unknown", "", " ", "NA")
  taxfillmessage <- "Consider using tax_fill_unknowns() to help make taxa uniquely identifiable"
  if (anyNA(phyloseq::tax_table(ps))) {
    message("NAs detected in phyloseq tax_table:\n", taxfillmessage)
  } else if (any(nchar(phyloseq::tax_table(ps)) < min_tax_length)) {
    message("Short values detected in phyloseq tax_table (nchar<", min_tax_length, ") :\n", taxfillmessage)
  } else if (any(phyloseq::tax_table(ps) %in% suspicious_names)) {
    message("Suspicious values detected in phyloseq tax_table:\n", taxfillmessage)
    message("Detected: '", paste(unique(unclass(phyloseq::tax_table(ps))[phyloseq::tax_table(ps) %in% suspicious_names]), collapse = "', '"), "'\n")
  }

  return(ps)
}
