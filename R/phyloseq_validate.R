#' Check for (and fix) common problems with phyloseq objects
#'
#' - Ensures the storage mode of a phyloseq object's otu table is "double" instead of e.g. "integer".
#' - Checks for, and warns about, common uninformative entries in the tax_table which often cause unwanted results in
#'
#' @param ps phyloseq object
#' @param verbose print informative messages if true
#'
#' @return possibly modified phyloseq object
#' @export
#'
#' @examples
#' data(dietswap, package = 'microbiome')
#'
#' phyloseq_validate(dietswap, verbose = TRUE)
#' # no messages means no problems detected
#'
phyloseq_validate <- function(ps, verbose = TRUE) {

  # check and fix storage mode
  if (storage.mode(phyloseq::otu_table(ps)) != "double") {
    if (verbose) {
      message("Setting OTU table's storage.mode to 'double'")
    }
    storage.mode(phyloseq::otu_table(ps)) <- "double"
  }

  # check tax_table for uninformative entries
  suspicious_names <- c("g__", "f__", "unknown", "Unknown", "", " ", "NA")
  if (anyNA(phyloseq::tax_table(ps))){
    message("NAs detected in phyloseq tax_table:\nConsider using tax_fill_unknowns() to help make taxa uniquely identifiable\n")
  }
  if (any(phyloseq::tax_table(ps) %in% suspicious_names)) {
    message("Suspicious values detected in phyloseq tax_table:\nConsider using tax_fill_unknowns() to help make taxa uniquely identifiable")
    message("Detected: '", paste(unique(unclass(phyloseq::tax_table(ps))[phyloseq::tax_table(ps) %in% suspicious_names]), collapse = "', '"), "'\n")
  }

  return(ps)
}
