#' Filter phyloseq samples by sample_data variables
#'
#' Keep only samples with sample_data matching one or more conditions.
#' Use this function as you would use use dplyr::filter(), but with a phyloseq object!
#'
#' @param ps phyloseq object
#' @param ...
#' passed directly to dplyr::filter (see examples and ?dplyr::filter)
#' @param .target which slot of phyloseq to use for filtering by,
#' currently only "sample_data" supported
#' @param .keep_all_taxa if FALSE (the default),
#' remove taxa which are no longer present in the dataset after filtering
#'
#' @return phyloseq object (with filtered sample_data)
#' @export
#'
#' @seealso \code{\link[dplyr]{filter}} explains better how to give arguments to this function
#' @seealso \code{\link{tax_filter}} for filtering taxa (not samples)
#'
#' @examples
#' library(phyloseq)
#' library(microbiome)
#' library(dplyr)
#'
#' data("enterotype")
#' enterotype
#' sample_data(enterotype)[1:10, 1:5]
#'
#' # keep only samples with seqtech not equal to sanger
#' ps1 <- ps_filter(enterotype, SeqTech != "Sanger")
#' ps1
#' sample_data(ps1)[1:10, 1:5]
#'
#' # keep only samples with no NAs in any samples
#' ps2 <- enterotype %>% ps_filter(across(everything(), ~ !is.na(.)))
#' ps2
#' sample_data(ps2)[1:8, 1:8]
#'
#' # ps2 is equivalent to dropping samples with incomplete sample_variables and tax_filtering 0s
#' ps3 <- enterotype %>%
#'   ps_drop_incomplete() %>%
#'   tax_filter(prev_detection_threshold = 1e-20, is_counts = FALSE)
#' # we needed to set a low detection threshold because this example data is proportions
#' identical(ps2, ps3) # TRUE
#'
#' # function will give warning if some of the otu_values are negative
#' # (which may happen when filtering data that has e.g. clr-transformed taxa abundances)
#' # as it attempts to discard any taxa that become always absent/0 after filtering (by default)
#' # set .keep_all_taxa = TRUE to avoid this filtering behaviour, which is unwanted in this case
#' enterotype %>%
#'   microbiome::transform("clr") %>%
#'   ps_filter(SeqTech == "Sanger", .keep_all_taxa = TRUE)
ps_filter <- function(ps,
                      ...,
                      .target = "sample_data",
                      .keep_all_taxa = FALSE) {
  if (!inherits(ps, "phyloseq")) {
    stop("ps must be a phyloseq object. It is of class: ", class(ps))
  }

  if (!identical(.target, "sample_data")) {
    stop("Only .target = 'sample_data', has been implemented so far.")
  }
  # TODO: see if it is useful to facilitate
  # filtering by variables in other phyloseq slots

  df <- samdatAsDataframe(ps)
  df <- dplyr::filter(df, ...)
  phyloseq::sample_data(ps) <- df

  # remove taxa that now have zero counts (or relative abundance)
  # across all remaining samples
  if (isFALSE(.keep_all_taxa)) ps <- tax_filter_zeros(ps)
  return(ps)
}

# helper function used here and in ps_join
# removes all taxa which sum to zero across all samples
# (phyloseq::taxa_sums(ps) == 0)
# provides helpful warning if otu_table contains negative values
tax_filter_zeros <- function(ps) {
  # remove taxa that now have zero counts (or relative abundance)
  # across all remaining samples
  if (any(phyloseq::otu_table(ps) < 0)) {
    warning(
      "Removing taxa whose abundance across filtered samples is equal to zero.",
      "\nThis may not result in the desired outcome, ",
      "as some values in the otu_table are negative.",
      "\nAvoid performing transformations, ",
      "e.g. clr, before using `ps_filter()`, or set .keep_all_taxa = TRUE "
    )
  }
  return(phyloseq::prune_taxa(taxa = phyloseq::taxa_sums(ps) != 0, x = ps))
}
