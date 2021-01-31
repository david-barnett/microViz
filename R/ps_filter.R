#' Filter phyloseq samples by sample_data variables
#'
#' Wrapper for dplyr::filter function. All dplyr functionality
#'
#' @param ps phyloseq object
#' @param ... passed directly to dplyr::filter (see examples and ?dplyr::filter)
#' @param .target which slot of phyloseq to mutate, currently only "sample_data" supported
#' @param .keep_all_taxa if FALSE (the default) taxa which are no longer present in the dataset after filtering are removed
#'
#' @return phyloseq object (with filtered sample_data)
#' @export
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
#' ps1 <- ps_filter(enterotype, SeqTech != "Sanger")
#' ps1
#' sample_data(ps1)[1:10, 1:5]
#'
#' ps2 <- enterotype %>% ps_filter(across(everything(), ~ !is.na(.)))
#' ps2
#' sample_data(ps2)[1:8, 1:8]
#'
#' # function will give warning if some of the otu_values are negative
#' # unless you set .keep_all_taxa = TRUE
#' enterotype %>%
#'   microbiome::transform("clr") %>%
#'   ps_filter(SeqTech == "Sanger", .keep_all_taxa = TRUE)
ps_filter <- function(ps, ..., .target = "sample_data", .keep_all_taxa = FALSE) {
  if (!inherits(ps, "phyloseq")) {
    stop("ps must be a phyloseq object. It is of class: ", class(ps))
  }

  if (!identical(.target, "sample_data")) {
    stop("Only .target = 'sample_data', has been implemented so far.")
  }
  # TODO: see if it is useful to facilitate filtering by variables in other phyloseq slots

  df <- data.frame(phyloseq::sample_data(ps))
  df <- dplyr::filter(df, ...)
  phyloseq::sample_data(ps) <- df

  if (isFALSE(.keep_all_taxa)) {
    # remove taxa that now have zero counts (or relative abundance) across all remaining samples
    if (any(phyloseq::otu_table(ps) < 0)) {
      warning(
        "Removing taxa whose abundance across filtered samples is equal to zero.",
        "\nThis may not result in the desired outcome, as some values in the otu_table are negative.",
        "\nAvoid performing transformations e.g. clr before using this function, or set .keep_all_taxa = TRUE "
      )
    }
    ps <- phyloseq::prune_taxa(taxa = phyloseq::taxa_sums(ps) != 0, x = ps)
  }

  return(ps)
}