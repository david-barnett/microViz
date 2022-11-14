#' Filter rare and/or low abundance taxa from a phyloseq object
#'
#' Removes taxa (from all samples) that do not meet a given criterion or combination of criteria.
#' If a value for min_prevalence, min_total_abundance or min_sample_abundance is 1 or greater, then
#' it is treated as an absolute minimum number of samples/reads. If <1, it is treated as proportion of all samples/reads.
#' This function is designed to work with counts. otu_table must contain counts particularly if you want to set a non-zero value for min_total_abundance.
#'
#' @param ps phyloseq or psExtra (ideally with count data available)
#' @param min_prevalence
#' number or proportion of samples that a taxon must be present in
#' @param prev_detection_threshold
#' min required counts (or value) for a taxon to be considered present
#' in that sample (or set undetected arg)
#' @param min_total_abundance
#' minimum total readcount of a taxon, summed across all samples
#'  (can be proportion of all counts)
#' @param min_sample_abundance
#' taxa must have at least this many reads in one or more samples
#' (or proportion of that sample's reads)
#' @param tax_level
#' if given, aggregates data at named taxonomic rank before filtering,
#' but returns phyloseq at the ORIGINAL level of aggregation!
#' @param names_only
#' if names_only is true return only names of taxa, not the phyloseq
#' @param use_counts
#' expect count data in phyloseq otu_table? default is TRUE
#' @param undetected
#' e.g. 0, value at (or below) which a taxon is considered not present in that sample.
#' If set, this overrides prev_detection_threshold.
#' @param verbose message about proportional prevalence calculations?
#'
#' @return
#' filtered phyloseq object AT ORIGINAL LEVEL OF AGGREGATION
#' (not at the level in tax_level)
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' data("dietswap", package = "microbiome")
#' # Dropping rare and low abundance taxa #
#' # Filter at unique taxa level, keeping only those with a prevalence of 10% or more
#' # and at least 10 thousand reads when summed across all samples.
#' # Then aggregate to Family level taxonomy.
#' dietswap %>%
#'   tax_filter(min_prevalence = 0.1, min_total_abundance = 10000) %>%
#'   tax_agg("Family")
#'
#' # Keeping ubiquitous families #
#' # keep only families that have at least 1000 counts present in 90% of samples
#' # then aggregate the remaining taxa at 'Genus' level
#' dietswap %>%
#'   tax_filter(
#'     tax_level = "Family", min_prevalence = 0.90,
#'     prev_detection_threshold = 1000
#'   ) %>%
#'   tax_agg("Genus")
tax_filter <- function(ps,
                       min_prevalence = 1,
                       prev_detection_threshold = 1,
                       min_total_abundance = 0,
                       min_sample_abundance = 0,
                       tax_level = NA,
                       names_only = FALSE,
                       use_counts = TRUE,
                       undetected = NULL,
                       verbose = TRUE) {
  stopif_ps_extra(ps, argName = "ps")
  # save original data
  input <- ps

  # get counts, check for proportional data
  if (isTRUE(use_counts)) {
    ps <- ps_counts(input, warn = "error")
  } else {
    ps <- ps_get(input)
  }

  # preserve original tax table
  original_taxtab <-
    data.frame(tt_get(input), check.names = FALSE, stringsAsFactors = FALSE)

  # convert min prevalence to an absolute number
  # (if given as a proportion i.e. <1)
  if (min_prevalence < 1) {
    nsamp <- phyloseq::nsamples(ps)
    mp_prop <- min_prevalence
    min_prevalence <- base::ceiling(min_prevalence * nsamp)
    if (!isFALSE(verbose)) {
      message(
        "Proportional min_prevalence given: ", mp_prop,
        " --> min ", min_prevalence, "/", nsamp, " samples."
      )
    }
  }

  # convert min total abundance to an absolute number
  # (if given as a proportion of total reads i.e. <1)
  if (min_total_abundance < 1 && min_total_abundance > 0) {
    counts <- sum(phyloseq::sample_sums(ps))
    mtotAb_prop <- min_total_abundance
    min_total_abundance <- ceiling(min_total_abundance * counts)
    if (!isFALSE(verbose)) {
      message(
        "Proportional min_total_abundance given: ", mtotAb_prop,
        " --> min ", min_total_abundance, "/", counts, " reads."
      )
    }
  }

  # aggregate ps object for computation if requested at given taxonomic level
  if (is.null(tax_level) || is.na(tax_level) || tax_level %in% c("none", "asv", "given")) {
    # aggregation NOT requested
    tax_level <- "taxon" # unique
  } else if (tax_level %in% phyloseq::rank_names(ps)) {
    # perform requested taxonomic aggregation
    ps <- tax_agg(ps = ps, rank = tax_level) %>% ps_get()
  } else {
    stop("invalid tax_level: ", tax_level)
  }

  # get otu table with taxa as rows
  otu <- unclass(phyloseq::otu_table(ps))
  if (!phyloseq::taxa_are_rows(ps)) otu <- t(otu)

  # tax ranks table
  taxtab <- data.frame(phyloseq::tax_table(ps), check.names = FALSE, stringsAsFactors = FALSE)

  # alternative way of specifying prev_detection_threshold.
  if (!identical(undetected, NULL)) {
    prev_detection_threshold <- undetected + 1e-100
  }

  # calculate taxonwise stats
  tax_info <- data.frame(
    taxon = phyloseq::taxa_names(ps),
    prevalence = microbiome::prevalence(
      x = ps, count = TRUE, include.lowest = TRUE,
      detection = prev_detection_threshold
    ),
    total_counts = phyloseq::taxa_sums(ps),
    max_abundance = apply(otu, MARGIN = 1, FUN = max),
    stringsAsFactors = FALSE
  )
  tax_info <- cbind(tax_info, taxtab)

  # get only names of taxa that meet minimum criteria
  taxaMeetingThreshold <- dplyr::filter(
    tax_info,
    .data$prevalence >= min_prevalence,
    .data$total_counts >= min_total_abundance,
    .data$max_abundance >= min_sample_abundance
  )[[tax_level]]

  # stop if no taxa are selected
  if (length(taxaMeetingThreshold) == 0) {
    stop("All taxa filtered out!")
  }

  # filter original input taxonomic table
  # (hence not discarding lower rank classifications)
  if (identical(tax_level, "taxon")) {
    tax_selection_vec <- rownames(original_taxtab) %in% taxaMeetingThreshold
  } else {
    tax_selection_vec <- original_taxtab[, tax_level] %in% taxaMeetingThreshold
  }

  # if names_only is true (default = false) return only (row)names of taxa
  if (names_only) {
    return(rownames(original_taxtab)[tax_selection_vec])
  }

  psOut <- ps_get(input)
  psOut <- phyloseq::prune_taxa(tax_selection_vec, x = psOut)
  if (!is(input, "psExtra")) {
    return(psOut)
  }

  out <- modify_psExtra(psExtra = input, ps = psOut)
  if (isTRUE(use_counts)) { # TODO won't be necessary if modify_psExtra propagates changes itself!
    out <- modify_psExtra(psExtra = out, counts = phyloseq::prune_taxa(
      taxa = tax_selection_vec, x = out@counts
    ))
  }
  return(out)
}
