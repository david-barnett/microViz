#' Filter rare taxa from a phyloseq object
#'
#' Takes a phyloseq object and filtering criteria arguments
#'
#' Removes taxa (from all samples) that do not meet a given criterion or combination of criteria.
#' If a value is 1 or greater, it is treatd as an absolute minimum number of samples/reads. If <1, it is treated as proportion of all samples/reads.
#'
#' @param ps phyloseq object with unfiltered COUNTS
#' @param min_prevalence number or proportion of samples that a taxon must be present in
#' @param prev_detection_threshold min counts for a taxon to be considered present in that sample
#' @param min_total_abundance minimum total readcount of a taxon, summed across all samples (can be proportion of all counts)
#' @param min_sample_abundance if 1 or greater, treat as absolute min abundance, if <1 treat as proportion of sample counts
#' @param tax_level if given, aggregates data at named taxonomic rank before filtering, but returns phyloseq at the ORIGINAL level of aggregation!
#' @param names_only if names_only is true return only names of taxa, not the phyloseq
#'
#' @return filtered phyloseq object AT ORIGINAL LEVEL OF AGGREGATION (not at the level in tax_level)
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
#'   tax_filter(tax_level = "Family", min_prevalence = 0.90, prev_detection_threshold = 1000) %>%
#'   tax_agg("Genus")
tax_filter <- function(
                       ps = ps,
                       min_prevalence = 1,
                       prev_detection_threshold = 1,
                       min_total_abundance = 1,
                       min_sample_abundance = 1,
                       tax_level = NA,
                       names_only = FALSE) {

  # preserve original phyloseq
  ps1 <- ps
  # preserve original tax table
  original_taxtab <- as.data.frame(phyloseq::tax_table(ps))

  # check for proportional arguments
  # convert min prevalence to an absolute number (if given as a proportion i.e. <1)
  if (min_prevalence < 1) {
    nsamp <- phyloseq::nsamples(ps)
    mp_prop <- min_prevalence
    min_prevalence <- base::ceiling(min_prevalence * nsamp)
    message("Proportional min_prevalence given: ", mp_prop, " --> min ", min_prevalence, "/", nsamp, " samples.")
  }
  # convert min total abundance to an absolute number (if given as a proportion of total reads i.e. <1)
  if (min_total_abundance < 1) {
    counts <- sum(microbiome::readcount(ps))
    mtotAb_prop <- min_total_abundance
    min_total_abundance <- ceiling(min_total_abundance * counts)
    message("Proportional min_total_abundance given: ", mtotAb_prop, " --> min ", min_total_abundance, "/", counts, " reads.")
  }

  # aggregate ps object for computation if requested at given taxonomic level
  if (is.null(tax_level) || is.na(tax_level) || tax_level %in% c("none", "asv", "given")) {
    # aggregation NOT requested
    tax_level <- "taxon" # unique
  } else if (tax_level %in% phyloseq::rank_names(ps)) {
    # perform requested taxonomic aggregation
    ps <- microbiome::aggregate_taxa(ps, level = tax_level)
  } else {
    stop("invalid tax_level: ", tax_level)
  }

  # otu table
  otu <- microbiome::abundances(ps)
  # tax ranks table
  taxtab <- as.data.frame(phyloseq::tax_table(ps))

  # calculate taxonwise stats
  tax_info <- data.frame(
    taxon = microbiome::taxa(ps),
    prevalence = microbiome::prevalence(ps, count = T, include.lowest = TRUE, detection = prev_detection_threshold),
    total_counts = phyloseq::taxa_sums(ps),
    max_abundance = apply(otu, 1, max)
  )
  tax_info <- cbind(tax_info, taxtab)

  # get only names of taxa that meet minimum criteria
  taxaMeetingThreshold <- dplyr::filter(
    tax_info,
    .data$prevalence >= min_prevalence,
    .data$total_counts >= min_total_abundance,
    .data$max_abundance >= min_sample_abundance
  )[[tax_level]]

  # throw warning and return NA if no taxa are selected
  if (length(taxaMeetingThreshold) == 0) {
    stop("All taxa filtered out!")
  }

  # filter original input taxonomic table (hence not discarding lower rank classifications)
  if (tax_level == "taxon") {
    tax_selection_vec <- rownames(original_taxtab) %in% taxaMeetingThreshold
  } else {
    tax_selection_vec <- original_taxtab[, tax_level] %in% taxaMeetingThreshold
  }

  # if names_only is true (default = false) return only (row)names of taxa
  if (names_only) {
    return(rownames(original_taxtab)[tax_selection_vec])
  } else {
    # subset taxa in phyloseq object: ps1 is original un-aggregated phyloseq object
    return(phyloseq::prune_taxa(tax_selection_vec, x = ps1))
  }
}
